#import the package
library(randomForest)
library(gbm)
library(caret)
# Set random seed to make results reproducible:
set.seed(19)
# Choose a dataset
forestdf <- participant_greenness %>% select("Participant_200m_Mean_ndvi", 
                                             "Participant_300m_Mean_ndvi",
                                             "age", "race.desc", "bmi", "gend.desc", 
                                             "TrafficExposure", "addtm", "dailymeanPM25",
                                             "avg_max_ozone", "Temp_AVG", "Rel_humidity",
                                             "u_3hpma_n", "u_hpmma_n")
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(forestdf)/2)
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(forestdf), size = data_set_size)
# Assign the data to the correct sets
training <- forestdf[indexes,]
validation1 <- forestdf[-indexes,]

#bring in occupation

# training = data.frame(training[training$rentown_parcel=="Own",])
# Perform training:
rf_classifier = randomForest(Participant_200m_Mean_ndvi ~ inc.desc+edu.desc+age+
                               race.desc+gend.desc+NEAR_DIST_MajorRoads+
                               PopDen+hznum+medu.desc, 
                             type="unsupervised",
                             data=training, ntree=300, mtry=8, importance=TRUE)
rf_classifier
# CUR_TOTAL

rf_classifier1 = randomForest(inc.desc ~ Participant_200m_Mean_ndvi+Participant_200m_Mean_LAI+
                                Parcel_ndvi_Mean+hznum+medu.desc+CUR_TOTAL+
                                edu.desc+age+rentown_parcel+
                                race.desc+gend.desc+NEAR_DIST_MajorRoads+PopDen, type = "unsupervised",
                             data=training, ntree=100, mtry=6, importance=TRUE)
rf_classifier1

rf_classifier2 = randomForest(u_3hpma_n ~ Participant_300m_Mean_ndvi+age+
                                race.desc+bmi+gend.desc+TrafficExposure+addtm+
                                dailymeanPM25+avg_max_ozone+Temp_AVG+Rel_humidity, 
                              type = "unsupervised",
                              data=training, ntree=100, mtry=6, importance=TRUE)
rf_classifier2

# -ntree defines the number of trees to be generated. It is typical to test a range of values for this parameter (i.e. 100,200,300,400,500) and choose the one that minimises the OOB estimate of error rate.
# -mtry is the number of features used in the construction of each tree. These features are selected at random, which is where the "random" in "random forests" comes from. The default value for this parameter, when performing classification, is sqrt(number of features).
# -importance enables the algorithm to calculate variable importance.

varImpPlot(rf_classifier)
# -MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance 
# when that particular variable is omitted from the training set. Caveat: if two variables 
# are somewhat redundant, then omitting one of them may not lead to massive gains in 
# prediction performance, but would make the second variable more important.

# -MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this, if you 
# use this feature to split the data, how pure will the nodes be? Highest purity 
# means that each node contains only elements of a single class. Assessing the decrease 
# in GINI when that feature is omitted leads to an understanding of how important that 
# feature is to split the data correctly.



prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)


# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-5],type="response")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(validation1$Species)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

boost=gbm(Parcel_ndvi_Mean ~ inc.desc+edu.desc+age+
                   race.desc+gend.desc+NEAR_DIST_MajorRoads+
                   PopDen+CUR_TOTAL+hznum+medu.desc,
                 data = training, distribution = "gaussian", n.trees = 3000,
                 shrinkage = 0.001, interaction.depth = 4, bag.fraction = 0.9,
                 verbose = TRUE, cv.folds = 6, train.fraction = 0.8)
boost
# get MSE and compute RMSE
sqrt(min(boost$cv.error))
## [1] 0.039

# plot loss function as a result of n trees added to the ensemble
gbm.perf(boost, method = "cv")

hyper_grid <- expand.grid(
  shrinkage = c(.001, 0.01, .1, .3),
  interaction.depth = c(1, 3, 5, 6, 7),
  n.minobsinnode = c(1, 5, 10, 15, 20),
  #bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
## [1] 81

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Sale_Price ~ .,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


summary(boost)

# predict values for test data
pred <- predict(boost, n.trees = boost$n.trees, validation1)

# results
caret::RMSE(pred, validation1$Participant_200m_Mean_ndvi)
## [1] 20681.88