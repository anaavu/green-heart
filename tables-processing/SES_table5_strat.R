#######################################################################################
## Author: Anagha Uppal
## Date: Feb 2021
## Purpose: Table 5 for SES 2nd Paper
## Inputs: 
## Outputs: 
#######################################################################################
## To do: 


fit_w<-glm(mig~edurec+sexrecode+scale(age)+scale(I(age^2)),data=newpums, family=binomial, subset= race_eth=="0nh_white")


stargazer(fit_w,fit_b,fit_h,fit_a,fit_o, type = "html", style = "demography", ci = T,  model.names = T, column.labels = c("white" , "black", "hispanic", "asian", "other"),keep.stat = c("n"))
