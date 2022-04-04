pg_for_traffic <- filter(participant_greenness, u_pga_n < 10000)
pg_for_traffic_nosmk <- filter(nonsmokers, u_pga_n < 3000 & u_bma_n < 200)
check.model <- lm(u_pga_n ~TrafDensit, data = pg_for_traffic)
check.model <- gam(log(u_bma_n) ~s(TrafficExposure), data = pg_for_traffic_nosmk)
summary(check.model)
gam.check(check.model)
plot(check.model, all.terms=TRUE)
ggplot(pg_for_traffic_nosmk, aes(u_bma_n, RoadlengthtimesTraffic) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))
check.model <- lm(u_pga_n ~TrafDensit, data = participant_greenness)
summary(check.model)
check.model <- lm(u_bma_n ~TrafDensit, data = participant_greenness)
summary(check.model)
check.model <- lm(u_pga_n ~TrafDensit, data = participant_greenness)
summary(check.model)


# Pay attention from roadlengthtimesTraffic or Roadlength WHEN NOT 0