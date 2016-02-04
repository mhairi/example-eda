library(car)

model_data <-
dd %>%
  filter(impressions > 100) %>%
  filter(country != '') %>%
  filter(ecpm < 30)

model <- lm(ecpm ~ network + channel + country, data = model_data)

Anova(model)
