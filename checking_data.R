library(ggplot2)
library(dplyr)
library(scales)
library(corrgram)

load('clean_data.RData')

### Revenue over time ###

dd %>%
  filter(impressions > 100) %>%
  sample_frac(0.1) %>%
  mutate(date = as.Date(date)) %>%
ggplot() +
  aes(y = revenue, x = date, colour = network) +
  geom_point() +
  scale_x_date(breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### eCPM over time ###

dd %>%
  filter(impressions > 100) %>%
  filter(ecpm < 30) %>%
  sample_frac(0.1) %>%
  mutate(date = as.Date(date))  %>% 
ggplot() +
  aes(y = ecpm, x = date, colour = network) +
  geom_point() +
  scale_x_date(breaks = date_breaks("months")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Looking for correlations ###

dd %>%
  group_by(network, channel) %>%
  summarise(total = sum(dau),
            n     = n()) %>%
ggplot +
  aes(x = network, y = channel, fill = total) +
  geom_raster()

dd %>%
  filter(country %in% countries$country[1:10]) %>%
  group_by(network, country) %>%
  summarise(total = sum(dau),
            n     = n()) %>%
ggplot +
  aes(x = country, y = network, fill = total) +
  geom_raster() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dd %>%
  filter(country %in% countries$country[1:10]) %>%
  group_by(channel, country) %>%
  summarise(total = sum(dau),
            n     = n()) %>%
  ggplot +
  aes(x = country, y = channel, fill = total) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



