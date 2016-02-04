### On different platforms ###

dd %>%
  filter(!is.na(ecpm) & is.finite(ecpm)) %>%
  group_by(network, channel) %>%
  summarise(ecpm = mean(ecpm),
            n     = n()) %>%
  ggplot +
  aes(x = channel, y = network, fill = ecpm) %>%
  geom_raster()

### In different countries ###

dd %>%
  filter(country %in% countries$country[1:10]) %>%
  filter(!is.na(ecpm) & is.finite(ecpm)) %>%
  group_by(network, country) %>%
  summarise(ecpm = mean(ecpm),
            n     = n()) %>%
  ggplot +
  aes(x = country, y = network, fill = ecpm) %>%
  geom_raster() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

dd %>%
  filter(country %in% countries$country[1:10]) %>%
  filter(!is.na(ecpm) & is.finite(ecpm)) %>%
  group_by(country, network) %>%
  summarise(ecpm  = mean(ecpm),
            n     = n()) %>%
  mutate(percent_of_max = ecpm/max(ecpm)) %>% View
