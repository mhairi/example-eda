dd <- read.csv('ad_data.csv')
library(dplyr)
library(lubridate)
library(countrycode)

################
### Cleaning ###
################

dd <- 
dd %>%
  tbl_df %>%
  select(-appname) %>%
  rename(revenue = rev,
         impressions = Imp,
         dau = DAU) %>%
  mutate(ecpm = (revenue/impressions)*1000,
         ipu = (impressions/dau)*1000)

dd$date <- dmy(dd$date)
dd$country <- countrycode(dd$country, 'iso2c', 'country.name')

save(dd, file = 'clean_data.RData')

model_data <-
  dd %>%
  filter(impressions > 100) %>%
  filter(country != '') %>%
  filter(ecpm < 30)

### Channel ###

dd %>%
  group_by(channel) %>%
  summarise(revenue = sum(revenue),
           impressions = sum(impressions),
           dau = sum(dau)) %>%
  mutate(ecpm = (revenue/impressions)*1000,
         ipu = (impressions/dau)*1000)

### Network ###

dd %>%
  group_by(network) %>%
  summarise(revenue = sum(revenue),
            impressions = sum(impressions),
            dau = sum(dau)) %>%
  mutate(ecpm = (revenue/impressions)*1000,
         ipu = (impressions/dau)*1000)


### Country ###

countries <- 
dd %>%
  group_by(country) %>%
  summarise(revenue = sum(revenue),
            impressions = sum(impressions),
            dau = sum(dau)) %>%
  filter(impressions > 1000) %>%
  mutate(ecpm = (revenue/impressions)*1000,
         ipu = (impressions/dau)*1000) %>%
  arrange(desc(dau))
