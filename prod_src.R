library(tidyverse)
library(spatstat)

nukes <- read_csv('data/src/nuclear_weapons_stockpiles.csv', comment = '#') %>%
  rename(country = country_name) %>%
  filter(country != 'Israel') %>%
  mutate(country = if_else(country == 'United States', 'USA', if_else(country == 'United Kingdom', 'UK', country))) %>%
  group_by(country)

production <- nukes %>%
  mutate(produced = nuclear_weapons_stockpile - lag(nuclear_weapons_stockpile, default = 0)) %>%
  select(-nuclear_weapons_stockpile)

current <- {
  decomissioning <- production %>%
    filter(produced < 0) %>%
    summarise(decomissioned = -sum(produced))
  decomissioning

  joined <- production %>%
    filter(produced > 0) %>%
    left_join(decomissioning) %>%
    mutate(existing = cumsum(produced), decomissioned = coalesce(decomissioned, 0))

  eliminated <- joined %>%
    filter(existing <= decomissioned)

  eliminated %>%
    summarise(years = n(), exising = max(existing), el = max(decomissioned), leftover = max(existing) - max(decomissioned))

  modification <- joined %>%
    filter(existing > decomissioned) %>%
    summarise(produced = first(produced), year = min(year), existing = first(existing), el = first(decomissioned)) %>%
    mutate(modified = existing - el)
  modification

  current <-
    joined %>%
      filter(existing > decomissioned) %>%
      left_join(modification, by = c("country", "year")) %>%
      mutate(produced = coalesce(modified, produced.x),
             age = 2022 - year) %>%
      select(country, year, age, produced)
  current
}

{
  production %>%
    filter(produced > 0) %>%
    filter(country %in% c('Pakistan', 'China', 'North Korea', 'India', 'Israel')) %>%
    ungroup() %>%
    mutate(old = year < 2000) %>%
    group_by(old) %>%
    summarise(cnt = sum(produced))
}

current_stats <- current %>%
  summarise(current_nukes = sum(produced),
            average_age = weighted.mean(2022 - year, produced),
            median_age = weighted.median(2022 - year, produced),
            oldest_existing = min(year),
            oldest_age = 2022 - min(year),
            newest_existing = max(year),
            newest_age = 2022 - max(year),
            distinct_years = n())
current_stats

max_stats <- nukes %>%
  summarise( #current_nukes = last(nuclear_weapons_stockpile),
    max_nukes = max(nuclear_weapons_stockpile))
max_stats

production_stats <- production %>%
  filter(produced > 0) %>%
  summarise(first_produced = first(year),
            first_age = 2022 - first(year),
            last_produced = last(year),
            last_age = 2022 - last(year),
            years_produced = n(),
            total_produced = sum(produced))
production_stats

nukes_stats <-
  production_stats %>%
    full_join(max_stats, by = "country") %>%
    full_join(current_stats, by = "country") %>%
    mutate(
      current_nukes = coalesce(current_nukes, 0),
      distinct_years = coalesce(distinct_years, 0)
    )
nukes_stats

write_csv(nukes_stats, file = "data/gen/prod.csv")

all_produced <- production %>%
  filter(produced > 0) %>%
  mutate(age = 2022 - year)
write_csv(all_produced, 'data/gen/prod_all.csv')

write_csv(current, 'data/gen/prod_cur.csv')
