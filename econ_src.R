library(tidyverse)

sipri <- {
  s <- read_csv('data/src/sipri.csv')
  bind_rows(s %>%
              mutate(year = 2018, military_budget = `2018`) %>%
              select(country, year, military_budget),
            s %>%
              mutate(year = 2019, military_budget = `2019`) %>%
              select(country, year, military_budget),
            s %>%
              mutate(year = 2020, military_budget = `2020`) %>%
              select(country, year, military_budget),
            s %>%
              mutate(year = 2021, military_budget = `2021`) %>%
              select(country, year, military_budget)
  ) %>% mutate(military_budget = military_budget * 1e6)
}

ican <- {
  read_csv('data/src/ican.csv') %>%
    mutate(nukes_budget = budget * 1e9) %>%
    select(-budget)
}

gdp <- {
  g <- read_csv('data/src/gdp.csv')
  bind_rows(g %>%
              mutate(year = 2018, gdp = `2018`) %>%
              select(country, year, gdp),
            g %>%
              mutate(year = 2019, gdp = `2019`) %>%
              select(country, year, gdp),
            g %>%
              mutate(year = 2020, gdp = `2020`) %>%
              select(country, year, gdp),
            g %>%
              mutate(year = 2021, gdp = `2021`) %>%
              select(country, year, gdp)
  )
}

econ <- sipri %>% full_join(ican) %>% full_join(gdp)
write_csv(econ, 'data/gen/econ.csv')

econ_2021 <- econ %>% filter(year == 2021)
write_csv(econ_2021, 'data/gen/econ_2021.csv')
