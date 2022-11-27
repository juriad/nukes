library(tidyverse)
library(ggplot2)
library(corrplot)
library(ggrepel)
library(scales)

joined <- read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/treaties.csv')) %>%
  inner_join(read_csv('data/gen/econ_2021.csv')) %>%
  inner_join(read_csv('data/deployment.csv')) %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  inner_join(read_csv('data/policies.csv')) %>%
  mutate(
    paradigm = +(cat_paradigm == 'Euroamerican'),
    cat_triggers = paste(triggers),
    aging = if_else(median_age < average_age, "rather older", "rather newer")
  ) %>%
  filter(TRUE)


{
  source("graphs.R")

  input <- joined

  mygraphs(input, "average_age", "triggers", xtrans = "reverse", ybreaks = 5)
}

{
  source("graphs.R")

  input <- joined

  myboxplots(input, "cat_triggers", "average_age")

  myboxplots(input, "cat_continent", "first_test")
  myboxplots(input, "cat_continent", "average_age")

  myboxplots(input, "cat_paradigm", "sea")

  myboxplots(input, "aging", "average_age")
}

{
  source("graphs.R")

  input <- joined

  time <- c("first_test", "first_produced", "oldest_existing", "last_produced", "average_age", "median_age", "paradigm")
  alt_time <- c("first_age", "last_age", "oldest_age", "newest_existing", "newest_age")
  econ <- c("gdp", "military_budget", "nukes_budget",
            "gdp_per_nuke", "military_budget_per_nuke", "nukes_budget_per_nuke",
            "military_budget_gdp_rate", "nukes_budget_gdp_rate", "nukes_budget_military_budget_rate")
  good <- c("eligible", "asc_sign", "asc_sign_rate", "asc_rat", "asc_rat_rate", "declined", "dec_rate",
            "has_deployed", "deployed", "deployed_rate", "has_land", "land", "land_rate",
            "has_sea", "sea", "sea_rate", "sea_log", "subs", "ssb", "first_sea_missile")

  bad <- colnames(input) %>%
    setdiff(time) %>%
    setdiff(alt_time) %>%
    setdiff(good)

  crosscors(input, time, good, "pearson", "good")
  crosscors(input, time, good, "spearman", "good")

  crosscors(input, time, bad, "pearson", "bad")
  crosscors(input, time, bad, "spearman", "bad")

  crosscors(input, time, econ, "pearson", "econ", cutoff = 0.4, coef = "black")
  crosscors(input, time, econ, "spearman", "econ", cutoff = 0, coef = "black")
}
