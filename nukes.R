library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)
library(ggrepel)
library(scales)

joined <- read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/treaties.csv')) %>%
  inner_join(read_csv('data/economic.csv')) %>%
  inner_join(read_csv('data/deployment.csv')) %>%
  inner_join(read_csv('data/production.csv')) %>%
  mutate(
    neg_age = -average_age,
    asc_rat = ascended + ratified,
    asc_rat_rate = asc_rat / eligible,
    asc_sign = ascended + signed,
    asc_sign_rate = asc_sign / eligible,
    dec_rate = declined / eligible,
    nukes_budget_rate = nukes_budget / military_budget,
    nukes_gdp_rate = nukes_budget / gdp,
    nukes = deployed + reserve,
    all_nukes = deployed + reserve + retired,
    budget_per_nuke = nukes_budget / nukes,
    gdp_per_nuke = gdp / nukes,
    has_deployed = (deployed > 0) * 1,
    deployed_rate = deployed / nukes,
    has_retired = (retired > 0) * 1,
    has_land = (land > 0) * 1,
    has_sea = (sea > 0) * 1,
    has_air = (air > 0) * 1,
    land_rate = land / nukes,
    sea_rate = sea / nukes,
    air_rate = air / nukes,
    cat_sea = paste(sea > 0),
    sea_log = log10(sea + 1),
    paradigm = (cat_paradigm == 'Euroamerican') * 1,
  ) %>%
  filter(TRUE)


{
  source("graphs.R")

  input <- joined

  mygraphs(input, "first_test", "eligible")
  mygraphs(input, "first_test", "asc_sign")
  mygraphs(input, "first_test", "asc_sign_rate")
  mygraphs(input, "first_test", "asc_rat")
  mygraphs(input, "first_test", "asc_rat_rate")
  mygraphs(input, "first_test", "declined")
  mygraphs(input, "first_test", "dec_rate")

  mygraphs(input, "first_test", "has_deployed")
  mygraphs(input, "first_test", "deployed")
  mygraphs(input, "first_test", "deployed_rate")
  mygraphs(input, "first_test", "has_sea")
  mygraphs(input, "first_test", "sea")
  mygraphs(input, "first_test", "sea_rate")
  mygraphs(input, "first_test", "has_land")
  mygraphs(input, "first_test", "land")
  mygraphs(input, "first_test", "land_rate")
  mygraphs(input, "first_test", "subs")

  mygraphs(input, "average_age", "sea")
  mygraphs(input, "average_age", "land")

  mygraphs(input, "first_test", "ssb")
  mygraphs(input, "first_test", "first_sea_missile")
}


{
  source("graphs.R")

  input <- joined

  myboxplots(input, "cat_sea", "first_test")
  myboxplots(input, "cat_paradigm", "sea")
  myboxplots(input, "cat_continent", "first_test")
}

{
  source("graphs.R")

  input <- joined

  time <- c("first_test", "first_produced", "oldest_existing", "last_produced", "average_age", "median_age", "paradigm")
  alt_time <- c("first_age", "last_age", "oldest_age", "newest_existing", "newest_age", "neg_age")
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
}
