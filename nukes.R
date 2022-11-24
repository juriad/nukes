library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)
library(ggrepel)
library(scales)
library(Hmisc)
library(independence)

joined <- read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/treaties.csv')) %>%
  inner_join(read_csv('data/economic.csv')) %>%
  inner_join(read_csv('data/deployment.csv')) %>%
  inner_join(read_csv('data/production.csv')) %>%
  inner_join(read_csv('data/policies.csv')) %>%
  mutate(
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
    has_deployed = +(deployed > 0),
    deployed_rate = deployed / nukes,
    has_retired = +(retired > 0),
    has_land = +(land > 0),
    has_sea = +(sea > 0),
    has_air = +(air > 0),
    land_rate = land / nukes,
    sea_rate = sea / nukes,
    air_rate = air / nukes,
    cat_sea = paste(sea > 0),
    sea_log = log10(sea + 1),
    paradigm = +(cat_paradigm == 'Euroamerican'),
    cat_triggers = paste(triggers),
    aging = if_else(median_age < average_age, "", "")
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

  mygraphs(input, "first_test", "sea")
  mygraphs(input, "first_test", "land")

  mygraphs(input, "first_test", "ssb")
  mygraphs(input, "first_test", "first_sea_missile")
}

{
  source("graphs.R")

  input <- joined

  mygraphs(input, "average_age", "eligible", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_sign", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_sign_rate", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_rat", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_rat_rate", xtrans = "reverse")
  mygraphs(input, "average_age", "declined", xtrans = "reverse")
  mygraphs(input, "average_age", "dec_rate", xtrans = "reverse")

  mygraphs(input, "average_age", "has_deployed", xtrans = "reverse")
  mygraphs(input, "average_age", "deployed", xtrans = "reverse")
  mygraphs(input, "average_age", "deployed_rate", xtrans = "reverse")
  mygraphs(input, "average_age", "has_sea", xtrans = "reverse")
  mygraphs(input, "average_age", "sea", xtrans = "reverse")
  mygraphs(input, "average_age", "sea_rate", xtrans = "reverse")
  mygraphs(input, "average_age", "has_land", xtrans = "reverse")
  mygraphs(input, "average_age", "land", xtrans = "reverse")
  mygraphs(input, "average_age", "land_rate", xtrans = "reverse")
  mygraphs(input, "average_age", "subs", xtrans = "reverse")

  mygraphs(input, "average_age", "sea", xtrans = "reverse")
  mygraphs(input, "average_age", "land", xtrans = "reverse")

  mygraphs(input, "average_age", "ssb", xtrans = "reverse")
  mygraphs(input, "average_age", "first_sea_missile", xtrans = "reverse")

  mygraphs(input, "average_age", "triggers", xtrans = "reverse", ybreaks = 5)
}

{
  source("graphs.R")

  input <- joined %>% filter(country != "North Korea")

  mygraphs(input, "average_age", "gdp", xtrans = "reverse", type = "$")
  mygraphs(input, "average_age", "military_budget", xtrans = "reverse", type = "$")
  mygraphs(input, "average_age", "nukes_budget", xtrans = "reverse", type = "$")

  mygraphs(input, "average_age", "gdp_per_nuke", xtrans = "reverse", type = "$")
  mygraphs(input, "average_age", "budget_per_nuke", xtrans = "reverse", type = "$")

  mygraphs(input, "average_age", "nukes_gdp_rate", xtrans = "reverse")
  mygraphs(input, "average_age", "nukes_budget_rate", xtrans = "reverse")

  mygraphs(input, "nukes", "gdp_per_nuke")
}


{
  source("graphs.R")

  input <- joined

  myboxplots(input, "cat_sea", "first_test")
  myboxplots(input, "cat_sea", "average_age")

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
  econ <- c("gdp", "military_budget", "nukes_budget", "gdp_per_nuke", "budget_per_nuke", "nukes_gdp_rate", "nukes_budget_rate")
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

mygraphs(joined, "average_age", "oldest_existing", xtrans = "reverse")

{
  nukes <- bind_rows(
    joined %>%
      mutate(cnt = land, type = "land") %>%
      select(country, cnt, type, average_age),
    joined %>%
      mutate(cnt = sea, type = "sea") %>%
      select(country, cnt, type, average_age),
    joined %>%
      mutate(cnt = air, type = "air") %>%
      select(country, cnt, type, average_age)) %>%
    filter(cnt > 0)

  ggplot(nukes, aes(x = reorder(country, -average_age), y = cnt, fill = type)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_text(position = position_fill(vjust = 0.5), aes(label = cnt), size = 3) +
    scale_y_continuous(labels = scales::percent, name = "proportion") +
    scale_x_discrete(name="country") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggsave("plots/nukes-type-dist.png", width = 10, height = 10, unit = "cm", dpi = 300)

}
