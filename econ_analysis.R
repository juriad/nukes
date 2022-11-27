library(tidyverse)

joined <- read_csv('data/gen/econ_2021.csv') %>%
  inner_join(read_csv('data/deployment.csv')) %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  mutate(
    nukes = deployed + reserve,

    gdp_per_nuke = gdp / nukes,
    military_budget_per_nuke = military_budget / nukes,
    nukes_budget_per_nuke = nukes_budget / nukes,

    military_budget_gdp_rate = military_budget / gdp,
    nukes_budget_gdp_rate = nukes_budget / gdp,
    nukes_budget_military_budget_rate = nukes_budget / military_budget)

{
  source("graphs.R")

  input <- joined %>% filter(country != "North Korea")

  mygraphs(input, "average_age", "gdp", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "GDP")
  mygraphs(input, "average_age", "military_budget", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Military Budget")
  mygraphs(input, "average_age", "nukes_budget", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Nukes Budget")

  mygraphs(input, "average_age", "gdp_per_nuke", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "GDP / Nuke")
  mygraphs(input, "average_age", "military_budget_per_nuke", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Military Budget / Nuke")
  mygraphs(input, "average_age", "nukes_budget_per_nuke", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Nukes Budget / Nuke")

  mygraphs(input, "average_age", "military_budget_gdp_rate", dir = "econ", xtrans = "reverse", type = "%", xlabel = "Average Age", ylabel = "Military Budget / GDP")
  mygraphs(input, "average_age", "nukes_budget_gdp_rate", dir = "econ", xtrans = "reverse", type = "%", xlabel = "Average Age", ylabel = "Nukes Budget / GDP")
  mygraphs(input, "average_age", "nukes_budget_military_budget_rate", dir = "econ", xtrans = "reverse", type = "%", xlabel = "Average Age", ylabel = "Nukes Budget / Military Budget")
}

econ <- read_csv('data/gen/econ.csv') %>%
  inner_join(read_csv('data/deployment.csv')) %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  mutate(
    nukes = deployed + reserve,

    gdp_per_nuke = gdp / nukes,
    military_budget_per_nuke = military_budget / nukes,
    nukes_budget_per_nuke = nukes_budget / nukes,

    military_budget_gdp_rate = military_budget / gdp,
    nukes_budget_gdp_rate = nukes_budget / gdp,
    nukes_budget_military_budget_rate = nukes_budget / military_budget)

{
  source("graphs.R")

  input <- econ %>%
    filter(country != "North Korea")

  yearlys(input, "gdp", dir = "econ", type = "$", ylabel = "GDP")
  yearlys(input, "military_budget", dir = "econ", type = "$", ylabel = "Military Budget")
  yearlys(input, "nukes_budget", dir = "econ", type = "$", ylabel = "Nukes Budget")

  yearlys(input, "gdp_per_nuke", dir = "econ", type = "$", ylabel = "GDP / nuke")
  yearlys(input, "military_budget_per_nuke", dir = "econ", type = "$", ylabel = "Military Budget / Nuke")
  yearlys(input, "nukes_budget_per_nuke", dir = "econ", type = "$", ylabel = "Nukes Budget / Nuke")

  yearlys(input, "military_budget_gdp_rate", dir = "econ", type = "%", ylabel = "Military Budget / GDP")
  yearlys(input, "nukes_budget_gdp_rate", dir = "econ", type = "%", ylabel = "Nukes Budget / GDP")
  yearlys(input, "nukes_budget_military_budget_rate", dir = "econ", type = "%", ylabel = "Nukes Budget / Military Budget")

  yearlys(input, "gdp", dir = "econ", name = "gdp_without_usa_china", type = "$", secondary = c('China', 'USA'), ylabel = "GDP")
  yearlys(input, "military_budget", dir = "econ", name = "military_budget_without_usa_china", type = "$", secondary = c('China', 'USA'), ylabel = "Military Budget")
  yearlys(input, "nukes_budget", dir = "econ", name = "nukes_budget_without_usa", type = "$", secondary = c('USA'), ylabel = "Nukes Budget")
}

# mygraph(input, "nukes", "nukes_budget")
# mygraph(input, "average_age", "nukes")
