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
  mygraphs(input, "average_age", "military_budget", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Defense Spending")
  mygraphs(input, "average_age", "nukes_budget", dir = "econ", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Nuclear Arsenal Spending")

  mygraphs(input, "average_age", "gdp_per_nuke", dir = "econ-per-nuke", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "GDP / Nuke")
  mygraphs(input, "average_age", "military_budget_per_nuke", dir = "econ-per-nuke", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Defense Spending / Nuke")
  mygraphs(input, "average_age", "nukes_budget_per_nuke", dir = "econ-per-nuke", xtrans = "reverse", type = "$", xlabel = "Average Age", ylabel = "Nuclear Arsenal Spending / Nuke")

  mygraphs(input, "average_age", "military_budget_gdp_rate", dir = "econ", xtrans = "reverse", type = "%", xlabel = "Average Age", ylabel = "Defense Spending / GDP")
  mygraphs(input, "average_age", "nukes_budget_gdp_rate", dir = "econ", xtrans = "reverse", type = "%", xlabel = "Average Age", ylabel = "Nuclear Arsenal Spending / GDP")
  mygraphs(input, "average_age", "nukes_budget_military_budget_rate", dir = "econ", xtrans = "reverse", type = "%", xlabel = "Average Age", ylabel = "Nuclear Arsenal Spending / Defense Spending")
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
  yearlys(input, "military_budget", dir = "econ", type = "$", ylabel = "Defense Spending")
  yearlys(input, "nukes_budget", dir = "econ", type = "$", ylabel = "Nuclear Arsenal Spending")

  yearlys(input, "gdp_per_nuke", dir = "econ-per-nuke", type = "$", ylabel = "GDP / Nuke")
  yearlys(input, "military_budget_per_nuke", dir = "econ-per-nuke", type = "$", ylabel = "Defense Spending / Nuke")
  yearlys(input, "nukes_budget_per_nuke", dir = "econ-per-nuke", type = "$", ylabel = "Nuclear Arsenal Spending / Nuke")

  yearlys(input, "military_budget_gdp_rate", dir = "econ", type = "%", ylabel = "Defense Spending / GDP")
  yearlys(input, "nukes_budget_gdp_rate", dir = "econ", type = "%", ylabel = "Nuclear Arsenal Spending / GDP")
  yearlys(input, "nukes_budget_military_budget_rate", dir = "econ", type = "%", ylabel = "Nuclear Arsenal Spending / Defense Spending")

  yearlys(input, "gdp", dir = "econ", name = "gdp_without_usa_china", type = "$", secondary = c('China', 'USA'), ylabel = "GDP")
  yearlys(input, "military_budget", dir = "econ", name = "military_budget_without_usa_china", type = "$", secondary = c('China', 'USA'), ylabel = "Defense Spending")
  yearlys(input, "nukes_budget", dir = "econ", name = "nukes_budget_without_usa", type = "$", secondary = c('USA'), ylabel = "Nuclear Arsenal Spending")
}

# mygraph(input, "nukes", "nukes_budget")
# mygraph(input, "average_age", "nukes")
