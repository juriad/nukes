joined <- read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  inner_join(read_csv('data/policies.csv')) %>%
  mutate(
    paradigm = +(cat_paradigm == 'Euroamerican'),
    cat_triggers = paste(triggers),
    aging = if_else(median_age < average_age, "rather older", "rather newer")
  )

{
  source("graphs.R")

  input <- joined

  mygraphs(input, "average_age", "triggers", xtrans = "reverse", dir = "policies", ybreaks = 5, xlabel = "Average Age", ylabel = "Number of Triggers")
}

{
  source("graphs.R")

  input <- joined

  myboxplots(input, "cat_triggers", "average_age", dir = "policies", xlabel = "Number of Trigges", ylabel = "Average Age")
  myboxplots(input, "cat_policy", "average_age", dir = "policies", xlabel = "Policy", ylabel = "Average Age")
}
