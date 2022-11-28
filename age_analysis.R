library(tidyverse)

joined <-
  read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/deployment.csv')) %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  mutate(
    nukes = deployed + reserve,
  )

{
  source("graphs.R")

  input <- joined

  mygraphs(input, 'average_age', 'first_test', dir = "age", xtrans = "reverse", xlabel = "Average Age", ylabel = "First Test",
           # methods = c('pearson', 'spearman', 'kendall')
  )

  ranked <- input %>%
    arrange(first_test) %>%
    mutate(rank_first_test = dense_rank(first_test), rank_average_age = dense_rank(desc(average_age))) %>%
    select(country, first_test, rank_first_test, average_age, rank_average_age)

  mygraphs(ranked, 'rank_average_age', 'rank_first_test', dir="age", xlabel="Rank of Average Age", ylabel="Rank of First Test")

  mygraphs(input, 'average_age', 'nukes', dir = "age", xtrans = "reverse", xlabel = "Average Age", ylabel = "Number of Nukes")
  mygraphs(input, 'first_test', 'nukes', dir = "age", xlabel = "First Test", ylabel = "Number of Nukes")

}
