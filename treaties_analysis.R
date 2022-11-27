joined <- read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/treaties.csv')) %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  mutate(
    asc_rat = ascended + ratified,
    asc_rat_rate = asc_rat / eligible,
    asc_sign = ascended + signed,
    asc_sign_rate = asc_sign / eligible,
    dec_rate = declined / eligible
  )

{
  source("graphs.R")

  input <- joined

  mygraphs(input, "average_age", "eligible", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_sign", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_sign_rate", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_rat", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "asc_rat_rate", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "declined", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "dec_rate", dir = "treaties", xtrans = "reverse")
  mygraphs(input, "average_age", "years_to_ascend_ratify", dir = "treaties", xtrans = "reverse")
}
