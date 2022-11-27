joined <- read_csv('data/countries.csv') %>%
  inner_join(read_csv('data/treaties.csv')) %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  mutate(
    asc_rat = ascended + ratified,
    asc_rat_rate = asc_rat / eligible,
    asc_sign = ascended + signed,
    asc_sign_rate = asc_sign / eligible,
    dec_rate = declined / eligible,
    rat_sign_rate = ratified / signed
  )

{
  source("graphs.R")

  input <- joined

  mygraphs(input, "average_age", "eligible", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Eligible", type = "int")
  mygraphs(input, "average_age", "asc_sign", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Ascended or Signed", type = "int")
  mygraphs(input, "average_age", "asc_sign_rate", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Ascended or Signed / Eligible", type = "%", ymax = 1)
  mygraphs(input, "average_age", "asc_rat", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Ascended or Ratified", type = "int")
  mygraphs(input, "average_age", "asc_rat_rate", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Ascended or ratified / Eligible", type = "%", ymax = 1)
  mygraphs(input, "average_age", "declined", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Declined", type = "int", ybreaks = 5)
  mygraphs(input, "average_age", "dec_rate", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Declined / Eligible", type = "%", ymin = 0)
  mygraphs(input, "average_age", "years_to_ascend_ratify", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Years to Ascend or Ratify", type = "int", cpos = "tl")
  mygraphs(input, "average_age", "years_to_ascend_ratify_2022", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Years to Ascend or Ratify 2022", type = "int", cpos = "tl")

  mygraphs(input, "average_age", "rat_sign_rate", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Ratified / Signed", cpos = "br", type = "%", ymax = 1)


  mygraphs(input, "average_age", "years_to_ascend_ratify_median", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Years to Ascend or Ratify Median", type = "int", cpos = "tl")
  mygraphs(input, "average_age", "years_to_ascend_ratify_median_2022", dir = "treaties", xtrans = "reverse", xlabel = "Average Age", ylabel = "Years to Ascend or Ratify Median 2022", type = "int", cpos = "tl")


}
