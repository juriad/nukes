library(tidyverse)

source('graphs.R')

joined <- read_csv('data/deployment.csv') %>%
  inner_join(read_csv('data/gen/prod.csv')) %>%
  mutate(
    nukes = deployed + reserve,
    all_nukes = deployed + reserve + retired,

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
  )

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
    geom_text(position = position_fill(vjust = 0.5), aes(label = cnt), size = 3, hjust = 0.5, vjust = 0.5) +
    scale_y_continuous(labels = scales::percent, name = "Proportion") +
    scale_x_discrete(name = "Country") +
    scale_fill_discrete(name = "Type") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggsave(mkfile("deploy", "nukes-type-dist.png"), width = 10, height = 10, unit = "cm", dpi = 300)
}


{
  source("graphs.R")

  input <- joined

  mygraphs(input, "average_age", "has_deployed", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "deployed", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "deployed_rate", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "has_sea", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "sea", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "sea_rate", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "has_land", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "land", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "land_rate", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "subs", dir = "deploy", xtrans = "reverse")

  mygraphs(input, "average_age", "sea", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "land", dir = "deploy", xtrans = "reverse")

  mygraphs(input, "average_age", "ssb", dir = "deploy", xtrans = "reverse")
  mygraphs(input, "average_age", "first_sea_missile", dir = "deploy", xtrans = "reverse")
}

{
  source("graphs.R")

  input <- joined

  myboxplots(input, "cat_sea", "average_age", dir = "deploy")
}
