library(tidyverse)
library(spatstat)

source('graphs_common.R')


countries <- read_csv('data/countries.csv') %>% mutate(first_test_age = 2022 - first_test)
all_produced <- read_csv('data/gen/prod_all.csv')
current <- read_csv('data/gen/prod_cur.csv')

production <- function(input, name) {
  avg <- input %>%
    group_by(country) %>%
    summarise(
      middle = weighted.median(age, produced),
      mean = weighted.mean(age, produced),
      lower = weighted.quantile(age, produced, probs = 0.25),
      upper = weighted.quantile(age, produced, probs = 0.75),
      IQR = diff(c(lower, upper)),
      ymin = max(weighted.quantile(age, produced, probs = 0), lower - 1.5 * IQR),
      ymax = min(weighted.quantile(age, produced, probs = 1), upper + 1.5 * IQR),
      outliers = list(age[which(age > upper + 1.5 * IQR | age < lower - 1.5 * IQR)]))

  order <- (avg %>% arrange(desc(mean)))$country

  ggplot(input, aes(x = age, y = produced)) +
    geom_point(aes(color = country), size = 0.5) +
    geom_line(aes(color = country), linewidth = 0.2) +
    # geom_vline(data = avg, aes(xintercept = mean, color = country,), size = 0.1, alpha = 0.6, linetype = "solid") +
    # geom_vline(data = avg, aes(xintercept = middle, color = country), size = 0.1, alpha = 0.3, linetype = "dashed") +
    scale_y_continuous(trans = 'log10', breaks = trans_breaks('log10', function(x) round(10^x), n = 10), name = "Produced") +
    scale_x_reverse(name = "Age") +
    scale_color_manual(values = cs, name = "Country") +
    theme_bw()
  ggsave(mkfile("prod", "${name}.png", name = name), width = 15, height = 10, unit = "cm", dpi = 300)

  ggplot() +
    theme_bw() +
    geom_boxplot(data = avg,
                 aes(x = factor(country, levels = order),
                     ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax, colour = country),
                 stat = "identity") +
    geom_point(data = avg %>%
      filter(sapply(outliers, length) > 0) %>%
      select(country, outliers) %>%
      unnest(cols = outliers),
               aes(y = unlist(outliers), x = country)) +
    geom_point(data = avg, size = 2, aes(x = country, y = mean), shape = 4) +
    geom_point(data = countries, aes(x = country, y = first_test_age), shape = 2, size = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), trans = "reverse", name = "Age",
                       sec.axis = sec_axis(~. * -1 + 2022, name = "Year", breaks = scales::pretty_breaks(n = 10))) +
    scale_x_discrete(name = "Country") +
    scale_color_manual(values = cs, guide = "none") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggsave(mkfile("prod", "${name}-box.png", name = name), width = 10, height = 15, unit = "cm", dpi = 300)

  ggplot(input %>% filter(!country %in% c("USA", "Russia")),
         aes(x = year, y = produced)) +
    geom_bar(aes(fill = factor(country, levels = rev(c("China", "UK", "France", "Pakistan", "India", "North Korea")))), position = "stack", stat = 'identity') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8), name="Year") +
    scale_y_continuous(name="Produced") +
    scale_fill_manual(values = cs, name = "Country") +
    theme_bw()
  ggsave(mkfile("prod", "${name}-year-6.png", name = name), width = 15, height = 10, unit = "cm", dpi = 300)

  ggplot(input %>%
           mutate(country = if_else(country %in% c("USA", "Russia"), country, "Other")) %>%
           group_by(country, year) %>%
           summarise(produced = sum(produced)), aes(x = year, y = produced)) +
    geom_bar(aes(fill = country), position = "stack", stat = 'identity') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8), name="Year") +
    scale_y_continuous(name="Produced") +
    scale_fill_manual(values = cs, name = "Country") +
    theme_bw()
  ggsave(mkfile("prod", "${name}-year-3.png", name = name), width = 15, height = 10, unit = "cm", dpi = 300)
}

production(all_produced, "all")
production(current, "cur")

