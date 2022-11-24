library(tidyverse)
library(spatstat)
library(RColorBrewer)
library(scales)

countries <- read_csv('data/countries.csv') %>% mutate(first_test_age = 2022 - first_test)

nukes <- read_csv('data/nuclear_weapons_stockpiles.csv', comment = '#') %>%
  rename(country = country_name) %>%
  filter(country != 'Israel') %>%
  mutate(country = if_else(country == 'United States', 'USA', if_else(country == 'United Kingdom', 'UK', country))) %>%
  group_by(country)

production <- nukes %>%
  mutate(produced = nuclear_weapons_stockpile - lag(nuclear_weapons_stockpile, default = 0)) %>%
  select(-nuclear_weapons_stockpile)

current <- {
  decomissioning <- production %>%
    filter(produced < 0) %>%
    summarise(decomissioned = -sum(produced))
  decomissioning

  joined <- production %>%
    filter(produced > 0) %>%
    left_join(decomissioning) %>%
    mutate(existing = cumsum(produced), decomissioned = coalesce(decomissioned, 0))

  eliminated <- joined %>%
    filter(existing <= decomissioned)

  eliminated %>%
    summarise(years = n(), exising = max(existing), el = max(decomissioned), leftover = max(existing) - max(decomissioned))

  modification <- joined %>%
    filter(existing > decomissioned) %>%
    summarise(produced = first(produced), year = min(year), existing = first(existing), el = first(decomissioned)) %>%
    mutate(modified = existing - el)
  modification

  current <-
    joined %>%
      filter(existing > decomissioned) %>%
      left_join(modification, by = c("country", "year")) %>%
      mutate(produced = coalesce(modified, produced.x),
             age = 2022 - year) %>%
      select(country, year, age, produced)
  current
}

{
  production %>%
    filter(produced > 0) %>%
    filter(country %in% c('Pakistan', 'China', 'North Korea', 'India', 'Israel')) %>%
    ungroup() %>%
    mutate(old = year < 2000) %>%
    group_by(old) %>%
    summarise(cnt = sum(produced))
}

current_stats <- current %>%
  summarise(current_nukes = sum(produced),
            average_age = weighted.mean(2022 - year, produced),
            median_age = weighted.median(2022 - year, produced),
            oldest_existing = min(year),
            oldest_age = 2022 - min(year),
            newest_existing = max(year),
            newest_age = 2022 - max(year),
            distinct_years = n())
current_stats

max_stats <- nukes %>%
  summarise( #current_nukes = last(nuclear_weapons_stockpile),
    max_nukes = max(nuclear_weapons_stockpile))
max_stats

production_stats <- production %>%
  filter(produced > 0) %>%
  summarise(first_produced = first(year),
            first_age = 2022 - first(year),
            last_produced = last(year),
            last_age = 2022 - last(year),
            years_produced = n(),
            total_produced = sum(produced))
production_stats

nukes_stats <-
  production_stats %>%
    full_join(max_stats, by = "country") %>%
    full_join(current_stats, by = "country") %>%
    mutate(
      current_nukes = coalesce(current_nukes, 0),
      distinct_years = coalesce(distinct_years, 0)
    )
nukes_stats

write_csv(nukes_stats, file = "data/production.csv")

{
  all_produced <- production %>%
    filter(produced > 0) %>%
    mutate(age = 2022 - year)

  avg <- all_produced %>%
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

  ggplot(all_produced, aes(x = age, y = produced)) +
    geom_point(aes(color = country), size = 0.5) +
    geom_line(aes(color = country), linewidth = 0.2) +
    # geom_vline(data = avg, aes(xintercept = mean, color = country,), size = 0.1, alpha = 0.6, linetype = "solid") +
    # geom_vline(data = avg, aes(xintercept = middle, color = country), size = 0.1, alpha = 0.3, linetype = "dashed") +
    scale_y_continuous(trans = 'log10', breaks = trans_breaks('log10', function(x) round(10^x), n = 10)) +
    scale_x_reverse() +
    theme_bw()
  ggsave("plots/prod-all.png", width = 15, height = 10, unit = "cm", dpi = 300)

  ggplot() +
    theme_bw() +
    geom_boxplot(data = avg,
                 aes(x = factor(country, levels = order),
                     ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                 stat = "identity") +
    geom_point(data = avg %>%
      filter(sapply(outliers, length) > 0) %>%
      select(country, outliers) %>%
      unnest(cols = outliers),
               aes(y = unlist(outliers), x = country)) +
    geom_point(data = avg, size = 2, aes(x = country, y = mean), shape = 4) +
    geom_point(data = countries, aes(x = country, y = first_test_age), shape = 2, size = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), trans = "reverse", name = "age",
                       sec.axis = sec_axis(~. * -1 + 2022, name = "year", breaks = scales::pretty_breaks(n = 10))) +
    scale_x_discrete(name = "country") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggsave("plots/prod-all-box.png", width = 10, height = 15, unit = "cm", dpi = 300)
}

{
  avg <- current %>%
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

  ggplot(current, aes(x = age, y = produced)) +
    geom_point(aes(color = country), size = 0.5) +
    geom_line(aes(color = country), linewidth = 0.2) +
    # geom_vline(data = avg, aes(xintercept = mean, color = country,), size = 0.1, alpha = 0.6, linetype = "solid") +
    # geom_vline(data = avg, aes(xintercept = middle, color = country), size = 0.1, alpha = 0.3, linetype = "dashed") +
    scale_y_continuous(trans = 'log10', breaks = trans_breaks('log10', function(x) round(10^x), n = 10)) +
    scale_x_reverse() +
    theme_bw()
  ggsave("plots/prod-cur.png", width = 15, height = 10, unit = "cm", dpi = 300)

  ggplot() +
    theme_bw() +
    geom_boxplot(data = avg,
                 aes(x = factor(country, levels = order),
                     ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                 stat = "identity") +
    geom_point(data = avg %>%
      filter(sapply(outliers, length) > 0) %>%
      select(country, outliers) %>%
      unnest(cols = outliers),
               aes(y = unlist(outliers), x = country)) +
    geom_point(data = avg, size = 2, aes(x = country, y = mean), shape = 4) +
    geom_point(data = countries, aes(x = country, y = first_test_age), shape = 2, size = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), trans = "reverse", name = "age",
                       sec.axis = sec_axis(~. * -1 + 2022, name = "year", breaks = scales::pretty_breaks(n = 10))) +
    scale_x_discrete(name = "country") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  ggsave("plots/prod-cur-box.png", width = 10, height = 15, unit = "cm", dpi = 300)
}

{
  ggplot(current %>% filter(!country %in% c("USA", "Russia")),
         aes(x = year, y = produced)) +
    geom_bar(aes(fill = factor(country, levels = rev(c("China", "UK", "France", "Pakistan", "India", "North Korea")))), position = "stack", stat = 'identity') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_fill_manual(values = c("#FC8D62", "#66C2A5", "#8DA0CB", "#A6D854", "#FFD92F", "#E78AC3"),
                      #brewer.pal(n = 6, name = "Set2"),
                      breaks = c("China", "UK", "France", "Pakistan", "India", "North Korea"),
                      name = "country") +
    theme_bw()
  ggsave("plots/prod-year-3.png", width = 15, height = 10, unit = "cm", dpi = 300)

  ggplot(current %>%
           mutate(country = if_else(country %in% c("USA", "Russia"), country, "Other")) %>%
           group_by(country, year) %>%
           summarise(produced = sum(produced)), aes(x = year, y = produced)) +
    geom_bar(aes(fill = country), position = "stack", stat = 'identity') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    scale_fill_manual(values = c("#619CFF", "#F8766D", "#00BA38"),
                      breaks = c("USA", "Russia", "Other")) +
    theme_bw()
  ggsave("plots/prod-year-6.png", width = 15, height = 10, unit = "cm", dpi = 300)
}
