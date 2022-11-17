
without_russia <- treaties %>% filter(country != 'Russia')
without_usa <- treaties %>% filter(country != 'USA')
without_china <- treaties %>% filter(country != 'China')

only_russia <- treaties %>% filter(country == 'Russia')
only_usa <- treaties %>% filter(country == 'USA')
only_china <- treaties %>% filter(country == 'China')


# predict_without_russia <- lm(without_russia, formula = nukes ~ nukes_budget)
# predict_russia_nukes <- predict(predict_without_russia, only_russia %>% select(nukes_budget))
# geom_point(aes(x = predict_russia_nukes, y = only_russia$nukes_budget)) +
# geom_point(aes(x = only_russia$nukes, y = only_russia$nukes_budget)) +
# geom_text(aes(x = predict_russia_nukes, y = only_russia$nukes_budget, label = paste("Russia Prediction", round(predict_russia_nukes, 0)))) +
# geom_text(aes(x = only_russia$nukes, y = only_russia$nukes_budget, label = paste("Russia Now", only_russia$nukes)))
