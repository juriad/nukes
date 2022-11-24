mygraph <- function(input, X, Y, xtrans = "identity", ybreaks = 10, type = "number") {
  pear <- cor.test(input[[X]], input[[Y]], method = "pearson", use = "complete.obs")
  spear <- cor.test(input[[X]], input[[Y]], method = "spearman", use = "complete.obs", exact = FALSE)
  annotations <- data.frame(
    annotateText = paste(
      "Pearson", round(pear$estimate, 2), "\np-value", signif(pear$p.value, 2),
      "\nSpearman", round(spear$estimate, 2), "\np-value", signif(spear$p.value, 2),
      "\n"
      # "\nKendall", round(cor(input[X], input[Y], method = "kendall"), 2)
    )
  )

  if (type == "$") {
    ls <- label_number_si(prefix = "$")
  } else {
    ls <- waiver()
  }

  g <- ggplot(input) +
    update_geom_defaults("text", list(size = 3.5, hjust = "inward", vjust = "inward")) +
    aes_string(x = X, y = Y) +
    geom_point() +
    geom_line(na.rm = TRUE) +
    geom_rug(alpha = 0.4, size = 1.5) +
    geom_smooth(method = lm, formula = y ~ poly(x, 1), level = 0.95) +
    geom_label_repel(box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0.2, aes(label = country)) +
    geom_text(data = annotations, aes(x = if_else(xtrans == "identity", Inf, -Inf),
                                      y = if_else((pear$estimate < 0) == (xtrans == "identity"), Inf, -Inf),
                                      label = annotateText,
                                      hjust = 1, vjust = if_else((pear$estimate < 0) == (xtrans == "identity"), 1.3, 0))) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8), trans = xtrans) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = ybreaks), labels = ls) +
    theme_bw(
      # base_size = 14
    )

  if (type == "$") {
    g + coord_cartesian(ylim = c(0, NA))
  } else {
    g
  }
}

mygraphs <- function(input, X, Y, ...) {
  mygraph(input, X, Y, ...)
  ggsave(str_interp("plots/xy-${X}-${Y}.png"), width = 10, height = 10, dpi = 300, unit = "cm")
}

myboxplot <- function(input, X, Y) {
  cat_corr <- summary.aov(aov(reformulate(X, Y), data = input))
  annotations <- data.frame(
    annotateText = paste(
      "Anova",
      "\np-value", signif(cat_corr[[1]][["Pr(>F)"]][1], 2),
      "\n"
    )
  )

  avg <- input %>%
    group_by(.data[[X]]) %>%
    summarise(y = mean(.data[[Y]]), ymax = max(.data[[Y]]), country = NA)

  ggplot(data = input, aes_string(x = X, y = Y)) +
    update_geom_defaults("text", list(size = 3.5, hjust = "inward", vjust = "inward")) +
    geom_boxplot() +
    geom_point() +
    geom_label_repel(box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0.2, aes(label = country)) +
    geom_text(data = annotations, aes(x = if_else(head(avg, n = 1)$ymax < tail(avg, n = 1)$ymax, 0.5, Inf), y = Inf, label = annotateText), hjust = if_else(head(avg, n = 1)$ymax < tail(avg, n = 1)$ymax, 0, 1), vjust = 1.3) +
    geom_rug(sides = "l", alpha = 0.4, size = 1.5) +
    geom_point(data = avg, color = "red", size = 3, aes_string(x = X, y = "y")) +
    geom_rug(data = avg, sides = "l", color = "red", aes_string(x = X, y = "y"), alpha = 0.4, size = 1.5) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_bw(
      # base_size = 10
    )
}

myboxplots <- function(input, X, Y) {
  myboxplot(input, X, Y)
  ggsave(str_interp("plots/box-${X}-${Y}.png"), width = 10, height = 10, dpi = 300, unit = "cm")
}

crosscor <- function(input, Xs = NA, Ys = NA, method = "pearson", cutoff = 0.7, shape = "ellipse", coef = NULL) {
  cat <- c("country", "cat_sea", "cat_paradigm", "cat_continent", "cat_policy", "cat_triggers")

  t <- input %>% select(-cat)
  Ys <- setdiff(Ys, cat)

  Z <- cor(t, method = method, use = "pairwise.complete.obs")
  if (cutoff == 0) {
    Z1 <- Z
  } else if (cutoff > 0) {
    Z1 <- Z * (Z > cutoff | Z < -cutoff)
  } else {
    Z1 <- Z * (Z < -cutoff & Z > cutoff)
  }

  if (anyNA(Xs) & anyNA(Ys)) {
    corrplot(Z1,
             method = shape,
             addCoef.col = coef,
             type = "full",
             order = "AOE", #"FPC",
             diag = FALSE,
    )
  } else {
    if (anyNA(Xs)) {
      Z2 <- Z1[, Ys]
    } else {
      if (anyNA(Ys)) {
        Z2 <- Z1[Xs,]
      } else {
        Z2 <- Z1[Xs, Ys]
      }
    }

    print(Z2)

    print(rowSums(abs(Z2)))

    corrplot(Z2,
             method = shape,
             addCoef.col = coef,
             type = "full",
             diag = FALSE,
    )
  }
}

crosscors <- function(input, Xs = NA, Ys = NA, method = NA, suffix = NA, ...) {
  width <- 10.0 * length(Ys) / length(Xs)
  png(file = str_interp("plots/cc-${method}-${suffix}.png"), type = "cairo", res = 300, width = width, height = 10, unit = "cm")
  crosscor(input, Xs, Ys, method, ...)
  dev.off()
}
