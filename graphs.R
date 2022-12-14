source('graphs_common.R')

library(corrplot)
library(cowplot)

corner_text <- function(text, position = "tr", xtrans = "identity", xdist = 0, ydist = 0) {
  if (position == "none") {
    return
  }

  if (grepl("t", position)) {
    y <- Inf
    vjust <- 1.1 + ydist
  } else {
    y <- -Inf
    vjust <- 0 - ydist
  }

  if (grepl("l", position)) {
    if (xtrans != "reverse") {
      x <- -Inf
    } else {
      x <- Inf
    }
    hjust <- 0
  } else {
    if (xtrans != "reverse") {
      x <- Inf
    } else {
      x <- -Inf
    }
    hjust <- 1
  }

  text <- str_sub(str_replace_all(paste0("\n", str_trim(text), "\n"), "\n", paste0(strrep(" ", xdist), "\n", strrep(" ", xdist))), xdist + 2)

  geom_text(data = data.frame(label = text), aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust))
}

mygraph <- function(input, X, Y, xtrans = "identity", ybreaks = 10, type = "number", xlabel = waiver(), ylabel = waiver(), ymin = NA, ymax = NA, cpos = NULL, methods = c('pearson', 'spearman')) {
  pear <- cor.test(input[[X]], input[[Y]], method = "pearson", use = "complete.obs")
  spear <- cor.test(input[[X]], input[[Y]], method = "spearman", use = "complete.obs", exact = FALSE)
  kend <- cor.test(input[[X]], input[[Y]], method = "kendall", use = "complete.obs", exact = FALSE)

  annotations <- ""
  if ('pearson' %in% methods) {
    annotations <- paste0(
      annotations,
      "Pearson ", round(pear$estimate, 2),
      "\n",
      "p-value ", format(signif(pear$p.value, 2), scientific = FALSE),
      "\n"
    )
  }
  if ('spearman' %in% methods) {
    annotations <- paste0(
      annotations,
      "Spearman ", round(spear$estimate, 2),
      "\n",
      "p-value ", format(signif(spear$p.value, 2), scientific = FALSE),
      "\n"
    )
  }
  if ('kendall' %in% methods) {
    annotations <- paste0(
      annotations,
      "Kendall ", round(kend$estimate, 2),
      "\n",
      "p-value ", format(signif(kend$p.value, 2), scientific = FALSE),
      "\n"
    )
  }

  if (type == "$") {
    ls <- label_number_si(prefix = "$")
  } else if (type == "%") {
    ls <- label_percent()
    # } else if (type == "int") {
    # ls <- label_number(accuracy = 1)
  } else {
    ls <- waiver()
  }

  g <- ggplot(input) +
    update_geom_defaults("text", list(size = 3.5, hjust = "inward", vjust = "inward")) +
    aes_string(x = X, y = Y) +
    geom_line(na.rm = TRUE) +
    geom_point(aes(color = country)) +
    geom_rug(alpha = 0.4, size = 1.5, aes(colour = country)) +
    geom_smooth(method = lm, formula = y ~ poly(x, 1), level = 0.95) +
    geom_label_repel(box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0.3, aes(label = country)) +
    corner_text(annotations,
                position = if_else(is_null(cpos), if_else((pear$estimate < 0) == (xtrans != "reverse"), "tr", "br"), cpos),
                xtrans = xtrans, xdist = 2) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8), trans = xtrans, name = xlabel) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = ybreaks), labels = ls, name = ylabel) +
    scale_color_manual(values = cs, guide = "none") +
    theme_bw()

  if (type == "$") {
    g + coord_cartesian(ylim = c(0, ymax))
  } else if (type == "%") {
    g + coord_cartesian(ylim = c(ymin, ymax))
  } else if (type == "int") {
    g + coord_cartesian(ylim = c(0, ymax))
  } else {
    g
  }
}

mygraphs <- function(input, X, Y, dir = "", ...) {
  mygraph(input, X, Y, ...)
  d <- mkdir(dir)
  ggsave(mkfile(dir, "xy-${X}-${Y}.png", X = X, Y = Y), width = 10, height = 10, dpi = 300, unit = "cm")
}

yearly <- function(input, Y, xtrans = "identity", type = "number", ybreaks = 10, secondary = NULL, ylabel = waiver(), ...) {
  if (type == "$") {
    # label_number(scale_cut = cut_short_scale())
    ls <- label_number_si(prefix = "$")
  } else if (type == "%") {
    ls <- label_percent()
  } else {
    ls <- waiver()
  }

  g1 <- ggplot(input) +
    update_geom_defaults("text", list(size = 3.5, hjust = "inward", vjust = "inward")) +
    aes_string(x = "year", y = Y) +
    geom_point(aes(color = country)) +
    geom_line(na.rm = TRUE, aes(color = country)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4), trans = xtrans, name = "Year") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = ybreaks), labels = ls, name = ylabel) +
    scale_color_manual(values = cs, name = "Country") +
    theme_bw()

  if (is_null(secondary)) {
    g1
  } else {
    g2 <- ggplot(input %>% filter(!(country %in% secondary))) +
      update_geom_defaults("text", list(size = 3.5, hjust = "inward", vjust = "inward")) +
      aes_string(x = "year", y = Y) +
      geom_point(aes(color = country)) +
      geom_line(na.rm = TRUE, aes(color = country)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 4), trans = xtrans, name = "Year") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = ybreaks), labels = ls, name = ylabel) +
      scale_color_manual(values = cs, guide = "none") +
      theme_bw()
    plot_grid(g1, g2, labels = "AUTO", rel_widths = c(3, 2))
  }
}

yearlys <- function(input, Y, dir = "", name = NULL, secondary = NULL, ...) {
  yearly(input, Y, secondary = secondary, ...)
  if (is.null(name)) {
    name <- Y
  }
  ggsave(mkfile(dir, "yearly-${name}.png", name = name), width = if_else(is_null(secondary), 10, 15), height = 10, dpi = 300, unit = "cm")
}

myboxplot <- function(input, X, Y, xlabel = waiver(), ylabel = waiver(), cpos = NULL) {
  cat_corr <- summary.aov(aov(reformulate(X, Y), data = input))
  annotations <- paste0(
    "Anova",
    "\n",
    "p-value ", format(signif(cat_corr[[1]][["Pr(>F)"]][1], 2), scientific = FALSE)
  )

  avg <- input %>%
    group_by(.data[[X]]) %>%
    summarise(y = mean(.data[[Y]]), ymax = max(.data[[Y]]), country = NA)

  ggplot(data = input, aes_string(x = X, y = Y)) +
    update_geom_defaults("text", list(size = 3.5, hjust = "inward", vjust = "inward")) +
    geom_boxplot() +
    geom_point() +
    geom_label_repel(box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', min.segment.length = 0.3, aes(label = country)) +
    corner_text(annotations,
                position = if_else(is_null(cpos), if_else(head(avg, n = 1)$ymax < tail(avg, n = 1)$ymax, "tl", "tr"), cpos),
                xdist = 5, ydist = 0.2) +
    geom_rug(sides = "l", alpha = 0.4, size = 1.5) +
    geom_point(data = avg, color = "red", size = 3, aes_string(x = X, y = "y")) +
    geom_rug(data = avg, sides = "l", color = "red", aes_string(x = X, y = "y"), alpha = 0.4, size = 1.5) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10), name = ylabel) +
    scale_x_discrete(name = xlabel) +
    theme_bw()
}

myboxplots <- function(input, X, Y, dir = "", ...) {
  myboxplot(input, X, Y, ...)
  d <- mkdir(dir)
  ggsave(str_interp("${d}/box-${X}-${Y}.png"), width = 10, height = 10, dpi = 300, unit = "cm")
}

crosscor <- function(input, Xs = NA, Ys = NA, method = "pearson", cutoff = 0.7, shape = "ellipse", coef = NULL) {
  cat <- c("country", "cat_sea", "cat_paradigm", "cat_continent", "cat_policy", "cat_triggers", "aging")

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
