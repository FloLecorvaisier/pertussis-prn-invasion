## This scripts produces the figures used in the article. Note that all figures
## use the "Ubuntu" font but that figures can be produced even if the font
## is not installed (small changes in the aspect of the figures may thus occur).

library(binom)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(extrafont)
# font_import() # Only needed the first time the script runs.
loadfonts(quiet = T)

size_label_multiplot = 10
base_size = 10

greymin = 0
greymax = 90

res = 300

#### Proportion of PRN- strains ####

data_prn <- read.table("data/data-prn", header = T, sep = "\t")
count_strains <- with(data_prn, tapply(prn_def, list(year, prn_def), length))
count_strains[is.na(count_strains)] <- 0
data_count = data.frame(year = as.numeric(rownames(count_strains)),
                        prop = count_strains[, 1] / (count_strains[, 1] + count_strains[, 2]),
                        lower = binom.confint(x = count_strains[, 1], n = count_strains[, 1] + count_strains[, 2], methods = "wilson")$lower,
                        upper = binom.confint(x = count_strains[, 1], n = count_strains[, 1] + count_strains[, 2], methods = "wilson")$upper)

ggplot(data_count) +
  geom_point(aes(x = year, y = prop), 
             color = ifelse(data_count$year %in% 2007:2017, "black", "grey")) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = 0, 
                color = ifelse(data_count$year %in% 2007:2017, "black", "grey")) +
  scale_x_continuous(breaks = seq(min(data_count$year), max(data_count$year), 2)) +
  labs(x = "Year of collection", y = "Proportion of PRN- isolates") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal(base_family = "Ubuntu", base_size = base_size) +
  theme(axis.line = element_line(lineend = "square"))

png("figures/fig-prop.png", width = 9, height = 6, units = "cm", res = res, bg = "transparent")
last_plot()
dev.off()

#### Diversity isolates ####

data_prn <- read.table("data/data-prn", header = T, sep = "\t")
table_prn <- with(data_prn, as.data.frame(table(state, year, prn_def)))
table_prn2 <- data.frame(state = as.character(table_prn$state[table_prn$prn_def == "-"]),
                         year = as.numeric(as.character(table_prn$year[table_prn$prn_def == "-"])),
                         prop = table_prn$Freq[table_prn$prn_def == "-"] / 
                           (table_prn$Freq[table_prn$prn_def == "-"] + 
                              table_prn$Freq[table_prn$prn_def == "+"]),
                         freq = table_prn$Freq[table_prn$prn_def == "-"] + 
                           table_prn$Freq[table_prn$prn_def == "+"])
table_prn2$freq[table_prn2$freq > 75] <- 75 # To remove excessive values

ggplot(table_prn2) +
  geom_point(aes(x     = year, 
                 y     = state,
                 size  = ifelse(freq == 0, NA, freq),
                 color = prop)) +
  scale_size(range = c(1, 8), breaks = c(5, 25, 50, 75), labels = c("5", "25", "50", "75+")) +
  scale_color_gradientn(colors = paste0("grey", c(greymax, greymin)), breaks = c(0, 1), 
                        labels = c("100%\nPRN+",  "100%\nPRN-")) +
  scale_x_continuous(breaks = seq(2007, 2019, 2)) +
  guides(color = guide_colorbar(ticks = FALSE)) +
  labs(size = "Number\nof isolates", x = "Year of collection", y = "State of collection", color = element_blank()) +
  theme_minimal(base_family = "Ubuntu", base_size = base_size) +
  theme(panel.grid = element_blank(), axis.line = element_line(lineend = "square"), legend.ticks = element_blank())

png("figures/fig-isolates.png", width = 9, height = 10, units = "cm", res = res, bg = "transparent")
last_plot()
dev.off()

#### Diversity cofactors ####

## We create a series of plots showing that there is some variance between and
## within states for the different cofactors.
x_labels <- c("Median age\n(years)", "Vaccine coverage (%)\n", "Residential population\n(inhab./km²)", "Hospitals per 100,000\nresidential population", 
             "Average household\nsize", "Median income (1,000 $)\n", "Physicians per 10,000\nresidential population", "Average\ntemperature (°C)")

list_data_cofac <- list()
for (file in list.files("data/cofactors/")) {
  list_data_cofac[[gsub("data-", "\\1", file)]] <- read.table(paste0("data/cofactors/", file), header = T, sep = "\t")
}

list_plots_range_cofac <- list()
for (df in names(list_data_cofac)) {
  data <- list_data_cofac[[df]]
  data <- data[data$year <= 2017, ]
  colnames(data)[3] <- "value"
  
  if (df != "coverage") {
    mean <- tapply(data$value, data$state, mean)
    sd <- tapply(data$value, data$state, sd)
    min <- tapply(data$value, data$state, min)
    max <- tapply(data$value, data$state, max)
    st <- unique(data$state)
    data = data.frame(mean, sd, min, max, st)
    ggplot(data) +
      geom_segment(aes(x = min, xend = max, y = st, yend = st)) +
      scale_fill_viridis_c(option = "E") +
      labs(y = "State of collection", x = x_labels[names(list_data_cofac) == df]) +
      theme_minimal(base_family = "Ubuntu", base_size = base_size) +
      theme(axis.line = element_line(lineend = "square")) -> list_plots_range_cofac[[df]]
  }
  else { # Because there is no inter-annual, intra-state variation in the vaccine, it has its own plot.
    ggplot(data) +
      geom_point(aes(x = value, y = state)) +
      labs(y = "", x = x_labels[names(list_data_cofac) == df]) +
      theme_minimal(base_family = "Ubuntu", base_size = base_size) +
      theme(axis.line = element_line(lineend = "square")) -> list_plots_range_cofac[[df]]
  }
}

ggarrange(plotlist = list_plots_range_cofac, ncol = 4, nrow = 2, labels = LETTERS[1:8], 
          font.label = list(family = "Ubuntu",  size = size_label_multiplot), hjust = 0)

png("figures/fig-cofactors.png", width = 19, height = 18, units = "cm", res = res, bg = "transparent")
last_plot()
dev.off()

#### Robustness analysis GLM(M) ####

x_labels <- c("a[0]", "a[f]", "n", "p[1](0)", "sigma[e]", "t[m]+1")
data_robust_glmm <- read.table("data/analyzes-summaries/summary-robust-glmm", header = T, sep = "\t")

data_robust <- data_robust_glmm
data_robust$tm[data_robust$param == "tm"] <- data_robust$tm[data_robust$param == "tm"] + 1

list_plot_robust_glm <- list()
for (param in unique(data_robust_glmm$param)) {
  data <- data_robust[data_robust$param == param, c(param, "alpha")]
  colnames(data)[1] <- "value"
  ggplot(data) +
    geom_hline(yintercept = .05, linetype = "dashed") +
    geom_line(aes(x = value, y = alpha)) +
    scale_color_manual(values = c("darkorchid", "darkolivegreen")) +
    labs(x = parse(text = "sigma[e]"), 
         y = parse(text = "alpha"),
         color = element_blank()) +
    coord_cartesian(ylim = c(0, .45)) +
    theme_minimal(base_family = "Ubuntu", base_size = base_size) +
    theme(legend.position = c(2/3, 2/3),
          legend.background = element_rect(fill = "white"),
          axis.line = element_line(lineend = "square")) -> list_plot_robust_glm[[param]]
}

ggarrange(plotlist = list_plot_robust_glm, labels = LETTERS[1:6],
          font.label = list(family = "Ubuntu", size = size_label_multiplot), hjust = 0)

png("figures/fig-robust.png", width = 9, height = 7, units = "cm", res = res, bg = "transparent")
list_plot_robust_glm$se
dev.off()


#### Precision ####

## We load the file containing the data we want to plot.
sum_af <- read.table("data/analyzes-summaries/summary-precision-1", header = T, sep = "\t")
sum_af$tm <- sum_af$tm + 1

## We mark if 0 is included in the 95% CI of af
sum_af$ov0 <- ifelse(sum_af$IC2.5 * sum_af$IC97.5 < 0, T, F)

base_a0 = .5
base_tm = 10
base_n = 200
base_prop = seq(0, 1, .25)
base_sr2 = 1

expr <- c("sum_af[sum_af$a0 == base_a0 & sum_af$tm == base_tm + 1 & sum_af$prop %in% base_prop & sum_af$sr2 == base_sr2, ]",
          "sum_af[sum_af$tm == base_tm + 1 & sum_af$n == base_n & sum_af$prop %in% base_prop & sum_af$sr2 == base_sr2, ]",
          "sum_af[sum_af$a0 == base_a0 & sum_af$n == base_n & sum_af$prop %in% base_prop & sum_af$sr2 == base_sr2, ]",
          "sum_af[sum_af$a0 == base_a0 & sum_af$n == base_n & sum_af$tm == base_tm + 1 & !sum_af$prop %in% c(.25, .75), ]")
param <- c("n", "a0", "tm", "prop")
param2 <- c(rep("prop", 3), "sr2")
x_labels <- c("n", "a[0]", "t[m]+1", "eta^2")
color_labels <- c(rep("eta^2", 3), "sigma[r]^2")

##### Probability of detection ####

list_plots_precision <- list()
for (i in 1:length(expr)) {
  df_precision <- as.data.frame(with(eval(parse(text = expr[i])), 
                                 table(ov0, eval(parse(text = param[i])), eval(parse(text = param2[i])))))
  df_precision <- with(df_precision, data.frame(precision = Freq[ov0 == F] / 1000,
                                        value = as.numeric(as.character(Var2[seq(2, nrow(df_precision), 2)])),
                                        color = df_precision$Var3[seq(2, nrow(df_precision), 2)],
                                        row.names = NULL))
  df_precision$lower <- binom.confint(df_precision$precision * 1000, 1000, methods = "wilson")$lower
  df_precision$upper <- binom.confint(df_precision$precision * 1000, 1000, methods = "wilson")$upper
  ggplot(df_precision) +
    geom_hline(yintercept = 0.05, linetype = "dashed") +
    geom_line(aes(x = value, y = precision, color = color, group = color)) +
    scale_color_manual(values = paste0("grey", round(seq(greymin, greymax, 
                                                         length.out = length(unique(df_precision$color)))))) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = parse(text = x_labels[i]),
         y = "Proba. of detection",
         color = parse(text = color_labels[i])) +
    theme_minimal(base_family = "Ubuntu", base_size = base_size) +
    theme(axis.line = element_line(lineend = "square"), panel.grid = element_blank()) -> list_plots_precision[[i]]
}

ggarrange(plotlist = list_plots_precision, labels = LETTERS[1:4], 
          font.label = list(family = "Ubuntu", size = size_label_multiplot), hjust = 0)

png("figures/fig-precision-1.png", width = 14, height = 10, units = "cm", res = res, bg = "transparent")
last_plot()
dev.off()

##### Standard deviation ####

list_plots_sd <- list()
for (i in 1:length(expr)) {
  sd <- with(eval(parse(text = expr[i])), tapply(sd, interaction(eval(parse(text = param[i])), eval(parse(text = param2[i])), sep = "|"), mean))
  df_precision <- data.frame(sd = sd, 
                         value = as.numeric(gsub("(.*)\\|.*", "\\1", names(sd))), 
                         color = gsub(".*\\|(.*)", "\\1", names(sd)), row.names = 1:length(sd))
  ggplot(df_precision) +
    geom_line(aes(x = value, y = sd, color = color, group = color)) +
    scale_color_manual(values = paste0("grey", round(seq(greymin, greymax, 
                                                         length.out = length(unique(df_precision$color)))))) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = parse(text = x_labels[i]),
         y = "Standard deviation",
         color = parse(text = color_labels[i])) +
    theme_minimal(base_family = "Ubuntu", base_size = base_size) +
    theme(axis.line = element_line(lineend = "square"), panel.grid = element_blank()) -> list_plots_sd[[i]]
}

ggarrange(plotlist = list_plots_sd, labels = LETTERS[1:4], 
          font.label = list(family = "Ubuntu", size = size_label_multiplot), hjust = 0)

png("figures/fig-sd-1.png", width = 14, height = 10, units = "cm", res = res, bg = "transparent")
last_plot()
dev.off()

#### Precision 2 ####

sum_data_af <- read.table("data/analyzes-summaries/summary-precision-2", header = T, sep = "\t")

sum_data_af$ov0 <- ifelse(sum_data_af$IC2.5 * sum_data_af$IC97.5 < 0, T, F)

expr_data <- c("sum_data_af[sum_data_af$x == 1 & sum_data_af$z == 1, ]",
               "sum_data_af[sum_data_af$y == 1 & sum_data_af$z == 1, ]",
               "sum_data_af[sum_data_af$x == 1 & sum_data_af$y == 1, ]")
param_data <- rep("prop", 3)
param_data2 <- c("y", "x", "z")
color_labels_data <- c("Sample size\nvariance", "Sample size\nmean", "Sampling\nfrequency")
color_values_data <- list(c("×1", "×0.5", "×0.25", "×0.125"), c("×1", "×2", "×4", "×8"), c("×1", "×2", "×3", "×4"))

##### Probability of detection ####

list_plots_precision_data <- list()
for (i in 1:length(expr_data)) {
  df_precision_data <- as.data.frame(with(eval(parse(text = expr_data[i])), 
                                      table(ov0, eval(parse(text = param_data[i])), eval(parse(text = param_data2[i])))))
  df_precision_data <- with(df_precision_data, data.frame(precision = Freq[ov0 == F] / 
                                                    tapply(df_precision_data$Freq, 
                                                           interaction(df_precision_data$Var2, df_precision_data$Var3), sum),
                                                  value = as.numeric(as.character(Var2[seq(2, nrow(df_precision_data), 2)])),
                                                  color = df_precision_data$Var3[seq(2, nrow(df_precision_data), 2)],
                                                  row.names = NULL))
  ggplot(df_precision_data) +
    geom_hline(yintercept = 0.05, linetype = "dashed") +
    geom_line(aes(x = value, y = precision, color = color, group = color)) +
    scale_color_manual(values = paste0("grey", round(seq(greymin, greymax, 
                                                         length.out = length(unique(df_precision_data$color))))), 
                       labels = color_values_data[[i]]) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = parse(text = "eta^2"),
         y = "Proba. of detection",
         color = color_labels_data[i]) +
    theme_minimal(base_family = "Ubuntu", base_size = base_size) +
    theme(axis.line = element_line(lineend = "square"), panel.grid = element_blank()) -> list_plots_precision_data[[i]]
}

ggarrange(plotlist = list_plots_precision_data, labels = LETTERS[1:3], 
          font.label = list(family = "Ubuntu", size = 10), hjust = 0, ncol = 1)

png("figures/fig-precision-2.png", width = 9, height = 15, units = "cm", bg = "transparent", res = res)
last_plot()
dev.off()

##### Standard deviation ####

list_plots_sd_data <- list()
for (i in 1:length(expr_data)) {
  sd <- with(eval(parse(text = expr_data[i])), 
             tapply(sd, 
                    interaction(eval(parse(text = param_data[i])), eval(parse(text = param_data2[i])), sep = "|"), 
                    mean))
  df_precision_data <- data.frame(sd = sd, 
                              value = as.numeric(gsub("(.*)\\|.*", "\\1", names(sd))), 
                              color = gsub(".*\\|(.*)", "\\1", names(sd)), row.names = 1:length(sd))
  ggplot(df_precision_data) +
    geom_line(aes(x = value, y = sd, color = color, group = color)) +
    scale_color_manual(values = paste0("grey", round(seq(greymin, greymax, 
                                                         length.out = length(unique(df_precision_data$color))))), 
                       labels = color_values_data[[i]]) +
    coord_cartesian(ylim = c(0, .4)) +
    labs(x = parse(text = "eta^2"),
         y = "Standard deviation",
         color = color_labels_data[i]) +
    theme_minimal(base_family = "Ubuntu", base_size = base_size) +
    theme(axis.line = element_line(lineend = "square"), panel.grid = element_blank()) -> list_plots_sd_data[[i]]
}

ggarrange(plotlist = list_plots_sd_data, labels = LETTERS[1:3], 
          font.label = list(family = "Ubuntu", size = 10), hjust = 0, ncol = 1)

png("figures/fig-sd-2.png", width = 9, height = 15, units = "cm", bg = "transparent", res = res)
last_plot()
dev.off()

