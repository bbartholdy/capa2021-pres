library(tidyverse)
library(here)
library(officer)
#library(flextable)
library(patchwork)
library(janitor)

options(ggplot2.discrete.fill = function() scale_fill_viridis_d(),
        ggplot2.continuous.colour = function() scale_color_viridis_c())

# set a consistent theme across plots
capapres_theme <- theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_line(colour = "#88AC82", size = 0.2), # gridlines matching slides colour
        panel.border = element_blank()) 

corr_counts <- readr::read_csv(here("data", "corr_counts.csv"))
corr_comb <- readr::read_csv(here("data", "corr_comb.csv"))
sol_corr <- readr::read_csv(here("data", "sol_corr.csv"))

# combine replicates using the mean
sol_counts <- sol_corr %>%
  group_by(solution, starch) %>%
  summarise(across(c(s, m, l, total), mean)) %>%
  summarise(across(where(is.numeric), sum, na.rm = T)) # combine wheat and potato counts in mixed treatment

sol_comb <- sol_corr %>%
  filter(solution != "mix") %>%
  group_by(solution, starch) %>%
  summarise(across(c("s","m","l","total"), mean)) %>%
  ungroup() %>%
  add_row(sol_corr[5:6,c("solution", "starch", "s","m","l","total")], ) %>%
  rename(treatment = solution) %>% # I should have done better naming the raw data...
  mutate(sample = "solution")

# Plots -------------------------------------------------------------------

# Box plots for summary statistics

solutions_bar <- sol_comb %>%
  ggplot(aes(x = treatment, y = total, fill = treatment, col = starch)) +
  geom_col(size = 2) +
  capapres_theme +
  theme(panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "left") +
  scale_color_viridis_d(begin = 0.5)
solutions_bar
ggsave(here("assets/img/solutions_bar.png"))

counts_boxplot <- corr_comb %>%
  filter(treatment != "control") %>%
  ggplot(aes(x = treatment, y = total, 
             shape = treatment)) +
  geom_boxplot(aes(fill = treatment), alpha = 0.5) +
  geom_jitter(aes(col = treatment), width = 0.3, size = 2) +
  scale_color_viridis_d() +
  capapres_theme +
  theme(panel.grid.major.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) # remove y-axis title
counts_boxplot
ggsave(here("assets/img/counts_boxplot.png"))

# Proportional incorporation

prop_data <- corr_counts %>%
  filter(treatment != "control") %>%
  mutate(sample = "sample") %>% # rename all sample IDs: individual sample names are irrelevant for next steps
  select(sample, starch, treatment, s, m, l, total) %>%
  bind_rows(sol_comb) %>%
  mutate(sample = as_factor(sample))

# Proportional incorporation plot

prop_bar <- prop_data %>%
  group_by(sample) %>%
  ggplot(aes(x = treatment, y = total, fill = reorder(sample, -total))) +
    geom_col(position = "dodge") +
    labs(x = "", y = "Total starch count", fill = "Type") +
    capapres_theme +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "left")
ggsave(here("assets/img/prop_bar.png"))

# Size ratio plots

# transpose data frames to create pie charts
# solution count data
ratio_sol_sep <- prop_data %>%
  filter(sample != "sample",
         treatment != "mix") %>% # removed mixed treatment
  select(!c(sample, treatment, total)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column("size") %>%
  row_to_names(row_number = 1) %>% # move first row to column names
  mutate(across(c(potato, wheat), as.numeric)) %>%
  mutate(wheat = wheat / sum(wheat)) %>%
  mutate(label_wheat = scales::percent(wheat)) %>%
  mutate(potato = potato/ sum(potato)) %>%
  mutate(label_potato = scales::percent(potato))

# sample count data
ratio_samp_sep <- prop_data %>%
  filter(sample != "solution",
         treatment != "mix") %>% # removed mixed treatment
  group_by(treatment, starch) %>%
  summarise(across(where(is.numeric), mean, na.rm = T)) %>%
  ungroup %>%
  select(!c(treatment, total)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column("size") %>% # create size variable
  row_to_names(row_number = 1) %>% # move first row to column names
  mutate(across(c(potato, wheat), as.numeric)) %>%
  mutate(wheat = wheat / sum(wheat)) %>%
  mutate(label_wheat = scales::percent(wheat)) %>%
  mutate(potato = potato/ sum(potato)) %>%
  mutate(label_potato = scales::percent(potato))

# transpose solution data frame
ratio_sol_mix <- prop_data %>%
  filter(sample != "sample",
         treatment == "mix") %>% # removed mixed treatment
  select(!c(sample, treatment, total)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column("size") %>%
  row_to_names(row_number = 1) %>% # move first row to column names
  mutate(across(c(potato, wheat), as.numeric)) %>%
  mutate(wheat = wheat / sum(wheat)) %>%
  mutate(label_wheat = scales::percent(wheat)) %>%
  mutate(potato = potato/ sum(potato, na.rm = T)) %>%
  mutate(label_potato = scales::percent(potato))

ratio_samp_mix <- prop_data %>%
  filter(sample != "solution",
         treatment == "mix") %>% # removed mixed treatment
  group_by(treatment, starch) %>%
  summarise(across(where(is.numeric), mean, na.rm = T)) %>%
  ungroup %>%
  select(!c(treatment, total)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column("size") %>%
  row_to_names(row_number = 1) %>% # move first row to column names
  mutate(across(c(potato, wheat), as.numeric)) %>%
  mutate(wheat = wheat / sum(wheat)) %>%
  mutate(label_wheat = scales::percent(wheat)) %>%
  mutate(potato = potato/ sum(potato, na.rm = T)) %>%
  mutate(label_potato = scales::percent(potato))

ratio_sep <- rbind(ratio_sol_sep, ratio_samp_sep)
ratio_sep$type <- c(rep("solution", 3), rep("sample", 3))
colnames(ratio_sep)[1] <- "size"

# combine the mixed-treatment data frames
ratio_mix <- rbind(ratio_sol_mix, ratio_samp_mix)
ratio_mix$type <- c(rep("solution", 3), rep("sample", 3))
colnames(ratio_mix)[1] <- "size"

# size ratios as pie charts
# pie chart for size ratios in wheat solution
pl_wheat1 <- ratio_sep %>%
  select(!contains("potato")) %>%
  ggplot(aes(x = type, y = wheat, fill = size)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_viridis_d(name = "Size", labels = c("large", "medium", "small")) +
  labs(subtitle = "Separate - wheat") +
  theme(plot.subtitle = element_text(size = 12))

pl_potato1 <- ratio_sep %>%
  select(!contains("wheat")) %>%
  ggplot(aes(x = type, y = potato, fill = size)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_viridis_d(name = "Size", labels = c("large", "medium", "small")) +
  labs(subtitle = "Separate - potato") +
  theme(plot.subtitle = element_text(size = 12))

# create data frame containing the difference between solution and sample
# single starch treatment
# ratio_diff_sep <- ratio_samp_sep[,2:3] - ratio_sol_sep[,2:3]
# ratio_diff_sep <- data.frame(size = c("s", "m", "l"), ratio_diff_sep)
# diff_sep <- ratio_diff_sep %>%
#   mutate(across(c(potato, wheat), function(x) signif(x * 100, 3)))
# # mixed starch treatment
# ratio_diff_mix <- ratio_samp_mix[,2:3] - ratio_sol_mix[,2:3]
# ratio_diff_mix <- data.frame(size = c("s", "m", "l"), ratio_diff_mix)
# diff_mix <- ratio_diff_mix %>%
#   mutate(across(c(potato, wheat), function(x) signif(x * 100, 3)))

# size ratios as pie charts
# pie chart for size ratios in wheat solution
pl_wheat2 <- ratio_mix %>%
  select(!contains("potato")) %>%
  ggplot(aes(x = type, y = wheat, fill = size)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_viridis_d(begin = 0.7,
                       name = "",
                       labels = c("large", "medium", "small")) +
  labs(subtitle = "Mix - wheat") +
  theme(plot.subtitle = element_text(size = 12))

pl_potato2 <- ratio_mix %>%
  mutate(potato = if_else(is.na(potato), 0, potato)) %>% # fix colour issue
  select(!contains("wheat")) %>%
  ggplot(aes(x = type, y = potato, fill = size)) +
  geom_bar(stat = "identity", width = 1, colour = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_viridis_d(begin = 0.7,
                       name = "",
                       labels = c("large", "medium", "small")) +
  labs(subtitle = "Mix - potato") +
  theme(plot.subtitle = element_text(size = 12))

ratio_plots_out <- (pl_wheat1 + pl_potato1) / (pl_wheat2 + pl_potato2) + 
  plot_layout(guides = "collect") #+ plot_annotation(tag_levels = "A")

# Tables ------------------------------------------------------------------

# proportional incorporation table

prop_tbl <- prop_data %>%
  group_by(sample, treatment, starch) %>%
  summarise(across(c(s, m, l, total), mean, na.rm = T))

prop_tbl <- split(prop_tbl, ~ sample)
prop_calc <- prop_tbl$sample[,4:7] / prop_tbl$solution[,4:7] %>%
  signif(3)

prop_tbl_out <- cbind(prop_tbl$sample[,2:3], prop_calc)
perc_tbl_out <- prop_tbl_out %>%
  mutate(across(where(is.numeric), scales::percent, accuracy = 0.01))
