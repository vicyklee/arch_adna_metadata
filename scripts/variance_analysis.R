#------------------------------------------------------------------------------#
# VARIANCE ANALYSIS
#------------------------------------------------------------------------------#
here::i_am("scripts/variance_analysis.R")
library(tidyverse)
library(here)
library(rstatix)
library(FSA)

# required for plotting
library(ggh4x)

source(here("scripts/plot_style.R"))
source(here("scripts/data_wrangling.R"))
source(here("scripts/functions.R"))

# Declare components of output path (folders with the same name should exist)
# e.g. "fig_output_path/file_version/file_name"
fig_output_path <- "figures"
res_output_path <- "output"
file_version <- ""

# Tranform the repsonse variable dataframe
y_long <- y %>% 
  rownames_to_column("CitationID") %>%
  pivot_longer(cols = -1, names_to = "data", values_to = "proportion") %>%
  mutate(CitationID = factor(CitationID, levels = CitationID_order), # relevel CitationID
         category = ifelse(data %in% extended_cols, # add categories
                           "Extended", "Core"),
         category = factor(category, levels = c("Core", "Extended")))

# Variance of the metadata fields
var_data <- y_long %>%
  group_by(data) %>%
  summarise( category = unique(category), median = median(proportion), variance = var(proportion)) %>%
  arrange(variance)

# Variance of the publications
var_publ <- y_long %>%
  group_by(CitationID) %>%
  summarise(median = median(proportion), variance = var(proportion)) %>%
  arrange(variance)

readr::write_csv(var_data,file = here(res_output_path,file_version,"var_data.csv"))
readr::write_csv(var_publ,file = here(res_output_path,file_version,"var_publ.csv"))

# Kruskal Wallis test
kwtest_data <- kruskal.test(proportion ~ data, y_long)
kwtest_publ <- kruskal.test(proportion ~ CitationID, y_long)
readr::write_lines(kwtest_data, file = here(res_output_path,file_version,"kwtest_data.txt"))
readr::write_lines(kwtest_publ, file = here(res_output_path,file_version,"kwtest_publ.txt"))
# Post hoc Dunn test
dunnTest_data <- FSA::dunnTest(proportion ~ data, y_long, method = "bonferroni")$res
readr::write_csv(dunnTest_data,file = here(res_output_path,file_version,"dunnTest_data.csv"))

# For plotting but too many comparisons to plot
# dunnTest_res <- dunnTest_res %>% filter(P.adj < 0.05) %>%
#   separate(Comparison, c("data1","data2"), " - ")
# comparisons <- lapply(1:nrow(dunnTest_res), function(i) c(dunnTest_res$data1[i], dunnTest_res$data2[i]))

# Kruskal Wallis effect size
effsize_data <- y_long %>% rstatix::kruskal_effsize(proportion ~ data, ci = T)
effsize_publ <- y_long %>% rstatix::kruskal_effsize(proportion ~ CitationID, ci = T)
readr::write_csv(effsize_data,file = here(res_output_path,file_version,"effsize_data.csv"))
readr::write_csv(effsize_publ,file = here(res_output_path,file_version,"effsize_publ.csv"))

# Plots
p_data_boxplot <- ggplot(y_long, 
                             aes(
                               y = interaction(
                                 factor(data, levels=rev(sort(unique(data)))),
                                 factor(category, levels=rev(sort(unique(category))))), 
                               x = proportion,
                               colour = data)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(alpha = 0.5, height = 0.25) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        # panel.border = element_rect(fill = NA, colour = NA),
        # axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        ggh4x.axis.nestline = element_line(linetype = 1),
        axis.text.y.left = element_text(margin = margin(l = 5, r = 2)),
        legend.position = "none") +
  scale_y_discrete(guide = "axis_nested", name = "Metadata", labels = lab_mapping) +
  scale_colour_manual(values = y_cols) +
  xlab("Proportion")

p_var_data <- ggplot(var_data) +
  geom_col(
    aes(
      x = variance, 
      y = 
        interaction(
          factor(data, levels=rev(sort(unique(data)))),
          factor(category, levels=rev(sort(unique(category))))
        ),
      fill = data)
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y =  element_blank(),
    # panel.border = element_rect(fill = NA, colour = "grey"),
    # axis.ticks.x = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    ggh4x.axis.nestline = element_line(linetype = 1),
    axis.text.y.left = element_text(margin = margin(l = 5, r = 2)),
    legend.position = "none") +
  xlab("Variance") +
  scale_y_discrete(guide = ggh4x::guide_axis_nested(), name = "Metadata") +
  scale_fill_manual(values = y_cols)

p_data_boxplot_var <- patchwork::wrap_plots(list(p_data_boxplot,
                                                 p_var_data + 
                                                   remove_y + 
                                                   theme(axis.text.y.left = element_blank(), 
                                                         ggh4x.axis.nestline = element_blank())),
                                            widths = c(2, 1)
)

savePDF(p_data_boxplot_var, here(fig_output_path,file_version,"y_boxplot_var.pdf"), width = 8, height = 4)

#------------------------------------------------------------------------------#
# sessionInfo.txt ####
#------------------------------------------------------------------------------#
writeLines(capture.output(sessionInfo()), here("sessionInfo","variance_analysis_sessionInfo.txt"))
