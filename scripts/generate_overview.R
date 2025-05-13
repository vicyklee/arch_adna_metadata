#------------------------------------------------------------------------------#
# GENERATE OVERVIEW
#------------------------------------------------------------------------------#
here::i_am("scripts/generate_overview.R")
library(tidyverse)
library(here)

# required for plotting
library(ggh4x)
library(MetBrewer)
library(RColorBrewer)
library(patchwork)

source(here("scripts/plot_style.R"))
source(here("scripts/data_wrangling.R"))
source(here("scripts/functions.R"))

# Declare components of output path (folders with the same name should exist)
# e.g. "fig_output_path/file_version/file_name"
fig_output_path <- "figures"
file_version <- ""

#------------------------------------------------------------------------------#
# Metadata reporting - Count ####
#------------------------------------------------------------------------------#
p1 <- ggplot(data_c_long %>% filter(category == "Core")) +
  geom_col(aes(x = count,
               y = factor(CitationID, levels = rev(CitationID_order)),
               fill = factor(presence_absence, levels = c("NA","absence","presence"))),
           position = "stack") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.position="top",
        legend.justification="right",
        plot.title = element_text(face="bold")) +
  xlab("Number of samples") +
  ylab("Publication") +
  ggh4x::facet_nested_wrap(~category + data,
                    labeller = as_labeller(lab_mapping),
                    nest_line = TRUE, nrow = 1) +
  scale_fill_manual(values = c("#0072b2","#cc79a7","grey"),
                    breaks = c("presence", "absence","NA"),
                    labels = c("Presence", "Absence","NA")) +
  ggtitle("The presence/absence of archaeological metadata reporting")

p2 <- ggplot(data_c_long %>% filter(category == "Extended")) +
  geom_col(aes(x = count,
               y = factor(CitationID, levels = rev(CitationID_order)),
               fill = factor(presence_absence, levels = c("NA","absence","presence"))),
           position = "stack") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="none",
        legend.title = element_blank()) +
  xlab("Number of samples") +
  ylab("Publication") +
  ggh4x::facet_nested_wrap(~category + data,
                    labeller = as_labeller(lab_mapping),
                    nest_line = TRUE, nrow = 1) +
  scale_fill_manual(values = c("#0072b2","#cc79a7","grey"),
                    breaks = c("presence", "absence","NA"),
                    labels = c("Presence", "Absence","NA")) 

p <- list(p1 + remove_x, p2)
p <- patchwork::wrap_plots(p, nrow = 2)
savePDF(p, file = here(fig_output_path,file_version,"overview_y_count.pdf"), width = 14, height = 10)

#------------------------------------------------------------------------------#
# Metadata reporting - Proportion ####
#------------------------------------------------------------------------------#
p1 <- ggplot(data_p_long %>% filter(category == "Core")) +
  geom_col(aes(x = proportion,
               y = factor(CitationID, levels = rev(CitationID_order)),
               fill = data),
           position = "dodge") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold")) +
  xlab("Proportion") +
  ylab("Publication") +
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  ggh4x::facet_nested_wrap(~category + data,
                    labeller = as_labeller(lab_mapping),
                    nest_line = TRUE, nrow = 1) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", n = 18)[1:7]) +
  ggtitle("The completeness of archaeological metadata reporting")

p2 <- ggplot(data_p_long %>% filter(category == "Extended")) +
  geom_col(aes(x = proportion,
               y = factor(CitationID, levels = rev(CitationID_order)),
               fill = data),
           position = "dodge") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face="bold")) +
  xlab("Proportion") +
  ylab("Publication") +
  scale_x_continuous(breaks=seq(0, 1, 0.5)) +
  ggh4x::facet_nested_wrap(~category + data,
                    labeller = as_labeller(lab_mapping),
                    nest_line = TRUE, nrow = 1) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", n = 18)[10:18])

p <- list(p1 + remove_x, p2)
p <- patchwork::wrap_plots(p, nrow = 2)
savePDF(p = p, file = here(fig_output_path,file_version,"overview_y_proportion.pdf"), width = 14, height = 10)

#------------------------------------------------------------------------------#
# Metadata reporting - distribution ####
#------------------------------------------------------------------------------#
p1 <- ggplot(data_p_long %>% filter(category == "Core")) +
  geom_histogram(aes(x = proportion, fill = data)) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face="bold"),
        legend.position = "none") +
  ggh4x::facet_nested_wrap(~category + data,
                    labeller = as_labeller(lab_mapping),
                    nest_line = TRUE,
                    scales = "free",
                    nrow = 1) +
  ylab("Count") +
  xlab("Proportion") +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", n = 18)[1:7])

p2 <- ggplot(data_p_long %>% filter(category == "Extended")) +
  geom_histogram(aes(x = proportion, fill = data)) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        plot.title = element_text(face="bold"),
        legend.position = "none") +
  ggh4x::facet_nested_wrap(~category + data,
                    labeller = as_labeller(lab_mapping),
                    nest_line = TRUE,
                    scales = "free",
                    nrow = 1) +
  ylab("Count") +
  xlab("Proportion") +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", n = 18)[10:18])

p <- list(p1 + remove_x, p2)
p <- patchwork::wrap_plots(p, nrow = 2)
savePDF(p, file = here(fig_output_path,file_version,"distributions_y.pdf"), width = 14, height = 4)

#------------------------------------------------------------------------------#
# Explanatory variables - incl. "_new" ####
#------------------------------------------------------------------------------#
p_year <- ggplot(metadata) +
  geom_point((aes(x = year_publication, y = factor(CitationID, levels = rev(CitationID_order))))) +
  xlab("Year of publication") +
  ylab("Publication") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks=seq(2013, 2024, 3)) 

p <- list(
  plot_x("arch_index", "Archaeology Index") + xlim(c(0,1)),
  plot_x("n_authors", "Number of authors") + remove_y,
  plot_x_sample("n_countries", "Number of countries") + remove_y,
  plot_x_sample("n_samples", "Number of samples") + remove_y,
  p_year + remove_y
)

p <- patchwork::wrap_plots(p, nrow = 1) + plot_layout(guides = "collect")
savePDF(p, file = here(fig_output_path,file_version,"overview_x_full.pdf"), width = 12, height = 4)

#------------------------------------------------------------------------------#
# Explanatory variables ####
#------------------------------------------------------------------------------#
p_year <- ggplot(metadata) +
  geom_point((aes(x = year_publication, y = factor(CitationID, levels = rev(CitationID_order)))), colour = MetBrewer::met.brewer("VanGogh1",7)[[5]]) +
  xlab("Year of publication") +
  ylab("Publication") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks=seq(2013, 2024, 3)) 

p <- list(
  plot_x("arch_index", "Archaeology Index", fill = MetBrewer::met.brewer("VanGogh1",7)[[1]]) + xlim(c(0,1)),
  plot_x("n_authors", "Number of authors", fill = MetBrewer::met.brewer("VanGogh1",7)[[2]]) + remove_y,
  plot_x("n_countries", "Number of countries", fill = MetBrewer::met.brewer("VanGogh1",7)[[3]]) + remove_y,
  plot_x("n_samples", "Number of samples", fill = MetBrewer::met.brewer("VanGogh1",7)[[4]]) + remove_y,
  p_year + remove_y
)

p <- patchwork::wrap_plots(p, nrow = 1) + plot_layout(guides = "collect")
savePDF(p, file = here(fig_output_path,file_version,"overview_x.pdf"), width = 12, height = 4)

#------------------------------------------------------------------------------#
# Explanatory variables - distribution ####
#------------------------------------------------------------------------------#
p <- list(
  plot_x_dist("arch_index", "Archaeology Index", fill = MetBrewer::met.brewer("VanGogh1",7)[[1]]),
  plot_x_dist("n_authors", "Number of authors", fill = MetBrewer::met.brewer("VanGogh1",7)[[2]]) + theme(axis.title.y = element_blank()),
  plot_x_dist("n_countries", "Number of countries", fill = MetBrewer::met.brewer("VanGogh1",7)[[3]]) + theme(axis.title.y = element_blank()),
  plot_x_dist("n_samples", "Number of samples", fill = MetBrewer::met.brewer("VanGogh1",7)[[4]]) + theme(axis.title.y = element_blank()),
  plot_x_dist("year_publication", "Year of publication", fill = MetBrewer::met.brewer("VanGogh1",7)[[5]]) + scale_x_continuous(breaks=seq(2013, 2024, 3)),
  plot_x_dist("n_authors_log", "log(Number of authors)", fill = MetBrewer::met.brewer("VanGogh1",7)[[2]]) + theme(axis.title.y = element_blank()),
  plot_x_dist("n_countries_log", "log(Number of countries)", fill = MetBrewer::met.brewer("VanGogh1",7)[[3]]) + theme(axis.title.y = element_blank()),
  plot_x_dist("n_samples_log", "log(Number of samples)", fill = MetBrewer::met.brewer("VanGogh1",7)[[4]]) + theme(axis.title.y = element_blank())
)

p <- patchwork::wrap_plots(p, nrow = 2)
savePDF(p, file = here(fig_output_path,file_version,"distributions_x_combined.pdf"), width = 8, height = 4)
 