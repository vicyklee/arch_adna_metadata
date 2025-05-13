#------------------------------------------------------------------------------#
# PRINCIPAL COMPONENT ANALYSIS
#------------------------------------------------------------------------------#
here::i_am("scripts/pca.R")
library(tidyverse)
library(here)

# required for stat
library(FactoMineR)

# required for plotting
library(factoextra)
library(MetBrewer)
# library(patchwork)

source(here("scripts/plot_style.R"))
source(here("scripts/data_wrangling.R"))
source(here("scripts/functions.R"))

# Declare components of output path (folders with the same name should exist)
# e.g. "fig_output_path/file_version/file_name"
fig_output_path <- "figures"
res_output_path <- "output"
file_version <- ""

#------------------------------------------------------------------------------#
# Response variables ####
#------------------------------------------------------------------------------#
# pca objects from data_wrangling.R
pca_y_eig <- as.data.frame(pca_y$eig) %>% rownames_to_column(var = "PC")
readr::write_csv(pca_y_eig, here(res_output_path,file_version,"pca_y_eig.csv"))

# Plot
y_vars <- sort(names(y))
y_subtitle <- "Completeness of metadata"

# PCA graph of variables
p <- list()
for (i in c(1,3,5)){
  p[[i]] <- factoextra::fviz_pca_var(pca_y, repel = T, axes = c(i,i+1),
                                     labelsize = 3, title = element_blank(),
                                     col.var = y_vars, palette = y_cols) + theme_pca
  p[[i]]$labels$x <- sub("Dim", "PC", p[[i]]$labels$x)
  p[[i]]$labels$y <- sub("Dim", "PC", p[[i]]$labels$y)
  savePDF(p[[i]], file = here(fig_output_path,file_version,paste0("pca_y_", i, "_", i+1, ".pdf")), width = 4.5, height = 4.5)
}
# Remove empty list elements
p <- Filter(Negate(is.null), p)
# For supplementary
p <- patchwork::wrap_plots(p[2:3], nrows = 1) + patchwork::plot_annotation(tag_levels = 'A') & theme(plot.tag=element_text(size = 12, face = "bold"))
savePDF(p,file = here(fig_output_path,file_version,"pca_y_3to6.pdf"), height = 4.5)

# Scree plot
p <- factoextra::fviz_screeplot(pca_y, barcolor = NA, barfill = MetBrewer::met.brewer("Hiroshige")[10], ncp = 16, subtitle = y_subtitle)
p$labels$x <- "PC"
savePDF(p, file = here(fig_output_path,file_version,"pca_y_screeplot.pdf"), width = 6, height = 4)

# Contributions
p1 <- list()
p2 <- list()
for (i in 1:6){
  p1[[i]] <- factoextra::fviz_contrib(pca_y, choice = "var", axes = i, color = NA, fill = MetBrewer::met.brewer("Hiroshige")[10], subtitle = y_subtitle)
  p1[[i]]$labels$title <- gsub("Dim-", "PC", p1[[i]]$labels$title)
  savePDF(p1[[i]], file = here(fig_output_path,file_version,paste0("pca_y_contrib_var_", i, ".pdf")), width = 6, height = 4)
  p2[[i]] <- factoextra::fviz_contrib(pca_y, choice = "ind", axes = i, color = NA, fill = MetBrewer::met.brewer("Hiroshige")[10], subtitle = y_subtitle)
  p2[[i]]$labels$title <- gsub("Dim-", "PC", p2[[i]]$labels$title)
  savePDF(p2[[i]], file = here(fig_output_path,file_version,paste0("pca_y_contrib_ind_", i, ".pdf")), width = 6, height = 4)
}
p <- c(p2,p1)
p <- patchwork::wrap_plots(p, nrows = 2) + patchwork::plot_annotation(tag_levels = 'A') & theme(plot.tag=element_text(size = 12, face = "bold"))
savePDF(p, file = here(fig_output_path,file_version,"pca_y_contrib_combined.pdf"), height = 14, width = 22)

#------------------------------------------------------------------------------#
# Explanatory variables ####
#------------------------------------------------------------------------------#
# pca objects from data_wrangling.R
pca_x_eig <- as.data.frame(pca_x$eig) %>% rownames_to_column(var = "PC")
readr::write_csv(pca_x_eig, here(res_output_path,file_version,"pca_x_eig.csv"))

# Plot
x_vars <- sort(names(x))
x_subtitle <- "Explanatory variables"

# PCA graph of variables
p <- list()
for (i in c(1,3)){
  p[[i]] <- factoextra::fviz_pca_var(pca_x, repel = T, axes = c(i,i+1),
                    labelsize = 3, title = element_blank(),
                    col.var = x_vars, palette = x_cols) + theme_pca
  p[[i]]$labels$x <- sub("Dim", "PC", p[[i]]$labels$x)
  p[[i]]$labels$y <- sub("Dim", "PC", p[[i]]$labels$y)
  savePDF(p[[i]], file = here(fig_output_path,file_version,paste0("pca_x_", i, "_", i+1, ".pdf")), width = 4.5, height = 4.5)
}
# Remove empty list elements
p <- Filter(Negate(is.null), p)
p <- patchwork::wrap_plots(p, nrows = 1) + patchwork::plot_annotation(tag_levels = 'A') & theme(plot.tag=element_text(size = 12, face = "bold"))
savePDF(p,file = here(fig_output_path,file_version,"pca_x_1to3.pdf"), height = 4.5)

# Scree plot
p <- factoextra::fviz_screeplot(pca_x, barcolor = NA, barfill = MetBrewer::met.brewer("VanGogh1")[2], ncp = 5, subtitle = x_subtitle)
p$labels$x <- "PC"
savePDF(p, file = here(fig_output_path,file_version,"pca_x_screeplot.pdf"), width = 6, height = 4)

# Contributions
p1 <- list()
p2 <- list()
for (i in 1:3){
  p1[[i]] <- factoextra::fviz_contrib(pca_x, choice = "var", axes = i, color = NA, fill = MetBrewer::met.brewer("VanGogh1")[2], subtitle = x_subtitle)
  p1[[i]]$labels$title <- gsub("Dim-", "PC", p1[[i]]$labels$title)
  savePDF(p1[[i]], file = here(fig_output_path,file_version,paste0("pca_x_contrib_var_", i, ".pdf")), width = 6, height = 4)
  p2[[i]] <- factoextra::fviz_contrib(pca_x, choice = "ind", axes = i, color = NA, fill = MetBrewer::met.brewer("VanGogh1")[2], subtitle = x_subtitle)
  p2[[i]]$labels$title <- gsub("Dim-", "PC", p2[[i]]$labels$title)
  savePDF(p2[[i]], file = here(fig_output_path,file_version,paste0("pca_x_contrib_ind_", i, ".pdf")), width = 6, height = 4)
}
p <- c(p2,p1)
p <- patchwork::wrap_plots(p, nrows = 2) + patchwork::plot_annotation(tag_levels = 'A') & theme(plot.tag=element_text(size = 12, face = "bold"))
savePDF(p, file = here(fig_output_path,file_version,"pca_x_contrib_combined.pdf"), height = 8, width = 16)

#------------------------------------------------------------------------------#
# sessionInfo.txt ####
#------------------------------------------------------------------------------#
writeLines(capture.output(sessionInfo()), here("sessionInfo","pca_sessionInfo.txt"))

