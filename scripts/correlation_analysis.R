#------------------------------------------------------------------------------#
# CORRELATION ANALYSIS
#------------------------------------------------------------------------------#
here::i_am("scripts/correlation_analysis.R")
library(tidyverse)
library(here)
library(GGally)

source(here("scripts/plot_style.R"))
source(here("scripts/data_wrangling.R"))
source(here("scripts/functions.R"))

# Declare components of output path (folders with the same name should exist)
# e.g. "fig_output_path/file_version/file_name"
fig_output_path <- "figures"
file_version <- ""
#------------------------------------------------------------------------------#
# Response variables ####
#------------------------------------------------------------------------------#
# Exploratory scatterplot
p <- GGally::ggpairs(y, upper = "blank", progress = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "grey", fill = NA)) 
savePDF(p, file = here(fig_output_path,file_version,"scatterplot_y.pdf"), width = 22, height = 20)

# Spearman test
method <- "spearman"
# Correlation matrix
cor_mat <- cor(y, method = method)
# Adjusted P-value matrix
p_adjusted_mat <- psych::corr.test(y,  method = method, adjust = "bonferroni")$p

# Rename row and col names
rownames(cor_mat) <- lab_mapping[rownames(cor_mat)]
colnames(cor_mat) <- lab_mapping[colnames(cor_mat)]
rownames(p_adjusted_mat) <- lab_mapping[rownames(p_adjusted_mat)]
colnames(p_adjusted_mat) <- lab_mapping[colnames(p_adjusted_mat)]
pdf(here(fig_output_path,file_version,paste0("corrplot_y_",method,".pdf")))
corrplot::corrplot(cor_mat, p.mat = p_adjusted_mat, diag = FALSE,
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'yellow2', type='upper',
                   tl.srt = 45, , tl.cex = 1, cl.align.text = "l")
invisible(dev.off())

## Kendall test ####
method <- "kendall"
# Correlation matrix
cor_mat <- cor(y, method = method)
# Adjusted P-value matrix
p_adjusted_mat <- psych::corr.test(y,  method = method, adjust = "bonferroni")$p

# Rename row and col names
rownames(cor_mat) <- lab_mapping[rownames(cor_mat)]
colnames(cor_mat) <- lab_mapping[colnames(cor_mat)]
rownames(p_adjusted_mat) <- lab_mapping[rownames(p_adjusted_mat)]
colnames(p_adjusted_mat) <- lab_mapping[colnames(p_adjusted_mat)]

# Plot
pdf(here(fig_output_path,file_version,paste0("corrplot_y_",method,".pdf")))
corrplot::corrplot(cor_mat, p.mat = p_adjusted_mat, diag = FALSE,
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'yellow2', type='upper',
                   tl.srt = 45, , tl.cex = 1, cl.align.text = "l")
invisible(dev.off())

#------------------------------------------------------------------------------#
# Explanatory variables ####
#------------------------------------------------------------------------------#
# Exploratory scatterplot
p <- GGally::ggpairs(x, progress = FALSE) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "grey", fill = NA)) 
savePDF(p, file = here(fig_output_path,file_version,"scatterplot_x.pdf"), width = 22, height = 20)

## Spearman test #####
method <- "spearman"
# Correlation matrix
cor_mat <- cor(x, method = method)
# Adjusted P-value matrix
p_adjusted_mat <- psych::corr.test(x,  method = method, adjust = "bonferroni")$p

# Rename row and col names
rownames(cor_mat) <- lab_mapping[rownames(cor_mat)]
colnames(cor_mat) <- lab_mapping[colnames(cor_mat)]
rownames(p_adjusted_mat) <- lab_mapping[rownames(p_adjusted_mat)]
colnames(p_adjusted_mat) <- lab_mapping[colnames(p_adjusted_mat)]

# Plot
pdf(here(fig_output_path,file_version,paste0("corrplot_x_",method,".pdf")))
corrplot::corrplot(cor_mat, p.mat = p_adjusted_mat, diag = FALSE,
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'yellow2', type='upper',
                   tl.srt = 45, , tl.cex = 1, cl.align.text = "l")
invisible(dev.off())

## Kendall test ####
method <- "kendall"
# Correlation matrix
cor_mat <- cor(x, method = method)
# Adjusted P-value matrix
p_adjusted_mat <- psych::corr.test(x,  method = method, adjust = "bonferroni")$p

# Rename row and col names
rownames(cor_mat) <- lab_mapping[rownames(cor_mat)]
colnames(cor_mat) <- lab_mapping[colnames(cor_mat)]
rownames(p_adjusted_mat) <- lab_mapping[rownames(p_adjusted_mat)]
colnames(p_adjusted_mat) <- lab_mapping[colnames(p_adjusted_mat)]

# Plot
pdf(here(fig_output_path,file_version,paste0("corrplot_x_",method,".pdf")))
corrplot::corrplot(cor_mat, p.mat = p_adjusted_mat, diag = FALSE,
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'yellow2', type='upper',
                   tl.srt = 45, , tl.cex = 1, cl.align.text = "l")
invisible(dev.off())

#------------------------------------------------------------------------------#
# Response ~ Explanatory variables ####
#------------------------------------------------------------------------------#
p <- plot_yx(y, x)
savePDF(p, file = here(fig_output_path,file_version,"scatterplot_yx.pdf"), width = 8, height = 10)

## Spearman test ####
method = "spearman"
# Correlation matrix
cor_mat <- cor(x, y, method = method)
# Adjusted P-value matrix
p_adjusted_mat <- psych::corr.test(x, y, method = method, adjust = "bonferroni")$p.adj

# Rename row and col names
rownames(cor_mat) <- lab_mapping[rownames(cor_mat)]
colnames(cor_mat) <- lab_mapping[colnames(cor_mat)]
rownames(p_adjusted_mat) <- lab_mapping[rownames(p_adjusted_mat)]
colnames(p_adjusted_mat) <- lab_mapping[colnames(p_adjusted_mat)]

# Plot
pdf(here(fig_output_path,file_version,paste0("corrplot_yx_",method,".pdf")), width = 10, height = 3.5)
corrplot::corrplot(cor_mat, p.mat = p_adjusted_mat,
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'yellow2',
                   tl.srt = 45, , tl.cex = 1, cl.align.text = "l", cl.ratio = 0.2)
invisible(dev.off())

## Kendall test ####
method = "kendall"
# Correlation matrix
cor_mat <- cor(x, y, method = method)
# Adjusted P-value matrix
p_adjusted_mat <- psych::corr.test(x, y, method = method, adjust = "bonferroni")$p.adj

# Rename row and col names
rownames(cor_mat) <- lab_mapping[rownames(cor_mat)]
colnames(cor_mat) <- lab_mapping[colnames(cor_mat)]
rownames(p_adjusted_mat) <- lab_mapping[rownames(p_adjusted_mat)]
colnames(p_adjusted_mat) <- lab_mapping[colnames(p_adjusted_mat)]

# Plot
pdf(here(fig_output_path,file_version,paste0("corrplot_yx_",method,".pdf")), width = 10, height = 3.5)
corrplot::corrplot(cor_mat, p.mat = p_adjusted_mat,
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'yellow2',
                   tl.srt = 45, , tl.cex = 1, cl.align.text = "l", cl.ratio = 0.2)
invisible(dev.off())


#------------------------------------------------------------------------------#
# sessionInfo.txt ####
#------------------------------------------------------------------------------#
writeLines(capture.output(sessionInfo()), here("sessionInfo","correlation_analysis_sessionInfo.txt"))
