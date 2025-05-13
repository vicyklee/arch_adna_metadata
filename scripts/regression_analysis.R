#------------------------------------------------------------------------------#
# REGRESSION ANALYSIS
#------------------------------------------------------------------------------#
here::i_am("scripts/regression_analysis.R")
library(tidyverse)
library(here)

# required for stat
library(betareg)
library(sandwich)
library(lmtest)
library(car)

# required for plotting
library(patchwork)

source(here("scripts/plot_style.R"))
source(here("scripts/data_wrangling.R"))
source(here("scripts/functions.R"))

# Declare components of output path (folders with the same name should exist)
# e.g. "fig_output_path/file_version/file_name"
fig_output_path <- "figures"
res_output_path <- "output"
file_version <- ""

#------------------------------------------------------------------------------#
# Fractional logistic regression - simple ####
#------------------------------------------------------------------------------#
fraclogit <- apply_fit_glm(Y = y, X = x, scale = T,
                           loop_x = T, p_adjust = T, family = binomial(link = "logit"),
                           p_adjust_method = "bonferroni", vce = "HC1")

#------------------------------------------------------------------------------#
## Results ####
#------------------------------------------------------------------------------#
# Summary table
fraclogit_summary <- fraclogit$summary %>%
  group_by(y) %>%
  mutate(alpha.adj = 0.05 / n(),
         ci.lower = estimate - qnorm(1 - alpha.adj / 2) * std.error,
         ci.upper = estimate + qnorm(1 - alpha.adj / 2) * std.error,
         y = as.factor(y),
         y = factor(y, levels = y_lab_order)) %>%
  ungroup() 

readr::write_csv(fraclogit_summary, here(res_output_path,file_version,"fraclogit_summary.csv"))

#------------------------------------------------------------------------------#
## Plots ####
#------------------------------------------------------------------------------#
# Heatmap 
heatmap_fraclogit <- plot_coef_heatmap(fraclogit$summary)
savePDF(heatmap_fraclogit,file = here(fig_output_path,file_version,"heatmap_fraclogit.pdf"), width = 8)

# CI plot
ci_plot_fraclogit <- ggplot(fraclogit_summary,
                            aes(x = estimate,
                                y = factor(term, levels=rev(sort(unique(term)))),
                                alpha = sig)) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) +
  geom_point(position = position_dodge(width = 0.5), colour = "#56b4e9") +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0, position = position_dodge(width = 0.5), colour = "#56b4e9") +
  facet_wrap(~y, nrow = 2, labeller = as_labeller(lab_mapping)) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey"),
    axis.ticks.x = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    panel.spacing.x = unit(4, "mm")) +
  scale_alpha_discrete(name = "Adjusted p-value",
                       breaks = c("*", ""),
                       labels = c(expression(""< 0.05), expression("">= 0.05)),
                       range = c(0.2, 1)) +
  xlab("Coefficient estimate") +
  scale_y_discrete(labels = lab_mapping)

savePDF(ci_plot_fraclogit, file = here(fig_output_path,file_version,"ci_coef_fraclogit.pdf"), width = 14, height = 5)

# Scatterplot with model fit
y_long <- y %>% 
  rownames_to_column(var = "CitationID") %>%
  pivot_longer(cols = 2:ncol(.), names_to = "y", values_to = "proportion") %>%
  mutate(y = as.factor(y),
         y = factor(y, levels = y_lab_order))

x_long <- as.data.frame(scale(x)) %>% 
  rownames_to_column(var = "CitationID") %>%
  pivot_longer(cols = 2:ncol(.), names_to = "term", values_to = "value")

yx_long <- full_join(y_long, x_long, relationship = "many-to-many", by = "CitationID")

fraclogit_pred <- lapply(fraclogit$model_list, function(z) as.data.frame(predict(z$model, cbind(y,scale(x)), type = "response"))
                          %>% rownames_to_column(var = "CitationID")) %>%
  bind_rows(.id = "formula") %>%
  separate(formula, c("y","term"), " ~ ") %>%
  rename(pred = colnames(.)[4]) %>%
  mutate(model = "fractional_logit") %>%
  left_join(yx_long) %>% mutate(model = as.factor(model),
                            model = factor(model, levels = c("fractional_logit","xbx")),
                            y = as.factor(y),
                            y = factor(y, levels = y_lab_order))

fraclogit_pred_summary <- fraclogit_pred %>%
  mutate(y = as.factor(y),
         y = factor(y, levels = y_lab_order)) %>%
  left_join(fraclogit_summary)

scatterplot_fraclogit <- ggplot(yx_long, aes(y = proportion, x = value)) +
  geom_point(size = 0.5, colour = "grey60") +
  geom_line(data = fraclogit_pred_summary, aes(y = pred, x = value, alpha = sig), linewidth = 0.6, colour = "#56b4e9") +
  ylab("Proportion") +
  xlab("Z-score") +
  facet_grid(y ~ term, scales = "free_x", labeller = as_labeller(lab_mapping)) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        panel.border = element_rect(colour = "grey", fill = NA),
        legend.title = element_text()) +
  scale_y_continuous(breaks=seq(0, 1, 0.5), limits = c(0,1)) +
  scale_alpha_discrete(name = "Adjusted p-value",
                       breaks = c("*", ""),
                       labels = c(expression(""< 0.05), expression("">= 0.05)),
                       range = c(0.3, 1))

savePDF(scatterplot_fraclogit, file = here(fig_output_path,file_version,"scatterplot_fraclogit.pdf"), width = 11, height = 10)

#------------------------------------------------------------------------------#
# Extended-support beta regression - simple ####
#------------------------------------------------------------------------------#
xbx <- apply_fit_betareg(Y = y, X = x, scale = T,
                         loop_x = T, p_adjust = T,
                         p_adjust_method = "bonferroni")

#------------------------------------------------------------------------------#
## Model comparison ####
#------------------------------------------------------------------------------#
model_names <- c("fractional_logit", "xbx")
model_disp_names <- c("Fractional logit", "XBX")

# logLik 
logLik <- lapply(list(fraclogit, xbx), function(x) stat_fn(x, metric = "logLik"))
names(logLik) <- model_names 
logLik <- bind_rows(logLik, .id = "model") %>%
  group_by(y, term) %>%
  arrange(y, term) %>%
  mutate(max = as.factor(ifelse(logLik == max(logLik), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names),
         y = as.factor(y),
         y = factor(y, levels = y_lab_order)) 

# AIC and BIC
aic <- lapply(list(fraclogit, xbx), function(x) stat_fn(x, metric = "AIC"))
names(aic) <- model_names 
aic <- bind_rows(aic, .id = "model") %>%
  group_by(y, term) %>%
  arrange(y, term) %>%
  mutate(min = as.factor(ifelse(AIC == min(AIC), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names ),
         y = as.factor(y),
         y = factor(y, levels = y_lab_order))

bic <- lapply(list(fraclogit, xbx), function(x) stat_fn(x, metric = "BIC"))
names(bic) <- model_names 
bic <- bind_rows(bic, .id = "model") %>%
  group_by(y, term) %>%
  arrange(y, term) %>%
  mutate(min = as.factor(ifelse(BIC == min(BIC), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names ),
         y = as.factor(y),
         y = factor(y, levels = y_lab_order))

# pseudoR2
pseudoR2 <- lapply(list(fraclogit, xbx), function(x) get_pseudoR2(x))
names(pseudoR2) <- model_names 
pseudoR2 <- bind_rows(pseudoR2, .id = "model") %>%
  group_by(y, term) %>%
  arrange(y, term) %>%
  mutate(max = as.factor(ifelse(pseudoR2 == max(pseudoR2), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names ),
         y = as.factor(y),
         y = factor(y, levels = y_lab_order))
#------------------------------------------------------------------------------#
## Results ####
#------------------------------------------------------------------------------#
summary <- bind_rows(fraclogit$summary, xbx$summary, .id = "model") %>%
  group_by(y, model) %>%
  mutate(alpha.adj = 0.05 / n(),
         ci.lower = estimate - qnorm(1 - alpha.adj / 2) * std.error,
         ci.upper = estimate + qnorm(1 - alpha.adj / 2) * std.error,
         y = as.factor(y),
         y = factor(y, levels = y_lab_order)) %>%
  ungroup() 

summary$model <- as.factor(summary$model)
levels(summary$model) <- model_names 

summary <- summary %>%
  left_join(aic, by = c("model","y","term")) %>%
  left_join(bic %>% select(-min), by = c("model","y","term")) %>%
  left_join(pseudoR2, by = c("model","y","term"))

readr::write_csv(summary, here(res_output_path,file_version,"summary_sr.csv"))

#------------------------------------------------------------------------------#
## Plots ####
#------------------------------------------------------------------------------#
# Log-(pseudo)likelihood plot
logLik_plot <- ggplot(logLik) +
  geom_point(aes(x = logLik, y = factor(term, levels=rev(sort(unique(term)))), colour = factor(model, levels=rev(sort(unique(model))))),
             position = position_dodge(width = 0.5)) +
  facet_wrap(~y, scales = "free_x", labeller = as_labeller(lab_mapping), nrow = 2) +
  scale_y_discrete(labels = lab_mapping) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  guides(alpha="none") +
  xlab("Log-(pseudo)likelihood")

# AIC plot
aic_plot <- ggplot(aic) +
  geom_point(aes(x = AIC, y = factor(term, levels=rev(sort(unique(term)))), colour = factor(model, levels=rev(sort(unique(model))))),
             position = position_dodge(width = 0.5)) +
  facet_wrap(~y, scales = "free_x", labeller = as_labeller(lab_mapping), nrow = 2) +
  scale_y_discrete(labels = lab_mapping) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  guides(alpha="none")

# BIC plot
bic_plot <- ggplot(bic) +
  geom_point(aes(x = BIC, y = factor(term, levels=rev(sort(unique(term)))), colour = factor(model, levels=rev(sort(unique(model))))),
             position = position_dodge(width = 0.5)) +
  facet_wrap(~y, scales = "free_x", labeller = as_labeller(lab_mapping), nrow = 2) +
  scale_y_discrete(labels = lab_mapping) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  guides(alpha="none")

# Pseudo R2 plot
pseudoR2_plot <- ggplot(pseudoR2) +
  geom_point(aes(x = pseudoR2, y = factor(term, levels=rev(sort(unique(term)))), colour = factor(model, levels=rev(sort(unique(model))))),
             position = position_dodge(width = 0.5)) +
  facet_wrap(~y, scales = "free_x", labeller = as_labeller(lab_mapping), nrow = 2) +
  scale_y_discrete(labels = lab_mapping) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks=seq(0, 1, 0.5), limits = c(0,1)) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  guides(alpha="none") +
  xlab(bquote("Pseudo" ~ R^2))

# All model comparison statistics
modelCompar_plot <- patchwork::wrap_plots(list(logLik_plot, aic_plot, bic_plot, pseudoR2_plot), nrow = 4) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(tag_levels = 'A') & theme(plot.tag=element_text(size = 12, face = "bold"))

savePDF(modelCompar_plot, file = here(fig_output_path,file_version,"modelCompar_fraclogit_xbx.pdf"), width = 14, height = 16)

# CI plots
ci_plot <- ggplot(summary, aes(x = estimate, y = factor(term, levels=rev(sort(unique(term)))),
                               colour = factor(model, levels=rev(sort(unique(model)))), alpha = sig, shape = min)) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~y, nrow = 2, labeller = as_labeller(lab_mapping)) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey"),
    axis.ticks.x = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    panel.spacing.x = unit(4, "mm")) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  scale_alpha_discrete(name = "Adjusted p-value",
                       breaks = c("*", ""),
                       labels = c(expression(""< 0.05), expression("">= 0.05)),
                       range = c(0.2, 1)) +
  scale_shape_manual(values = c(19,1),
                     name = "min(AIC)",
                     breaks = c("1","0"),
                     labels = c("TRUE", "FALSE")) +
  xlab("Coefficient estimate") +
  scale_y_discrete(labels = lab_mapping) +
  guides(col = guide_legend(order = 1), alpha = guide_legend(order = 2), shape = guide_legend(order = 3))

savePDF(ci_plot, file = here(fig_output_path,file_version,"ci_coef_fraclogit_xbx.pdf"), width = 14, height = 5)

# Scatterplot with model fit
xbx_pred <- lapply(xbx$model_list, function(z) as.data.frame(predict(z$model, cbind(y,scale(x)), type = "response"))
                   %>% rownames_to_column(var = "CitationID")) %>%
  bind_rows(.id = "formula") %>%
  separate(formula, c("y","term"), " ~ ") %>%
  rename(pred = colnames(.)[4]) %>%
  mutate(model = "xbx") %>%
  left_join(yx_long)

mod_pred <- bind_rows(fraclogit_pred,xbx_pred) %>%
  mutate(model = as.factor(model),
         model = factor(model, levels = c("fractional_logit","xbx")),
         y = as.factor(y),
         y = factor(y, levels = y_lab_order)) %>%
  left_join(summary)

scatterplot <- ggplot(yx_long, aes(y = proportion, x = value)) +
  geom_point(size = 0.5, colour = "grey60") +
  geom_line(data = mod_pred, aes(y = pred, x = value, colour = model, alpha = sig, linetype = min), linewidth = 0.6) +
  ylab("Proportion") +
  xlab("Z-score") +
  facet_grid(y ~ term, scales = "free_x", labeller = as_labeller(lab_mapping)) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0),
        panel.border = element_rect(colour = "grey", fill = NA),
        legend.title = element_text()) +
  scale_y_continuous(breaks=seq(0, 1, 0.5), limits = c(0,1)) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  scale_alpha_discrete(name = "Adjusted p-value",
                       breaks = c("*", ""),
                       labels = c(expression(""< 0.05), expression("">= 0.05)),
                       range = c(0.3, 1)) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        name = "min(AIC)",
                        breaks = c("1","0"),
                        labels = c("TRUE", "FALSE")) +
  guides(col = guide_legend(order = 1), alpha = guide_legend(order = 2), shape = guide_legend(order = 3))

savePDF(scatterplot, file = here(fig_output_path,file_version,"scatterplot_fraclogit_xbx.pdf"), width = 11, height = 10)

#------------------------------------------------------------------------------#
# Multiple regression ####
#------------------------------------------------------------------------------#
# Fractional logistic regression
fraclogit_mr <- apply_fit_glm(Y = y, X = x, scale = F,
                               loop_x = F, p_adjust = F,
                               family = binomial(link = "logit"), vce = "HC1")

# VIF to check collinearity
vif_fraclogit_mr <- plot_vif(get_vif_tbl(fraclogit_mr) %>% mutate(y = as.factor(y), y = factor(y, levels = c(y_lab_order))), palette = x_cols) + ggtitle("Fractional logistic regression")
savePDF(vif_fraclogit_mr, file = here(fig_output_path,file_version,"vifplot_fraclogit_mr.pdf"), width = 8, height = 6)
# The models with significant coefficient estimates are inflated in variance due to multicollinearity

# Extended-support beta regression
xbx_mr <- apply_fit_betareg(Y = y, X = x, scale = T,
                            loop_x = F, p_adjust = F)

# VIF to check collinearity
vif_xbx_mr <- plot_vif(get_vif_tbl(xbx_mr) %>% mutate(y = as.factor(y), y = factor(y, levels = c(y_lab_order))), palette = x_cols) + ggtitle("Extended-support beta regression")
savePDF(vif_xbx_mr, file = here(fig_output_path,file_version,"vifplot_xbx_mr.pdf"), width = 8, height = 6)
# The models with significant coefficient estimates are inflated in variance due to multicollinearity

#------------------------------------------------------------------------------#
# Fractional logistic regression - multiple with PCs ####
#------------------------------------------------------------------------------#
fraclogit_mr_pc <- apply_fit_glm(Y = y, X = pca_x$ind$coord[,1:3], scale = F,
                                  loop_x = F, p_adjust = F,
                                  family = binomial(link = "logit"), vce = "HC1")
# VIF to check collinearity
vif_fraclogit_mr_pc <- plot_vif(get_vif_tbl(fraclogit_mr_pc) %>% mutate(y = as.factor(y), y = factor(y, levels = c(y_lab_order))), palette = pc_cols1) + ggtitle("Fractional logistic regression")
savePDF(vif_fraclogit_mr_pc, file = here(fig_output_path,file_version,"vifplot_fraclogit_mr_pc.pdf"), width = 8, height = 6)

#------------------------------------------------------------------------------#
## Results ####
#------------------------------------------------------------------------------#
# Wald test for linear hypothesis
waldtest_fraclogit_mr_pc <- bind_rows(
  lapply(fraclogit_mr_pc$model_list, function (x)
    as.data.frame(
      car::linearHypothesis(x$model, vcov. = sandwich::vcovHC(x$model, type = "HC1"), hypothesis.matrix = c("Dim.1 = 0","Dim.2 = 0", "Dim.3 = 0"), test = "Chisq")
    )
  )
  , .id = "y"
)

waldtest_fraclogit_mr_pc_res <- waldtest_fraclogit_mr_pc %>%
  select(-c(Res.Df,Df)) %>%
  drop_na() %>%
  as_tibble()

# Summary
fraclogit_summary_mr_pc <- fraclogit_mr_pc$summary %>%
  group_by(y) %>%
  mutate(ci.lower = estimate - qnorm(1 - 0.05 / 2) * std.error,
         ci.upper = estimate + qnorm(1 - 0.05 / 2) * std.error) %>%
  ungroup() %>%
  full_join(waldtest_fraclogit_mr_pc_res) %>%
  mutate(sig_modified = ifelse(sig != "", "*",""),
         waldtest_sig = ifelse(`Pr(>Chisq)` < 0.05, "TRUE", "FALSE"),
         y = factor(y, levels = y_lab_order))

readr::write_csv(fraclogit_summary_mr_pc, here(res_output_path,file_version,"fraclogit_summary_mr_pc.csv"))

#------------------------------------------------------------------------------#
## Plots ####
#------------------------------------------------------------------------------#
# Heatmap
heatmap_fraclogit_mr_pc <- plot_coef_heatmap_waldtest(fraclogit_summary_mr_pc) +
  theme(legend.key.height = unit(0.15,"in"))
savePDF(heatmap_fraclogit_mr_pc,file = here(fig_output_path,file_version,"heatmap_fraclogit_mr_pc.pdf"), width = 8)

# CI plot
ci_mr_pc_plot_fraclogit <- ggplot(fraclogit_summary_mr_pc,
                                  aes(x = estimate, y = factor(term, levels=rev(sort(unique(term)))),
                                      alpha = sig_modified, shape = waldtest_sig)) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) +
  geom_point(position = position_dodge(width = 0.5), colour = "#56b4e9") +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0, position = position_dodge(width = 0.5), colour = "#56b4e9") +
  facet_wrap(~y, nrow = 2, labeller = as_labeller(lab_mapping)) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey"),
    axis.ticks.x = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    panel.spacing.x = unit(4, "mm")) +
  scale_alpha_discrete(name = "Coefficient\np-value",
                       breaks = c("*", ""),
                       labels = c(expression(""< 0.05), expression("">= 0.05)),
                       range = c(0.2, 1)) +
  scale_shape_manual(values = c(19,1),
                     name = "Wald test\non model fit",
                     breaks = c("TRUE","FALSE"),
                     labels = c(expression("p" < 0.05), expression("p" >= 0.05))) +
  xlab("Coefficient estimate") +
  scale_y_discrete(labels = lab_mapping) +
  guides(alpha = guide_legend(order = 2), shape = guide_legend(order = 3))

savePDF(ci_mr_pc_plot_fraclogit, file = here(fig_output_path,file_version,"ci_coef_fraclogit_mr_pc.pdf"), width = 14, height = 5)

#------------------------------------------------------------------------------#
# Extended-support beta regression - multiple with PCs ####
#------------------------------------------------------------------------------#
xbx_mr_pc <- apply_fit_betareg(Y = y, X = pca_x$ind$coord[,1:3], scale = F,
                               loop_x = F, p_adjust = F)
# VIF to check collinearity
vif_xbx_mr_pc <- plot_vif(get_vif_tbl(xbx_mr_pc) %>% mutate(y = as.factor(y), y = factor(y, levels = c(y_lab_order))), palette = pc_cols1) + ggtitle("Extended-support beta regression")
savePDF(vif_xbx_mr_pc, file = here(fig_output_path,file_version,"vifplot_xbx_mr_pc.pdf"), width = 8, height = 6)

#------------------------------------------------------------------------------#
## Model comparison ####
#------------------------------------------------------------------------------#
waldtest_xbx_mr_pc <- bind_rows(
  lapply(xbx_mr_pc$model_list, function (x)
    as.data.frame(
      car::linearHypothesis(x$model, hypothesis.matrix = c("Dim.1 = 0","Dim.2 = 0", "Dim.3 = 0"), test = "Chisq")
    )
  )
  , .id = "y"
)

waldtest_res <- bind_rows(waldtest_fraclogit_mr_pc %>% mutate(model = "fractional_logit"),
                          waldtest_xbx_mr_pc %>% mutate(model = "xbx")) %>% 
  select(-c(Res.Df,Df)) %>%
  drop_na() %>%
  as_tibble()

# logLik
logLik_mr_pc <- lapply(list(fraclogit_mr_pc, xbx_mr_pc), function(x) stat_fn(x, metric = "logLik"))
names(logLik_mr_pc) <- model_names 
logLik_mr_pc <- bind_rows(logLik_mr_pc, .id = "model") %>%
  group_by(y) %>%
  arrange(y) %>%
  mutate(max = as.factor(ifelse(logLik == max(logLik), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names )) %>%
  full_join(waldtest_res) %>%
  mutate(waldtest_sig = ifelse(`Pr(>Chisq)` < 0.05, "TRUE", "FALSE"),
         y = factor(y, levels = y_lab_order)) %>%
  select(-term)

# AIC and BIC
aic_mr_pc <- lapply(list(fraclogit_mr_pc, xbx_mr_pc), function(x) stat_fn(x, metric = "AIC"))
names(aic_mr_pc) <- model_names 
aic_mr_pc <- bind_rows(aic_mr_pc, .id = "model") %>%
  group_by(y) %>%
  arrange(y) %>%
  mutate(min = as.factor(ifelse(AIC == min(AIC), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names )) %>%
  full_join(waldtest_res) %>%
  mutate(waldtest_sig = ifelse(`Pr(>Chisq)` < 0.05, "TRUE", "FALSE"),
         y = factor(y, levels = y_lab_order)) %>%
  select(-term)

bic_mr_pc <- lapply(list(fraclogit_mr_pc, xbx_mr_pc), function(x) stat_fn(x, metric = "BIC"))
names(bic_mr_pc) <- model_names 
bic_mr_pc <- bind_rows(bic_mr_pc, .id = "model") %>%
  group_by(y) %>%
  arrange(y) %>%
  mutate(min = as.factor(ifelse(BIC == min(BIC), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names )) %>%
  full_join(waldtest_res) %>%
  mutate(waldtest_sig = ifelse(`Pr(>Chisq)` < 0.05, "TRUE", "FALSE"),
         y = factor(y, levels = y_lab_order)) %>%
  select(-term)

# pseudoR2
pseudoR2_mr_pc <- lapply(list(fraclogit_mr_pc, xbx_mr_pc), function(x) get_pseudoR2(x))
names(pseudoR2_mr_pc) <- model_names 
pseudoR2_mr_pc <- bind_rows(pseudoR2_mr_pc, .id = "model") %>%
  group_by(y) %>%
  arrange(y) %>%
  mutate(max = as.factor(ifelse(pseudoR2 == max(pseudoR2), 1,0)),
         model = as.factor(model),
         model = factor(model, levels = model_names )) %>%
  full_join(waldtest_res) %>%
  mutate(waldtest_sig = ifelse(`Pr(>Chisq)` < 0.05, "TRUE", "FALSE"),
         y = factor(y, levels = y_lab_order)) %>%
  select(-term)

#------------------------------------------------------------------------------#
## Results ####
#------------------------------------------------------------------------------#
summary_mr_pc <- bind_rows(fraclogit_mr_pc$summary, xbx_mr_pc$summary, .id = "model") %>%
  group_by(y, model) %>%
  mutate(ci.lower = estimate - qnorm(1 - 0.05 / 2) * std.error,
         ci.upper = estimate + qnorm(1 - 0.05 / 2) * std.error) %>%
  ungroup() 

summary_mr_pc$model <- as.factor(summary_mr_pc$model)
levels(summary_mr_pc$model) <- model_names 

summary_mr_pc <- summary_mr_pc %>%
  left_join(aic_mr_pc) %>%
  left_join(bic_mr_pc %>% select(-min)) %>%
  left_join(pseudoR2_mr_pc) %>%
  mutate(sig_modified = ifelse(sig != "", "*",""),
         waldtest_sig = ifelse(`Pr(>Chisq)` < 0.05, "TRUE", "FALSE"),
         modelFit_pass = ifelse(min == "1" & waldtest_sig == "TRUE", "TRUE", "FALSE"),
         y = factor(y, levels = y_lab_order))

readr::write_csv(summary_mr_pc, here(res_output_path,file_version,"summary_mr_pc.csv"))

#------------------------------------------------------------------------------#
## Plots ####
#------------------------------------------------------------------------------#
# Log-(pseudo)likelihood plot
logLik_mr_pc_plot <- ggplot(logLik_mr_pc) +
  geom_point(aes(x = logLik, y = factor(y, levels=rev(sort(unique(y)))),
                 colour = factor(model, levels=rev(sort(unique(model)))),
                 shape = waldtest_sig),
             position = position_dodge(width = 0.5)) +
  scale_y_discrete(labels = lab_mapping) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  scale_shape_manual(values = c(19,1),
                     name = "Wald test",
                     breaks = c("TRUE","FALSE"),
                     labels = c(expression("p" < 0.05), expression("p" >= 0.05))) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  guides(alpha="none", col = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  xlab("Log-(pseudo)likelihood")

# AIC plot
aic_mr_pc_plot <- ggplot(aic_mr_pc) +
  geom_point(aes(x = AIC, y = factor(y, levels=rev(sort(unique(y)))),
                 colour = factor(model, levels=rev(sort(unique(model)))),
                 shape = waldtest_sig),
             position = position_dodge(width = 0.5)) +
  scale_y_discrete(labels = lab_mapping) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  scale_shape_manual(values = c(19,1),
                     name = "Wald test",
                     breaks = c("TRUE","FALSE"),
                     labels = c(expression("p" < 0.05), expression("p" >= 0.05))) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  guides(alpha="none", col = guide_legend(order = 1), shape = guide_legend(order = 2))

# BIC plot
bic_mr_pc_plot <- ggplot(bic_mr_pc) +
  geom_point(aes(x = BIC, y = factor(y, levels=rev(sort(unique(y)))),
                 colour = factor(model, levels=rev(sort(unique(model)))),
                 shape = waldtest_sig),
             position = position_dodge(width = 0.5)) +
  scale_y_discrete(labels = lab_mapping) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  scale_shape_manual(values = c(19,1),
                     name = "Wald test",
                     breaks = c("TRUE","FALSE"),
                     labels = c(expression("p" < 0.05), expression("p" >= 0.05))) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  guides(alpha="none", col = guide_legend(order = 1), shape = guide_legend(order = 2))

# Pseudo R2 plot
pseudoR2_mr_pc_plot <- ggplot(pseudoR2_mr_pc) +
  geom_point(aes(x = pseudoR2, y = factor(y, levels=rev(sort(unique(y)))),
                 colour = factor(model, levels=rev(sort(unique(model)))),
                 shape = waldtest_sig),
             position = position_dodge(width = 0.5)) +
  scale_y_discrete(labels = lab_mapping) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE), breaks=seq(0, 1, 0.5), limits = c(0,1)) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(4, "mm")) +
  scale_shape_manual(values = c(19,1),
                     name = "Wald test",
                     breaks = c("TRUE","FALSE"),
                     labels = c(expression("p" < 0.05), expression("p" >= 0.05))) +
  scale_alpha_discrete(range = c(0.3, 1)) +
  guides(alpha="none", col = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  xlab(bquote("Pseudo" ~ R^2))

# All model comparison statistics
modelCompar_mr_pc_plot <- patchwork::wrap_plots(list(logLik_mr_pc_plot, aic_mr_pc_plot + remove_y, bic_mr_pc_plot + remove_y, pseudoR2_mr_pc_plot + remove_y), nrow = 1) + patchwork::plot_layout(guides = "collect")
savePDF(modelCompar_mr_pc_plot, file = here(fig_output_path,file_version,"modelCompar_fraclogit_xbx_mr_pc.pdf"), width = 10, height = 5)

# CI plot
ci_mr_pc_plot <- ggplot(summary_mr_pc, aes(x = estimate, y = factor(term, levels=rev(sort(unique(term)))),
                                           colour = factor(model, levels=rev(sort(unique(model)))), alpha = sig_modified, shape = modelFit_pass)) +
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.2) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(xmin = ci.lower, xmax = ci.upper), width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~y, nrow = 2, labeller = as_labeller(lab_mapping)) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_rect(fill = NA, colour = "grey"),
    axis.ticks.x = element_line(colour = "grey"),
    axis.title.y = element_blank(),
    panel.spacing.x = unit(4, "mm")) +
  scale_colour_manual(values = c("#56b4e9", "#e69f00"),
                      name = "Model",
                      breaks = model_names ,
                      labels = model_disp_names ) +
  scale_alpha_discrete(name = "P-value",
                       breaks = c("*", ""),
                       labels = c(expression(""< 0.05), expression("">= 0.05)),
                       range = c(0.2, 1)) +
  scale_shape_manual(values = c(19,1),
                     name = "min(AIC) &\nPr(>Chisq) < 0.05",
                     breaks = c("TRUE","FALSE"),
                     labels = c("TRUE", "FALSE")) +
  xlab("Coefficient estimate") +
  scale_y_discrete(labels = lab_mapping) +
  guides(col = guide_legend(order = 1), alpha = guide_legend(order = 2), shape = guide_legend(order = 3))

savePDF(ci_mr_pc_plot, file = here(fig_output_path,file_version,"ci_coef_fraclogit_xbx_mr_pc.pdf"), width = 14, height = 5)

#------------------------------------------------------------------------------#
# sessionInfo.txt ####
#------------------------------------------------------------------------------#
writeLines(capture.output(sessionInfo()), here("sessionInfo","regression_analysis_sessionInfo.txt"))