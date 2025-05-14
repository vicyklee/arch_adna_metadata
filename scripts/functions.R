#------------------------------------------------------------------------------#
# Plotting overview ####
#------------------------------------------------------------------------------#
plot_x_sample <- function(x, xlab) {
  ggplot(metadata, aes(fill = dataset)) +
    geom_col(aes(x = !!sym(x), y = factor(CitationID, levels = rev(CitationID_order))),
             position = "dodge") +
    xlab(xlab) +
    ylab("Publication") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_fill_manual(values = MetBrewer::met.brewer("Hokusai2", n = 2), breaks = c("new", "all"))
}

plot_x <- function(x, xlab, fill = "grey") {
  ggplot(metadata %>% filter(dataset == "all")) +
    geom_col(aes(x = !!sym(x), y = factor(CitationID, levels = rev(CitationID_order))),
             position = "dodge", fill = fill) +
    xlab(xlab) +
    ylab("Publication") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
}

plot_x_dist <- function(x, xlab, fill = "grey") {
  ggplot(metadata %>% filter(dataset == "all")) +
    geom_histogram(aes(x = !!sym(x)), fill = fill) +
    # geom_density(aes(x = !!sym(x)), fill = fill) +
    xlab(xlab) +
    ylab("Count") +
    # ylab("Density") +
    theme_minimal() 
}

plot_yx <- function(y, x) {
  y <- y %>% 
    rownames_to_column(var = "obs") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "y", values_to = "proportion")

  x <- x %>%
    rownames_to_column(var = "obs") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "x", values_to = "value")
  
  yx <- full_join(y, x, relationship = "many-to-many", by = "obs")
  
  p <- ggplot(yx, aes(y = proportion, x = value)) +
    geom_point(size = 1) +
    # geom_smooth(method="glm", formula = 'y ~ x', method.args=list(family=family)) +
    ylab("Proportion") +
    facet_grid(y ~ x, scales = "free") +
    theme_minimal() +
    theme(strip.text.y = element_text(angle = 0),
          panel.border = element_rect(colour = "grey", fill = NA),
          axis.title.x = element_blank()) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE))
  
  return(p)
}

#------------------------------------------------------------------------------#
# Fit a (quasi-)binomial model ####
#------------------------------------------------------------------------------#
fit_glm <- function(y, x, data, family = family, ...){
  # y: a string of the name of the response variable
  # x: a vector of strings of the names of predictor variables
  # no interaction terms in this function, only addition
  
  formula_str <- paste(y, "~", paste(x, collapse = " + "), sep = " ")
  
  output <- tryCatch(
    {
      model <- glm(formula_str, family = family, data = data, ...)
      return(list(model = model, warning = NULL))
    },
    warning = function(w) {
      model <- glm(formula_str, family = family, data = data, ...)
      return(list(model = model, warning = w))
    }
  )
  
  return(output)
}

#------------------------------------------------------------------------------#
# Apply univariate (quasi-)binomial (multiple) regression ####
#------------------------------------------------------------------------------#
apply_fit_glm <- function(Y, X, scale = F, family = family, exclude_x = NULL, loop_x = F, p_adjust = F, p_adjust_method = NULL, vce = NULL, ...) {
  # Y: a dataframe of response variables
  # X: a dataframe of explanatory variables
  # family: glm() family 
  # exclude_x: any predictor variables to be excluded from the dataframe 
  # loop_x: looping over x to fit simple regression for every pair of response and predictor
  # p_adjust: adjusted P-value for multiple testing
  # p_adjust_method: "bonferroni"/"sidak"
  # ...: any other parameters available for glm()
  
  
  if (!is.null(exclude_x)) { X <- X %>% select(-all_of(exclude_x)) } # remove selected explanatory variable(s) from the X dataframe
  if (scale == T) { X <- as_tibble(scale(X),rownames = NA) } # scale the values of the explanatory variables
  
  Y.colnames <- colnames(Y)
  X.colnames <- colnames(X)
  data <- cbind(Y, X)
  
  if (loop_x == T) {
    pairs <- crossing(Y.colnames,X.colnames) %>% mutate(names = paste(Y.colnames, "~", X.colnames))
    model_list <- lapply(1:nrow(pairs), function(x) fit_glm(y = pairs[x,1], x = pairs[x,2], data = data, family = family, ...))
    names(model_list) <- pairs$names
    # Check warnings
    indices_with_warnings <- which(sapply(model_list, function(model) !is.null(model$warning)))
    models_with_warnings <- pairs$names[indices_with_warnings]
  } else {
    # Multiple regression
    # Fit the model to the data for every response variable
    model_list <- lapply(Y.colnames, function(x) fit_glm(y = x, x = X.colnames, data = data, family = family, ...))
    names(model_list) <- Y.colnames
    # Check warnings
    indices_with_warnings <- which(sapply(model_list, function(model) !is.null(model$warning)))
    models_with_warnings <- Y.colnames[indices_with_warnings]
  }
  
  # Summary for every response variable
  summary_list <- lapply(model_list, function(x) broom::tidy(x$model))
  
  if (!is.null(vce)) {
    summary_list <- lapply(model_list, function(x) broom::tidy(lmtest::coeftest(x$model, vcov = sandwich::vcovHC(x$model, type=vce))))
  }
  
  summary <- bind_rows(summary_list,.id = "y") %>%
    filter(term != "(Intercept)")  %>%
    mutate(sig = ifelse(p.value < 0.001, "***",ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", ""))))
  
  if (loop_x == T) {
    summary$y <- sub(" ~.*", "", summary$y)
    
    if (p_adjust == T) {
      
      if (is.null(p_adjust_method)) { p_adjust_method = "bonferroni" }
      
      pValue_list <- split(summary$p.value, summary$y)
      
      if (p_adjust_method == "bonferroni") {
        adjPValue_list <- lapply(pValue_list, p.adjust, method = "bonferroni")
      }
      if (p_adjust_method == "sidak") {
        adjPValue_list <- lapply(pValue_list, function(x) 1-(1-x)^(length(x)))
      }
      
      summary <- cbind(summary, p.adj = unlist(adjPValue_list))
      row.names(summary) <- NULL
      summary <- summary %>%
        select(-sig) %>% 
        mutate(sig = ifelse(p.adj < 0.001, "***",
                            ifelse(p.adj < 0.01, "**",
                                   ifelse(p.adj < 0.05, "*",""))))
    }
  }
  
  return(list(model_list = model_list, models_with_warnings = models_with_warnings, summary = summary, Y = Y, X = X, data = data, family = family))
} 

#------------------------------------------------------------------------------#
# Fit beta regression ####
#------------------------------------------------------------------------------#
fit_betareg <- function(y, x, data, vardisp = F, ...){
  # y: a string of the name of the response variable
  # x: a vector of strings of the names of predictor variables
  # no interaction terms in this function, only addition
  # vardisp: variable dispersion model using the same regressors to account for heteroskedasticity if true
  
  if (vardisp == T) {
    formula_str <- paste(y, "~", paste(x, collapse = " + "), "|", paste(x, collapse = " + "), sep = " ")
  } else {
    formula_str <- paste(y, "~", paste(x, collapse = " + "), sep = " ")
  }
  
  output <- tryCatch(
    {
      model <- betareg::betareg(formula = formula_str, data = data, ...)
      return(list(model = model, warning = NULL))
    },
    warning = function(w) {
      model <- betareg::betareg(formula = formula_str, data = data, ...)
      return(list(model = model, warning = w))
    }
  )
  
  return(output)
}

#------------------------------------------------------------------------------#
# Apply beta regression ####
#------------------------------------------------------------------------------#
apply_fit_betareg <- function(Y, X, scale = F, exclude_x = NULL, loop_x = F, p_adjust = F, p_adjust_method = NULL, ...) {
  # Y: a dataframe of response variables
  # X: a dataframe of explanatory variables
  # exclude_x: any predictor variables to be excluded from the dataframe 
  # loop_x: looping over x to fit simple regression for every pair of response and predictor
  # p_adjust: adjusted P-value for multiple testing
  # p_adjust_method: "bonferroni"/"sidak"
  # ...: any other parameters available for betareg()
  
  
  if (!is.null(exclude_x)) { X <- X %>% select(-all_of(exclude_x)) } # remove selected explanatory variable(s) from the X dataframe
  if (scale == T) { X <- as_tibble(scale(X),rownames = NA) } # scale the values of the explanatory variables
  
  Y.colnames <- colnames(Y)
  X.colnames <- colnames(X)
  data <- cbind(Y, X)
  
  if (loop_x == T) {
    pairs <- crossing(Y.colnames,X.colnames) %>% mutate(names = paste(Y.colnames, "~", X.colnames))
    model_list <- lapply(1:nrow(pairs), function(x) fit_betareg(y = pairs[x,1], x = pairs[x,2], data = data, ...))
    names(model_list) <- pairs$names
    # Check warnings
    indices_with_warnings <- which(sapply(model_list, function(model) !is.null(model$warning)))
    models_with_warnings <- pairs$names[indices_with_warnings]
  } else {
    # Multiple regression
    # Fit the model to the data for every response variable
    model_list <- lapply(Y.colnames, function(x) fit_betareg(y = x, x = X.colnames, data = data, ...))
    names(model_list) <- Y.colnames
    # Check warnings
    indices_with_warnings <- which(sapply(model_list, function(model) !is.null(model$warning)))
    models_with_warnings <- Y.colnames[indices_with_warnings]
  }
  
  # Summary for every response variable
  summary_list <- lapply(model_list, function(x) broom::tidy(x$model))
  summary <- bind_rows(summary_list,.id = "y") %>%
    filter(component == "mu",
           term != "(Intercept)") %>%
    mutate(sig = ifelse(p.value < 0.001, "***",ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", ""))))
  
  summary_phi <- bind_rows(summary_list,.id = "y") %>%
    filter(component == "phi",
           term != "(Intercept)") %>%
    mutate(sig = ifelse(p.value < 0.001, "***",ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", ""))))
  
  if (loop_x == T) {
    summary$y <- sub(" ~.*", "", summary$y)
    summary_phi$y <- sub(" ~.*", "", summary_phi$y)
    
    if (p_adjust == T) {
      
      if (is.null(p_adjust_method)) { p_adjust_method = "bonferroni" }
      
      pValue_list <- split(summary$p.value, summary$y)
      
      if (p_adjust_method == "bonferroni") {
        adjPValue_list <- lapply(pValue_list, p.adjust, method = "bonferroni")
      }
      if (p_adjust_method == "sidak") {
        adjPValue_list <- lapply(pValue_list, function(x) 1-(1-x)^(length(x)))
      }
      
      summary <- cbind(summary, p.adj = unlist(adjPValue_list))
      row.names(summary) <- NULL
      summary <- summary %>%
        select(-sig) %>% 
        mutate(sig = ifelse(p.adj < 0.001, "***",
                            ifelse(p.adj < 0.01, "**",
                                   ifelse(p.adj < 0.05, "*", ""))))
    }
  }
  
  return(list(model_list = model_list, models_with_warnings = models_with_warnings, summary = summary, summary_phi = summary_phi, Y = Y, X = X, data = data))
}

#------------------------------------------------------------------------------#
# Plot regression results ####
#------------------------------------------------------------------------------#
plot_coef_heatmap <- function(summary, limits = NULL) {
  
  summary <- summary %>%
    filter(term != "(Intercept)") %>%
    mutate(y = as.factor(y),
           y = factor(y, levels = y_lab_order)) %>%
    arrange(y,term) %>%
    group_by(term) %>%
    mutate(y0 = 1:n()) %>%
    ungroup() %>%
    group_by(y) %>%
    mutate(term0 = n():1) %>%
    ungroup() 
  
  if (is.null(limits))
  {
    max_unsigned_estimate <- max(abs(summary$estimate))
    limits <- c(-max_unsigned_estimate, max_unsigned_estimate)
  }
  
  ggplot(
    summary, 
    aes(y = factor(term, levels=rev(sort(unique(term)))),, x = y)
  ) + 
    geom_tile(colour = "grey", aes(fill = estimate), linewidth = 0.5) +
    geom_text(aes(label = sig), colour = "grey30", size = 5, vjust = 0.8) +
    labs(fill=expression(hat(beta))) +
    scale_fill_gradientn(colors = c("#F7AA14FF","#F5D000FF","#F7E690FF","white","#1BB6AFFF","#088BBEFF","#172869FF"), limits = limits) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=0),
          panel.grid = element_blank(),
          axis.title = element_blank())+
    scale_y_discrete(labels = lab_mapping) +
    scale_x_discrete(labels = lab_mapping, position = "top") +
    # scale_alpha_discrete(name = "P-value",
    #                      breaks = c("*", ""),
    #                      labels = c(expression(""< 0.05), expression("">= 0.05)),
    #                      range = c(0, 1))  +
    coord_equal() +
    guides(size = "none")
}

plot_coef_heatmap_waldtest <- function(summary, limits = NULL) {
  
  summary <- summary %>%
    filter(term != "(Intercept)") %>%
    mutate(y = as.factor(y),
           y = factor(y, levels = y_lab_order)) %>%
    arrange(y,term) %>%
    group_by(term) %>%
    mutate(y0 = 1:n()) %>%
    ungroup() %>%
    group_by(y) %>%
    mutate(term0 = n():1) %>%
    ungroup() 
  
  if (is.null(limits))
  {
    max_unsigned_estimate <- max(abs(summary$estimate))
    limits <- c(-max_unsigned_estimate, max_unsigned_estimate)
  }
  
  ggplot(
    summary, 
    aes(y = factor(term, levels=rev(sort(unique(term)))),, x = y)
  ) + 
    geom_tile(colour = "grey", aes(fill = estimate), linewidth = 0.5) +
    geom_text(aes(label = sig, alpha = waldtest_sig), colour = "grey30", size = 5, vjust = 0.8) +
    labs(fill=expression(hat(beta))) +
    scale_fill_gradientn(colors = c("#F7AA14FF","#F5D000FF","#F7E690FF","white","#1BB6AFFF","#088BBEFF","#172869FF"), limits = limits) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, hjust=0),
          panel.grid = element_blank(),
          axis.title = element_blank())+
    scale_y_discrete(labels = lab_mapping) +
    scale_x_discrete(labels = lab_mapping, position = "top") +
    scale_alpha_discrete(name = "P-value",
                         breaks = c("*", ""),
                         labels = c(expression(""< 0.05), expression("">= 0.05)),
                         range = c(0, 1))  +
    coord_equal() +
    guides(size = "none",alpha = "none")
}

#------------------------------------------------------------------------------#
# Plot VIF ####
#------------------------------------------------------------------------------#
get_vif_tbl <- function(object){
  vif_list <- lapply(1:length(object$model_list), function(i) { car::vif(object$model_list[[i]]$model) })
  vif <- do.call(rbind, vif_list)
  row.names(vif) <- names(object$model_list)
  vif <- as.data.frame(vif) %>% rownames_to_column(var = "y") %>% as_tibble()
  return(vif)
}

plot_vif <- function(vif_tbl, palette){
  vif_tbl <- vif_tbl %>%
    pivot_longer(cols = 2:ncol(.), names_to = "x", values_to = "VIF")
  
  ggplot(vif_tbl) +
    geom_col(aes(x = VIF,
                 y = factor(x, levels=rev(sort(unique(x)))), fill = x),
             position = "dodge") +
    geom_vline(xintercept=c(5, 10), colour = "red", linetype='dashed') +
    # ylab("Explanatory variable") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.y = element_blank()) +
    guides(fill = guide_legend(title = NULL)) +
    facet_wrap(~y, labeller = as_labeller(lab_mapping), scales = "free_x") +
    scale_fill_manual(values = palette) +
    scale_y_discrete(labels = lab_mapping)
}

#------------------------------------------------------------------------------#
# Save PDF ####
#------------------------------------------------------------------------------#
savePDF <- function(p, file, width, height, ...) {
  pdf(file = file, width = width, height = height, ...)
  pdf.options(encoding = 'CP1250')
  print(p)
  invisible(dev.off())
}

#------------------------------------------------------------------------------#
# Generate model fit statistics ####
#------------------------------------------------------------------------------#
stat_fn <- function(x, metric) {
  if (metric == "logLik") {res <- sapply(x$model_list, function(p) logLik(p$model))}
  if (metric == "AIC") {res <- sapply(x$model_list, function(p) AIC(p$model))}
  if (metric == "BIC") {res <- sapply(x$model_list, function(p) BIC(p$model))}
  
  res <- as.data.frame(res) %>%
    rownames_to_column("formula") %>%
    separate(formula, c("y","term"), " ~ ")
  colnames(res)[3] <- metric
  
  
  return(res)
}

get_pseudoR2 <- function(x){
  y <- x$Y
  pseudoR2 <- data.frame()
  for (i in colnames(y)) {
    for (j in names(x$model_list)) {
      pseudoR2[i,j] <- cor(y[[i]], predict(x$model_list[[j]]$model))^2
    }
  }
  
  pseudoR2 <- pseudoR2 %>%
    rownames_to_column(var = "y") %>%
    pivot_longer(2:ncol(.), names_to = "formula", values_to = "pseudoR2") %>%
    separate(formula, c("formula_y","term"), " ~ ") %>%
    filter(y == formula_y) %>%
    select(-formula_y)
  
  return(pseudoR2)
}

