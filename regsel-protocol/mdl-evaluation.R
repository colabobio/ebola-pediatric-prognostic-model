# Some utility functions in R for model evaluation

# Partial effects (log odds scale) plot from RMS package. For more info check Harrel's
# Regression Modeling Strategies (pp 281)
parteff_plot <- function(model, data, dir, fn, ...) {
  require(rms)
  dd <- datadist(data)
  options(datadist='dd')
  g <- ggplot(Predict(model), sepdiscrete='vertical', vnames='names', rdata=data, histSpike.opts=list(frac=function(model) .1*model/max(model) ))
  
  plot(g)
  
  pdf(paste0(dir, "/", fn), useDingbats=FALSE) 
  plot(g)
  dev.off()  
}

# Calibration plot, from https://darrendahly.github.io/post/homr/
cal_plot <- function(model, data, model_name, pred_var, model_label, dir, fn, ...) {
  require(tidyverse)
  require(viridis)
  require(gridExtra)
  
  # The calibration plot        
  g1 <- mutate(data, bin = ntile(get(pred_var), 10)) %>% 
    # Bin prediction into 10ths
    group_by(bin) %>%
    mutate(n = n(), # Get ests and CIs
           bin_pred = mean(get(pred_var)), 
           bin_prob = mean(as.numeric(out) ), 
           se = sqrt((bin_prob * (1 - bin_prob)) / n), 
           ul = bin_prob + 1.96 * se, 
           ll = bin_prob - 1.96 * se,
           ul = ifelse(ul>0,ul,0),
           ll = ifelse(ll >0, ll,0),
           ul = ifelse(ul<1,ul,1),
           ll = ifelse(ll <1, ll,1)) %>%
    ungroup() %>%
    ggplot(aes(x = bin_pred, y = bin_prob, ymin = ll, ymax = ul)) +
    geom_pointrange(size = 0.2, color = "black",aes(color='black'),show.legend =TRUE) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    geom_abline() + # 45 degree line indicating perfect calibration
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", 
                color = "black", formula = y~-1 + x,show.legend =TRUE) + 
    # straight line fit through estimates
    geom_smooth(aes(x = get(pred_var), y = as.numeric(out) ),
                color = "red", se = FALSE, method = "loess") +
    # loess fit through estimates
    ylab("Observed Probability") +
    xlab("") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +    
    ggtitle(model_label) + 
    theme_bw()
  
  
  # The distribution plot   
  xlabel='Calibrated Probability'
  if(model_name =='original'){
    xlabel='Predicted Probability'
  }
  g2 <- ggplot(data, aes(x = get(pred_var))) +
    geom_histogram(fill = "black", bins = 200) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
    xlab(xlabel) +
    ylab("") + 
    scale_y_continuous(breaks = c(0, 80)) +
    theme_bw() +    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  # Combine them    
  g <- arrangeGrob(g1, g2, respect = TRUE, heights = c(1, 0.4), ncol = 1)
  
  plot(g)
  
  pdf(paste0(dir, "/", fn), useDingbats=FALSE) 
  plot(g)
  dev.off()
}

# Receiver Operator Curve plots with pROC package:
# https://cran.r-project.org/web/packages/pROC/index.html
roc_plot <- function(obs, probs, dir, fn, ...) {
  require(pROC)
  rocobj <- plot.roc(obs, probs,
                     main = "Confidence intervals", 
                     percent=TRUE,
                     ci = TRUE,                  # compute AUC (of AUC by default)
                     print.auc = TRUE)           # print the AUC (will contain the CI)
  ciobj <- ci.se(rocobj,                         # CI of sensitivity
                 specificities = seq(0, 100, 5)) # over a select set of specificities
  
  plot(ciobj, type = "shape", col = "#1c61b6AA")     # plot as a blue shape
  plot(ci(rocobj, of = "thresholds", thresholds = "best")) # add one threshold                   
  
  dev.copy(pdf, paste0(dir, "/", fn))
  dev.off()
}