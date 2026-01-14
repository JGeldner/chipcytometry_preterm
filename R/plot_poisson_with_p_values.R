plot_poisson_with_p_values <- function(data, response_var, group_var, title, cohort=NA, additional_effect=NA, method="emm") {
  
  data[[response_var]]<-as.numeric(data[[response_var]])
  
  data[[group_var]]<-factor(as.character(data[[group_var]]), levels = c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"))
  
  # 1. Poisson model
  formula <- as.formula(paste(response_var, "~", group_var))
  
  if(!is.na(additional_effect)){
  formula <- as.formula(paste(response_var, "~", group_var, "+", additional_effect))
  }
  
  poisson_model <- glm(formula, family = poisson(link = "log"), data = data)
  
  # 2. Estimated Marginal Means
  emmeans_model <- emmeans(poisson_model, group_var)
  
  # 3. Extract EMMs and p-values from post-hoc comparisons
  emm_df <- as.data.frame(emmeans_model, type = "response")  # EMMs on response scale
  
  # Calculate pairwise comparisons and p-values
  posthoc_comparisons <- contrast(emmeans_model, method = "pairwise", adjust = "fdr")
  posthoc_df <- as.data.frame(posthoc_comparisons)
  
  # Print pairwise comparisons for checking
  print(posthoc_df)
  
  # 4. ggplot for visualization
  p <- ggplot(emm_df, aes(x = get(group_var), y = rate, ymin = asymp.LCL, ymax = asymp.UCL, color=get(group_var)))+
    geom_pointrange(size=1) +
    theme_minimal() +
    labs(
      title = paste(title, group_var),
      x = group_var,
      y = "estimated mean count",
      caption = "Error bars represent 95% confidence interval"
    ) + scale_color_manual(breaks=c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"),
                           values=c( "#103F91", "#FF8F6B" , "#C13B1B", "#861513", "#2D2D2E"),
                           labels=c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"))+
    theme(axis.text.x = element_text(angle = 45, size=20),
          plot.title = element_text(size=25),
          axis.text.y=element_text(size=10),
          plot.caption = element_text(size=10),
          axis.title = element_text(size=15),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15))+
    labs(color="LD Group")

  if(method=="IRR"){
    
    results<-tidy(poisson_model, exponentiate = TRUE, conf.int = TRUE)%>%mutate(
    group = stringr::str_replace(term, "group_lockdown_sum", ""))
    
    emm_df$rate<-results$estimate[match(emm_df$group_lockdown_sum, results$group)]
    emm_df$asymp.LCL<-results$conf.low[match(emm_df$group_lockdown_sum, results$group)]
    emm_df$asymp.UCL<-results$conf.high[match(emm_df$group_lockdown_sum, results$group)]
    
    emm_df$rate[emm_df$group_lockdown_sum=="Non LD_Pre LD"]<-1
    emm_df$asymp.LCL[emm_df$group_lockdown_sum=="Non LD_Pre LD"]<-1
    emm_df$asymp.UCL[emm_df$group_lockdown_sum=="Non LD_Pre LD"]<-1
    
    p <- ggplot(emm_df, aes(x = get(group_var), y = rate, ymin = asymp.LCL, ymax = asymp.UCL, color=get(group_var)))+
    geom_pointrange(size=1) +
    theme_minimal() +
    labs(
      title = paste(title, group_var),
      x = group_var,
      y = "Incidence Rate Ratio",
      caption = "Error bars represent 95% confidence interval"
    ) + scale_color_manual(breaks=c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"),
                           values=c( "#13F91", "#FF8F6B" , "#C13B1B", "#861513", "#2D2D2E"),
                           labels=c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"))+
    theme(axis.text.x = element_text(angle = 45, size=20),
          plot.title = element_text(size=25),
          axis.text.y=element_text(size=10),
          plot.caption = element_text(size=10),
          axis.title = element_text(size=15),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15))+
    labs(color="LD Group")
  }   else if(method=="odds"){
    
     # 1. Poisson model
  
    formula <- as.formula(paste(response_var, "~", group_var))
  
  if(!is.na(additional_effect)){
  formula <- as.formula(paste(response_var, "~", group_var, "+", additional_effect))
  }
  
  data[[response_var]][data[[response_var]]>0]<-1
  
  binomial_model <- glm(formula, family = binomial(), data = data)
  
  # 2. Estimated Marginal Means
  emmeans_model <- emmeans(binomial_model, group_var, type="response")
  
  # 3. Extract EMMs and p-values from post-hoc comparisons
  emm_df <- as.data.frame(emmeans_model)  # EMMs on response scale
  
  # Calculate pairwise comparisons and p-values
  posthoc_comparisons <- contrast(emmeans_model, method = "pairwise", adjust = "fdr")
  posthoc_df <- as.data.frame(posthoc_comparisons)
  
  # Print pairwise comparisons for checking
  print(posthoc_df)
    
    
    p <- ggplot(emm_df, aes(x = get(group_var), y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color=get(group_var)))+
    geom_pointrange(size=1) +
    theme_minimal() +
    labs(
      title = paste(title, group_var),
      x = group_var,
      y = "Probability",
      caption = "Error bars represent 95% confidence interval"
    ) + scale_color_manual(breaks=c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"),
                           values=c( "#103F91", "#FF8F6B" , "#C13B1B", "#861513", "#2D2D2E"),
                           labels=c("Non-LD_Pre LD", "LD_Post expo", "LD_w/o expo", "LD_Pre expo", "Non-LD_Post LD"))+
    theme(axis.text.x = element_text(angle = 45, size=20),
          plot.title = element_text(size=25),
          axis.text.y=element_text(size=10),
          plot.caption = element_text(size=10),
          axis.title = element_text(size=15),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15))+
    labs(color="LD Group")
}

  
# 5. Add p-values and stars from post-hoc comparisons to the plot
  
# calculate distance in the plot
  
  posthoc_df$diff<-rep(NA, nrow(posthoc_df))
  posthoc_df$y_offset<-rep(NA, nrow(posthoc_df)) 
  
  for(i in 1:nrow(posthoc_df)){
    
    group_names <- strsplit(as.character(posthoc_df$contrast[i]), " - ")[[1]]
    if(method=="odds"){
     group_names <- strsplit(as.character(posthoc_df$contrast[i]), " / ")[[1]] 
    }
    group1 <- gsub("[()]", "", group_names[1])  # Remove round brackets if present
    group2 <- gsub("[()]", "", group_names[2])  # Remove round brackets if present
    
    posthoc_df$diff[i]<-abs((which(levels(factor(data[[group_var]])) == group1)- which(levels(factor(data[[group_var]])) == group2)))
    
    posthoc_df$y_offset[i] <- max(emm_df$asymp.UCL)
    
    if(posthoc_df$diff[i]==1){
      posthoc_df$y_offset[i] <-posthoc_df$y_offset[i] +0.05
      
    } else if (posthoc_df$diff[i]==2){
      posthoc_df$y_offset[i] <-posthoc_df$y_offset[i] +0.25
      
      
    } else if (posthoc_df$diff[i]==3){
       posthoc_df$y_offset[i] <-posthoc_df$y_offset[i] +0.65
       
    } else if (posthoc_df$diff[i]==4){
       posthoc_df$y_offset[i] <-posthoc_df$y_offset[i] +1.05
       
    }
    
  }
  
    posthoc_df$y_offset[posthoc_df$diff==2][2]<-posthoc_df$y_offset[posthoc_df$diff==2][2]+0.2
    
    posthoc_df$y_offset[posthoc_df$diff==3][2]<-posthoc_df$y_offset[posthoc_df$diff==3][2]+0.2
  
  
  for(i in 1:nrow(posthoc_df)) {
    # Extract p-value
    p_value <- posthoc_df$p.value[i]
    comparison_label <- paste0("p = ", round(p_value, 4))
    
    # Add stars for significance
    if(p_value < 0.001) {
      comparison_label <- paste0(comparison_label, " ***")
    } else if(p_value < 0.01) {
      comparison_label <- paste0(comparison_label, " **")
    } else if(p_value < 0.05) {
      comparison_label <- paste0(comparison_label, " *")
    }
    
    # Extract group names from contrast and clean them (remove round brackets)
    group_names <- strsplit(as.character(posthoc_df$contrast[i]), " - ")[[1]]
    group1 <- gsub("[()]", "", group_names[1])  # Remove round brackets if present
    group2 <- gsub("[()]", "", group_names[2])  # Remove round brackets if present
    
    # Compute x positions for the groups being compared
    x1 <- which(levels(factor(data[[group_var]])) == group1)
    x2 <- which(levels(factor(data[[group_var]])) == group2)
    
    # Calculate x position as the midpoint between the two groups being compared
    x_position <- (x1 + x2) / 2
    
    # Adjust y position dynamically based on the maximum mean and p-value significance
    #y_offset <- max(emm_df$rate) + 0.2 + (i * 0.1)  # Slight increase in offset for each comparison
    y_offset<-posthoc_df$y_offset[i]
    
    # Add annotation for the p-value
    p <- p + 
      annotate("text", x = x_position,
               y = y_offset+0.1, label = comparison_label, size = 5, hjust = 0.5)
        # Draw square brackets using grid::segmentsGrob (straight lines for the bracket)
    p <- p + annotation_custom(
      grob = grid::segmentsGrob(gp = gpar(lwd=4)),
      xmin = x1+0.015, xmax = x2-0.015, ymin = y_offset + 0.025, ymax = y_offset + 0.025
    )
    
    # Add vertical lines at the ends of the square bracket
    p <- p + annotation_custom(
      grob = grid::segmentsGrob(gp = gpar(lwd=4)),
      xmin = x1+0.015, xmax = x1+0.015, ymin = y_offset + 0.025, ymax = y_offset - 0.025
    )
    
    p <- p + annotation_custom(
      grob = grid::segmentsGrob(gp = gpar(lwd=4)),
      xmin = x2-0.015, xmax = x2-0.015, ymin = y_offset + 0.025, ymax = y_offset - 0.025
    )
  }
  
  # 6. Display the plot
  #print(p)
  #
  if(is.na(cohort)){
    name<-paste(response_var, "_", group_var, ".png", sep="")
  } else {
    name<-paste(response_var, "_", group_var, "_", cohort, ".png", sep="")
    }
  ggsave( plot = p ,filename=name, width=10, height = 10, units="in")
  
  return(poisson_model)
}