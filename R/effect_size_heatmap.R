eff_size_heatmap<-function(metaMat, featureMat, 
                        logistic=TRUE, 
                        filename="_effect_size_plot_heatmap.tiff", 
                        rename=NULL, 
                        signif_only=TRUE, 
                        fontsize=80, 
                        q_cutoff=NA,
                        timepoints,
                        d_col = c("blue", "white", "red"),
                        order,
                        label_angel=0,
                        width=20,
                        legend_breaks = NA,
                        overall_effect=NA,
                        overall_effect_function="sum",
                        custom_sort=NULL,
                        hjust=0.5,
                        vjust=0.3,
                        group_levels=NULL,
                        group_colors=NULL,
                        title=NULL,
                        manual_order_TP3=NA){
  
  getVariableType <- function (values, variable) {
    if (is.numeric (values) &&
        all (values %in% c (0, 1)) &&
        !(!is.null (typeContinuous) &&
          variable %in% typeContinuous) &&
        !(!is.null (typeCategorical) && variable %in% typeCategorical)) {
      return ("binary") # treat as binary
    } # this fulfils criteria of binary (0, 1) and not in the special cases
    else if ((is.numeric (values) ||
              (!is.null (typeContinuous) &&
               variable %in% typeContinuous)) &&
             !(!is.null (typeCategorical) && variable %in% typeCategorical)) {
      return ("continuous") # treat as continuous
    } # this fulfils criteria of not being restricted to 0, 1; still numeric,
        # or guaranteed continuous (redundant?); and not categorical
    else {
      return ("categorical") # treat as categorical
    } # default as categorical
  }
  
 # featureMat<-na.omit(featureMat)

 
   colnames(featureMat)->features
  
  colnames(metaMat)->covariates
  
  output<-list()

  for(k in unique(timepoints)){
    
    meta_tmp<-as.data.frame(metaMat[timepoints==k,])
    colnames(meta_tmp)<-colnames(metaMat)
    feature_tmp<-as.data.frame(featureMat[timepoints==k,])
  
  for(i in seq_along(features)) {

   # print(paste("i=", i))
    
    somePs <- vector(length = length(covariates))
    someDs <- vector(length = length(covariates))
    some_intLow <- vector(length = length(covariates))
    some_intHigh <- vector(length = length(covariates))

    if ((var(feature_tmp[, i], na.rm = TRUE) == 0 ||
         length(na.exclude(feature_tmp[, i])) < 2) ) {
      # if variance of a feature == 0 OR
        # less than 2 elements of a feature are non-NA:
        # set all associations for that feature to NA
      somePs[seq_along(covariates)] <- NA
      someDs[seq_along(covariates)] <- NA

      return(c(as.numeric(somePs), as.numeric(someDs)))
    }

    subMerge <- meta_tmp
    subMerge$FeatureValue <- feature_tmp [,i]

    for (j in seq_along(covariates)) {
    #print(paste("j=", j))
      aFeature <- as.character (features [i])
      aCovariate <- as.character (covariates [j])

      aD <- NA_real_
      aP <- NA_real_
      aintLOW<- NA_real_
      aintHigh<- NA_real_

	variableType <- getVariableType (na.exclude(subMerge[[aCovariate]]), aCovariate)

  
	
	conVar <- TRUE
  varX <- na.exclude (cbind (subMerge [[aCovariate]], subMerge [["FeatureValue"]]))

  if (sum (! is.na (subMerge [["FeatureValue"]])) < 1 ||
      # TRUE if only NA-values
      nrow (varX) <= 2 ||
      # TRUE if if there are not at least three rows without NAs
      length (unique (varX [, 1])) < 2 ||
      # TRUE if there are not at least two different metadata values in non-NA subset
      length (unique (varX [, 2])) < 2) {
      # TRUE if there are not at least two different feature values in non-NA subset
        conVar <- FALSE
  }


  subSubMerge <- na.exclude(subMerge[, c(aCovariate, "FeatureValue")])

  if (logistic == TRUE && conVar) {
      formulaNull <- paste0 ("stats::glm (FeatureValue ~ 1, data = subSubMerge, family = \"binomial\")", collapse = "")
      formulaVar <- paste0 ("stats::glm (FeatureValue ~ ", aCovariate, ", data = subSubMerge, family = \"binomial\")", collapse = "")
      

      lmNull <- eval (parse (text = as.character (formulaNull)))
      lmVar <- eval (parse (text = as.character (formulaVar)))

      aP <- lmtest::lrtest (lmNull, lmVar)$'Pr(>Chisq)' [2]
      
      if (variableType == "categorical") {
        aD <- Inf
          aintLow<-Inf
        aintHigh<-Inf
      } else if (variableType == "binary") {
        tmp <- stats::cor.test (subMerge [, aCovariate],
                               subMerge [, "FeatureValue"],
                               )
        aD<-tmp$estimate
        aintLow<-tmp$conf.int[1]
        aintHigh<-tmp$conf.int[2]
        
      } else  if (variableType == "continuous") {
        tmp <- effsize::cliff.delta(
          f=as.vector (
            na.exclude (
              subMerge [subMerge [["FeatureValue"]] == 0, aCovariate])),
          d=as.vector (
            na.exclude (
              subMerge [subMerge [["FeatureValue"]] == 1, aCovariate])))
        
        aD<-tmp$estimate
        aintLow<-tmp$conf.int[1]
        aintHigh<-tmp$conf.int[2]
      }


    }

    else if (variableType == "categorical" && conVar) {
      # KW test if false binary and 	# SKF20200221

      aP <- stats::kruskal.test (
        g = as.factor(subMerge [[aCovariate]]),
        x = subMerge [["FeatureValue"]])$p.value

      aD <- Inf
      aintLow<-Inf
      aintHigh<-Inf
    }

    else if (variableType == "binary" && conVar) {
      # MWU test if binary and 	# SKF20200221

      aP <- stats::wilcox.test (
          subMerge [subMerge [[aCovariate]] == 0, "FeatureValue"],
          subMerge [subMerge [[aCovariate]] == 1, "FeatureValue"])$p.value

        tmp <- effsize::cliff.delta(
          f=as.vector (
            na.exclude (
              subMerge [subMerge [[aCovariate]] == 0, "FeatureValue"])),
          d=as.vector (
            na.exclude (
              subMerge [subMerge [[aCovariate]] == 1, "FeatureValue"])))
        
        aD<-tmp$estimate
        aintLow<-tmp$conf.int[1]
        aintHigh<-tmp$conf.int[2]
    }

    else if (variableType == "continuous" && conVar) {
      # spearman test if continuous and numerical 	# SKF20200221

      aP <- stats::cor.test (subMerge [, aCovariate],
                             subMerge [, "FeatureValue"],
                             method = "spearman")$p.value
        tmp <- stats::cor.test (subMerge [, aCovariate],
                               subMerge [, "FeatureValue"],
                               method="spearman")
        aD<-tmp$estimate
        aintLow<-tanh(atanh(aD) - (1.96/sqrt(nrow(subMerge-3))))
        aintHigh<-tanh(atanh(aD) + (1.96/sqrt(nrow(subMerge-3))))
        
    }

    else if (variableType == "categorical" && conVar) {  # now never happens, probably 	# SKF20200221
#      else if (con2 && !con5) {  # kruskal-wallis test if
                                  # not binary AND not numerical
      aP <- stats::kruskal.test (
        g = as.factor(subMerge [[aCovariate]]),
        x = subMerge [["FeatureValue"]])$p.value

      aD <- Inf
      aintLow<-Inf
      aintHigh<-Inf
    }

      somePs[j] <- aP
      someDs[j] <- aD
      some_intLow[j] <- aintLow
      some_intHigh[j] <- aintHigh

    } # for j


    output[[as.character(k)]][[features[i]]]<-list("Ds"=someDs, "Ps"=somePs, intLow=some_intLow, intHigh=some_intHigh)
  } 
  
  }
  
#plotting

for(i in 1:length(output)){
  as.data.frame(do.call(rbind, output[[i]]))->output[[i]]
  output[[i]]$metavariable<-features
  rownames(output[[i]])<-features

  order_data<-order(unlist(output[[i]]$Ds), decreasing = TRUE)
  output[[i]]<-output[[i]][order_data,]
  output[[i]]$metavariable<-factor(as.character(rownames(output[[i]])),levels=rownames(output[[i]]))

  output[[i]]<-na.omit(output[[i]])
  
  
if(!is.null(rename)){
  
  rename$new_name<-stringr::str_replace(rename$new_name, "\n", " ")
  
  output[[i]]$metavariable<-as.character(output[[i]]$metavariable)
  output[[i]]$metavariable<-rename$new_name[match(output[[i]]$metavariable, rename$old_name)]
  output[[i]]$metavariable<-factor(output[[i]]$metavariable, levels=output[[i]]$metavariable[length(output[[i]]$metavariable):1])
}
  
  output[[i]]$timepoint<-rep(unique(timepoints)[i], nrow(output[[i]]))
  
}
  
  


  dat<-rlist::list.rbind(output)
  apply(dat, 2, unlist)->dat
  dat<-as.data.frame(dat)
  dat$Ds<-as.numeric(dat$Ds)
    dat$Ps<-as.numeric(dat$Ps)
   dat$intLow<-as.numeric(dat$intLow)
   dat$intHigh<-as.numeric(dat$intHigh)

dat_out<-dat   
if(signif_only){
    dat%>%group_by(metavariable)%>%filter(any(Ps < 0.05))%>%ungroup()->dat
}
  
if(!is.na(q_cutoff)){
    dat%>%group_by(metavariable)%>%filter(any(abs(Ds) > q_cutoff))%>%ungroup()->dat
 }

   
paste("rev(unique(c(", stringr::str_flatten(paste("sort(dat$metavariable[dat$timepoint== '", order, "'])", sep=""), collapse = ","), ")))->levels")->tmp

eval(parse(text=tmp))

#rev(unique(c(sort(dat$metavariable[dat$timepoint=="TP1"]), 
#             sort(dat$metavariable[dat$timepoint=="TP2"]), 
#             sort(dat$metavariable[dat$timepoint=="TP3"]))))->levels


for(i in unique(timepoints)){
  
  dat[dat$timepoint==i,]->tmp
  
  levels[!levels%in%tmp$metavariable]->metas
  
  dat<-rbind(dat, data.frame("Ds"=rep(NA, length(metas)), 
                             "Ps"=rep(NA, length(metas)), 
                             "intLow"=rep(NA, length(metas)), 
                             "intHigh"=rep(NA, length(metas)), 
                             "metavariable"=metas, 
                             "timepoint"=rep(i, length(metas))))
  
}

if(overall_effect_function=="sum"){
dat<- dat%>%group_by(metavariable)%>%mutate("eff_sum"=sum(abs(na.omit(Ds))))%>%as.data.frame()
} else if (overall_effect_function=="mean"){
  dat<- dat%>%group_by(metavariable)%>%mutate("eff_sum"=mean(abs(na.omit(Ds))))%>%as.data.frame()
} else {
dat <- dat %>%
  group_by(metavariable) %>%
  mutate(
    eff_sum = {
      sig_idx <- which(Ps < 0.05)
      if (length(sig_idx) > 0) {
        Ds[sig_idx][which.max(abs(Ds[sig_idx]))]
      } else {
        0 # if none are significant, set to 0
      }
    }
  ) %>%
  ungroup()
  
  dat <- dat %>%
  mutate(
    ordering_value = ifelse(eff_sum >= 0, 
                            eff_sum,               # for positives: sort descending
                            -(10+eff_sum) )   # for negatives: sort ascending after all positives
  )
  
}

dat$timepoint<-factor(dat$timepoint, levels = c(order, " ", "  "))

if(overall_effect=="point"){

p<-ggplot(dat, aes(x=.data$timepoint, y=factor(.data$metavariable, levels = levels)))+
  geom_tile(aes(fill=.data$Ds), color="black", linewidth=2)+
  geom_point(aes(size=eff_sum, x=" "))+
    scale_fill_gradient2(name = "Effect\nsize",
                        low = d_col[1],
                        mid = d_col[2],
                        high = d_col[3],
                        midpoint = 0,
                        na.value="white",
                        guide = guide_colorbar (display = "gradient"))+
  scale_size(range = c(2, 20), name= "Effect\nsize\nsum")+
    theme_minimal() +
    theme(axis.text.x = element_text(size = 80,
                                    hjust = 1,
                                    vjust = 0.3),
            axis.text.y = element_text(size = 80,
                                       hjust = 1,
                                       vjust = 0.35),
            plot.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.text = element_text(size=50),
            legend.key.size = unit(4, "cm"),
            legend.title = element_text(size=60, margin = margin(b = 20)),
            axis.text.x.bottom =element_text(angle = label_angel, hjust=0.5),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) +
      labs(x = "Timepoint",
           y = "Omics features")
} else if (overall_effect=="scale"){

#alternative: sort by eff size and draw scale

#levels<-unique(dat$metavariable[order(dat$eff_sum)])

dat<-dat[!dat$metavariable=="MN",]
  
  
group_df <- tibble(
  metavariable = unlist(groups),
  group = rep(names(groups), lengths(groups))
)

dat$group<-group_df$group[match(dat$metavariable, group_df$metavariable)]

dat <- dat %>%
  group_by(group) %>%
  arrange(eff_sum, .by_group = TRUE) %>%
  ungroup()


# Create factor with desired order
metavariable_levels <- dat %>%
  distinct(group, metavariable, eff_sum) %>%
  arrange(group, eff_sum) %>%
  pull(metavariable)

dat <- dat %>%
  mutate(metavariable = factor(metavariable, levels = metavariable_levels))

#dat <- dat %>% mutate(tile_size = ifelse(Ps < 0.05, 6, 2))
dat <- dat %>% mutate(tile_size = ifelse(Ps < 0.05, 6, 0))

dat$group<-factor(dat$group, levels=c("descending_late", "descending_early", "ascending_late", "ascending_early", "hill", "valley"))


p<-ggplot(dat, aes(x=timepoint, y=metavariable))+ 
  geom_tile(aes(fill=Ds), color="black", size=2)+ 
  scale_fill_gradient2(name = "Effect\nsize", 
                       low = d_col[1], mid = d_col[2], high = d_col[3], 
                       midpoint = 0, na.value="white", 
                       guide = guide_colorbar (display = "gradient", order=1))+ 
#  geom_tile(data=filter(dat, Ps<0.05), aes(x=timepoint, y=metavariable, size=tile_size), fill=NA, color="black")+
  geom_point(data=filter(dat, Ps<0.05), aes(x=timepoint, y=metavariable, size=tile_size), shape=8, color="black", stroke=3, alpha=0.75)+ 
#  scale_size_continuous(name="P-value\n<0.05", range = c(6,6), labels = c(""))+ 
  scale_size_continuous(name="P-value\n<0.05", range = c(6,6), labels = c(""))+
#  guides(size = guide_legend(order=2, keywidth = unit(4, "cm"), keyheight = unit(2, "cm"), ))+
  guides(size = guide_legend(order=2,  keywidth = unit(4, "cm"), keyheight = unit(2, "cm"), override.aes = list(size=10)))+ 
  guides(shape = guide_legend(order=2))+
  ggnewscale::new_scale_fill()+ 
  geom_tile(aes(fill=eff_sum, x=length(unique(dat$timepoint))+1, width=0.5))+ 
  scale_fill_viridis(name="Cum.\neffect\nsize", guide=guide_colorbar(order=3))+ 
  ggnewscale::new_scale_fill()+
  geom_tile(aes(fill=group, x=length(unique(dat$timepoint))+1.6, width=0.5))+ 
  scale_fill_manual(name="Trajectory\ngroup", values=c("#9933FF", "#FFC000", "#00CCFF", "#FF66FF", "#FF0000", "#00FF00"), labels=NULL, guide=guide_legend(order=4))+ 
  facet_grid(rows = vars(group), scales = "free_y", space = "free_y")+
  theme_minimal() + 
  theme(axis.text.x =element_text(size = 80, hjust = hjust, vjust = vjust, angle = label_angel),
        axis.text.y = element_text(size = 80, hjust = 1, vjust = 0.35), 
        plot.title = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(), 
        legend.text = element_text(size=50), 
        legend.key.size = unit(4, "cm"), 
        legend.title = element_text(size=60, margin = margin(b = 20)), 
       # axis.text.x.bottom =element_text(angle = label_angel, hjust=0.5), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        strip.text = element_blank(),
        legend.box.margin = margin(t=0, r=0, b=0, l=60),
        legend.spacing.y = unit(4, "cm")) + 
  labs(x = "Timepoint", y = "Omics features")


} else {
  
  dat<-dat[!dat$metavariable=="MN",]
  
  
group_df <- tibble(
  metavariable = unlist(groups),
  group = rep(names(groups), lengths(groups))
)

dat <- dat %>%
  left_join(group_df, by = "metavariable")

dat <- dat %>%
  group_by(group) %>%
  arrange(eff_sum, .by_group = TRUE) %>%
  ungroup()


# Create a new unique label for each group-metavariable
dat <- dat %>%
  mutate(group_metavariable = paste(group, metavariable, sep = "_"))

# Create levels ordered by group and eff_sum
group_metavariable_levels <- dat %>%
  distinct(group_metavariable, group, ordering_value) %>%
  arrange(group, ordering_value) %>%
  pull(group_metavariable)


if(overall_effect_function=="manual"){
  for(k in names(groups)){
    group_metavariable_levels[stringr::str_detect(group_metavariable_levels, k)]<-paste(k, "_", rev(groups[[k]]), sep="")
  }
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "EXPANDED")]<-paste("EXPANDED_", rev(groups[["EXPANDED"]]), sep="")
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "REDUCED")]<-paste("REDUCED_", rev(groups[["REDUCED"]]), sep="")
  
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "PREDICTOR")]<-paste("PREDICTOR", rev(groups[["REDUCED"]]), sep="")
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "SEPSIS-MARKER")]<-paste("SEPSIS-MARKER", rev(groups[["REDUCED"]]), sep="")
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "SEQUELAE")]<-paste("REDUCED_", rev(groups[["REDUCED"]]), sep="")
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "SEQ")]<-paste("REDUCED_", rev(groups[["REDUCED"]]), sep="")
#  group_metavariable_levels[stringr::str_detect(group_metavariable_levels, "PRED")]<-paste("REDUCED_", rev(groups[["REDUCED"]]), sep="")
}

# Assign the ordered factor
dat <- dat %>%
  mutate(group_metavariable = factor(group_metavariable, levels = group_metavariable_levels))


#dat <- dat %>% mutate(tile_size = ifelse(Ps < 0.05, 6, 2))
dat <- dat %>% mutate(tile_size = ifelse(Ps < 0.05, 6, 0))

dat$group<-factor(dat$group, levels=group_levels)


format_tp_labels <- function(labels) {
  sapply(labels, function(label) {
    if (label == "TP1c") {
      "TP1<sub>C</sub>"
    } else if (label == "TP1p") {
      "TP1<sub>P</sub>"
    } else {
      label  # leave unchanged
    }
  })
}

x_labs<-format_tp_labels(order)

group_labels<-data.frame("group"=names(groups),
                         "y"=(lengths(groups)+1)/2)


  dat <- dat %>% mutate(tile_size = ifelse(Ps < 0.05, 6, 2))
  p<-ggplot(dat, aes(x=timepoint, y=group_metavariable))+
  geom_tile(aes(fill=Ds), color="black", size=2)+ 
  scale_fill_gradient2(name = "Effect\nsize", 
                       low = d_col[1], mid = d_col[2], high = d_col[3], 
                       midpoint = 0, na.value="white", 
                       guide = guide_colorbar (display = "gradient", order=1))+ 
#  geom_tile(data=filter(dat, Ps<0.05), aes(x=timepoint, y=group_metavariable, size=tile_size), fill=NA, color="black")+ 
  geom_point(data=filter(dat, Ps<0.05), aes(x=timepoint, y=group_metavariable, size=tile_size), shape=8, color="black", stroke=3)+ 
#  scale_size_continuous(name="P-value\n<0.05", range = c(6,6), labels = c(""))+ 
  scale_size_continuous(name="P-value\n<0.05", range = c(6,6), labels = c(""))+
#  guides(size = guide_legend(keywidth = unit(4, "cm"), keyheight = unit(2, "cm")))+
  guides(size = guide_legend(order=2,  keywidth = unit(4, "cm"), keyheight = unit(2, "cm"), override.aes = list(size=10)))+ 
  guides(shape = guide_legend(order=2))+
  ggnewscale::new_scale_fill()+ 
  geom_tile(aes(fill=group, x=length(unique(dat$timepoint))+0.7, width=0.25))+ 
  scale_fill_manual(values=group_colors, labels=NULL, guide="none")+
  scale_y_discrete(labels = function(x) gsub(".*_", "", x))+
  scale_x_discrete(labels=x_labs)+
  geom_text(data=group_labels, aes(x=length(unique(dat$timepoint))+0.7, y=y, label=group), size=16, color="white", angle=90, vjust=0.5)+
  facet_grid(rows = vars(group), scales = "free_y", space = "free_y")+
    labs(title=title)+
  theme_minimal() + 
  theme(axis.text.x = ggtext::element_markdown(size = 80, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 80, hjust = 1, vjust = 0.35), 
        plot.title = element_text(size = 80, hjust=0.5),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        legend.text = element_text(size=50), 
        legend.key.size = unit(4, "cm"), 
        legend.title = element_text(size=60, margin = margin(b = 20)),
        legend.justification = c(1,0.9),
        axis.text.x.bottom =element_text(angle = label_angel, hjust=0.5), 
        plot.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
       strip.text = element_blank()) + 
  labs(x = "Timepoint", y = "Omics features")
}

ggsave(filename=filename, plot=p, width=width, height = 3+(length(levels)))

return(dat_out)


}
