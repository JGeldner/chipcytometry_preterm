#Swimmer plot
#Figur 6
data_swimmer <- cohort_H
name<-"_cohort_H_"

data<-data_swimmer[c("group_lockdown_sum_976")]
data$pat<-data_swimmer$MHH_PatID
data$id<-1:nrow(data_swimmer)
data$expected_dob<-data_swimmer$expected_birth
data$end_pregnancy<-as.Date(data_swimmer$birth_date)
data$start_pregnancy<-as.Date(data$expected_dob)-280
data$start_infancy<-data$end_pregnancy

data$end_infancy<-as.Date(data$start_pregnancy)+(rep(23.03571, nrow(data_swimmer))*28)
data$start_pregnancy<-as.Date(data$start_pregnancy)
data$start_infancy<-as.Date(data$start_infancy)


data[order(data$start_pregnancy),]->data_ordered
data_ordered$id<-1:nrow(data_swimmer)
data_ordered_p<-data_ordered
colnames(data_ordered_p)[6]<-"start"
colnames(data_ordered_p)[5]<-"end"
data_ordered_p$color<-rep("Pregnancy", nrow(data_ordered_p))
data_ordered_i<-data_ordered
colnames(data_ordered_i)[7]<-"start"
colnames(data_ordered_i)[8]<-"end"
data_ordered_i$color<-rep("Infancy", nrow(data_ordered_i))

rbind(data_ordered_p[c("start", "end", "color", "id",  "group_lockdown_sum_976")],
      data_ordered_i[c("start", "end", "color", "id",  "group_lockdown_sum_976")])->data_ordered

#rbind(data_ordered_p[c("start", "end", "color", "id",  "group_lockdown_sum_v2")],
#      data_ordered_i[c("start", "end", "color", "id",  "group_lockdown_sum_v2")])->data_ordered

#rbind(data_ordered_p[c("start", "end", "color", "id",  "group_lockdown_p")],
#      data_ordered_i[c("start", "end", "color", "id",  "group_lockdown_p")])->data_ordered

#rbind(data_ordered_p[c("start", "end", "color", "id",  "group_lockdown_p_v2")],
#      data_ordered_i[c("start", "end", "color", "id",  "group_lockdown_p_v2")])->data_ordered

#rbind(data_ordered_p[c("start", "end", "color", "id",  "group_lockdown_i")],
#      data_ordered_i[c("start", "end", "color", "id",  "group_lockdown_i")])->data_ordered


colnames(data_ordered)<-c("start", "end", "color", "id", "group")
p<-ggplot()+
        geom_segment(data=data_ordered, aes(x=start, y=id, xend=end, color=group), linewidth=2) +
        #lockdown rectangle
        geom_rect(aes(xmin=as.Date("2020-03-16"), 
                      xmax=as.Date("2022-04-02"), 
                      ymin=-2, ymax=nrow(data_swimmer)+1), 
                    fill="grey", alpha=0.3, color="black", linewidth=1)+
        #binary lockdown group rectangle
     #   geom_rect(aes(xmin=as.Date("2017-09-01"), 
    #                  xmax=as.Date("2023-02-01"), 
     #                 ymin=min(data_ordered$id[data_ordered$lockdown_sum>12.464]-0.5), 
      #                ymax=max(data_ordered$id[data_ordered$lockdown_sum>12.464])+0.5), 
       #             fill="grey", alpha=0.3, color="black", linewidth=0)+
      #  geom_point(data=data_ordered[!is.na(data_ordered$tp_sepsis),], aes(x=tp_sepsis, y=id))+
        geom_point(data=data_ordered[!is.na(data_ordered$end)&data_ordered$color=="Pregnancy",], aes(x=end, y=id, color="Birth"), pch=1
                   , size=2, position = position_identity(), fill="white", show.legend = TRUE, stroke=1.2)+
    geom_hline(yintercept = 47.5, linewidth=1, color="black", linetype=2)+
    geom_hline(yintercept = 121.5, linewidth=1, color="black", linetype=2)+
        guides(color = guide_legend(
                    override.aes = list(
                    shape = c(NA, NA, NA, NA, NA, 1),  # last one is for "Birth"
                    size = c(NA, NA, NA, NA, NA, 5), # only "Birth" larger
                    stroke= c(NA, NA, NA, NA, NA, 3)   #only "Birth" larger
                      )
                        ))+
        #scale_x_date(labels = date_format("%m-%Y"), date_breaks = "2 month")+
        #scale_x_date(date_labels="%m-%Y", date_breaks = "2 month")+
        scale_color_manual(breaks=c("Non-LD_Post LD", "LD_Pre expo", "LD_w/o expo", "LD_Post expo", "Non-LD_Pre LD",  "Birth"),
                           values=c("#333241", "#861513" , "#fb0207",    "#ff866b", "#103f91",  "lightgrey"),
                           labels=c("Non-LD_Post LD", "LD_Pre expo", "LD_w/o expo", "LD_Post expo", "Non-LD_Pre LD",  "Birth"))+
       #  guides(colour = guide_legend(override.aes = list(shape = c(NA, NA, NA, NA, 1), size=5, color=c( "#FF8F6B" , "#C13B1B",    "#41A5EE", "#103F91", "darkgrey"))))+
       #scale_color_manual(breaks=c( "LD",  "Non-LD", "Birth"),
       #                   values=c( "#c78172" ,    "#57566e","lightgrey"))+
      #guides(colour = guide_legend(override.aes = list(shape = c(NA, NA,  1), size=5, color=c( "#c78172" ,    "#57566e","darkgrey"))))+
         #scale_color_manual(values=c("lightgreen", "cyan"))+
        labs(x = "Follow-up time", y = "Preterm infant (n = 202)", color="") +
        annotate(geom="text", x=as.Date("2021-05-01"), y=8, label="LOCKDOWN", size=9, hjust=0.55)+
        annotate(geom="text", x=as.Date("2024-01-01"), y=49.6, label="Cutoff = 9.76 mo.", size=9, hjust=0.55)+
        annotate(geom="text", x=as.Date("2024-10-15"), y=123.5, label="Cutoff = 9.76 mo.", size=9, hjust=0.55)+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0, hjust=0.5, size=30),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = c(0.1625,0.85),
             legend.text = element_text(size=27),
             axis.title.y = element_text(size=35),
             axis.title.x = element_text(size=35)
              )

ggsave(paste("Fig_6_swimmer_plot", name, ".tiff", sep=""), plot=p, width=11, height =18)

#Figure 4
data_swimmer<-all_metadata[!duplicated(all_metadata$pseudonym),]

data<-data_swimmer[c("pseudonym")]

data$TP1<-rep(NA, 114)
data$TP2<-rep(NA, 114)
data$TP3<-rep(NA, 114)

for(i in 1:114){
  sort(all_metadata$xxx_sampling_date[all_metadata$pseudonym==data$pseudonym[i]])->tmp
  data$TP1[i]<-as.Date(tmp[1])
  data$TP2[i]<-as.Date(tmp[2])
  data$TP3[i]<-as.Date(tmp[3])
}

data$id<-1:nrow(data_swimmer)
data$expected_dob<-data_swimmer$xxx_expected_birth_date
data$end_pregnancy<-as.Date(data_swimmer$xxx_birth_date)
data$start_pregnancy<-as.Date(data$expected_dob)-280
data$start_infancy<-data$end_pregnancy

data$end_infancy<-as.Date(data$start_pregnancy)+(rep(23.03571, nrow(data_swimmer))*28)
data$start_pregnancy<-as.Date(data$start_pregnancy)
data$start_infancy<-as.Date(data$start_infancy)
data$BPD<-data_swimmer$bpd_binary_I
data$sepsis<-data_swimmer$xxx_sepsis_lons_LT
data$sepsis[stringr::str_detect(data$sepsis, "NA")]<-NA
data %>%
  separate(sepsis, into = c("sepsis1", "sepsis2", "sepsis3"),
           sep = "_", fill = "right", remove = FALSE)->data
data$sepsis1<-as.Date(data$start_infancy)+as.numeric(data$sepsis1)
data$sepsis2<-as.Date(data$start_infancy)+as.numeric(data$sepsis2)
data$sepsis3<-as.Date(data$start_infancy)+as.numeric(data$sepsis3)

data[order(data$start_pregnancy),]->data_ordered
data_ordered$id<-1:nrow(data)
data_ordered_p<-data_ordered
colnames(data_ordered_p)[8]<-"start"
colnames(data_ordered_p)[7]<-"end"
data_ordered_p$color<-rep("Pregnancy", nrow(data_ordered_p))
data_ordered_i<-data_ordered
colnames(data_ordered_i)[9]<-"start"
colnames(data_ordered_i)[10]<-"end"
data_ordered_i$color<-rep("Infancy", nrow(data_ordered_i))

rbind(data_ordered_p[c("start", "end", "color", "id",  "BPD", "sepsis1", "sepsis2", "sepsis3", "TP1", "TP2", "TP3")],
      data_ordered_i[c("start", "end", "color", "id",  "BPD", "sepsis1", "sepsis2", "sepsis3", "TP1", "TP2", "TP3")])->data_ordered

data_ordered$TP1<-as.Date(data_ordered$TP1)
data_ordered$TP2<-as.Date(data_ordered$TP2)
data_ordered$TP3<-as.Date(data_ordered$TP3)

p<-ggplot()+
        geom_segment(data=data_ordered, aes(x=start, y=id, xend=end, color=color), linewidth=3) +
        #bpd
        #geom_point(data=data_ordered[data_ordered$color=="Pregnancy"&data_ordered$BPD==1,], aes(x=start-20, y=id, color="BPD"), shape=17, size=7)+
        #sepsis
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$sepsis1),], aes(x=sepsis1, y=id, color="Sepsis"), shape=19, size=3)+
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$sepsis2),], aes(x=sepsis2, y=id, color="Sepsis"), shape=19, size=3)+
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$sepsis3),], aes(x=sepsis3, y=id, color="Sepsis"), shape=19, size=3)+
        #sampling
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$TP1),], aes(x=TP1, y=id, color="Sampling"), shape="\u2759", size=3)+
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$TP2),], aes(x=TP2, y=id, color="Sampling"), shape="\u2759", size=3)+
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$TP3),], aes(x=TP3, y=id, color="Sampling"), shape="\u2759", size=3)+
        guides(color = guide_legend(
                    override.aes = list(
                    shape = c(NA, NA, 19, 15),  # Preg, Inf BPD, Sepsis, TP
                    size = c(NA, NA, 9, 9), 
                    stroke= c(NA, NA, NA, NA)
                      )
                        ))+
        scale_color_manual(breaks=c("Pregnancy", "Infancy", "Sepsis", "Sampling"),
                           values=c("#4CC0E4", "#4CE48E", "red", "black"),
                           labels=c("Pregnancy", "Infancy", "LONS", "Blood\nsampling"))+
        labs(x = "Follow-up time", y = "Preterm infant", color="") +
        scale_x_date(expand=c(0.01, 0.01))+
        scale_y_discrete(expand=c(0.01, 0.01))+
        geom_text(aes(label=paste("Mean age at\nLONS:\n18", "\u00B1", "11 days", sep=""), x=as.Date("2022-01-01"), y=17), size=40*(5/14))+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0, hjust=0.5, size=40),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = c(0.1625,0.84),
             legend.text = element_text(size=40),
             axis.title.y = element_text(size=40),
             axis.title.x = element_text(size=40),
             legend.key.height = unit(2.3, "cm"),
             legend.box.margin = margin(0,0,0,0)
              )

ggsave("Fig_4_swimmer_plot.tiff", plot=p, width=11, height =15)



#Figure 1
data_swimmer<-all_metadata[!duplicated(all_metadata$pseudonym),]

data<-data_swimmer[c("pseudonym")]

data$TP1<-rep(NA, 114)
data$TP2<-rep(NA, 114)
data$TP3<-rep(NA, 114)

for(i in 1:114){
  sort(all_metadata$xxx_sampling_date[all_metadata$pseudonym==data$pseudonym[i]])->tmp
  data$TP1[i]<-as.Date(tmp[1])
  data$TP2[i]<-as.Date(tmp[2])
  data$TP3[i]<-as.Date(tmp[3])
}

data$id<-1:nrow(data_swimmer)
data$expected_dob<-data_swimmer$xxx_expected_birth_date
data$end_pregnancy<-as.Date(data_swimmer$xxx_birth_date)
data$start_pregnancy<-as.Date(data$expected_dob)-280
data$start_infancy<-data$end_pregnancy

data$end_infancy<-as.Date(data$start_pregnancy)+(rep(23.03571, nrow(data_swimmer))*28)
data$start_pregnancy<-as.Date(data$start_pregnancy)
data$start_infancy<-as.Date(data$start_infancy)

data[order(data$start_pregnancy),]->data_ordered
data_ordered$id<-1:nrow(data)
data_ordered_p<-data_ordered
colnames(data_ordered_p)[8]<-"start"
colnames(data_ordered_p)[7]<-"end"
data_ordered_p$color<-rep("Pregnancy", nrow(data_ordered_p))
data_ordered_i<-data_ordered
colnames(data_ordered_i)[9]<-"start"
colnames(data_ordered_i)[10]<-"end"
data_ordered_i$color<-rep("Infancy", nrow(data_ordered_i))

rbind(data_ordered_p[c("start", "end", "color", "id",  "TP1", "TP2", "TP3")],
      data_ordered_i[c("start", "end", "color", "id",   "TP1", "TP2", "TP3")])->data_ordered

data_ordered$TP1<-as.Date(data_ordered$TP1)
data_ordered$TP2<-as.Date(data_ordered$TP2)
data_ordered$TP3<-as.Date(data_ordered$TP3)

p<-ggplot()+
        geom_segment(data=data_ordered, aes(x=start, y=id, xend=end, color=color), linewidth=3) +
  
        #sampling
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$TP1),], aes(x=TP1, y=id, color="Sampling"), shape="\u2759", size=3)+
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$TP2),], aes(x=TP2, y=id, color="Sampling"), shape="\u2759", size=3)+
        geom_point(data=data_ordered[data_ordered$color=="Infancy"&!is.na(data_ordered$TP3),], aes(x=TP3, y=id, color="Sampling"), shape="\u2759", size=3)+
        guides(color = guide_legend(
                    override.aes = list(
                    shape = c(NA, NA, 15),  # Preg, Inf BPD, Sepsis, TP
                    size = c(NA, NA,  9), 
                    stroke= c(NA, NA,  NA)
                      )
                        ))+
        scale_color_manual(breaks=c("Pregnancy", "Infancy",  "Sampling"),
                           values=c("#4CC0E4", "#4CE48E",  "black"),
                           labels=c("Pregnancy", "Infancy", "Blood\nsampling"))+
        labs(x = "Follow-up time", y = "Preterm infant", color="") +
        scale_x_date(expand=c(0.01, 0.01))+
        scale_y_discrete(expand=c(0.01, 0.01))+
        theme_bw()+
        theme(axis.text.x=element_text(angle=0, hjust=0.5, size=40),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = c(0.1625,0.85),
             legend.text = element_text(size=40),
             axis.title.y = element_text(size=40),
             axis.title.x = element_text(size=40),
             legend.key.height = unit(2.3, "cm")
              )

ggsave("Fig_1_swimmer_plot.tiff", plot=p, width=11, height =15)

