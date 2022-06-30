#### Meadows Paper ####
## Function: summarises MXL outputs
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 24/03/2022
## TODO: summarise LCM too



# Read-in Model WTP: -----------------------------------------------------


#### Pooled ####

Pooled_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_Levels_FullSample_2022_03_24_WTP.csv"))

Pooled_ConditionalWTPSummary <-data.frame(cbind("b_Cost"=Pooled_ConditionalWTP$b_Cost.post.mean,
                                                "b_Plants5"=Pooled_ConditionalWTP$b_Plants5.post.mean,
                                                "b_Plants10"=Pooled_ConditionalWTP$b_Plants10.post.mean,
                                                "b_NativeDecrease"=Pooled_ConditionalWTP$b_NativeDecrease.post.mean,
                                                "b_NativeIncrease"=Pooled_ConditionalWTP$b_NativeIncrease.post.mean,
                                                "b_Appearance_W"=Pooled_ConditionalWTP$b_Appearance_W.post.mean,
                                                "b_Appearance_WYB"=Pooled_ConditionalWTP$b_Appearance_WYB.post.mean,
                                                "b_Appearance_WYBPR"=Pooled_ConditionalWTP$b_Appearance_WYBPR.post.mean,
                                                "b_PollinatorQualityMedium"=Pooled_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
                                                "b_PollinatorQualityHigh"=Pooled_ConditionalWTP$b_PollinatorQualityHigh.post.mean))
Labels <- c(
  "Cost",
  "Plants5",
  "Plants10",
  "NativeDecrease",
  "NativeIncrease",
  "Appearance_W",
  "Appearance_WYB",
  "Appearance_WYBPR",
  "PollinatorQualityMedium",
  "PollinatorQualityHigh"
)

ggsave(melt(Pooled_ConditionalWTPSummary) %>% 
ggplot(aes(x = value, y = variable, group=variable,fill = variable)) + 
  geom_density_ridges()+
  geom_vline(xintercept = 0)+
  scale_y_discrete(name="Attribute",
                   label=Labels)+
  ggtitle("Distribution of individual-level attribute WTP.")+ 
  scale_fill_manual(name="Attributes",
                    values=c(
                      "black",
                      "springgreen",
                      "springgreen4",
                      "gold3",
                      "red4",
                      "cornsilk",
                      "cornsilk1",
                      "cornsilk3",
                      "dodgerblue1",
                      "dodgerblue4"
                    ),
                    label=Labels,
                    guide=guide_legend(reverse = TRUE))+
  theme(axis.text = element_text(size=12),legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"),legend.text = element_text(size=12)),
device = "jpeg",
filename = "WTPDensity_2022_03_24.jpeg",
width=30,height=25,units = "cm",dpi=500)

# Pooled_ConditionalWTPSummary %>% summarise(across(everything(),list(mean)))


write.csv(round(cbind(data.frame("Mean" = t(
  data.frame(Pooled_ConditionalWTPSummary %>% summarise(across(
    everything(), list(mean)
  )))
)), data.frame("SD" = t(
  data.frame(Pooled_ConditionalWTPSummary %>% summarise(across(
    everything(), list(sd)
  )))
))),2))








#### Pooled with Covariates ####

Covariates_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_Levels_Covariates_2022_03_14_WTP.csv"))
Covariates_ConditionalWTPSummary <-
  data.frame(
    cbind(
      "b_Cost" = Covariates_ConditionalWTP$b_Cost.post.mean,
      "b_Plants5" =
        Covariates_ConditionalWTP$b_Plants5.post.mean,
      "b_Plants10" =
        Covariates_ConditionalWTP$b_Plants10.post.mean,
      "b_NativeDecrease" =
        Covariates_ConditionalWTP$b_NativeDecrease.post.mean,
      "b_NativeIncrease" =
        Covariates_ConditionalWTP$b_NativeIncrease.post.mean,
      "b_Appearance_W" =
        Covariates_ConditionalWTP$b_Appearance_W.post.mean,
      "b_Appearance_WYB" =
        Covariates_ConditionalWTP$b_Appearance_WYB.post.mean,
      "b_Appearance_WYBPR" =
        Covariates_ConditionalWTP$b_Appearance_WYBPR.post.mean,
      "b_PollinatorQualityMedium" =
        Covariates_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
      "b_PollinatorQualityHigh" =
        Covariates_ConditionalWTP$b_PollinatorQualityHigh.post.mean
    )
  )

# Covariates_ConditionalWTPSummary %>% summarise(across(everything(),list(mean)))



#### Annual WTP: ####
Annual_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_LevelsNoCovariates_Annual_FullSample_2022_03_24_WTP.csv"))


Annual_ConditionalWTPSummary <-
  data.frame(
    cbind(
      "b_Cost" = Annual_ConditionalWTP$b_Cost.post.mean,
      "b_Plants5" =
        Annual_ConditionalWTP$b_Plants5.post.mean,
      "b_Plants10" =
        Annual_ConditionalWTP$b_Plants10.post.mean,
      "b_NativeDecrease" =
        Annual_ConditionalWTP$b_NativeDecrease.post.mean,
      "b_NativeIncrease" =
        Annual_ConditionalWTP$b_NativeIncrease.post.mean,
      "b_Appearance_W" =
        Annual_ConditionalWTP$b_Appearance_W.post.mean,
      "b_Appearance_WYB" =
        Annual_ConditionalWTP$b_Appearance_WYB.post.mean,
      "b_Appearance_WYBPR" =
        Annual_ConditionalWTP$b_Appearance_WYBPR.post.mean,
      "b_PollinatorQualityMedium" =
        Annual_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
      "b_PollinatorQualityHigh" =
        Annual_ConditionalWTP$b_PollinatorQualityHigh.post.mean
    )
  )
# Annual_ConditionalWTPSummary %>% summarise(across(everything(), list(mean)))



#### Control WTP: ####
Control_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_LevelsNoCovariates_Control_FullSample_2022_03_24_WTP.csv"))

Control_ConditionalWTPSummary <-
  data.frame(
    cbind(
      "b_Cost" = Control_ConditionalWTP$b_Cost.post.mean,
      "b_Plants5" =
        Control_ConditionalWTP$b_Plants5.post.mean,
      "b_Plants10" =
        Control_ConditionalWTP$b_Plants10.post.mean,
      "b_NativeDecrease" =
        Control_ConditionalWTP$b_NativeDecrease.post.mean,
      "b_NativeIncrease" =
        Control_ConditionalWTP$b_NativeIncrease.post.mean,
      "b_Appearance_W" =
        Control_ConditionalWTP$b_Appearance_W.post.mean,
      "b_Appearance_WYB" =
        Control_ConditionalWTP$b_Appearance_WYB.post.mean,
      "b_Appearance_WYBPR" =
        Control_ConditionalWTP$b_Appearance_WYBPR.post.mean,
      "b_PollinatorQualityMedium" =
        Control_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
      "b_PollinatorQualityHigh" =
        Control_ConditionalWTP$b_PollinatorQualityHigh.post.mean
    )
  )
# Control_ConditionalWTPSummary %>% summarise(across(everything(), list(mean)))


#### Perennial WTP: ####

Perennial_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_LevelsNoCovariates_Perennials_FullSample_2022_03_24_WTP.csv"))


Perennial_ConditionalWTPSummary <-
  data.frame(
    cbind(
      "b_Cost" = Perennial_ConditionalWTP$b_Cost.post.mean,
      "b_Plants5" =
        Perennial_ConditionalWTP$b_Plants5.post.mean,
      "b_Plants10" =
        Perennial_ConditionalWTP$b_Plants10.post.mean,
      "b_NativeDecrease" =
        Perennial_ConditionalWTP$b_NativeDecrease.post.mean,
      "b_NativeIncrease" =
        Perennial_ConditionalWTP$b_NativeIncrease.post.mean,
      "b_Appearance_W" =
        Perennial_ConditionalWTP$b_Appearance_W.post.mean,
      "b_Appearance_WYB" =
        Perennial_ConditionalWTP$b_Appearance_WYB.post.mean,
      "b_Appearance_WYBPR" =
        Perennial_ConditionalWTP$b_Appearance_WYBPR.post.mean,
      "b_PollinatorQualityMedium" =
        Perennial_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
      "b_PollinatorQualityHigh" =
        Perennial_ConditionalWTP$b_PollinatorQualityHigh.post.mean
    )
  )
Perennial_ConditionalWTPSummary %>% summarise(across(everything(), list(mean)))


#### Summarising Together ####


WTPByModel <-
  cbind(rbind(
    cbind(data.frame("Mean" = t(
      data.frame(Pooled_ConditionalWTPSummary %>% summarise(across(
        everything(), list(mean)
      )))
    )), data.frame("SD" = t(
      data.frame(Pooled_ConditionalWTPSummary %>% summarise(across(
        everything(), list(sd)
      )))
    ))),
    cbind(data.frame("Mean" = t(
      data.frame(Annual_ConditionalWTPSummary %>% summarise(across(
        everything(), list(mean)
      )))
    )), data.frame("SD" = t(
      data.frame(Annual_ConditionalWTPSummary %>% summarise(across(
        everything(), list(sd)
      )))
    ))),
    cbind(data.frame("Mean" = t(
      data.frame(Control_ConditionalWTPSummary %>% summarise(across(
        everything(), list(mean)
      )))
    )), data.frame("SD" = t(
      data.frame(Control_ConditionalWTPSummary %>% summarise(across(
        everything(), list(sd)
      )))
    ))),
    cbind(data.frame("Mean" = t(
      data.frame(Perennial_ConditionalWTPSummary %>% summarise(across(
        everything(), list(mean)
      )))
    )), data.frame("SD" = t(
      data.frame(Perennial_ConditionalWTPSummary %>% summarise(across(
        everything(), list(sd)
      )))
    )))),
    data.frame("Treatment"=rbind(
      data.frame("Treatment"=rep("Pooled", each = 10)),
      data.frame("Treatment"=rep("Annual", each = 10)),
      data.frame("Treatment"=rep("Control", each = 10)),
      data.frame("Treatment"=rep("Perennial", each = 10)))),
    data.frame("Attribute"=rep(
      c(
        "Cost",
        "Plants5",
        "Plants10",
        "NativeDecrease",
        "NativeIncrease",
        "Appearance_W",
        "Appearance_WYB",
        "Appearance_WYBPR",
        "PollinatorQualityMedium",
        "PollinatorQualityHigh"
      ),
      times = 4)))


ggsave(
  ggplot(WTPByModel, aes(x = Attribute, y = Mean, fill = Attribute)) + 
    geom_bar(stat =  "identity") +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD)) + 
    facet_wrap( ~ Treatment,scales="free_x") + 
    scale_fill_manual(
      "Attributes",
      values = c(
        "Cost" = "black",
        "Plants5" = "green",
        "Plants10" = "green",
        "NativeDecrease" = "yellow",
        "NativeIncrease" = "yellow",
        "Appearance_W" = "lightblue",
        "Appearance_WYB" = "blue",
        "Appearance_WYBPR" = "darkblue",
        "PollinatorQualityMedium" = "red",
        "PollinatorQualityHigh" = "darkred"
      )
    ) + ggtitle("WTP By Attribute and Treatment.") + ylab("Mean Attribute WTP in £py") +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))+coord_flip(),
  device = "jpeg",
  filename = "WTPByModelByTreatmentPlot_2022_03_24.jpeg",
  width=30,height=25,units = "cm",dpi=500)


# scale_y_continuous(limits = c(-50, 75), breaks = seq(-50, 75, 5))+


Labels <- c(
  "Plants5",
  "Plants10",
  "NativeDecrease",
  "NativeIncrease",
  "Appearance_W",
  "Appearance_WYB",
  "Appearance_WYBPR",
  "PollinatorQualityMedium",
  "PollinatorQualityHigh"
)

# Pooled_ConditionalWTPSummary



WTPSummaries <- rbind(
  cbind(Pooled_ConditionalWTPSummary %>% select(!starts_with("b_cost")),"Treatment"=rep("Pooled",nrow(Pooled_ConditionalWTPSummary))),
  cbind(Annual_ConditionalWTPSummary %>% select(!starts_with("b_cost")),"Treatment"=rep("Annual",nrow(Annual_ConditionalWTPSummary))),
  cbind(Control_ConditionalWTPSummary %>% select(!starts_with("b_cost")),"Treatment"=rep("Control",nrow(Control_ConditionalWTPSummary))),
  cbind(Perennial_ConditionalWTPSummary %>% select(!starts_with("b_cost")),"Treatment"=rep("Perennial",nrow(Perennial_ConditionalWTPSummary))))%>% 
  melt(id.vars="Treatment") 
  
  ggsave(ggplot(WTPSummaries,aes(x = value, y = variable, group=variable,fill = variable)) + 
           geom_density_ridges()+
           geom_vline(xintercept = 0)+
           facet_wrap( ~ Treatment,scales="free_x",labeller = as_labeller(
             c('Pooled' = paste0("Pooled Sample (N = ", nrow(Pooled_ConditionalWTP),")"),
               'Annual' = paste0("Annual Sample (N = ", nrow(Annual_ConditionalWTPSummary),")"),
               'Control' = paste0("Control Sample (N = ", nrow(Control_ConditionalWTPSummary),")"),
               'Perennial' = paste0("Perennial Sample (N = ", nrow(Perennial_ConditionalWTPSummary),")")
               )))+
           scale_y_discrete(name="Attribute",
                                                                     label=Labels)+
           ggtitle("Distribution of individual-level attribute WTP.")+ 
           scale_fill_manual(name="Attributes",
                             values=c(
                               "springgreen",
                               "springgreen4",
                               "gold3",
                               "red4",
                               "cornsilk",
                               "cornsilk1",
                               "cornsilk3",
                               "dodgerblue1",
                               "dodgerblue4"
                             ),
                             label=Labels,
                             guide=guide_legend(reverse = TRUE))+
           theme(axis.text = element_text(size=12),legend.background=element_blank(),
                 legend.box.background = element_rect(colour="black"),legend.text = element_text(size=12)),
device = "jpeg",
filename = "WTPDensityByTreatmentPlot_2022_03_24.jpeg",
width=30,height=25,units = "cm",dpi=500)

  
  

# Pooled Model But Separate Treatments: -----------------------------------
  
database <- data.frame(read.csv("database_Meadows_2022_03_11.csv",encoding="latin1"))
  
  Pooled_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_Levels_FullSample_2022_03_24_WTP.csv"))
  
  Pooled_ConditionalWTPSummary <-data.frame(cbind("b_Cost"=Pooled_ConditionalWTP$b_Cost.post.mean,
                                                  "b_Plants5"=Pooled_ConditionalWTP$b_Plants5.post.mean,
                                                  "b_Plants10"=Pooled_ConditionalWTP$b_Plants10.post.mean,
                                                  "b_NativeDecrease"=Pooled_ConditionalWTP$b_NativeDecrease.post.mean,
                                                  "b_NativeIncrease"=Pooled_ConditionalWTP$b_NativeIncrease.post.mean,
                                                  "b_Appearance_W"=Pooled_ConditionalWTP$b_Appearance_W.post.mean,
                                                  "b_Appearance_WYB"=Pooled_ConditionalWTP$b_Appearance_WYB.post.mean,
                                                  "b_Appearance_WYBPR"=Pooled_ConditionalWTP$b_Appearance_WYBPR.post.mean,
                                                  "b_PollinatorQualityMedium"=Pooled_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
                                                  "b_PollinatorQualityHigh"=Pooled_ConditionalWTP$b_PollinatorQualityHigh.post.mean))
  
Pooled_ConditionalWTPSummary <- cbind(Pooled_ConditionalWTPSummary,data.frame("Type"=database %>% distinct(type,ID)))
TreatmentSummaryFromPooled <- rbind(
    cbind(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type>0,1:10] ,"Treatment"=rep("Pooled",nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type>0,1:10]))),
    cbind(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==1,1:10],"Treatment"=rep("Control",nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==1,1:10]))),
    cbind(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==2,1:10],"Treatment"=rep("Perennial",nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==2,1:10]))),
    cbind(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==3,1:10],"Treatment"=rep("Annual",nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==3,1:10]))))%>% 
    melt(id.vars="Treatment")   
  
Labels <- c(
  "Cost",
  "Plants5",
  "Plants10",
  "NativeDecrease",
  "NativeIncrease",
  "Appearance_W",
  "Appearance_WYB",
  "Appearance_WYBPR",
  "PollinatorQualityMedium",
  "PollinatorQualityHigh"
)
ggsave(ggplot(TreatmentSummaryFromPooled,aes(x = value, y = variable, group=variable,fill = variable)) + 
         geom_density_ridges()+
         geom_vline(xintercept = 0)+
         facet_wrap( ~ Treatment,labeller = as_labeller(
           c('Pooled' = paste0("Pooled Sample (N = ", nrow(Pooled_ConditionalWTP),")"),
             'Annual' = paste0("Annual Sample (N = ", nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==1,1:10]),")"),
             'Control' = paste0("Control Sample (N = ", nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==2,1:10]),")"),
             'Perennial' = paste0("Perennial Sample (N = ", nrow(Pooled_ConditionalWTPSummary[Pooled_ConditionalWTPSummary$Type.type==3,1:10]),")")
           )))+
         scale_y_discrete(name="Attribute",
                          label=Labels)+
         ggtitle("Distribution of individual-level attribute WTP.")+ 
         scale_fill_manual(name="Attributes",
                           values=c("black",
                             "springgreen",
                             "springgreen4",
                             "gold3",
                             "red4",
                             "cornsilk",
                             "cornsilk1",
                             "cornsilk3",
                             "dodgerblue1",
                             "dodgerblue4"
                           ),
                           label=Labels,
                           guide=guide_legend(reverse = TRUE))+
         theme(axis.text = element_text(size=12),legend.background=element_blank(),
               legend.box.background = element_rect(colour="black"),legend.text = element_text(size=12)),
       device = "jpeg",
       filename = "WTPDensityByTreatmentPlotFromPooled_2022_03_24.jpeg",
       width=30,height=25,units = "cm",dpi=500)




# Density Plots By City ---------------------------------------------------


Pooled_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_Levels_FullSample_2022_03_24_WTP.csv"))
Bristol_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_LevelsNoCovariates_Bristol_FullSample_2022_03_24_WTP.csv"))
Leeds_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_LevelsNoCovariates_Leeds_FullSample_2022_03_24_WTP.csv"))
Edinburgh_ConditionalWTP <- data.frame(read.csv("Meadows_MXL_LevelsNoCovariates_Edinburgh_FullSample_2022_03_24_WTP.csv"))

Pooled_ConditionalWTP <- data.frame(Pooled_ConditionalWTP %>% select(ends_with("post.mean")))
Bristol_ConditionalWTP <- data.frame(Bristol_ConditionalWTP %>% select(ends_with("post.mean")))
Leeds_ConditionalWTP <- data.frame(Leeds_ConditionalWTP %>% select(ends_with("post.mean")))
Edinburgh_ConditionalWTP <- data.frame(Edinburgh_ConditionalWTP %>% select(ends_with("post.mean")))

# 
# Pooled_ConditionalWTPSummary <-data.frame(cbind("b_Cost"=Pooled_ConditionalWTP$b_Cost.post.mean,
#                                                 "b_Plants5"=Pooled_ConditionalWTP$b_Plants5.post.mean,
#                                                 "b_Plants10"=Pooled_ConditionalWTP$b_Plants10.post.mean,
#                                                 "b_NativeDecrease"=Pooled_ConditionalWTP$b_NativeDecrease.post.mean,
#                                                 "b_NativeIncrease"=Pooled_ConditionalWTP$b_NativeIncrease.post.mean,
#                                                 "b_Appearance_W"=Pooled_ConditionalWTP$b_Appearance_W.post.mean,
#                                                 "b_Appearance_WYB"=Pooled_ConditionalWTP$b_Appearance_WYB.post.mean,
#                                                 "b_Appearance_WYBPR"=Pooled_ConditionalWTP$b_Appearance_WYBPR.post.mean,
#                                                 "b_PollinatorQualityMedium"=Pooled_ConditionalWTP$b_PollinatorQualityMedium.post.mean,
#                                                 "b_PollinatorQualityHigh"=Pooled_ConditionalWTP$b_PollinatorQualityHigh.post.mean))
Labels <- c(
  "Plants5",
  "Plants10",
  "NativeDecrease",
  "NativeIncrease",
  "Appearance_W",
  "Appearance_WYB",
  "Appearance_WYBPR",
  "PollinatorQualityMedium",
  "PollinatorQualityHigh"
)


CitySummary <- rbind(
  cbind(Pooled_ConditionalWTP %>% select(!starts_with("b_cost")),"City"=rep("Pooled",nrow(Pooled_ConditionalWTP))),
  cbind(Bristol_ConditionalWTP %>% select(!starts_with("b_cost")),"City"=rep("Bristol",nrow(Bristol_ConditionalWTP))),
  cbind(Leeds_ConditionalWTP %>% select(!starts_with("b_cost")),"City"=rep("Leeds",nrow(Leeds_ConditionalWTP))),
  cbind(Edinburgh_ConditionalWTP %>% select(!starts_with("b_cost")),"City"=rep("Edinburgh",nrow(Edinburgh_ConditionalWTP))))%>% 
  melt(id.vars="City") 

ggsave(ggplot(CitySummary,aes(x = value, y = variable, group=variable,fill = variable)) + 
         geom_density_ridges()+
         geom_vline(xintercept = 0)+
         facet_wrap( ~ City,scales="free_x",labeller = as_labeller(
           c('Pooled' = paste0("Pooled Sample (N = ", nrow(Pooled_ConditionalWTP),")"),
             'Bristol' = paste0("Bristol Sample (N = ", nrow(Bristol_ConditionalWTP),")"),
             'Leeds' = paste0("Leeds Sample (N = ", nrow(Leeds_ConditionalWTP),")"),
             'Edinburgh' = paste0("Edinburgh Sample (N = ", nrow(Edinburgh_ConditionalWTP),")")
           )))+
         scale_y_discrete(name="Attribute",
                          label=Labels)+
         ggtitle("Distribution of individual-level attribute WTP.")+ 
         scale_fill_manual(name="Attributes",
                           values=c(
                             "springgreen",
                             "springgreen4",
                             "gold3",
                             "red4",
                             "cornsilk",
                             "cornsilk1",
                             "cornsilk3",
                             "dodgerblue1",
                             "dodgerblue4"
                           ),
                           label=Labels,
                           guide=guide_legend(reverse = TRUE))+
         theme(axis.text = element_text(size=12),legend.background=element_blank(),
               legend.box.background = element_rect(colour="black"),legend.text = element_text(size=12)),
       device = "jpeg",
       filename = "WTPDensityByCity_2022_03_24.jpeg",
       width=30,height=25,units = "cm",dpi=500)


