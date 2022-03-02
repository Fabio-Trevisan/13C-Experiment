library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)
library(agricolae)


#read csv & rename columns ####
table <- read.csv("DATA_Evapotransp._R.csv", sep=";",
                  header=T)
name <- "Transpiration_Rate"
vector <- seq(0, 1.5, 0.1)

#or
table <- read.csv("DATA_Photos.Rate_R.csv", sep=";",
                  header=T)
name <- "Photosynthetic_Rate"
vector <- seq(0, 3.5, 0.5)

#or
table <- read.csv("DATA_Stom.Cond._R.csv", sep=";",
                  header=T)
name <- "Stomatal_Conductance"
vector <- seq(0,0.15,0.01)

colnames(table) <- c("Treatment", "Barley", "Cucumber", "Maize", "Tomato")



#melt & group & remove missing values ####
table1 <- melt(table, id=c("Treatment"))

table_1_sum<-table1 %>% 
  group_by(variable)

Table_2_sum <- table_1_sum %>% drop_na(value)



#raw boxplots ####
#p<-ggplot(Table_2_sum, aes(x= Treatment, value, fill=Treatment)) + theme_bw() + geom_boxplot() +
  ##facet_wrap(~variable, scales="free") + ylab(name) + xlab("Treatments") + scale_fill_manual(values=c("white","brown","red", "black"))

#ggsave(filename = paste(name, "_RAW.pdf", sep = ""), plot = last_plot(), dpi = 600, units = "cm", width = 92, height = 45, scale = 1)


# + geom_jitter() removed because of too many points!!


#complete boxplots ####
my_order<- c("C", "P", "Fe", "P.Fe")
ggplot(Table_2_sum, aes(x= Treatment, value, fill=Treatment))+  
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(width=0.5)+ 
  labs(caption ="(based on ADC instruement. Fabio Trevisan)")+ 
  theme(legend.position = "NONE") + theme_classic() +
  scale_y_continuous(breaks=vector) +
  facet_wrap(~variable, scales="free") + ylab(name) + xlab("Treatments") +
  scale_x_discrete(limits=my_order) +
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3", "slateblue3"))
  

ggsave(filename = paste(name, ".pdf", sep = ""), plot = last_plot(), dpi = 600, units = "cm", width = 60, height = 50, scale = 0.5)



#statistics ####
##anova
OneWay_Anova_Boxplot <- lapply(split(Table_2_sum, Table_2_sum[["variable"]]), function(i){ 
  aov(value ~ Treatment, data = i)
})
sink(paste(name, "OneWay_Anova_Boxplot.csv", sep = "_"))
OneWay_Anova_Boxplot
sink(NULL)

#Tukey
##HSD complete
HSD_Boxplot <- lapply(names(OneWay_Anova_Boxplot), function(i){ 
  HSD.test(OneWay_Anova_Boxplot[[i]], "Treatment")
})
names(HSD_Boxplot) <- names(OneWay_Anova_Boxplot)

##HSD groups only
HSD_Boxplot_groups <- lapply(names(OneWay_Anova_Boxplot), function(i){
  as.data.frame(HSD_Boxplot[[i]][["groups"]])
})
names(HSD_Boxplot_groups) <- names(OneWay_Anova_Boxplot)

sink(paste(name, "HSD_Boxplot.csv", sep = "_"))
HSD_Boxplot_groups
sink(NULL)