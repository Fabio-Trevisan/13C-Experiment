library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)


table <- read.csv("Stom.Cond._R_data .csv", sep=";",
                  header=T)

#table<-table %>% select(1,2,3:6)
table

table1<-melt(table, id=c("Species"))
melt(table, id=c("Species")) 

#or
table_1_sum<-table1 %>% 
  group_by(Species)

#remove missing values
Table_2_sum <- table_1_sum %>% drop_na(value)


my_comparisons <-list(c("C","P"), c("C","Fe"), c("C","P/Fe"), c("P","Fe"), c("P","P/Fe"),c("Fe","P/Fe"))
compare_means(value ~ variable, group.by =  "Species", data = Table_2_sum, method = "t.test") 
p<-ggplot(Table_2_sum, aes(x= variable, value, fill=variable)) + theme_bw() + geom_boxplot() +
  facet_wrap(~Species, scales="free") + ylab("Stomatal Conductance") + xlab("Treatments") + scale_fill_manual(values=c("white","brown","red", "black"))
p+stat_compare_means(comparisons = my_comparisons, label = "p.ajust",  method = "t.test", hide.ns = TRUE)+
  theme_bw()+ylab("Stomatal Conductance")

ggsave(filename = "Stom.Cond..pdf", plot = last_plot(), dpi = 600, units = "cm", width = 92, height = 45, scale = 1)


# + geom_jitter() removed because of too many points!!


#more complete boxplot to implement in the automated generation above
my_order<- c("C", "-P", "-Fe", "-P/-Fe")
ggplot(Cucumber_Norm_StomConduc, aes(x=Treatment, y=Values))+  
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(width=0.5, fill=c("grey77", "skyblue3", "darkorange2", "slateblue3"))+ 
  ylab(expression("Stomatal Conductance (mmol m"^-2*" s"^-1*")")) + 
  xlab("Treatment") + 
  ggtitle("Cucumber")+ 
  labs(caption ="(based on ADC instruement. Fabio Trevisan)")+ 
  theme(legend.position = "NONE") + theme_classic() +  
  theme(plot.title = element_text(size=30, face="bold", hjust=0.5))+ 
  scale_y_continuous(breaks=seq(0,0.10,0.025), limits=c(0.01,0.1))+
  scale_x_discrete(limits=my_order)


#reorder boxes in the boxplot according to median
ggplot(Cucumber_Norm_StomConduc, 
       
       aes(x=reorder(Treatment,Values), y=Values))+
  
  stat_boxplot(geom="errorbar", width=0.2) + 
  geom_boxplot(width=0.5, fill=c("grey77", "skyblue3", "darkorange2", "slateblue3")) + 
  ylab(expression("Stomatal Conductance (mmol m"^-2*" s"^-1*")")) + 
  xlab("Treatment") + 
  ggtitle("Cucumber")+ 
  labs(caption ="(based on ADC instruement. Fabio Trevisan)")+ 
  theme(legend.position = "NONE") + theme_classic() +  
  theme(plot.title = element_text(size=30, face="bold", hjust=0.4))+ 
  scale_y_continuous(breaks=seq(0,0.10,0.025), limits = c(0.01,0.1))