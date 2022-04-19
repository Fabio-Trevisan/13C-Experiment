library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr)
library(dplyr)



#Scatter line + error bars (STD.Err) --> calculate means and errors in R ####

table2 <- read.csv("DATA_13C_IRMS_R.csv", sep=";",
                  header=T)
OR

table2 <- read.csv("DATA_13C_IRMS_R_2.0.csv", sep=";",
                   header=T)

Summary_table <- ddply(table2, c("Treatment", "Time", "Species_Tissue"), summarise,
               N    = sum(!is.na(Value)),
               mean = mean(Value, na.rm=TRUE),
               sd   = sd(Value, na.rm=TRUE),
               se   = sd / sqrt(N))
Summary_table


f2 <- ggplot(Summary_table, aes(x = Time, y = mean, group = Treatment, colour = Treatment)) + 
  geom_line(aes(group = Treatment)) + 
  geom_point(aes(shape = Treatment)) + 
  scale_shape_manual(values = c(15:18)) +
  scale_color_manual(values=c("grey77", "darkorange2", "skyblue3", "slateblue3"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, group = Treatment), width = 0.5) +
  theme_bw() + 
  scale_y_continuous(breaks=seq(-40,-11,1)) +
  scale_x_continuous(breaks=seq(-17,16,1))
f2 + facet_wrap(~Species_Tissue, scales="free", ncol = 2) + 
  ylab("13C") + 
  xlab("Time (Days)") 


ggsave(filename = "13C_Scatter-lines_errorbars(bw).pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)

OR

ggsave(filename = "13C_Scatter-lines_errorbars(bw)2.0.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)
