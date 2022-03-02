library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr)
library(dplyr)



#Scatter line + error bars (STD.Err) --> calculate means and errors in R ####

table2 <- read.csv("DATA_Masses.csv", sep=";",
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
  scale_color_manual(values=c("darkorange2", "skyblue3", "slateblue3", "grey77"))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se, group = Treatment), width = 0.5) +
  theme_bw() + 
  scale_x_continuous(breaks=seq(0,16,1))
f2 + facet_wrap(~Species_Tissue, scales="free", ncol = 2) + 
  ylab("Mass (g)") + 
  xlab("Time (Days)") 


ggsave(filename = "Masses_Scatter-lines_errorbars(bw).pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)
