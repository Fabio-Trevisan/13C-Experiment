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
  theme_bw() 
f2 + facet_wrap(~Species_Tissue, scales = "free",  ncol = 2) + 
  ylab("13C") + 
  xlab("Time (Days)")

or
f2 + facet_wrap(~Species_Tissue, scales = "free_y",  ncol = 2) + 
  geom_blank(aes(y = -39.5)) +
  geom_blank(aes(y = -27)) +
  ylab("13C") + 
  xlab("Time (Days)")
 
or
f2 + facet_wrap(~Species_Tissue, scales = "free_y",  ncol = 2) + 
  geom_blank(aes(y = -36.5)) +
  geom_blank(aes(y = -30.8)) +
  ylab("13C") + 
  xlab("Time (Days)")

or
f2 + facet_wrap(~Species_Tissue, scales = "free_y",  ncol = 2) + 
  geom_blank(aes(y = -18.2)) +
  geom_blank(aes(y = -12)) +
  ylab("13C") + 
  xlab("Time (Days)")

or
f2 + facet_wrap(~Species_Tissue, scales = "free_y",  ncol = 2) + 
  geom_blank(aes(y = -39.5)) +
  geom_blank(aes(y = -31)) +
  ylab("13C") + 
  xlab("Time (Days)")
ggsave(filename = "000Tomato.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)


ggsave(filename = "13C_Scatter-lines_errorbars(bw).pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)

OR

ggsave(filename = "13C_Scatter-lines_errorbars(bw)2.0.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)


write.table(Summary_table, "13C_summary_statistics.csv", quote = FALSE, sep = ";")

OR

write.table(Summary_table, "13C_summary_statistics2.0.csv", quote = FALSE, sep = ";")
