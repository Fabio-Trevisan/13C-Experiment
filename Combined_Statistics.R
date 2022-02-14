library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr)
library(dplyr)

#calculate means and errors in R 
table <- read.csv("13C_IRMS_R_data.csv", sep=";",
                   header=T)

Summary_table <- ddply(table, c("Treatment", "Time", "Species_Tissue"), summarise,
                       N    = sum(!is.na(Value)),
                       mean = mean(Value, na.rm=TRUE),
                       sd   = sd(Value, na.rm=TRUE),
                       se   = sd / sqrt(N))


#non serve 
#my_comparisons <-list(c("C","-P"), c("C","-Fe"), c("C","-P/-Fe"), c("-P","-Fe"), c("-P","-P/-Fe"),c("-Fe","-P/-Fe"))
#compare_means(Value ~ Treatment, group.by =  c("Time","Species_Tissue"), data = table, method = "t.test") 



#Holm-Sidak normality check and/or qq-plot



#one way ANOVA with Tukey as post hoc test
