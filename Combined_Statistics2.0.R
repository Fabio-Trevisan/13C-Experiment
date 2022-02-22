library(readr)
library(ggpubr)
library(tidyverse)
library(plyr)
library(dplyr)
library(rstatix)
library(purrr)
library(agricolae)
library(ARTofR)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                                            --
##BASIC_STATISTICS_2.0------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#Read CSV----
table <- read.csv("13C_IRMS_R_data.csv", sep=";",
                  header=T)

vector_Species_Tissue <- c("Tomato_Root",
                            "Tomato_Shoot",
                            "Cucumber_Root",
                            "Cucumber_Shoot",
                            "Maize_Root",
                            "Maize_Shoot",
                            "Barley_Root",
                            "Barley_Shoot")



#create Subsets according to Species_Tissue----
Subsets <- lapply(vector_Species_Tissue, function(i){ 
  i <- subset(table, Species_Tissue == i)
})

names(Subsets) <- vector_Species_Tissue



#transform variable to factor----
table$Time <- factor(table$Time)
lapply(vector_Species_Tissue, function(i){
  Subsets[[i]][["Time"]] <- factor(Subsets[[i]][["Time"]])
})



#2way ANOVA----
##Multiple Species_Tissue
TwoWay_Anova <- lapply(split(table, table$Species_Tissue), function(i){
  anova(lm(Value ~ Treatment * Time, data = i))
})
View(TwoWay_Anova[["Species_Tissue"]])
write.table(TwoWay_Anova, file = "TwoWay_Anova_results.csv", quote = FALSE, sep = ";")



#1way ANOVA----
##Treatment
OneWay_Anova_Tr <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Time"]]), function(i){ 
    aov(Value ~ Treatment, data = i)
  })
})
names(OneWay_Anova_Tr) <- vector_Species_Tissue

##Time
OneWay_Anova_Ti <- lapply(vector_Species_Tissue, function(m){
 lapply(split(Subsets[[m]], Subsets[[m]][["Treatment"]]), function(i){ 
    aov(Value ~ Time, data = i)
  })
})
names(OneWay_Anova_Ti) <- vector_Species_Tissue

##OneWayAnova save
  sink("OneWayAnova_Results_Tr.txt")
  OneWay_Anova_Tr 
  sink(NULL)
  
  sink("OneWayAnova_Results_Ti.txt")
  OneWay_Anova_Ti 
  sink(NULL)



#Tukey as post hoc test----
##Treatment
HSD_Tr <- lapply(vector_Species_Tissue, function(m){
  lapply(names(OneWay_Anova_Tr[[m]]), function(i){ 
    HSD.test(OneWay_Anova_Tr[[m]][[i]], "Treatment")
  })
})
names(HSD_Tr) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Tr[[i]]) 
  names(HSD_Tr[[i]]) <- list
}

##Time
HSD_Ti <- lapply(vector_Species_Tissue, function(m){
  lapply(names(OneWay_Anova_Ti[[m]]), function(i){ 
    HSD.test(OneWay_Anova_Ti[[m]][[i]], "Time")
  })
})
names(HSD_Ti) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Ti[[i]]) 
  names(HSD_Ti[[i]]) <- list
}

##HSD_test save
##Treatment
HSD_Tr_groups <- lapply(vector_Species_Tissue, function(i){
  lapply(names(OneWay_Anova_Tr[[i]]), function(m){
    as.data.frame(HSD_Tr[[i]][[m]][["groups"]])
  })
})
names(HSD_Tr_groups) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Tr[[i]]) 
  names(HSD_Tr_groups[[i]]) <- list
}
write.table(HSD_Tr_groups, "HSD_Tr.csv", quote = FALSE, sep = ";")

##Time
HSD_Ti_groups <- lapply(vector_Species_Tissue, function(i){
  lapply(names(OneWay_Anova_Ti[[i]]), function(m){
    as.data.frame(HSD_Ti[[i]][[m]][["groups"]])
  })
})
names(HSD_Ti_groups) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Ti[[i]]) 
  names(HSD_Ti_groups[[i]]) <- list
}
write.table(HSD_Ti_groups, "HSD_Ti.csv", quote = FALSE, sep = ";")



#Assumptions----
# 1. Homogeneity of variances
plot(TwoWay_Anova_tr, 1)
levene_test(Value ~ Treatment * Time, data = Tomato_Root)

# 2. Normality
plot(TwoWay_Anova_tr, 2)

## Extract the residuals
aov_residuals <- residuals(object = TwoWay_Anova_tr)
## Run Shapiro-Wilk test
shapiro.test(x = aov_residuals)
shapiro.test(Tomato_Root)

#Shapiro-Wilk test for all single treatments
SW_test <- table %>%
  group_by(Time, Treatment, Species_Tissue) %>%
  shapiro_test(Value)
View(SW_test)
write.table(SW_test, file = "ShapiroWilk_test_results.csv", quote = FALSE, sep = ";")
