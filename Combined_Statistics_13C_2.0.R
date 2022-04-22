library(readr)
library(ggpubr)
library(tidyverse)
library(plyr)
library(dplyr)
library(rstatix)
library(purrr)
library(agricolae)


#13C statistics 2.0 (extended)####

#for Time ####
#Read CSV ####
table <- read.csv("DATA_13C_IRMS_R_2.0.csv", sep=";",
                  header=T)

vector_Species_Tissue <- c("Barley_Root",
                           "Barley_Shoot",
                           "Cucumber_Root",
                           "Cucumber_Shoot",
                           "Maize_Root",
                           "Maize_Shoot",
                           "Tomato_Root",
                           "Tomato_Shoot")



#create Subsets according to Species_Tissue ####
Subsets <- lapply(vector_Species_Tissue, function(i){ 
  i <- subset(table, Species_Tissue == i)
})

names(Subsets) <- vector_Species_Tissue



#transform variable to factor ####
table$Time <- factor(table$Time)

Subsets[["Barley_Root"]][["Time"]] <- factor(Subsets[["Barley_Root"]][["Time"]])
Subsets[["Barley_Shoot"]][["Time"]] <- factor(Subsets[["Barley_Shoot"]][["Time"]])
Subsets[["Cucumber_Root"]][["Time"]] <- factor(Subsets[["Cucumber_Root"]][["Time"]])
Subsets[["Cucumber_Shoot"]][["Time"]] <- factor(Subsets[["Cucumber_Shoot"]][["Time"]])
Subsets[["Maize_Root"]][["Time"]] <- factor(Subsets[["Maize_Root"]][["Time"]])
Subsets[["Maize_Shoot"]][["Time"]] <- factor(Subsets[["Maize_Shoot"]][["Time"]])
Subsets[["Tomato_Root"]][["Time"]] <- factor(Subsets[["Tomato_Root"]][["Time"]])
Subsets[["Tomato_Shoot"]][["Time"]] <- factor(Subsets[["Tomato_Shoot"]][["Time"]])



#Assumptions ####
## 1. Homogeneity of variances
##Treatment*Time
Levene_test <- lapply(split(table, table$Species_Tissue), function(i){
  levene_test(Value ~ Treatment * Time, data = i)
})

##2. Normality
##Shapiro-Wilk test for all single treatments
SW_test <- table %>%
  group_by(Time, Treatment, Species_Tissue) %>%
  shapiro_test(Value)
View(SW_test)
write.table(SW_test, file = "13C_ShapiroWilk_test_results_2.0.csv", quote = FALSE, sep = ";")

##3. Indipendency
Data are indepent by experimental design!
  
  
  
  #2way ANOVA ####
##Multiple Species_Tissue
TwoWay_Anova <- lapply(split(table, table$Species_Tissue), function(i){
  anova(lm(Value ~ Treatment * Time, data = i))
})
View(TwoWay_Anova[["Species_Tissue"]])
write.table(TwoWay_Anova, file = "13C_TwoWay_Anova_results_2.0.csv", quote = FALSE, sep = ";")

sink("13C_TwoWay_Anova_results2_2.0.csv")
TwoWay_Anova
sink(NULL)


#1way ANOVA ####
##Time.for tukey
OneWay_Anova_Ti <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Treatment"]]), function(i){ 
    aov(Value ~ Time, data = i)
  })
})
names(OneWay_Anova_Ti) <- vector_Species_Tissue

##Time.for print
OneWay_Anova_Ti2 <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Treatment"]]), function(i){ 
    anova(lm(Value ~ Time, data = i))
  })
})
names(OneWay_Anova_Ti2) <- vector_Species_Tissue

##OneWayAnova save
sink("13C_OneWayAnova_Results_Ti_2.0.csv")
OneWay_Anova_Ti2 
sink(NULL)



#Tukey as post hoc test ####
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
sink("13C_HSD_Ti_2.0.csv")
HSD_Ti_groups 
sink(NULL)



#for Treatment ####
#Read CSV ####
table <- read.csv("DATA_13C_IRMS_R.csv", sep=";",
                  header=T)

vector_Species_Tissue <- c("Barley_Root",
                           "Barley_Shoot",
                           "Cucumber_Root",
                           "Cucumber_Shoot",
                           "Maize_Root",
                           "Maize_Shoot",
                           "Tomato_Root",
                           "Tomato_Shoot")



#create Subsets according to Species_Tissue ####
Subsets <- lapply(vector_Species_Tissue, function(i){ 
  i <- subset(table, Species_Tissue == i)
})

names(Subsets) <- vector_Species_Tissue



#transform variable to factor ####
table$Time <- factor(table$Time)

Subsets[["Barley_Root"]][["Time"]] <- factor(Subsets[["Barley_Root"]][["Time"]])
Subsets[["Barley_Shoot"]][["Time"]] <- factor(Subsets[["Barley_Shoot"]][["Time"]])
Subsets[["Cucumber_Root"]][["Time"]] <- factor(Subsets[["Cucumber_Root"]][["Time"]])
Subsets[["Cucumber_Shoot"]][["Time"]] <- factor(Subsets[["Cucumber_Shoot"]][["Time"]])
Subsets[["Maize_Root"]][["Time"]] <- factor(Subsets[["Maize_Root"]][["Time"]])
Subsets[["Maize_Shoot"]][["Time"]] <- factor(Subsets[["Maize_Shoot"]][["Time"]])
Subsets[["Tomato_Root"]][["Time"]] <- factor(Subsets[["Tomato_Root"]][["Time"]])
Subsets[["Tomato_Shoot"]][["Time"]] <- factor(Subsets[["Tomato_Shoot"]][["Time"]])



#1way ANOVA ####
##Treatment.for tukey
OneWay_Anova_Tr <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Time"]]), function(i){ 
    aov(Value ~ Treatment, data = i)
  })
})
names(OneWay_Anova_Tr) <- vector_Species_Tissue


##Treatment.for print
OneWay_Anova_Tr2 <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Time"]]), function(i){ 
    anova(lm(Value ~ Treatment, data = i))
  })
})
names(OneWay_Anova_Tr2) <- vector_Species_Tissue


##OneWayAnova save
sink("13C_OneWayAnova_Results_Tr_2.0.csv")
OneWay_Anova_Tr2 
sink(NULL)




#Tukey as post hoc test ####
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
sink("13C_HSD_Tr_2.0.csv")
HSD_Tr_groups 
sink(NULL)
