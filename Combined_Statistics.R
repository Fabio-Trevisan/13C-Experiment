library(readr)
library(ggpubr)
library(tidyverse)
library(plyr)
library(dplyr)
library(rstatix)
library(purrr)

#calculate means and errors in R 
table <- read.csv("13C_IRMS_R_data.csv", sep=";",
                   header=T)

#non serve 
#Summary_table <- ddply(table, c("Treatment", "Time", "Species_Tissue"), summarise,
                       #N    = sum(!is.na(Value)),
                       #mean = mean(Value, na.rm=TRUE),
                       #sd   = sd(Value, na.rm=TRUE),
                       #se   = sd / sqrt(N))

#non serve 
#my_comparisons <-list(c("C","-P"), c("C","-Fe"), c("C","-P/-Fe"), c("-P","-Fe"), c("-P","-P/-Fe"),c("-Fe","-P/-Fe"))
#compare_means(Value ~ Treatment, group.by =  c("Time","Species_Tissue"), data = table, method = "t.test") 


#creare Subsets according to Species_Tissue
Tomato_Root <- subset(table, Species_Tissue == "Tomato_Root")
head(Tomato_Root)
Tomato_Shoot <- subset(table, Species_Tissue == "Tomato_Shoot")
head(Tomato_Shoot)
Cucumber_Root <- subset(table, Species_Tissue == "Cucumber_Root")
head(Cucumber_Root)
Cucumber_Shoot <- subset(table, Species_Tissue == "Cucumber_Shoot")
head(Cucumber_Shoot)
Maize_Root <- subset(table, Species_Tissue == "Maize_Root")
head(Maize_Root)
Maize_Shoot <- subset(table, Species_Tissue == "Maize_Shoot")
head(Maize_Shoot)
Barley_Root <- subset(table, Species_Tissue == "Barley_Root")
head(Barley_Root)
Barley_Shoot <- subset(table, Species_Tissue == "Barley_Shoot")
head(Barley_Shoot)

#transform variable to factor
table$Time <- factor(table$Time)


#Shapiro-Wilk normality check and/or qq-plot and/or density plot
#for all single treatments
SW_test <- table %>%
  group_by(Time, Treatment, Species_Tissue) %>%
  shapiro_test(Value)
View(SW_test)
write.table(SW_test, file = "ShapiroWilk_test_results.csv", quote = FALSE, sep = ";")

#non serve 
#density_plot <- ggdensity(Species_Tissue$Value, main = "Density plot 13C", xlab = "Delta 13C")
#density_plot

#non serve 
#qq_plot <- ggqqplot(Species_Tissue$Value)
#qq_plot



#1way ANOVA (NOT WORKING)
OneWay_Anova_Treatment <- lapply(split(table, list(table$Species_Tissue,table$Time)), 
                                 anova(lm(Value ~ Treatment, data = table)))


#2way ANOVA

##Single Species_Tissue
Tomato_Root$Time <- factor(Tomato_Root$Time)
TwoWay_Anova_tr <- aov(Value ~ Treatment * Time, data = Tomato_Root)

##Multiple Species_Tissue
TwoWay_Anova <- lapply(split(table, table$Species_Tissue), function(i){
  anova(lm(Value ~ Treatment * Time, data = i))
})
summary(TwoWay_Anova)


# Tukey as post hoc test
#work only on sigle anovas not on lapply list --> maybe try sapply/tapply
TukeyHSD(TwoWay_Anova_tr)


# 1. Homogeneity of variances
plot(TwoWay_Anova_tr, 1)
levene_test(Value ~ Treatment * Time, data = Tomato_Root)

# 2. Normality
plot(TwoWay_Anova_tr, 2)

## Extract the residuals
aov_residuals <- residuals(object = TwoWay_Anova_tr)
## Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )





#Kruskal Wallis NOT WORKING
##Single Species_Tissue
Tomato_Root$Time <- factor(Tomato_Root$Time)
TwoWay_KW_tr <- kruskal.test(Value ~ Treatment * Time, data = Tomato_Root)

##Multiple Species_Tissue
TwoWay_KW <- lapply(split(table, table$Species_Tissue), function(i){
  kruskal.test(lm(Value ~ Treatment * Time, data = i))
})


kruskal.test(Value ~ Treatment, data = table)
#or
kruskal_test(table, Value ~ Treatment)

