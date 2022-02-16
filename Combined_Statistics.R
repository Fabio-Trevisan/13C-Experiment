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
##BASIC_STATISTICS------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#calculate means and errors in R----

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


#creare Subsets according to Species_Tissue----
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



#transform variable to factor----
table$Time <- factor(table$Time)


#Shapiro-Wilk normality check and/or qq-plot and/or density plot----
#for all single treatments
SW_test <- table %>%
  group_by(Time, Treatment, Species_Tissue) %>%
  shapiro_test(Value)
View(SW_test)
write.table(SW_test, file = "ShapiroWilk_test_results.csv", quote = FALSE, sep = ";")

#non serve 
density_plot <- ggdensity(Tomato_Root$Value, main = "Density plot 13C", xlab = "Delta 13C")
density_plot

#non serve
qq_plot <- ggqqplot(Tomato_Root$Value)
qq_plot



#1way ANOVA (TO OPTIMIZE)----
##Treatment
OneWay_Anova_Tr_tr <- lapply(split(Tomato_Root, Tomato_Root$Time), function(i){ 
                                 anova(lm(Value ~ Treatment, data = i))
})
write.table(OneWay_Anova_Tr_tr, file = "OneWay_Anova_Tr_tr.csv", quote = FALSE, sep = ";")

OneWay_Anova_Tr_ts <- lapply(split(Tomato_Shoot, Tomato_Shoot$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

OneWay_Anova_Tr_br <- lapply(split(Barley_Root, Barley_Root$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

OneWay_Anova_Tr_bs <- lapply(split(Barley_Shoot, Barley_Shoot$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

OneWay_Anova_Tr_mr <- lapply(split(Maize_Root, Maize_Root$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

OneWay_Anova_Tr_ms <- lapply(split(Maize_Shoot, Maize_Shoot$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

OneWay_Anova_Tr_cr <- lapply(split(Cucumber_Root, Cucumber_Root$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

OneWay_Anova_Tr_cs <- lapply(split(Cucumber_Shoot, Cucumber_Shoot$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

##Time
OneWay_Anova_Ti_tr <- lapply(split(Tomato_Root, Tomato_Root$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_ts <- lapply(split(Tomato_Shoot, Tomato_Shoot$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_br <- lapply(split(Barley_Root, Barley_Root$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_bs <- lapply(split(Barley_Shoot, Barley_Shoot$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_mr <- lapply(split(Maize_Root, Maize_Root$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_ms <- lapply(split(Maize_Shoot, Maize_Shoot$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_cr <- lapply(split(Cucumber_Root, Cucumber_Root$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

OneWay_Anova_Ti_cs <- lapply(split(Cucumber_Shoot, Cucumber_Shoot$Treatment), function(i){ 
  anova(lm(Value ~ Time, data = i))
})

##test
OneWay_Anova_xx <- aov(Value ~ Treatment, data = Tomato_Root)
summary(OneWay_Anova_xx)


#2way ANOVA----
##Single Species_Tissue
Tomato_Root$Time <- factor(Tomato_Root$Time)
TwoWay_Anova_tr <- aov(Value ~ Treatment * Time, data = Tomato_Root)
summary(TwoWay_Anova_tr)
...*8


##Multiple Species_Tissue
TwoWay_Anova <- lapply(split(table, table$Species_Tissue), function(i){
  anova(lm(Value ~ Treatment * Time, data = i))
})
View(TwoWay_Anova[["Species_Tissue"]])

write.table(TwoWay_Anova, file = "TwoWay_Anova_results.csv", quote = FALSE, sep = ";")


# Tukey as post hoc test----
#work only on sigle anovas not on lapply list
TukeyHSD(OneWay_Anova_Tr_tr)

Tukey_test <- HSD.test(OneWay_Anova_xx, "Treatment")
Tukey_test2 <- HSD.test(OneWay_Anova_Tr_tr["0"], "Treatment")




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



#Kruskal Wallis NOT WORKING ----
##Single Species_Tissue
TwoWay_KW_tr <- kruskal.test(Value ~ Treatment * Time, data = Tomato_Root)

##Multiple Species_Tissue
TwoWay_KW <- lapply(split(table, table$Species_Tissue), function(i){
  kruskal.test(lm(Value ~ Treatment * Time, data = i))
})


kruskal.test(Value ~ Treatment, data = table)
#or
kruskal_test(table, Value ~ Treatment)

dunn test