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


#Read CSV----

table <- read.csv("13C_IRMS_R_data.csv", sep=";",
                   header=T)

#calculate means and errors in R----
#non serve 
#Summary_table <- ddply(table, c("Treatment", "Time", "Species_Tissue"), summarise,
                       #N    = sum(!is.na(Value)),
                       #mean = mean(Value, na.rm=TRUE),
                       #sd   = sd(Value, na.rm=TRUE),
                       #se   = sd / sqrt(N))



#create Subsets according to Species_Tissue----
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




#transform variable to factor----
table$Time <- factor(table$Time)
Tomato_Root$Time <- factor(Tomato_Root$Time)
Tomato_Shoot$Time <- factor(Tomato_Shoot$Time)
Cucumber_Root$Time <- factor(Cucumber_Root$Time)
Cucumber_Shoot$Time <- factor(Cucumber_Shoot$Time)
Maize_Root$Time <- factor(Maize_Root$Time)
Maize_Shoot$Time <- factor(Maize_Shoot$Time)
Barley_Root$Time <- factor(Barley_Root$Time)
Barley_Shoot$Time <- factor(Barley_Shoot$Time)




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





#1way ANOVA----
##Treatment
OneWay_Anova_Tr_tr <- lapply(split(Tomato_Root, Tomato_Root$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_ts <- lapply(split(Tomato_Shoot, Tomato_Shoot$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_br <- lapply(split(Barley_Root, Barley_Root$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_bs <- lapply(split(Barley_Shoot, Barley_Shoot$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_mr <- lapply(split(Maize_Root, Maize_Root$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_ms <- lapply(split(Maize_Shoot, Maize_Shoot$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_cr <- lapply(split(Cucumber_Root, Cucumber_Root$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

OneWay_Anova_Tr_cs <- lapply(split(Cucumber_Shoot, Cucumber_Shoot$Time), function(i){ 
  aov(Value ~ Treatment, data = i)
})

##Time
OneWay_Anova_Ti_tr <- lapply(split(Tomato_Root, Tomato_Root$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_ts <- lapply(split(Tomato_Shoot, Tomato_Shoot$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_br <- lapply(split(Barley_Root, Barley_Root$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_bs <- lapply(split(Barley_Shoot, Barley_Shoot$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_mr <- lapply(split(Maize_Root, Maize_Root$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_ms <- lapply(split(Maize_Shoot, Maize_Shoot$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_cr <- lapply(split(Cucumber_Root, Cucumber_Root$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})

OneWay_Anova_Ti_cs <- lapply(split(Cucumber_Shoot, Cucumber_Shoot$Treatment), function(i){ 
  aov(Value ~ Time, data = i)
})


#backup option
OneWay_Anova_Tr_tr2 <- lapply(split(Tomato_Root, Tomato_Root$Time), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})
View(OneWay_Anova_Tr_tr2)
write.table(OneWay_Anova_Tr_tr2, file = "OneWay_Anova_Tr_tr.csv", quote = FALSE, sep = ";")


##test
OneWay_Anova_xx <- aov(Value ~ Treatment, data = Tomato_Root)
OneWay_Anova_Tr_tr["0"]

#ispezione
sapply(OneWay_Anova_Tr_tr, class)


#OneWayAnova save
{
  sink("OneWayAnova_Treatment_Results.txt")
  OneWay_Anova_Tr_tr 
  OneWay_Anova_Tr_ts
  OneWay_Anova_Tr_bs
  OneWay_Anova_Tr_mr
  OneWay_Anova_Tr_ms
  OneWay_Anova_Tr_cr 
  OneWay_Anova_Tr_cs 
  sink(NULL)
  
  sink("OneWayAnova_Time_Results.txt")
  OneWay_Anova_Ti_tr
  OneWay_Anova_Ti_ts
  OneWay_Anova_Ti_br
  OneWay_Anova_Ti_bs
  OneWay_Anova_Ti_mr
  OneWay_Anova_Ti_ms
  OneWay_Anova_Ti_cr
  OneWay_Anova_Ti_cs
  sink(NULL)
}



# Tukey as post hoc test----
#Single Treatment/Time
Tukey_test <- HSD.test(OneWay_Anova_Tr_tr[["0"]], "Treatment")
Tukey_test <- Tukey_test["groups"]
write.table(Tukey_test, file = "Tukey_test.csv", quote = FALSE, sep = ";")

sapply(OneWay_Anova_Ti_tr, summary)


#Multiple Treatment/Time 
##Treatment
###more lists because of different times of sampling points, otherwise 1 list would be enought --> see ##Time
list_Tr1 <- names(OneWay_Anova_Tr_tr)
HSD_Tr_tr <- lapply(list_Tr1, function(i){ 
  HSD.test(OneWay_Anova_Tr_tr[[i]], "Treatment")
})

HSD_Tr_ts <- lapply(list_Tr1, function(i){ 
  HSD.test(OneWay_Anova_Tr_ts[[i]], "Treatment")
})

list_Tr2 <- names(OneWay_Anova_Tr_br)
HSD_Tr_br <- lapply(list_Tr2, function(i){ 
  HSD.test(OneWay_Anova_Tr_br[[i]], "Treatment")
})

HSD_Tr_bs <- lapply(list_Tr2, function(i){ 
  HSD.test(OneWay_Anova_Tr_bs[[i]], "Treatment")
})

HSD_Tr_mr <- lapply(list_Tr2, function(i){ 
  HSD.test(OneWay_Anova_Tr_mr[[i]], "Treatment")
})

HSD_Tr_ms <- lapply(list_Tr2, function(i){ 
  HSD.test(OneWay_Anova_Tr_ms[[i]], "Treatment")
})

list_Tr3 <- names(OneWay_Anova_Tr_cr)
HSD_Tr_cr <- lapply(list_Tr3, function(i){ 
  HSD.test(OneWay_Anova_Tr_cr[[i]], "Treatment")
})

HSD_Tr_cs <- lapply(list_Tr3, function(i){ 
  HSD.test(OneWay_Anova_Tr_cs[[i]], "Treatment")
})

##Time
list_Ti <- names(OneWay_Anova_Ti_tr)
HSD_Ti_tr <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_tr[[i]], "Time")
})

HSD_Ti_ts <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_ts[[i]], "Time")
})

HSD_Ti_br <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_br[[i]], "Time")
})

HSD_Ti_bs <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_bs[[i]], "Time")
})

HSD_Ti_mr <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_mr[[i]], "Time")
})

HSD_Ti_ms <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_ms[[i]], "Time")
})

HSD_Ti_cr <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_cr[[i]], "Time")
})

HSD_Ti_cs <- lapply(list_Ti, function(i){ 
  HSD.test(OneWay_Anova_Ti_cs[[i]], "Time")
})



#save2.0
##Treatment{
{
list0 <- list(1, 2, 3, 4, 5, 6, 7)
HSD_Tr_tr_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_tr[[i]][["groups"]])
})
#messing up with the lables--> write.table(HSD_Tr_tr_groups, "HSD_Tr_tr.csv", quote = FALSE, sep = ";")
## code here under the caption is more reliable
sink("HSD_Ti.csv")
HSD_Ti_groups 
sink(NULL)

HSD_Tr_ts_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_ts[[i]][["groups"]])
})
write.table(HSD_Tr_ts_groups, "HSD_Tr_ts.csv", quote = FALSE, sep = ";")

HSD_Tr_br_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_br[[i]][["groups"]])
})
write.table(HSD_Tr_br_groups, "HSD_Tr_br.csv", quote = FALSE, sep = ";")

HSD_Tr_bs_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_bs[[i]][["groups"]])
})
write.table(HSD_Tr_bs_groups, "HSD_Tr_bs.csv", quote = FALSE, sep = ";")

HSD_Tr_mr_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_mr[[i]][["groups"]])
})
write.table(HSD_Tr_mr_groups, "HSD_Tr_mr.csv", quote = FALSE, sep = ";")

HSD_Tr_ms_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_ms[[i]][["groups"]])
})
write.table(HSD_Tr_ms_groups, "HSD_Tr_ms.csv", quote = FALSE, sep = ";")

HSD_Tr_cr_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_cr[[i]][["groups"]])
})
write.table(HSD_Tr_cr_groups, "HSD_Tr_cr.csv", quote = FALSE, sep = ";")

HSD_Tr_cs_groups <- lapply(list0, function(i){ 
  as.data.frame(HSD_Tr_cs[[i]][["groups"]])
})
write.table(HSD_Tr_cs_groups, "HSD_Tr_cs.csv", quote = FALSE, sep = ";")
}

##Time
{
  list1 <- list(1, 2, 3, 4)
  HSD_Ti_tr_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_tr[[i]][["groups"]])
  })
  write.table(HSD_Ti_tr_groups, "HSD_Ti_tr.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_ts_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_ts[[i]][["groups"]])
  })
  write.table(HSD_Ti_ts_groups, "HSD_Ti_ts.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_br_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_br[[i]][["groups"]])
  })
  write.table(HSD_Ti_br_groups, "HSD_Ti_br.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_bs_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_bs[[i]][["groups"]])
  })
  write.table(HSD_Ti_bs_groups, "HSD_Ti_bs.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_mr_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_mr[[i]][["groups"]])
  })
  write.table(HSD_Ti_mr_groups, "HSD_Ti_mr.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_ms_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_ms[[i]][["groups"]])
  })
  write.table(HSD_Ti_ms_groups, "HSD_Ti_ms.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_cr_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_cr[[i]][["groups"]])
  })
  write.table(HSD_Ti_cr_groups, "HSD_Ti_cr.csv", quote = FALSE, sep = ";")
  
  HSD_Ti_cs_groups <- lapply(list1, function(i){ 
    as.data.frame(HSD_Ti_cs[[i]][["groups"]])
  })
  write.table(HSD_Ti_cs_groups, "HSD_Ti_cs.csv", quote = FALSE, sep = ";")
}




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
