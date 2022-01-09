library(dplyr)
library(ggplot2)
library(tidyverse)
library("ggpubr")
library(readxl)
library(zoo)
library(lubridate)
library(writexl)
library(tidyr)
library(car)

setwd("D:/Data Sains/Data Ular")
mydata <- read_excel("data-oc-ow-pf.xlsx")

oc <- mydata$oviduct_condition
ow <- mydata$oviduct_width
pf <- mydata$primary_follicles

#leveneTest(ow~oc, data = mydata)
#as.numeric(data_oc_ow_pf$oviduct_width)
#shapiro.test(data_oc_ow_pf$primary_follicles)

#Uji Kruskal Wallis
kruskal.test(ow ~ oc)
kruskal.test(pf ~ oc)

#One Way Anova Test
model_ow <-lm(ow ~ oc)
anova(model_ow)

model_pf <-lm(pf ~ oc)
anova(model_pf)

#Uji Lanjut (Tukey)
TukeyHSD(aov(model_ow))

TukeyHSD(aov(model_pf))

#Grafik OC ~ OW
pVal <- anova(model_ow)$'Pr(>F)'[1]
eq = paste0("p-value = ", sprintf("%1.3f",pVal))

box_plot_ow <- ggboxplot(mydata, x="oviduct_condition", y="oviduct_width",add = "mean_se",
                xlab="Oviduct Condition",ylab="Oviduct Width (mm)") +
                theme_bw() + geom_text(x = 0.8, y = 16, label =eq) 
box_plot_ow

line_plot_ow <- ggline(mydata, x="oviduct_condition", y="oviduct_width", 
                 add = "mean_se",ylab = "Oviduct Width (mm)", xlab = "Oviduct Condition",) + theme_bw() + geom_text(x = 1, y = 6.5, label =eq)
line_plot_ow

#Grafik OC ~ PF
pVal1 <- anova(model_pf)$'Pr(>F)'[1]
eq1 = paste0("p-value = ", sprintf("%1.3f",pVal1))

box_plot_pf <- ggboxplot(mydata, x="oviduct_condition", y="primary_follicles",add = "mean_se",
                         xlab="Oviduct Condition",ylab="Primary Follicles (mm)") + theme_bw() + geom_text(x = 0.8, y = 6.5, label =eq1) 
box_plot_pf

line_plot_pf <- ggline(mydata, x="oviduct_condition", y="primary_follicles", 
       add = "mean_se",ylab = "Oviduct Width (mm)", xlab = "Primary Follicles (mm)",) + theme_bw() + geom_text(x = 1, y = 2.6, label =eq)
line_plot_pf

