setwd("C:/Users/Admin/Desktop/Younes/Montpellier SupAgro/Plant Sciences/UE3/Fast-X")

##Importation des librairies
library(ggplot2)
library(cowplot)
library(dplyr)
library("gridExtra")
library(tidyverse)

lathyrus_X<-read.table("alignment_lathyrus_X.csv",header=FALSE,dec=".",sep=";")
vicia_X<-read.table("alignment_vicia_X.csv",header=FALSE,dec=".",sep=";")

##Création de jeux de données selon synonymes ou non synonymes

NS_lathyrus_X<-lathyrus_X %>%
  select(V4,V5,V6,V7,V8,V9,V10)

S_lathyrus_X<-lathyrus_X %>%
  select(V12,V13,V14,V15,V16,V17,V18)

NS_vicia_X<-vicia_X %>%
  select(V4,V5,V6,V7,V8,V9,V10)

S_vicia_X<-vicia_X %>%
  select(V12,V13,V14,V15,V16,V17,V18)

##Création d'un dataframe pour lat et vic pour les barplots


lat<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)#7 premiers = NS / 7 après = S
vic<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)#7 premiers = NS / 7 après = S
for (i in 1:7){
  lat[i]=NS_lathyrus_X[1,i]
  lat[i+7]=S_lathyrus_X[1,i]
  vic[i]=NS_vicia_X[1,i]
  vic[i+7]=S_vicia_X[1,i]
}

mutation_type=c("NS","NS","NS","NS","NS","NS","NS","S","S","S","S","S","S","S")
frequency=c("1/8","2/8","3/8","4/8","5/8","6/8","7/8","1/8","2/8","3/8","4/8","5/8","6/8","7/8")
lathyrus_X_table<-data.frame(cbind(frequency,mutation_type,lat))
vicia_X_table<-data.frame(cbind(frequency,mutation_type,vic))

lathyrus_X_table$lat <- as.numeric(lathyrus_X_table$lat)
vicia_X_table$vic <- as.numeric(vicia_X_table$vic)

###Création des barplots


lathyrus_barplot<-ggplot(data=lathyrus_X_table,aes(x=frequency,y=lat,fill=mutation_type))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  xlab(" ")+
  ylab("Number of SNPs")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Greens")

lathyrus_barplot

vicia_barplot<-ggplot(data=vicia_X_table,aes(x=frequency,y=vic,fill=mutation_type))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  xlab(" ")+
  ylab("Number of SNPs")+
  theme_minimal()+
  scale_fill_brewer(palette="Greens")

vicia_barplot

##Fusion des barplots en 1 image

plot_grid(lathyrus_barplot, vicia_barplot, labels=c("A", "B"), ncol = 2, nrow = 1)
