setwd("C:/Users/Admin/Desktop/Younes/Montpellier SupAgro/Plant Sciences/UE3/Fast-X")

##Importation des librairies
library(ggplot2)
library(cowplot)
library(dplyr)
library("gridExtra")
library(tidyverse)

rm(list=ls())

lathyrus_X<-read.table("alignment_lathyrus_X.csv",header=TRUE,dec=".",sep=";")
vicia_X<-read.table("alignment_vicia_X.csv",header=TRUE,dec=".",sep=";")
lathyrus_A<-read.table("alignment_lathyrus_A.csv",header=TRUE,dec=".",sep=";")
vicia_A<-read.table("alignment_vicia_A.csv",header=TRUE,dec=".",sep=";")
lathyrus_A<-read.table("alignment_lathyrus_A.csv",header=TRUE,dec=".",sep=";")
DFE<-read.table("DFE_results.csv",header=TRUE,dec=",",sep=";")

tot_lat_NS_X=lathyrus_X[1,3]
tot_lat_S_X=lathyrus_X[8,3]
tot_lat_NS_A=lathyrus_A[1,3]
tot_lat_S_A=lathyrus_A[8,3]

tot_vic_NS_X=vicia_X[1,3]
tot_vic_S_X=vicia_X[8,3]
tot_vic_NS_A=vicia_A[1,3]
tot_vic_S_A=vicia_A[8,3]

for (i in 2:7){
  tot_lat_NS_X=tot_lat_NS_X+lathyrus_X[i,3]
  tot_lat_S_X=tot_lat_S_X+lathyrus_X[i+7,3]
  tot_lat_NS_A=tot_lat_NS_A+lathyrus_A[i,3]
  tot_lat_S_A=tot_lat_S_A+lathyrus_A[i+7,3]
  
  tot_vic_NS_X=tot_vic_NS_X+vicia_X[i,3]
  tot_vic_S_X=tot_vic_S_X+vicia_X[i+7,3]
  tot_vic_NS_A=tot_vic_NS_A+vicia_A[i,3]
  tot_vic_S_A=tot_vic_S_A+vicia_A[i+7,3]
}

lathyrus_X[,3][lathyrus_X$Mutation.Type=='NS']<-lathyrus_X[,3][lathyrus_X$Mutation.Type=='NS']/tot_lat_NS_X
lathyrus_X[,3][lathyrus_X$Mutation.Type=='S']<-lathyrus_X[,3][lathyrus_X$Mutation.Type=='S']/tot_lat_S_X
lathyrus_A[,3][lathyrus_A$Mutation.Type=='NS']<-lathyrus_A[,3][lathyrus_A$Mutation.Type=='NS']/tot_lat_NS_A
lathyrus_A[,3][lathyrus_A$Mutation.Type=='S']<-lathyrus_A[,3][lathyrus_A$Mutation.Type=='S']/tot_lat_S_A

vicia_X[,3][vicia_X$Mutation.Type=='NS']<-vicia_X[,3][vicia_X$Mutation.Type=='NS']/tot_vic_NS_X
vicia_X[,3][vicia_X$Mutation.Type=='S']<-vicia_X[,3][vicia_X$Mutation.Type=='S']/tot_vic_S_X
vicia_A[,3][vicia_A$Mutation.Type=='NS']<-vicia_A[,3][vicia_A$Mutation.Type=='NS']/tot_vic_NS_A
vicia_A[,3][vicia_A$Mutation.Type=='S']<-vicia_A[,3][vicia_A$Mutation.Type=='S']/tot_vic_S_A


##Création jeux de données DFE

DFE_lathyrus<-DFE %>% 
  filter(species=="Lathyrus pratensis")

DFE_vicia<-DFE %>% 
  filter(species=="Vicia cracca")

DFE_A<-DFE %>% 
  filter(gene=="A")

DFE_X<-DFE %>% 
  filter(gene=="X")


###Création des barplots

lathyrus_barplot_X<-ggplot(data=lathyrus_X,aes(x=as.factor(Frequency),y=Number.of.SNPs,fill=Mutation.Type))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  xlab(" ")+
  ylab("Proportion of SNPs")+
  theme_minimal()+
  scale_fill_brewer(palette="Greens")+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("1" = "{1/14 ; 13/14}", "2" = "{2/14 ; 12/14}", "3" = "{3/14 ; 11/14}","4" = "{4/14 ; 10/14}", "5" = "{5/14 ; 9/14}", "6" = "{6/14 ; 8/14}","7"="{7/14}"))


lathyrus_barplot_A<-ggplot(data=lathyrus_A,aes(x=as.factor(Frequency),y=Number.of.SNPs,fill=Mutation.Type))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  xlab(" ")+
  ylab("Proportion of SNPs")+
  theme_minimal()+
  scale_fill_brewer(palette="Greens")+
  scale_x_discrete(labels=c("1" = "{1/14 ; 13/14}", "2" = "{2/14 ; 12/14}", "3" = "{3/14 ; 11/14}","4" = "{4/14 ; 10/14}", "5" = "{5/14 ; 9/14}", "6" = "{6/14 ; 8/14}","7"="{7/14}"))


###VICIA

vicia_barplot_X<-ggplot(data=vicia_X,aes(x=as.factor(Frequency),y=Number.of.SNPs,fill=Mutation.Type))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  xlab(" ")+
  ylab("Proportion of SNPs")+
  theme_minimal()+
  scale_fill_brewer(palette="Greens")+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("1" = "{1/14 ; 13/14}", "2" = "{2/14 ; 12/14}", "3" = "{3/14 ; 11/14}","4" = "{4/14 ; 10/14}", "5" = "{5/14 ; 9/14}", "6" = "{6/14 ; 8/14}","7"="{7/14}"))




vicia_barplot_A<-ggplot(data=vicia_A,aes(x=as.factor(Frequency),y=Number.of.SNPs,fill=Mutation.Type))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  xlab(" ")+
  ylab("Proportion of SNPs")+
  theme_minimal()+
  scale_fill_brewer(palette="Greens")+
  scale_x_discrete(labels=c("1" = "{1/14 ; 13/14}", "2" = "{2/14 ; 12/14}", "3" = "{3/14 ; 11/14}","4" = "{4/14 ; 10/14}", "5" = "{5/14 ; 9/14}", "6" = "{6/14 ; 8/14}","7"="{7/14}"))


##DFE BARPLOTS
#Barplot population ?

DFE_barplot_pop_A<-ggplot(DFE_A, aes(species, values)) +
  geom_col(aes(fill = type), position = position_dodge(0.8), width = 0.7)+
  theme(legend.position="none")+
  ylim(-0.05,1)+
  geom_errorbar(
    aes(ymin = conf_int_min, ymax = conf_int_max, group = type),
    width = 0.2, position = position_dodge(0.8))+
    xlab(" Populations ")+
    ylab(" ")



DFE_barplot_pop_X<-ggplot(DFE_X, aes(species, values)) +
  geom_col(aes(fill = type), position = position_dodge(0.8), width = 0.7)+
  theme(legend.position="none")+
  ylim(-0.05,1)+
  geom_errorbar(
    aes(ymin = conf_int_min, ymax = conf_int_max, group = type),
    width = 0.2, position = position_dodge(0.8))+
  xlab(" Populations ")+
  ylab(" ")

DFE_barplot_pop_X

#Barplot genes ?

DFE_barplot_gene_lat<-ggplot(DFE_lathyrus, aes(gene, values)) +
  geom_col(aes(fill = type), position = position_dodge(0.8), width = 0.7)+
  theme(legend.position="none")+
  ylim(-0.05,1)+
  geom_errorbar(
    aes(ymin = conf_int_min, ymax = conf_int_max, group = type),
    width = 0.2, position = position_dodge(0.8))+
  xlab(" Gene ")+
  ylab(" ")

DFE_barplot_gene_lat


DFE_barplot_gene_vic<-ggplot(DFE_vicia, aes(gene, values)) +
  geom_col(aes(fill = type), position = position_dodge(0.8), width = 0.7)+
  theme(legend.position="none")+
  ylim(-0.05,1)+
  geom_errorbar(
    aes(ymin = conf_int_min, ymax = conf_int_max, group = type),
    width = 0.2, position = position_dodge(0.8))+
  xlab(" Gene ")+
  ylab(" ")

DFE_barplot_gene_vic


##Fusion des barplots en 1 image

plot_grid(lathyrus_barplot_X, lathyrus_barplot_A, labels=c("Lathyrus X", "Lathyrus A"), ncol = 2, nrow = 1)

plot_grid(vicia_barplot_X, vicia_barplot_A, labels=c("Vicia X", "Vicia A"), ncol = 2, nrow = 1)

plot_grid(DFE_barplot_pop_A, DFE_barplot_pop_X, labels=c( "A", "X"), ncol = 2, nrow = 1)
plot_grid(DFE_barplot_gene_lat, DFE_barplot_gene_vic, labels=c( "Lathyrus", "Vicia"), ncol = 2, nrow = 1)

