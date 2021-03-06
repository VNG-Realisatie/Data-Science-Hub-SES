#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 2/4
# scope : Kmediods (PAM) cluster definition and -loading
# techniques : Principal component (PCA), aggregating, plotting
# requirements: R Statistics version  (3.60=<)
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders (GGD HvB), Ester de Jonge (GGD ZHZ)
# DataScienceHub @ JADS, GGD Hart voor Brabant, GGD Zuid-Holland Zuid
# lud 2020-03-23
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

packrat::init()

#packages
packages <- c("tools","here","tidyverse","naniar", "haven", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot","weights", "RColorBrewer",
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","dlookr", "aod", "janitor", "descr", "rlang")
#install packages which are not available
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE)
#review
#sessionInfo()

#-------------------------------------------------------------------------------
# Global settings

#root location of this procedure (working directory)
root <- getwd()
root

#GGD dep.
#ggd <- 'HVB' #Hart voor Brabant
ggd <- 'ZHZ' #Zuid-Holland Zuid
#ggd <- 'UTR' #Utrecht

#set graphs location
plots.loc <- paste0(root,'/PLOTS/',ggd,'/')

#set data location 
data.loc <- paste0(root,'/DATA/',ggd,'/')

#options
set.seed(123)  # for reproducibility
options(digits=15)

#number of clusters (Kmeans, Hierarchical Cluster Analysis) 
#always (re)check the optimal number of clusters!!!
#see DIMENSION.R section II. 'Optimal number of clusters'
k <- 9

#number of factors (PCA)
f <- 4

#rotation (PCA)
#Varimax returns factors that are orthogonal; Oblimin allows the factors to not be orthogonal.
#rotation <- "varimax" #uncorrelated independent variables
rotation <- "oblimin" #correlated independent variables

#dimension plots
graph_height <- 9
png_height <- 600
aspect_ratio <- 2
dpi <- 320 #retina

#qualitative color scheme from ColorBrewer
ncolors <- k+1 #number of clusters plus group of outliers
colors_cust <- brewer.pal(ncolors, "Paired")


#-------------------------------------------------------------------------------
# LOAD DATA (result from previous procedure '01 DIMENSION.R')

load(file = paste0(data.loc,"FINAL_DF",".Rda"))
z <- FINAL_DF 

#clustering method results (as input)
clustering <- "kmeans"
#clus <- z$cl_pam

#-------------------------------------------------------------------------------
# WEIGHT

#weight variable 
z$case_wei <- as.numeric(as.character(z$ewGGD))
#disable weight
#z$case_wei <- 1

weight_on <- max(z$case_wei)>1

#create vector of weights for entire population
wei_vec <- as.vector(z$case_wei) 


#-------------------------------------------------------------------------------
# Cluster membership distribution

vulnerable <- wpct(z$vulnerable, weight=wei_vec, na.rm=FALSE)*100
print(vulnerable)

write.table(vulnerable, file = paste0(data.loc,"weigthed_vulnerable_distribution",clustering,".csv"))


#Cluster membership distribution (weigthed) (pct)
vulnerable_clus <- wpct(z$cl_pam, weight=wei_vec, na.rm=FALSE)*100
vulnerable_clus <- as.data.frame(vulnerable_clus)
vulnerable_clus$vulnerable_clus <- round(vulnerable_clus$vulnerable_clus, digits=1) 
vulnerable_clus$cl_pam <- as.numeric(row.names(vulnerable_clus))

plot.title = paste0('cluster ',clustering,' membership distribution (weighted=',weight_on ,')')
cluster_dis <-  ggplot(vulnerable_clus, aes(x =factor(cl_pam),y=vulnerable_clus,fill=factor(cl_pam))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  geom_text(aes(label=vulnerable_clus,color="#ababab"),vjust=-0.1) +
  theme(legend.position = "none") +
  labs(x = "cluster", y="%") +
  geom_bar(stat="identity", width = 0.7)
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_distribution_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)

write.table(vulnerable_clus , file = paste0(data.loc,"weighted_vulnerable_distribution_clusters_",clustering,".csv"))


#-------------------------------------------------------------------------------
# WEIGHTED SCORES PER CLUSTER


#-------------------------------------------------------------------------------

qs <- z 
cols_report <- c("cl_pam", "case_wei", "GGEES203", "GGRLS202", "GGADS201", "LFT0109", "score_zw") 
qs_s <- qs[,cols_report]

qs_s[] <- lapply(qs_s, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

#weighted means of outcome * cluster

a <- qs_s %>%
  #subset(!is.na(cl_pam)) %>%
  mutate(as.factor(cl_pam)) %>%
  group_by(cl_pam) %>%
  summarise(
    leeftijd_w = weighted.mean(LFT0109, case_wei,na.rm = TRUE),
    samenloop_w = weighted.mean(score_zw, case_wei,na.rm = TRUE),
    eenzaam_w = weighted.mean(GGEES203, case_wei,na.rm = TRUE),
    regie_w = weighted.mean(GGRLS202, case_wei,na.rm = TRUE),
    depri_w = weighted.mean(GGADS201, case_wei,na.rm = TRUE)
  )

print(a)

write.table(a , file = paste0(data.loc,"Weigthed_means_outcome_clusters_kmeans",".csv"))



#Cluster membership * municipality
qg <- z
cols_geo <- c("cl_pam", "case_wei", "Gemeentecode") 
qg_s <- qg[,cols_geo]



gem <- qg_s %>%
  subset(!is.na(cl_pam)) %>%
  mutate(as.factor(cl_pam))


#crosstab incidentie clusters * gemeente
ct_gemeente <- crosstab(gem$Gemeentecode, gem$cl_pam, weight = gem$case_wei,chisq = TRUE,cell.layout = TRUE,
                           dnn = c("gemeente","cluster"),
                           expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE )

prop<- as.data.frame(ct_gemeente$prop.row*100)

#library(xlsx)
#write.xlsx(prop, "mydata.xlsx") 

plot.title = paste0('cluster ',clustering,' distribution * gemeente (weighted=',weight_on ,')')
cluster_gem_dis <-  ggplot(prop, aes(x =factor(gemeente),y=Freq,fill=factor(cluster), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "gemeente", y="%") +
  scale_fill_discrete(name = "cluster") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_gem_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_gemeente_distribution_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#-------------------------------------------------------------------------------
# Dispersion plots 


#-------------------------------------------------------------------------------


#please note that the weighted mean presented in the box plots is indicated by the red dot

#leeftijd
z$leeftijd <- as.numeric(z$LFT0109)


plot.title = paste0('cluster ',clustering,' * leeftijd (weighted=',weight_on ,')')
bp3 <- ggplot(z, aes(x = cl_pam, y = leeftijd, weight = case_wei,fill=cl_pam)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + 
  #stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  geom_point(data=a,aes(x=cl_pam,y=leeftijd_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  xlab("cluster") +
  ylab("Leeftijd") +
  ggtitle(plot.title)
bp3
plot.nme = paste0(ggd,'_cluster_',clustering,'_leeftijd_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#samenloop / multi-problematiek
z$samenloop <- as.numeric(z$score_zw)

plot.title = paste0('cluster ',clustering,' * samenloop (weighted=',weight_on ,')')
bp4 <- ggplot(z, aes(x = cl_pam, y = samenloop, weight = case_wei, fill=cl_pam)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=cl_pam,y=samenloop_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  xlab("cluster") +
  ylab("Samenloop") +
  ggtitle(plot.title)
bp4
plot.nme = paste0(ggd,'_cluster_',clustering,'_samenloop_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#anst en depressie
z$GGADS201 <- as.numeric(z$GGADS201)

plot.title = paste0('cluster ',clustering,' * angst en depressie (weighted=',weight_on ,')')
bp5 <- ggplot(z, aes(x = cl_pam, y = GGADS201, weight = case_wei, fill=cl_pam)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=cl_pam,y=depri_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  xlab("cluster") +
  ylab("Risico op angststoornis of depressie") +
  ggtitle(plot.title)
bp5
plot.nme = paste0(ggd,'_cluster_',clustering,'_angstdepressie_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#regie op het leven
#score 7 t/m 19: onvoldoende eigen regie
z$GGRLS202 <- as.numeric(z$GGRLS202)

plot.title = paste0('cluster ',clustering,' * regie op leven (weighted=',weight_on ,')')
bp6 <- ggplot(z, aes(x = cl_pam, y = GGRLS202, weight = case_wei, fill=cl_pam)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=cl_pam,y=regie_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  xlab("cluster") +
  ylab("Regie op het leven") +
  ggtitle(plot.title)
bp6
plot.nme = paste0(ggd,'_cluster_',clustering,'_regieopleven_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#eenzaamheid
#ernstig eenzaam (9-10) en zeer ernstig eenzaam (11)
z$GGEES203 <- as.numeric(z$GGEES203)

plot.title = paste0('cluster ',clustering,' * eenzaamheid (weighted=',weight_on ,')')
bp7 <- ggplot(z, aes(x = cl_pam, y = GGEES203, weight = case_wei,fill=cl_pam)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=cl_pam,y=eenzaam_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  xlab("cluster") +
  ylab("Eenzaamheid") + 
  ggtitle(plot.title)
bp7
plot.nme = paste0(ggd,'_cluster_',clustering,'_eenzaamheid_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#-------------------------------------------------------------------------------
# Crosstabs (WEIGHTED)


#-------------------------------------------------------------------------------



ct <- z 
 # %>% subset(!is.na(cl_pam))

#crosstab incidentie eenzaamheid
ct_prop <- crosstab(ct$cl_pam, ct$GGEES208, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
               dnn = c("cluster", "lvl"),
               expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * eenzaamheid (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_eenzaamheid_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie angst depressie
ct_prop <- crosstab(ct$cl_pam, ct$GGADA202, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * angst en depressie (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_angstdepressie_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)



#crosstab incidentie regie op het leven
ct_prop <- crosstab(ct$cl_pam, ct$GGRLS203, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * regie op het leven (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_regieleven_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie ervaren gezondheid
ct_prop <- crosstab(ct$cl_pam, ct$KLGGA207, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * ervaren gezondheid (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_ervarengezondheid_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie langdurige ziekte of -aandoening
ct_prop <- crosstab(ct$cl_pam, ct$CALGA260, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * langdurige ziekte of -aandoening (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_langdurigziek_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)



#crosstab incidentie beperking gezondheid
ct_prop <- crosstab(ct$cl_pam, ct$CALGA261, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * beperkt in activiteiten vanwege gezondheid  (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_beperkt_gezondheid_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie beperking horen, zien of mobiliteit
ct_prop <- crosstab(ct$cl_pam, ct$LGBPS209, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * beperking horen, zien of mobiliteit (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_beperkinghzm_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie beperking mobiliteit
ct_prop <- crosstab(ct$cl_pam, ct$LGBPS205, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * beperking mobiliteit (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_beperking_mobi_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)



#crosstab incidentie moeite rondkomen
ct_prop <- crosstab(ct$cl_pam, ct$MMIKB201, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * moeite rondkomen (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_beperking_rondkomen_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie vermoeidheid
ct_prop <- crosstab(ct$cl_pam, ct$GGADB201, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * vermoeidheid zonder duidelijke redenen (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_vermoeidheid_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie mantelzorg ontvangen
ct_prop <- crosstab(ct$cl_pam, ct$MCMZOS304, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * mantelzorg ontvangen (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_mantelzorg_ontvangen_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)


#crosstab incidentie mantelzorger
ct_prop <- crosstab(ct$cl_pam, ct$MCMZGS203, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                    dnn = c("cluster", "lvl"),
                    expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

prop<- as.data.frame(ct_prop$prop.row*100)
prop

plot.title = paste0('cluster ',clustering,' * mantelzorger (weighted=',weight_on ,')')
cluster_dis <-  ggplot(prop, aes(x =factor(cluster),y=Freq,fill=factor(lvl), label=round(Freq,0))) +
  ggtitle(plot.title) +
  theme_minimal() + 
  theme(legend.position = "right") +
  labs(x = "cluster", y="%") +
  scale_fill_discrete(name = " ") +
  geom_bar(position="stack",stat="identity", width = 0.7) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_mantelzorger_weighted_',weight_on ,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)



#loopje uitwerken
#scat = function(x_var) {
  
#  ct_prop <- crosstab(ct$cl_pam, .data[[x_var]], weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
 #                     dnn = c("cluster", "lvl"),
#                      expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
  
#  prop<- as.data.frame(ct_prop$prop.row*100)
#  print(prop)
 #}

#scat(ct$GGEES208)


#gest. hh inkomen in kwintielen ct$inkkwin_2016 or KwintielInk
#lager is slechter
ink <- crosstab(ct$cl_pam, ct$KwintielInk, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                  dnn = c("cluster", "Inkomen huishouden"),
                  expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
income<- ink$prop.row*100
income


#werkloos en werkzoekend
wkl <- crosstab(ct$cl_pam, ct$MMWSA207, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
               dnn = c("cluster", "Werkloos en werkzoekend"),
               expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

unemployed<- wkl$prop.row*100
unemployed


#opleiding
opl <- crosstab(ct$cl_pam, ct$Opleiding_samind, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Opleiding"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)

education<- opl$prop.row*100
education



#pensioen
pen <- crosstab(ct$cl_pam, ct$MMWSA206, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Pensioen"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
pensionado<- pen$prop.row*100
pensionado



#huisvrouw/-man
huis <- crosstab(ct$cl_pam, ct$MMWSA210, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Huisvrouw/-man"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
home<- huis$prop.row*100
home

#vrijwilligerswerk
vw <- crosstab(ct$cl_pam, ct$MMVWB201, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
               dnn = c("cluster", "Vrijwilligerswerk"),
               expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
care<- vw$prop.row*100
care

#eenoudergezin
op <- crosstab(ct$cl_pam, ct$AGHHS201, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
               dnn = c("cluster", "Eenoudergezin"),
               expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
soloparent<- op$prop.row*100
soloparent




#-------------------------------------------------------------------------------
#Correlation matrix

#change first variable name to target the desirec cluster. e.g. cluster 2 is 'clus2'
#dim_var <- c("clus1","cl_pam","leeftijd70eo","gez_slecht","inkomenlaag_dich", "dagactiviteit","geenbetaaldwerk","opl_lm", "ziek_lt",
#             "depri_hg", "MCMZOS305","CALGA260", "CALGA261","LGBPS209") 

#cl <- FINAL_DF[, dim_var]

#cl[] <- lapply(cl, function(x) {
#  if(is.factor(x)) as.numeric(as.character(x)) else x
#})
#sapply(cl, class)

#cl <- subset(cl1,cl_pam>0)
#cl$cl_pam <- NULL
#cor_cl <- cor(cl, method = "pearson", use = "complete.obs")
#cor_cl

#corrplot(cor_cl, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)



#-------------------------------------------------------------------------------
# III TSNE (DR) > KMEANS (CL) > PCA (DR)

#Samenloop van kenmerken binnen een cluster
#-------------------------------------------------------------------------------

#Perspective : SES, situational

#leeftijd70eo : leeftijd 70 en ouder 
#ervarengezondheid_dich : gezondheid slecht 
#inkomenlaag_dich : inkomen laag
#werkopleiding_dich : betaald werk, student 
#geenbetaaldwerk : geen betaald werk REMOVED
#opl_lm : opleiding volwassenen laag midden
#ziek_lt : langdurige ziekten
#MCMZOS304_dich : mantelzorg ontvangen laatste 12 maanden
#mantelzorg_dich : mantelzorg gegeven laatste 12 maanden
#vrijwilligerswerk_dich : verricht vrijwilligerswerk
#vriendenkring_dich: vriendenkring is beperkt

dim_var <- c("cl_pam","leeftijd70eo","ervarengezondheid_dich","inkomenlaag_dich", "werkopleiding_dich", "vrijwilligerswerk_dich",
             "opl_lm", "ziek_lt", "mantelzorg_dich", "MCMZOS304_dich", "vriendenkring_dich") 

#relevant variables for PCA
sit <- z[, dim_var]

#all numeric values
sit[] <- lapply(sit, function(x) {
  if(is.factor(x) | is.character(x)) as.numeric(as.character(x)) else x
})

sit[sit == 9] <- NA

#cluster member in PAM method
sit <- sit[ which(sit$cl_pam>0), ]


sapply(sit, function(x) sum(is.na(x)))



#missing data imputation
#method : Multivariate Imputation via Chained Equations, logreg(Logistic Regression) for binary variables

#initial run to auto-determine powerful predictors for imputation
ini <- mice(sit,pred=quickpred(sit, mincor=.35),seed=500, print=F)
#predictor matrix
(pred <- ini$pred)

save(pred, file = "prediction_matrix_situational.RData")

#pred <- load("prediction_matrix_situational.RData")

#final run
imp_data <- mice(sit,method = "logreg", pred=pred,m=5,maxit=30,seed=500, print=T)

summary(imp_data)

sit <- complete(imp_data,1)


#stats on missing values (post-imputation)
sapply(sit, function(x) sum(is.na(x)))

dim(sit)


#loop through clusters and print loadings per dimension
for(i in 1:k) {

#manual cluster number 
#i <- 1
#filter by cluster number  
v <-subset(sit,cl_pam==i)

dimens_comp<-NULL

dimens_comp<- principal(v[,-which(names(v)=="cl_pam")], nfactors = f, residuals = FALSE,rotate=rotation,n.obs=NA, covar=FALSE,
                        scores=TRUE,missing=TRUE,impute="median")

plot.nme = paste0(ggd,'_pca_biplot_situational_cluster_',i,'.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

pca_plot <- plot(dimens_comp$values, type="b")
pca_plot
dev.off()

prop.table(dimens_comp$values)

print(loadings(dimens_comp), cutoff=0.45)

#dimens_comp$loadings

#biplot of the first three rotated PCs
pca_plots <- biplot(dimens_comp, choose=1:3, cutl=.4, smoother=TRUE)
pca_plots

}
