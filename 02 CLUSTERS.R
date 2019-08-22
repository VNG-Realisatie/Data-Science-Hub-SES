#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 2/4
# scope : Kmeans cluster definition and -loading
# techniques : Principal component (PCA), aggregating, plotting
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders
# DataScienceHub @ JADS, GGD Hart voor Brabant
# lud 2019-08-22
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#packages
packages <- c("tools","here","tidyverse","naniar", "haven", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot","weights",
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","dlookr", "aod", "janitor", "descr")
#install packages which are not available
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE)
#review
sessionInfo()


#-------------------------------------------------------------------------------
# Global settings

#root location of this procedure (working directory)
root <- getwd()
root

#GGD 
#ggd <- 'HVB' #Hart voor Brabant
ggd <- 'ZHZ' #Zuid-Holland Zuid

#set graphs location (if na, create directory first)
plots.loc <- paste0(root,'/PLOTS/',ggd,'/')

#set data location (if na, create directory first)
data.loc <- paste0(root,'/DATA/',ggd,'/')

#set library location (if na, create directory first)
lib.loc <- paste0(root,'/LIB/')

#functions (make sure multimerge.R resides in the LIB directory)
#if(!exists("multimerge", mode="function")) source(paste0(lib.loc,"multimerge.R"),local = TRUE)

#options
set.seed(123)  # for reproducibility
options(digits=3)

#number of clusters (Kmeans, Hierarchical Cluster Analysis) 
#always (re)check the optimal number of clusters!!!
#see DIMENSION.R section II. 'Optimal number of clusters'
k <- 8

#number of factors (PCA)
f <- 4

#rotation (PCA)
#distinct dimensions
rotation <- "varimax"

#clustering method results (as input)
clustering <- "kmeans"

#dimension charts
#height <- 7
graph_height <- 8
png_height <- 600
aspect_ratio <- 2

#custom color scheme
colors_cust <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#ff0000", "#ababab")


#-------------------------------------------------------------------------------
# LOAD DATA (result from procedure 01 'DIMENSION')

load(file = paste0(data.loc,"FINAL_DF",".Rda"))
z <- FINAL_DF 


z$cl_kmeans <- as.numeric(as.character(z$cl_kmeans))

#z$cl_kmeans[is.na(z$cl_kmeans)] <- 0

#-------------------------------------------------------------------------------
# WEIGHT

#weight variable (change name variable 'z$ewGGD' if needed into 'z$....')
z$case_wei <- z$ewGGD

z$case_wei <- as.numeric(as.character(z$case_wei))
wei_vec <- as.vector(z$case_wei) 


#-------------------------------------------------------------------------------
# Cluster membership distribution

vulnerable_num <- as.numeric(z$vulnerable)

vulnerable <- wpct(vulnerable_num, weight=wei_vec, na.rm=FALSE)*100
print(vulnerable)

write.table(vulnerable, file = paste0(data.loc,"weigthed_vulnerable_distribution_population",".csv"))

#cluster cl_kmeans membership distribution
clusters <- z %>%
  subset(!is.na(cl_kmeans),select = c(cl_kmeans)) 

cl_kmeans_num <- as.numeric(clusters$cl_kmeans)

#Cluster membership distribution (weigthed) (pct)
vulnerable_clus <- wpct(cl_kmeans_num, weight=wei_vec, na.rm=FALSE)*100
vulnerable_clus <- as.data.frame(vulnerable_clus)
vulnerable_clus$cl_kmeans <- row.names(vulnerable_clus)

plot.title = paste0('cluster ',clustering,' membership distribution (weighted)')
cluster_dis <-  ggplot(vulnerable_clus, aes(x =factor(cl_kmeans),y=vulnerable_clus)) +
  ggtitle(plot.title) +
  labs(x = "cluster", y="%") +
    geom_bar(stat="identity", width = 0.7)
cluster_dis
plot.nme = paste0(ggd,'_cluster_',clustering,'_membership_distribution.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)

write.table(vulnerable_clus , file = paste0(data.loc,"weighted_vulnerable_distribution_clusters_",clustering,".csv"))


#-------------------------------------------------------------------------------
# WEIGHTED SCORES PER CLUSTER


#-------------------------------------------------------------------------------

qs <- z 
cols_report <- c("cl_kmeans", "case_wei", "GGEES203", "GGRLS202", "GGADS201", "LFT0109", "samenloop") 
qs_s <- qs[,cols_report]

qs_s[] <- lapply(qs_s, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

#weighted means of outcome * cluster

a <- qs_s %>%
  subset(!is.na(cl_kmeans)) %>%
  mutate(as.factor(cl_kmeans)) %>%
  group_by(cl_kmeans) %>%
  summarise(
    leeftijd_w = weighted.mean(LFT0109, case_wei,na.rm = TRUE),
    samenloop_w = weighted.mean(samenloop, case_wei,na.rm = TRUE),
    eenzaam_w = weighted.mean(GGEES203, case_wei,na.rm = TRUE),
    regie_w = weighted.mean(GGRLS202, case_wei,na.rm = TRUE),
    depri_w = weighted.mean(GGADS201, case_wei,na.rm = TRUE)
  )

print(a)

write.table(a , file = paste0(data.loc,"Weigthed_means_outcome_clusters_kmeans",".csv"))



#Cluster membership * municipality
qg <- z
cols_geo <- c("cl_kmeans", "case_wei", "Gemeentecode") 
qg_s <- qg[,cols_geo]

gem <- qg_s %>%
  subset(!is.na(cl_kmeans)) %>%
  mutate(as.factor(cl_kmeans))
gem

#crosstab incidentie clusters * gemeente
ct_gemeente <- crosstab(gem$Gemeentecode, gem$cl_kmeans, weight = gem$case_wei,chisq = TRUE,cell.layout = TRUE,
                           dnn = c("gemeente","cluster"),
                           expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE )
ct_gemeente


#-------------------------------------------------------------------------------
# Dispersion plots 


#-------------------------------------------------------------------------------


#please note that the weighted mean presented in the box plots is indicated by the red dot

#leeftijd
#z$leeftijd <- as.numeric(z$leeftijd)
z$leeftijd <- as.numeric(z$LFT0109)

plot.nme = paste0(ggd,'_cluster_',clustering,'_leeftijd.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)
bp3 <- ggplot(z, aes(x = factor(cl_kmeans), y = leeftijd, weight = case_wei,fill=as.factor(cl_kmeans), na.rm=TRUE)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + 
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  geom_point(data=a,aes(x=factor(cl_kmeans),y=leeftijd_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  ggtitle("Leeftijd")
bp3
dev.off()


#samenloop / multi-problematiek
z$samenloop <- as.numeric(z$samenloop)

plot.nme = paste0(ggd,'_cluster_',clustering,'_samenloop.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)
bp4 <- ggplot(z, aes(x = factor(cl_kmeans), y = samenloop, weight = case_wei),na.rm=TRUE) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=factor(cl_kmeans),y=samenloop_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  ggtitle("Samenloop")
bp4
dev.off()


#anst en depressie
z$GGADS201 <- as.numeric(z$GGADS201)

plot.nme = paste0(ggd,'_cluster_',clustering,'_angstdepressie.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)
bp5 <- ggplot(z, aes(x = factor(cl_kmeans), y = GGADS201, weight = case_wei, ylab('angst en depressie')),na.rm=TRUE) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=factor(cl_kmeans),y=depri_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  ggtitle("Angst en depressie")
bp5
dev.off()


#regie op het leven
#score 7 t/m 19: onvoldoende eigen regie
z$GGRLS202 <- as.numeric(z$GGRLS202)

plot.nme = paste0(ggd,'_cluster_',clustering,'_regieopleven.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)
bp6 <- ggplot(z, aes(x = factor(cl_kmeans), y = GGRLS202, weight = case_wei, ylab('regie op het leven')),na.rm=TRUE) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=factor(cl_kmeans),y=regie_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  ggtitle("Regie op het leven")
bp6
dev.off()


#eenzaamheid
#ernstig eenzaam (9-10) en zeer ernstig eenzaam (11)
z$GGEES203 <- as.numeric(z$GGEES203)
plot.nme = paste0(ggd,'_cluster_',clustering,'_eenzaamheid.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

bp7 <- ggplot(z, aes(x = factor(cl_kmeans), y = GGEES203, weight = case_wei, ylab('eenzaamheid')),na.rm=TRUE) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  geom_point(data=a,aes(x=factor(cl_kmeans),y=eenzaam_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  ggtitle("Eenzaamheid")
bp7
dev.off()


#-------------------------------------------------------------------------------
# Crosstabs


#-------------------------------------------------------------------------------



ct <- z %>%
   subset(!is.na(cl_kmeans))

#crosstab incidentie eenzaamheid
ct_eenzaamheid <- crosstab(ct$cl_kmeans, ct$GGEES208, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
               dnn = c("cluster", "Eenzaamheid"),
               expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
ct_eenzaamheid



#crosstab incidentie mantelzorg geven
ct_mz <- crosstab(ct$cl_kmeans, ct$MCMZGA205, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                           dnn = c("cluster", "Mantelzorg verlenen"),
                  expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
ct_mz



#crosstab mantelzorg intensiteit
#ct_mz_int <- crosstab(ct$cl_kmeans, ct$MCMZGA201, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
#                  dnn = c("cluster", "Mantelzorg intensiteit"),
#                  expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
#ct_mz_int



#ervaren gezondheid
#3 is slecht of zeer slecht

gez <- crosstab(ct$cl_kmeans, ct$KLGGA207, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                      dnn = c("cluster", "Ervaren gezondheid"),
                      expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
gez


#heeft langdurig last van ziekte of aandoening
chron <- crosstab(ct$cl_kmeans, ct$CALGA260, weight = ct$case_wei, chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Langdurige ziekte of aandoening"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
chron


#gest. hh inkomen in kwintielen ct$inkkwin_2016 or KwintielInk
#lager is slechter
ink <- crosstab(ct$cl_kmeans, ct$KwintielInk, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                  dnn = c("cluster", "Inkomen huishouden"),
                  expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
ink


#moeite met rondkomen
#4 = grote moeite

rk <- crosstab(ct$cl_kmeans, ct$MMIKB201, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Moeite met rondkomen"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
rk


#werkloos en werkzoekend
wkl <- crosstab(ct$cl_kmeans, ct$MMWSA207, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
               dnn = c("cluster", "Werkloos en werkzoekend"),
               expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
wkl



#pensioen
pen <- crosstab(ct$cl_kmeans, ct$MMWSA206, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Pensioen"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
pen



#huisvrouw/-man
huis <- crosstab(ct$cl_kmeans, ct$MMWSA210, weight = ct$case_wei,chisq = TRUE,cell.layout = TRUE,
                dnn = c("cluster", "Huisvrouw/-man"),
                expected = FALSE, prop.c = FALSE, prop.r = TRUE, plot=FALSE,row.labels =TRUE,total.c = FALSE,total.r = FALSE)
huis




#-------------------------------------------------------------------------------
#Correlation matrix

#change first variable name to target the desirec cluster. e.g. cluster 2 is 'clus2'
#dim_var <- c("clus1","cl_kmeans","leeftijd70eo","gez_slecht","inkomenlaag_dich", "dagactiviteit","geenbetaaldwerk","opl_lm", "ziek_lt",
#             "depri_hg", "MCMZOS305","CALGA260", "CALGA261","LGBPS209") 

#cl <- FINAL_DF[, dim_var]

#cl[] <- lapply(cl, function(x) {
#  if(is.factor(x)) as.numeric(as.character(x)) else x
#})
#sapply(cl, class)

#cl <- subset(cl1,cl_kmeans>0)
#cl$cl_kmeans <- NULL
#cor_cl <- cor(cl, method = "pearson", use = "complete.obs")
#cor_cl

#corrplot(cor_cl, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)



#-------------------------------------------------------------------------------
# III TSNE (DR) > KMEANS (CL) > PCA (DR)

#Samenloop van kenmerken binnen een cluster
#-------------------------------------------------------------------------------

#Perspective : SES, situational
#leeftijd70eo leeftijd 70 en ouder 
#ervarengezondheid_dich gezondheid slecht 
#inkomenlaag_dich inkomen laag
#dagactiviteit betaald werk, vrijwilligerswerk, student 
#geenbetaaldwerk geen betaald werk
#opl_lm opleiding volwassenen laag midden
#ziek_lt langdurige ziekten
#MCMZOS304 mantelzorg ontvangen laatste 12 maanden
#MCMZGA205 mantelzorg gegeven laatste 12 maanden

dim_var <- c("cl_kmeans","leeftijd70eo","ervarengezondheid_dich","inkomenlaag_dich", "dagactiviteit",
             "opl_lm", "ziek_lt", "MCMZOS304", "MCMZGA205") 

#relevant variables for PCA
sit <- z[, dim_var]

#all numeric values
sit[] <- lapply(sit, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

sit[sit == 9] <- NA

#cluster member in kmeans method
sit <- sit[ which(sit$cl_kmeans>0), ]


sapply(sit, function(x) sum(is.na(x)))



#missing data imputation
#method : Multivariate Imputation via Chained Equations, logreg(Logistic Regression) for binary variables

#initial run to auto-determine powerful predictors for imputation
ini <- mice(sit,pred=quickpred(sit, mincor=.3),seed=500, print=F)
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
v <-subset(sit,cl_kmeans==i)

dimens_comp<-NULL

dimens_comp<- principal(v[,-which(names(v)=="cl_kmeans")], nfactors = f, residuals = FALSE,rotate=rotation,n.obs=NA, covar=FALSE,
                        scores=TRUE,missing=TRUE,impute="median")

#reset f based on the elbow 	

plot.nme = paste0(ggd,'_pca_biplot_situational_cluster_',i,'.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

pca_plot <- plot(dimens_comp$values, type="b")
pca_plot
dev.off()

prop.table(dimens_comp$values)

print(loadings(dimens_comp), cutoff=0.4)

#dimens_comp$loadings

#biplot of the first three rotated PCs
pca_plots <- biplot(dimens_comp, choose=1:3, cutl=.4, smoother=TRUE)
pca_plots

}
