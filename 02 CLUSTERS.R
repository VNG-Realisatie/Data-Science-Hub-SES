#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 2/4
# scope : Kmeans cluster definition and -loading
# techniques : Principal component (PCA), aggregating, plotting
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders
# DataScienceHub @ JADS, GGD Hart voor Brabant
# lud 2019-07-12
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#packages
packages <- c("tools","here","tidyverse","naniar", "haven", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot","weights",
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","dlookr", "aod", "janitor")
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

#set graphs location (if na, create directory first)
plots.loc <- paste0(root,'/PLOTS/')

#set data location (if na, create directory first)
data.loc <- paste0(root,'/DATA/')

#set library location (if na, create directory first)
lib.loc <- paste0(root,'/LIB/')

#functions (make sure multimerge.R resides in the LIB directory)
if(!exists("multimerge", mode="function")) source(paste0(lib.loc,"multimerge.R"),local = TRUE)

#options
set.seed(123)  # for reproducibility
options(digits=3)

#number of clusters (Kmeans, Hierarchical Cluster Analysis) 
#always (re)check the optimal number of clusters!!!
#see DIMENSION.R section II. 'Optimal number of clusters'
#determine the optimal number of clusters first before running the entire script
#run the script to the point (including) section II first
#the GAP plot will indicate the oprimal number of clusters
#adjust 'k' accordingly below. 
k <- 8

#number of factors (PCA)
f <- 4

#rotation (PCA)
#distinct dimensions
#rotation <- "varimax"
#natural dimensions
rotation <- "none"

#clustering method
clustering <- "kmeans"

#dimension charts
#height <- 7
graph_height <- 8
png_height <- 600
aspect_ratio <- 2

#color scheme
colors_cust <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#-------------------------------------------------------------------------------
# LOAD DATA 

load(file = paste0(data.loc,"FINAL_DF",".Rda"))
z <- FINAL_DF

#weegfactor vector
wei_vec <- as.vector(z$weegfactor3) 
wei_vec <- as.numeric(wei_vec)


#-------------------------------------------------------------------------------
#Clusters * leeftijd, * higher-order outcome indicators

#vulnerability distribution
vulnerable_num <- as.numeric(z$vulnerable)

vulnerable <- wpct(vulnerable_num, weight=wei_vec, na.rm=FALSE)
print(vulnerable)

write.table(vulnerable , file = paste0(data.loc,"weigthed_vulnerable_distribution_population",".csv"))

#cluster cl_kmeans membership distribution
clusters <- z %>%
  subset(!is.na(cl_kmeans),select = c(cl_kmeans)) 

cl_kmeans_num <- as.numeric(z$cl_kmeans)

#Cluster membership distribution (weigthed) (n)
plot.nme = paste0('cluster_',clustering,'_membership_distribution_weighted.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)
wtd.hist(cl_kmeans_num, weight=wei_vec,freq = FALSE)
dev.off()

#Cluster membership distribution (weigthed) (pct)
vulnerable_clus <- wpct(cl_kmeans_num, weight=wei_vec, na.rm=TRUE)

write.table(vulnerable_clus , file = paste0(data.loc,"Weigthed_vulnerable_distribution_clusters_",clustering,".csv"))

#Cluster membership distribution (unweigthed) (n)
plot.title = paste0('cluster ',clustering,' membership distribution (unweigthed)')
cluster_dis <- ggplot(clusters, aes(x = cl_kmeans)) +
  ggtitle(plot.title) +
  labs(x = "cluster") +
  geom_bar()
cluster_dis
plot.nme = paste0('Cluster_',clustering,'_membership_distribution_unweighted.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


#dispersion plots (of cases)
#please note that the mean presented in the box plots is not weigthed
#calculations of weighted means is done afterwards (see csv-file in DATA directory: weighted means of clusters)

#leeftijd
z$leeftijd <- as.numeric(z$leeftijd)

plot.nme = paste0('cluster_',clustering,'_leeftijd.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

bp3 <- boxplot(leeftijd~cl_kmeans,data=z, main="Leeftijd * cluster",
               xlab="cluster", ylab="leeftijd") 
bp3
dev.off()

#samenloop / multi-problematiek
z$samenloop <- as.numeric(z$samenloop)

plot.nme = paste0('cluster_',clustering,'_samenloop.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

bp4 <- boxplot(samenloop~cl_kmeans,data=z, main="samenloop * cluster",
               xlab="cluster", ylab="samenloop") 
bp4
dev.off()

#anst en depressie
z$GGADS201 <- as.numeric(z$GGADS201)
plot.nme = paste0('cluster_',clustering,'_angstdepressie.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

bp5 <- boxplot(GGADS201~cl_kmeans,data=z, main="Risico op angst en depressie * cluster",
               xlab="cluster", ylab="risico op angst en depressie") 
bp5
dev.off()

#regie op het leven
#score 7 t/m 19: onvoldoende eigen regie
z$GGRLS202 <- as.numeric(z$GGRLS202)
plot.nme = paste0('cluster_',clustering,'_regieopleven.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

bp6 <- boxplot(GGRLS202~cl_kmeans,data=z, main="Regie op het leven * cluster",
               xlab="cluster", ylab="regie op het leven") 
bp6
dev.off()

#eenzaamheid
#ernstig eenzaam (9-10) en zeer ernstig eenzaam (11
z$GGEES203 <- as.numeric(z$GGEES203)
plot.nme = paste0('cluster_',clustering,'_eenzaamheid.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height , width = png_height * aspect_ratio)

bp7 <- boxplot(GGEES203~cl_kmeans,data=z, main="Eenzaamheid * cluster",
               xlab="cluster", ylab="eenzaamheid") 
bp7
dev.off()



#-------------------------------------------------------------------------------
# IV WEIGHTED SCORES PER CLUSTER


#-------------------------------------------------------------------------------


qs <- z
cols_report <- c("cl_kmeans", "weegfactor3", "GGEES203", "GGRLS202", "GGADS201", "leeftijd", "samenloop") 
qs_s <- qs[,cols_report]

qs_s[] <- lapply(qs_s, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

#weighted means of age and outcome * cluster 
a <- qs_s %>%
  na.omit(cl_kmeans) %>%
  mutate(as.factor(cl_kmeans))  %>%
  group_by(cl_kmeans) %>%
  summarise(
    leeftijd_w = weighted.mean(leeftijd, weegfactor3),
    samenloop_w = weighted.mean(samenloop, weegfactor3),
    eenzaam_w = weighted.mean(GGEES203, weegfactor3),
    regie_w = weighted.mean(GGRLS202, weegfactor3),
    depri_w = weighted.mean(GGADS201, weegfactor3)
  )

print(a)

write.table(a , file = paste0(data.loc,"Weigthed_means_outcome_clusters_kmeans",".csv"))


#crosstab incidentie eenzaamheid
ct_eenzaamheid <- qs %>% 
  crosstab(cl_kmeans,GGEES208) %>% 
  adorn_crosstab("row") 
ct_eenzaamheid


#crosstab mantelzorg verlenen
ct_mz <- qs %>% 
  crosstab(cl_kmeans,MCMZGA205) %>% 
  adorn_crosstab("row") 
ct_mz


#crosstab mantelzorg intensiteit
ct_mz_int <- qs %>% 
  crosstab(cl_kmeans,MCMZGA201) %>% 
  adorn_crosstab("row") 
ct_mz_int


#ervaren gezondheid
#3 is slecht of zeer slecht
gez <- qs %>% 
  crosstab(cl_kmeans,KLGGA207) %>% 
  adorn_crosstab("row") 
gez


#heeft langdurig last van ziekte of aandoening
chron <- qs %>% 
  crosstab(cl_kmeans,CALGA260) %>% 
  adorn_crosstab("row") 
chron


#gest. hh inkomen in kwintielen
#lager is slechter
ink <- qs %>% 
  crosstab(cl_kmeans,inkkwin_2016) %>% 
  adorn_crosstab("row") 
ink

#moeite met rondkomen
#4 = grote moeite
rk <- qs %>% 
  crosstab(cl_kmeans,MMIKB201) %>% 
  adorn_crosstab("row") 
rk


#werkloos en werkzoekend
wkl <- qs %>% 
  crosstab(cl_kmeans,MMWSA207) %>% 
  adorn_crosstab("row") 
wkl


#pensioen
pen <- qs %>% 
  crosstab(cl_kmeans,MMWSA206) %>% 
  adorn_crosstab("row") 
pen


#huisvrouw/-man
huis <- qs %>% 
  crosstab(cl_kmeans,MMWSA210) %>% 
  adorn_crosstab("row") 
huis


#ethnicity
etni <- qs %>% 
  crosstab(cl_kmeans,etnicat) %>% 
  adorn_crosstab("row") 
etni


#todo: rewrite this part above with assign function


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
#gez_slecht gezondheid slecht 
#inkomenlaag_dich inkomen laag
#dagactiviteit betaald werk, vrijwilligerswerk, student 
#geenbetaaldwerk geen betaald werk
#opl_lm opleiding volwassenen laag midden
#ziek_lt langdurige ziekten
#MCMZOS304 mantelzorg ontvangen laatste 12 maanden
#MCMZGA205 mantelzorg gegeven laatste 12 maanden

dim_var <- c("cl_kmeans","leeftijd70eo","gez_slecht","inkomenlaag_dich", "dagactiviteit","geenbetaaldwerk",
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
imp2_data <- mice(sit,m=5,maxit=30,meth='logreg',seed=500)
summary(imp2_data)

sit <- complete(imp2_data,1)


#stats on missing values (post-imputation)
sapply(sit, function(x) sum(is.na(x)))

dim(sit)


#loop through clusters and print loadings per dimension
for(i in 1:k) {

#manual cluster number 
#i <- 1
#filter by cluster number  
v <-subset(sit,cl_kmeans==i)

#if PCA returns error for a certain cluster it results from lack of differentation on one or two features.
#in that case disbale for-loop and feed i with a cluster number manually
#disable the feature(s) for that particular clsuter by removing bracket below
#v$dagactiviteit <- NULL
#v$geenbetaaldwerk <- NULL

dimens_comp<-NULL

dimens_comp<- principal(v[,-which(names(v)=="cl_kmeans")], nfactors = f, residuals = FALSE,rotate=rotation,n.obs=NA, covar=FALSE,
                        scores=TRUE,missing=TRUE,impute="median")
prop.table(dimens_comp$values)

dimens_comp$loadings

}
