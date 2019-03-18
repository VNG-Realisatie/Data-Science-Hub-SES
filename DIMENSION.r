
#-------------------------------------------------------------------------------
# Roadmap Positive Health
# imputation, outlier analysis, dimension reduction, clustering, plotting
# author : Mark Henry Gremmen
# DataScienceHub @ JADS
# lud 2019-03-17
#-------------------------------------------------------------------------------
# Libraries

#packages
packages <- c("tools","here","tidyverse", "haven", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot", 
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest")
#if packages are not available on your computing set-up then remove bracket from next line, and afterwards re-instate bracket
#install.packages(packages)
lapply(packages,library,character.only = TRUE)
#review
sessionInfo()

#-------------------------------------------------------------------------------
# Global settings

#root location of this procedure (working directory)
root <- getwd()
root

#set graphs location
plots.loc <- paste0(root,'/PLOTS/')

#set data location
data.loc <- paste0(root,'/DATA/')

#functions
if(!exists("multimerge", mode="function")) source("D://DS/RSTAT/LIB/multimerge.R",local = TRUE)

#options
set.seed(123)
options(digits=3)

#number of clusters (Kmeans, HCA): always (re)check the optimal number of clusters!!!
#see section II. clustering tendency
k <- 7

#perplexity (Tsne)
#In Tsne, the perplexity may be viewed as a knob that sets the number of 
#effective nearest neighbors. It is comparable with the number of nearest neighbors k 
#value between 30 and 50 is usually fine
perplex <- 40

#number of factors
f <- 3

#rotation (PCA)
#rotation <- "varimax"
rotation <- "none"

#dimension charts
aspect_ratio <- 2.5
height <- 7


#-------------------------------------------------------------------------------
# Load data (3 options)

#1. load Spss data from location DATA
srce.loc <- paste0(data.loc,"GGD-MONITOR-2016.sav")
SOURCE_RAW <- read_spss(srce.loc)
SOURCE_RAW <- as.data.frame(SOURCE_RAW)

#2. load Spss data via modal window
#db = choose.files()

#SOURCE_RAW <- read_spss(db,
#                        to.data.frame=TRUE,
#                        use.value.labels=FALSE, na.action = na.omit)

#3. load csv data from location DATA
#SOURCE_RAW <- read.csv(srce.loc, header = TRUE, sep = ",", quote = "\"",
#         dec = ".", fill = TRUE)

#number of rows and columns in source dataframe
dim(SOURCE_RAW)
#meta-data
#str(SOURCE_RAW)
#head(SOURCE_RAW)

#save source as Rdata set
srce.nme <- file_path_sans_ext(basename(srce.loc))
save(SOURCE_RAW,file=paste0(data.loc,srce.nme,".Rda"))
x = load(paste0(data.loc,srce.nme,".Rda"))

#load Rdataset (to avoid damage to original dataset)
SOURCE = get(x)
rm(x)
rm(SOURCE_RAW)

#set respondent id variable
SOURCE$respondent_id <- SOURCE$volgnummer 
as.factor(SOURCE$respondent_id)

#attach respondent id to index of dataframe
has_rownames(SOURCE)
SOURCE <- remove_rownames(SOURCE)
SOURCE <- column_to_rownames(SOURCE, var = "respondent_id")

SOURCE$respondent_id <- SOURCE$volgnummer 
as.factor(SOURCE$respondent_id)

#GEO data (was supplied seperately) by GGD Hart van Brabant
geo.loc <- paste0(data.loc,"GGD-MONITOR-2016-GEO.sav")
GEO <- read_spss(geo.loc)
GEO <- as.data.frame(GEO)


#-------------------------------------------------------------------------------
# Subset(ting)

#HIGH-ORDER OUTCOME VARIABLES
#eenzaamheid_dich : (zeer) ernstig eenzaam (dichitoom)
#regie_dich : onvoldoende regie over eigen leven (dichitoom)
#GGADA202 : (mid-)Matig of hoog risico op angststoornis of depressie (dichitoom)


#PREDICTORS (20 vars)

#zinvolle dagbesteding 
#dagactiviteit : !betaald werk, vrijwilligerswerk, student (nog dischitomiseren/rescalen)

#gezondheid en beperkingen - 5 vars 
#ervarengezondheid_dich ; (zeer) slechte gezondheid (dichitoom)
#MMIKB201 : moeite met rondkomen (nog dischitomiseren)
#CALGA260 : Heeft langdurige ziekte(n) of aandoening(en) (dichitoom)
#CALGA261 : Is (ernstig) beperkt in activiteiten vanwege gezondheid (dichitoom)
#LGBPS205 : Heeft mobiliteitsbeperkingen (ofwel minimaal grote moeite met 1 vd 3 OECD items mbt mobiliteit) (dichitoom)

#proxy eenzaamheid - 5 vars
#GGEEB201 : !Kan praten over dagelijkse problemen (nog dischitomiseren)
#GGEEB203 : Ervaar leegte (nog dischitomiseren)
#GGEEB204 : !mensen om op terug te vallen bij narigheid (nog dischitomiseren)
#GGEEB207 : !Veel mensen om op te vertrouwen (nog dischitomiseren)
#GGEEB208 : !Voldoende mensen waarmee verbondenheid is (nog dischitomiseren)

#proxy regie op het leven - 4 vars
#GGRLB201 : Weinig controle over dingen die mij overkomen
#GGRLB202 : Sommige van mijn problemen kan ik met geen mogelijkheid oplossen
#GGRLB204 : Ik voel me vaak hulpeloos bij omgaan problemen van het leven
#GGRLB206 : Wat er in de toekomst met me gebeurt hangt voor grootste deel van mezelf af

#proxy voor angst en depressie (categorical) - 5 vars
#GGADB201 : Hoe vaak vermoeid zonder duidelijke reden? (nog dischitomiseren)
#GGADB202 : Hoe vaak zenuwachtig? (nog dischitomiseren)
#GGADB204 : Hoe vaak hopeloos? (nog dischitomiseren)
#GGADB207 : Hoe vaak somber of depressief? (nog dischitomiseren)
#GGADB210 : Hoe vaak afkeurenswaardig, minderwaardig of waardeloos? (nog dischitomiseren)

#scope
#title- and file name string 
dest.nme.var <- paste0("","zorgwekkend")

#variable seletion
cols <- c("eenzaamheid_dich","regie_dich","GGADS201","ervarengezondheid_dich",
          "MMIKB201","CALGA260","CALGA261","LGBPS205","GGEEB201","GGEEB203","GGEEB204","GGEEB207",
          "GGEEB208","GGRLB201","GGRLB202","GGRLB204","GGRLB206","GGADB201","GGADB202","GGADB204",
          "GGADB207","GGADB210","dagactiviteit","respondent_id") 

SOURCE_SUBSET <- SOURCE[,cols]
SOURCE_SUBSET <- as.data.frame(SOURCE_SUBSET)
#remove source file
rm(SOURCE)

#avoid conflict with original variable names
names(SOURCE_SUBSET)[names(SOURCE_SUBSET) == 'ervarengezondheid_dich'] <- 'gezondheidsbeleving'
  
#select cases 
#(potentieel) problematische gevallen op het vlak van 
#eenzaam en / of zonder regie, en / of angststoornis/depressie
SOURCE_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$eenzaamheid_dich==1
                                  | SOURCE_SUBSET$regie_dich==1 
                                  | SOURCE_SUBSET$GGADS201>24), ]

#remove outcome variables (keep relevant variables for the fingerprint)
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = -c(eenzaamheid_dich,regie_dich,GGADS201))


#Recoding -where needed- into new dichitomous variable with the same direction of the loadings
SOURCE_SUBSET$dagactiviteit_dich = recode(SOURCE_SUBSET$dagactiviteit, "1=0; 0=1;")

SOURCE_SUBSET$MMIKB201_dich = recode(SOURCE_SUBSET$MMIKB201, "4=1; 3=0; 2=0; 1=0")

SOURCE_SUBSET$GGEEB201_dich = recode(SOURCE_SUBSET$GGEEB201, "3=1; 2=0; 1=0")

SOURCE_SUBSET$GGEEB203_dich = recode(SOURCE_SUBSET$GGEEB203, "1=1; 2=0; 3=0")

SOURCE_SUBSET$GGEEB204_dich = recode(SOURCE_SUBSET$GGEEB204, "3=1; 2=0; 1=0")

SOURCE_SUBSET$GGEEB207_dich = recode(SOURCE_SUBSET$GGEEB207, "3=1; 2=0; 1=0")

SOURCE_SUBSET$GGEEB208_dich = recode(SOURCE_SUBSET$GGEEB208, "3=1; 2=0; 1=0")

SOURCE_SUBSET$GGRLB201_dich = recode(SOURCE_SUBSET$GGRLB201, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGRLB202_dich = recode(SOURCE_SUBSET$GGRLB202, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGRLB204_dich = recode(SOURCE_SUBSET$GGRLB204, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGRLB206_dich = recode(SOURCE_SUBSET$GGRLB206, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGADB201_dich = recode(SOURCE_SUBSET$GGADB201, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGADB202_dich = recode(SOURCE_SUBSET$GGADB202, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGADB204_dich = recode(SOURCE_SUBSET$GGADB204, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGADB207_dich = recode(SOURCE_SUBSET$GGADB207, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_SUBSET$GGADB210_dich = recode(SOURCE_SUBSET$GGADB210, "1=1; 2=1; 3=0; 4=0; 5=0")


#remove original pre-recoded variables
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = -c(MMIKB201,GGEEB201,GGEEB203,GGEEB204,GGEEB207,GGEEB208,GGRLB201,
                                                   GGRLB202,GGRLB204,GGRLB206,GGADB201,GGADB202,GGADB204,GGADB207,
                                                   GGADB210,dagactiviteit))

#appending '_dich' to variable name of the remaining variables not containing '_dich'
colnames(SOURCE_SUBSET) <- sub("^(?!.*_dich)(.*)", "\\1_dich", colnames(SOURCE_SUBSET), perl=TRUE)
#excpet for respondent id
SOURCE_SUBSET$respondent_id <- SOURCE_SUBSET$respondent_id_dich
SOURCE_SUBSET$respondent_id_dich <- NULL

#degrees of freedom
pred <- cols[1:(length(SOURCE_SUBSET)-1)]

pred_df <- length(pred)
pred_df 
#attach df to plot and -title
dest.nme.var <- paste0(dest.nme.var,"_df",pred_df)

#sample (for evaluation purposes)
#SOURCE_SUBSET <- SOURCE[sample(nrow(SOURCE), 15000), 1:pred_df]

head(SOURCE_SUBSET,2)
dim(SOURCE_SUBSET)


#-------------------------------------------------------------------------------
#keep track of respondent id

respondent_id  <- SOURCE_SUBSET$respondent_id
SEQ <- as.data.frame(respondent_id)

#dim(SOURCE_SUBSET)
dim(SEQ)

colnames(SEQ)[colnames(SEQ)=="respondent_id"] <- "volgnummer"
SEQ$respondent_id <- SEQ$volgnummer
#set index / rownames
SEQ <- column_to_rownames(SEQ, var = "respondent_id")
head(SEQ,5)


#-------------------------------------------------------------------------------
# Missing values and imputatation

head(SOURCE_SUBSET,3)

#relevant variables only!
SOURCE_SUBSET$respondent_id <- NULL

aggr_plot <- aggr(SOURCE_SUBSET, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(SOURCE_SUBSET), cex.axis=.7, gap=3, ylab=c(paste0("Histogram of missing data ",dest.nme.var),"Pattern"))
#stats on missing values
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))
#remove cases with missing values
#SOURCE_SUBSET <- na.omit(SOURCE_SUBSET)

#missing data imputation
imp_data <- mice(SOURCE_SUBSET,m=5,maxit=50,meth='pmm',seed=500)
summary(imp_data)
#apply to SOURCE_SUBSET
SOURCE_SUBSET <- complete(imp_data,1)

#missings after imputation
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))


#-------------------------------------------------------------------------------
# Re-attach respondent id

SOURCE_SUBSET <- cbind(SEQ,SOURCE_SUBSET)
SOURCE_SUBSET <- remove_rownames(SOURCE_SUBSET)
has_rownames(SOURCE_SUBSET)

SOURCE_SUBSET <- column_to_rownames(SOURCE_SUBSET, var = "volgnummer")

head(SOURCE_SUBSET,2)
#complete cases
n<-nrow(SOURCE_SUBSET) 
n


#-------------------------------------------------------------------------------
# Distance matrix

#for later

#-------------------------------------------------------------------------------
# Outliers

#plot of Mahalanobis : outliers in multi-dimensional data
outlier(SOURCE_SUBSET, plot=T, bad=10, na.rm=T)

# Calculate Mahalanobis with predictor variables
df2 <- SOURCE_SUBSET[, -1]    # Remove eenzaamheid variable
head(df2)
m_dist <- mahalanobis(df2, colMeans(df2), cov(df2))
SOURCE_SUBSET$MD <- round(m_dist, 1)

#head(SOURCE_SUBSET,2)

# Binary Outlier Variable
SOURCE_SUBSET$outlier <- "No"
SOURCE_SUBSET$outlier[SOURCE_SUBSET$MD > 19.9] <- "Yes"    # Threshold set to 20
#subset of regular/non-outlier cases
SOURCE_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$MD < 20), ]

head(SOURCE_SUBSET,2)

#remove outlier indicators
SOURCE_SUBSET$MD <- NULL
SOURCE_SUBSET$outlier <- NULL
dim(SOURCE_SUBSET)

#save subset to csv
cols.srce.nme <- paste0(data.loc, "cols-source-merged-",dest.nme.var, ".csv")
write.csv(SOURCE_SUBSET, file=cols.srce.nme)

#Bartlettas test of sphericity (test for homogeneity of variances)
bartlett.test(SOURCE_SUBSET)


#-------------------------------------------------------------------------------
# I. Dimensionality reduction (DR)


#-------------------------------------------------------------------------------
# I.1 TSNE (t-Distributed Stochastic Neighbor Embedding)

tsne_model_1 = Rtsne(SOURCE_SUBSET, check_duplicates=FALSE, pca=TRUE, perplexity=perplex, theta=0.5, dims=2)

#re-attach row id
Tsne_with_ID = cbind.data.frame(rownames(SOURCE_SUBSET),tsne_model_1$Y)
colnames(Tsne_with_ID)[0] <- paste(colnames(SOURCE_SUBSET)[0])

d_tsne = as.data.frame(Tsne_with_ID)   
remove_rownames(d_tsne)
d_tsne <- column_to_rownames(d_tsne, var = "rownames(SOURCE_SUBSET)")

dim(d_tsne)
colnames(d_tsne)[1] <- "V1"
colnames(d_tsne)[2] <- "V2"
head(d_tsne,2)

plot.title = paste0('TSNE raw cloud ',dest.nme.var, '')

## plotting the results without clustering
ggplot(d_tsne, aes(x=V1, y=V2)) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=15))) +
  xlab("") + ylab("") +
  ggtitle(plot.title) +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  scale_colour_brewer(palette = "Set2")
plot.nme = paste0('Rplot_tsne_raw_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = 8 , width = 8 * aspect_ratio)


## keeping original data
tsne_original=d_tsne
#head(tsne_original)


#-------------------------------------------------------------------------------
# II. Dimension Reduction > Clustering


#-------------------------------------------------------------------------------
# Clustering tendency

ct <- dist(d_tsne)
VAT(ct)
iVAT(ct)


#-------------------------------------------------------------------------------
# Optimal number of clusters

#gap statistic (light version)
#fviz_nbclust(d_tsne, kmeans, method = "gap_stat")

#heavy-duty version (when not converging within 10 iterations)
CusKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=50))
fviz_nbclust(d_tsne, FUNcluster=CusKmeansFUN, method="gap_stat")



#-------------------------------------------------------------------------------
# I.2.1 TSNE (DR) > kmeans


## Creating k-means clustering model, and assigning the result to the data used to create the tsne
fit_cluster_kmeans=kmeans(scale(d_tsne), k,iter.max = 1000,algorithm = c("Forgy"))  

#barplot(t(fit_cluster_kmeans$centers), beside=TRUE, xlab="cluster", ylab="value")

#fit_cluster_kmeans$cluster
#fit_cluster_kmeans$centers
fit_cluster_kmeans$size	


fit_cluster_kmeans$cluster <- factor(fit_cluster_kmeans$cluster)

tsne_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)
tsne_original$cl_kmeans <- as.factor(tsne_original$cl_kmeans)
tsne_original <- rename(tsne_original,pos_x=V1,pos_y=V2)

head(tsne_original,2)

plot.title = paste0('TSNE > Kmeans of ',dest.nme.var, ' k=',k,' perplexity=',perplex)
ggplot(tsne_original, aes(pos_x, pos_y, color = cl_kmeans)) + geom_point() + 
ggtitle(plot.title) +
#geom_text(aes(label=row.names(X)),color="#ababab") +
geom_text(aes(label = ""), size = 3, vjust = 1, color = "black")
plot.nme = paste0('Rplot_tsne_kmeans_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = 8 , width = 8 * aspect_ratio)




# I.2.2 TSNE (DR) > hierarchical clustering
## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne

#We use the Euclidean distance as distance metrics, and use 
#Ward's minimum variance method to perform agglomerative clustering.
fit_hca_tsne=hclust(dist(scale(d_tsne), method="euclidean"), method="ward.D2")
head(fit_hca_tsne)
# plot dendrogram

plot.nme = paste0('Rplot_tsne_hca_',dest.nme.var,'_k',k,'.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store)
plot(fit_hca_tsne, labels = FALSE, hang = -0.01, cex = 0.7,main = paste("TSNE > HCA of " , dest.nme.var , " k=",k,' perplexity=',perplex))
rect.hclust(fit_hca_tsne, k = k, border = 2:4) 
dev.off()


plot.nme = paste0('Rplot_tsne_hca_color_',dest.nme.var,'_k',k,'.png')
plot.store <-paste0(plots.loc,plot.nme)
dend <- as.dendrogram(fit_hca_tsne)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)
# Color the branches based on the clusters:
dend <- color_branches(dend, k = k) 
png(filename=plot.store)
plot(dend, main = paste("TSNE > HCA of " , dest.nme.var , " k=",k,' perplexity=',perplex))
rect.hclust(fit_hca_tsne, k = k, border = 2:4) 
dev.off()


class(tsne_original)
## setting k clusters as output
tsne_original$cl_hierarchical = factor(cutree(fit_hca_tsne, k=k)) 
#as.data.frame(tsne_original)

tsne_original$cl_hierarchical <-as.factor(tsne_original$cl_hierarchical)

head(tsne_original,2)


#-------------------------------------------------------------------------------
# DR > Clustering

# I.2.3 TSNE > DBSCAN
#Density-based spatial clustering of applications with noise

d_tsne_mat <- as.matrix(d_tsne)

kNNdistplot(d_tsne_mat, k=4)
abline(h=0.4, col="red")


db <- dbscan(d_tsne_mat,eps = .4, MinPts = 4)
db

hullplot(d_tsne_mat, db$cluster)
table(tsne_original$cl_kmeans,db$cluster)


pairs(d_tsne, col = db$cluster + 1L)

# Local outlier factor 
lof <- lof(d_tsne, k = 4)
pairs(d_tsne, cex = lof)

#OPTICS
opt <- optics(d_tsne, eps = 1, minPts = 4)
opt

opt <- extractDBSCAN(opt, eps_cl = .4)
plot(opt)

opt <- extractXi(opt, xi = .05)
opt
plot(opt)

hdb <- hdbscan(d_tsne, minPts = 4)

plot(hdb, show_flat = T)

colors <- mapply(function(col, i) adjustcolor(col, alpha.f = hdb$membership_prob[i]), 
                 palette()[hdb$cluster+1], seq_along(hdb$cluster))
plot(d_tsne, col=colors, pch=20)


#tsne_original$cl_kmeansoncomp <-as.integer(cl_kmeans_vec)

tsne_original$cl_hdbscan <-as.factor(hdb$cluster)


#-------------------------------------------------------------------------------
# compare cluster methods

#within.cluster.ss measurement shows how closely related objects are in 
#clusters; the smaller the value, the more closely related objects are within the cluster

#avg.silwidth is a measurement that considers how closely related objects 
#are within the cluster and how clusters are separated from each other.


#Kmeans 
cs = cluster.stats(dist(SOURCE_SUBSET),as.numeric(tsne_original$cl_kmeans))
cs[c("within.cluster.ss","avg.silwidth")]

#hca 
cs2 = cluster.stats(dist(SOURCE_SUBSET),as.numeric(tsne_original$cl_hierarchical))
cs2[c("within.cluster.ss","avg.silwidth")]

#hdbscan
#cs3 = cluster.stats(dist(SOURCE_SUBSET),as.numeric(tsne_original$cl_hdbscan))
#cs3[c("within.cluster.ss","avg.silwidth")]


#-------------------------------------------------------------------------------
# Writing cluster membership to csv

tsne_original_export <- tsne_original

cluster_membership_name <- paste0(data.loc,"cluster-membership-",dest.nme.var,"-k",k,"-p",perplex,".csv")
#clustermembership of both kmeans (on PCA comp)  and hierarchical clustering
write.csv(tsne_original_export, file=cluster_membership_name,row.names=TRUE)


#-------------------------------------------------------------------------------
# merging and writing original data, predictors, additional dichitomized varuables,clustering

u <- cbind.data.frame(SOURCE_SUBSET,tsne_original)
z <- multimerge( list (SOURCE, u, GEO) )
#colnames(z) <- gsub("^_", "", colnames(z))

head(z,2)

#clean NA notations
z %>% replace(., is.na(.), "") %>% stringr::str_replace_all("[0-9]", "")

class(z)
final_ds_name <- paste0(data.loc,"Xfinal-",dest.nme.var,"-k",k,"-p",perplex)
final_csv <- paste0(final_ds_name,".csv")
final_sav <- paste0(final_ds_name,".sav")

z <- as.data.frame(z)
dim(z)

head(z)
as.numeric(z$volgnummer)
as.factor(z$volgnummer)

z$zorgwekkend <- 0

z$zorgwekkend = recode(z$cl_kmeans, "9=1; 8=1; 7=1; 6=1; 5=1; 4=1; 3=1; 2=1; 1=1")

as.factor(z$zorgwekkend)

write.csv(z, file=final_csv,row.names=FALSE)
write_sav(z, final_sav)














#-------------------------------------------------------------------------------
# reporting


y <- z
cols2 <- c("weegfactor3","GGEES203","cl_kmeans")
y <- y[,cols2]


y$weegfactor3 <-as.numeric(y$weegfactor3)
y$GGEES203 <- as.numeric(y$GGEES203)


gd <- y %>% 
  group_by(cl_kmeans) %>% 
       summarise(eenzaamheid_avg = weighted.mean(GGEES203,weegfactor3,na.rm = TRUE))


#eenzaamheid * cluster
ggplot(data=gd, aes(x=cl_kmeans, y=eenzaamheid_avg), fill = as.factor(cl_kmeans)) +
  geom_bar(col="red", 
           fill="green", 
           alpha = .2,position = "dodge", stat = "summary", fun.y = "mean") +
  guides(fill=FALSE)  + 
  labs(title=paste0("Clusters * eenzaamheid of " , dest.nme.var , " k=",k)) +
  labs(x="cluster", y="avg(eenzaamheid)") + 
  scale_x_discrete(name ="cluster", limits=c(1:k))

plot.nme = paste0('Rplot_clusters_eenzaamheid_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = 8 , width = 8 * aspect_ratio)

plot.store








library(tmap)
data("NLD_prov")

tmap_mode('view')

tm_shape(NLD_prov) + tm_polygons('population') + tm_layout(basemaps = c('OpenStreetMap'))