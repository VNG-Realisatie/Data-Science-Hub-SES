#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 1/4
# scope : dimension reduction and clustering
# tecniques: imputation (IM), outlier analysis (OU), dimensional reduction (DR), clustering (CL), 
# approach: unsupervised
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
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot", "weights",  
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","dlookr", "aod")
#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE,quietly = TRUE)
#review packages
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
#always (re)check the optimal number of clusters first!!!
#see DIMENSION.R section II. 'Optimal number of clusters'
#determine the optimal number of clusters first before running the entire script
#run the script to the point (including) section II first
#the GAP plot will indicate the optimal number of clusters
#adjust 'k' accordingly below. 
k <- 8

#perplexity (Tsne)
#In Tsne, the perplexity may be viewed as a knob that sets the number of 
#effective nearest neighbors. It is comparable with the number of nearest neighbors k 
#value between 30 and 50 is usually fine
perplex <- 40

#number of factors (PCA)
f <- 4

#rotation (PCA)
#distinct dimensions
#rotation <- "varimax"
#natural dimensions
rotation <- "none"

#dimension charts
#height <- 7
graph_height <- 8
png_height <- 600
aspect_ratio <- 2

#color scheme
colors_cust <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#-------------------------------------------------------------------------------
# LOAD DATA 
# 3 options:

#1. load Spss data from location DATA
srce.loc <- paste0(data.loc,"GGD-MONITOR-2016.sav")
SOURCE_RAW <- read_spss(srce.loc)

#2. load Spss data via modal window
#db = choose.files()

#SOURCE_RAW <- read_spss(db,
#                        to.data.frame=TRUE,
#                        use.value.labels=FALSE)


#3. load csv data from location DATA
#SOURCE_RAW <- read.csv(srce.loc, header = TRUE, sep = ",", quote = "\"",
#         dec = ".", fill = TRUE)

#number of rows and columns in source dataframe

#head(SOURCE_RAW,2)
#str(SOURCE_RAW)
dim(SOURCE_RAW)


#-------------------------------------------------------------------------------
# IDENTIFIER 

#define respondent id
SOURCE_RAW$respondent_id <- SOURCE_RAW$volgnummer 
as.factor(SOURCE_RAW$respondent_id)

#mark respondent id with an x (index) to distinguish from regular index auto counts
SOURCE_RAW$respondent_id <- paste0("x",SOURCE_RAW$respondent_id)

#attach respondent id to index of dataframe
has_rownames(SOURCE_RAW)
SOURCE_RAW <- remove_rownames(SOURCE_RAW)
SOURCE_RAW <- column_to_rownames(SOURCE_RAW, var = "respondent_id")

#re-set respondent_id
SOURCE_RAW$respondent_id <- SOURCE_RAW$volgnummer 
as.factor(SOURCE_RAW$respondent_id)

#save source dataframe as Rdata set
save(SOURCE_RAW,file=paste0(data.loc,"SOURCE_RAW",".Rda"))

#GEO data was supplied seperately
#if included in the main dataframe please comment the following 3 lines
#... and the cbind of the GEO dataframe near the end of the procedure
geo.loc <- paste0(data.loc,"GGD-MONITOR-2016-GEO.sav")
GEO <- read_spss(geo.loc)
GEO <- as.data.frame(GEO)

GEO$respondent_id <- paste0("x",GEO$volgnummer)

GEO <- remove_rownames(GEO)
GEO <- column_to_rownames(GEO, var = "respondent_id")


#-------------------------------------------------------------------------------
#DATA PREPARATION

#HIGH-ORDER OUTCOME VARIABLES (as proxy for vulnerability) 
#eenzaamheid_dich : (zeer) ernstig eenzaam (dichitoom)
#regie_dich : onvoldoende regie over eigen leven (dichitoom)
#GGADA202 : (mid-)Matig of hoog risico op angststoornis of depressie (dichitoom)
#score_zw : hoge samenloop (op onderstaande items) (will be created onthefly) 

#PREDICTORS (20 vars)

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

#zinvolle dagbesteding - 1 var
#dagactiviteit : !betaald werk, vrijwilligerswerk, student (dichitoom)

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

#create new recoded variables
#ervarengezondheid_dich 1 '(zeer) slecht' 0 'matig tot (zeer) goed)'
SOURCE_RAW$ervarengezondheid_dich = recode(SOURCE_RAW$KLGGA207, "3=1; 1=0; 2=0; 9=NA")

#regie_dich 1 'onvoldoende regie' 0 'matig of veel regie'
SOURCE_RAW$regie_dich = recode(SOURCE_RAW$GGRLS203, "0=1; 1=0; 9=NA")

#eenzaamheid_dich 1 '(zeer) ernstig eenzaam' 0 'niet of matig eenzaam'
SOURCE_RAW$eenzaamheid_dich = recode(SOURCE_RAW$GGEES209, "1=1; 0=0; 8=0; 9=NA")

#dagactiviteit 'betaald werk, vrijwilligerswerk, student' (zinvolle dagbesteding)
SOURCE_RAW$dagactiviteit <- 0
SOURCE_RAW$dagactiviteit[SOURCE_RAW$MMWSA205==9 & SOURCE_RAW$MMVWB201==9 & SOURCE_RAW$MMWSA211==9] <- NA
SOURCE_RAW$dagactiviteit[SOURCE_RAW$MMWSA205==1 | SOURCE_RAW$MMVWB201==1 | SOURCE_RAW$MMWSA211==1] <- 1

#inkkwin_2016 'Gestandaardiseerd huishoudinkomen in kwintielen (numerieke variabele)'
#inkkwin_2016 1 '0 tot 20% (max 16.100 euro)' 2 '20 tot 40% (max 21.300 euro)' 3 '40 tot 60% (max 27.200 euro)'
#4 '60 tot 80% (max 35.100 euro)' 5 '80 tot 100% (> 35.100 euro)' 9 'onbekend'.

#inkomenzeerlaag_dich 1 'zeer laag, max 16.100 euro' 0 'hoger'
SOURCE_RAW$inkomenzeerlaag_dich = recode(SOURCE_RAW$inkkwin_2016, "1=1; 2=0; 3=0; 4=0; 5=0; 9=NA")

#inkomenlaag_dich 1 'laag, max 21.300 euro' 0 'hoger'
SOURCE_RAW$inkomenlaag_dich = recode(SOURCE_RAW$inkkwin_2016, "1=1; 2=1; 3=0; 4=0; 5=0; 9=NA")

#leeftijdcat6  'leeftijd in 6 categorieen obv geboortedatum' 
SOURCE_RAW$leeftijdcat6 = recode(SOURCE_RAW$Lftklassen, "1=1; 2:4=2; 5:7=3; 8:9=4; 10:11=5; 12:14=6; 99=NA")

#leeftijd70eo 'leeftijd 70 jaar of ouder'
SOURCE_RAW$leeftijd70eo = recode(SOURCE_RAW$Lftklassen, "11=1; 12=1; 13=1; 14=1; 10:11=5; 12:14=6; 99=NA; else=0")

#opl_lm opleiding laag midden
SOURCE_RAW$opl_lm = recode(SOURCE_RAW$opl_dichVM, "0=0; 1=1; 9=NA")

#ziek_lt langdurige ziekten
SOURCE_RAW$ziek_lt = recode(SOURCE_RAW$CALGB260, "1=1; 2=0; 9=NA")

#depri_hg matig of hoog risico op angst en depressie
SOURCE_RAW$depri_hg = recode(SOURCE_RAW$GGADA202, "0=0; 1=1; 9=NA")

#NORMALIZE DATA
#Recoding -where needed- into new dichotomous variable with the same direction of the loadings (0=NO ISSUE)
SOURCE_RAW$dagactiviteit_dich = recode(SOURCE_RAW$dagactiviteit, "1=0; 0=1;")

SOURCE_RAW$MMIKB201_dich = recode(SOURCE_RAW$MMIKB201, "4=1; 3=0; 2=0; 1=0")

SOURCE_RAW$GGEEB201_dich = recode(SOURCE_RAW$GGEEB201, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB203_dich = recode(SOURCE_RAW$GGEEB203, "1=1; 2=0; 3=0")

SOURCE_RAW$GGEEB204_dich = recode(SOURCE_RAW$GGEEB204, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB207_dich = recode(SOURCE_RAW$GGEEB207, "3=1; 2=0; 1=0")

SOURCE_RAW$GGEEB208_dich = recode(SOURCE_RAW$GGEEB208, "3=1; 2=0; 1=0")

SOURCE_RAW$GGRLB201_dich = recode(SOURCE_RAW$GGRLB201, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGRLB202_dich = recode(SOURCE_RAW$GGRLB202, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGRLB204_dich = recode(SOURCE_RAW$GGRLB204, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGRLB206_dich = recode(SOURCE_RAW$GGRLB206, "1=1; 2=0; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB201_dich = recode(SOURCE_RAW$GGADB201, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB202_dich = recode(SOURCE_RAW$GGADB202, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB204_dich = recode(SOURCE_RAW$GGADB204, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB207_dich = recode(SOURCE_RAW$GGADB207, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_RAW$GGADB210_dich = recode(SOURCE_RAW$GGADB210, "1=1; 2=1; 3=0; 4=0; 5=0")

SOURCE_ENRICHED <- SOURCE_RAW

#remove source raw file
rm(SOURCE_RAW)

save(SOURCE_ENRICHED,file=paste0(data.loc,"SOURCE_ENRICHED",".Rda"))


#-------------------------------------------------------------------------------
# Subsetting

#attributes seletion : first three are higher other outcome features
#if you add features, please do so add the end of the list
#only add dichotomized features (which indicicate experienced issues/vulnerability)
cols <- c("eenzaamheid_dich","regie_dich","GGADS201","ervarengezondheid_dich",
          "MMIKB201_dich","CALGA260","CALGA261","LGBPS205","GGEEB201_dich","GGEEB203_dich","GGEEB204_dich","GGEEB207_dich",
          "GGEEB208_dich","GGRLB201_dich","GGRLB202_dich","GGRLB204_dich","GGRLB206_dich","GGADB201_dich","GGADB202_dich","GGADB204_dich",
          "GGADB207_dich","GGADB210_dich","dagactiviteit_dich") 
SOURCE_SUBSET <- subset(SOURCE_ENRICHED, select = cols)

#append '_dich' to variable name of the remaining variables not containing '_dich'
colnames(SOURCE_SUBSET) <- sub("^(?!.*_dich)(.*)", "\\1_dich", colnames(SOURCE_SUBSET), perl=TRUE)

#scope
#title- and file name string 
dest.nme.var <- paste0("","zorgwekkend")

#proxy for level of issues: 'hoge samenloop / multi-problematiek' 
SOURCE_SUBSET$score_zw <- rowSums(SOURCE_SUBSET[,4:ncol(SOURCE_SUBSET)],na.rm=TRUE)

#store 'samenloop' -score in dataframe for later
samenloop <- SOURCE_SUBSET[,c("score_zw")]
samenloop <- as.data.frame(samenloop)
zw <- cbind(GEO,samenloop)
zw <- zw[,c("volgnummer","samenloop")] 

#determine base level of 'zorgwekkend' (score_sw) and 'gebrek aan regie' (GGADS201_dich) by (natural) binning
#based on population
bin_outcome <- SOURCE_SUBSET %>%
  mutate(zw_bin = binning(SOURCE_SUBSET$score_zw),
         rg_bin = binning(SOURCE_SUBSET$GGADS201_dich)
         )

#regie en samenloop
plot.nme = paste0('explore_regie_samenloop.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)

bp1 <- boxplot(GGADS201_dich~zw_bin,data=bin_outcome, main="Gebrek aan regie * samenloop",
        xlab="level of samenloop", ylab="gebrek aan regie", staplewex = 1) 
bp1
dev.off()

#samenloop en regie
plot.nme = paste0('explore_samenloop_regie.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
bp2 <- boxplot(score_zw~rg_bin,data=bin_outcome, main="Samenloop * gebrek aan regie",
              xlab="level of gebrek aan regie", ylab="samenloop", staplewex = 1) 
#text(x=fivenum(score_zw), labels =fivenum(score_zw), y=1.25)
bp2
dev.off()

#apply subsetting by base level of score_zw (samenloop) en GGADS201_dich (gebrek regie)

#select cases 
#kwetsbare of (dreigende) zorgwekkende gevallen op het vlak van 
#eenzaamheid en / of gebrek regie op het leven, en / of angststoornis/depressie en / of samenloop van problematiek
#adjust threshold gebrek aan regie en samenloop based on binning and boxplot results (see above)

SOURCE_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$eenzaamheid_dich==1
                                      | SOURCE_SUBSET$regie_dich==1 
                                      | SOURCE_SUBSET$GGADS201_dich>22
                                      | SOURCE_SUBSET$score_zw>5), ]

#remove select / outcome variables (keep relevant variables for the fingerprint)
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = -c(eenzaamheid_dich,regie_dich,GGADS201_dich,score_zw))

#in-scope : '(potentieel) kwetsbaren'
SOURCE_SUBSET$vulnerable_suspect <- 1

#predictors
pred <- cols[1:(ncol(SOURCE_SUBSET))]

pred_df <- length(pred)
pred_df 
#attach df value to plot and -title
dest.nme.var <- paste0(dest.nme.var,"_df",pred_df)

#draw sample (for evaluation purposes) or limitations computing set-up/size population
#sample_size <- 100000
#SOURCE_SUBSET <- SOURCE_SUBSET[sample(nrow(SOURCE), sample_size), 1:pred_df]

head(SOURCE_SUBSET,2)
dim(SOURCE_SUBSET)


#-------------------------------------------------------------------------------
#keep track of respondent id

respondent_id  <- row.names(SOURCE_SUBSET)
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
 
#stats on missing values (pre-imputation)
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))
md.pattern(SOURCE_SUBSET,plot = T)

#remove cases with missing values (don't do this unless there are very good reasons)
#SOURCE_SUBSET <- na.omit(SOURCE_SUBSET)

#missing data imputation
#method : Multivariate Imputation via Chained Equations, logreg (Logistic Regression) for binary features

#method vector
meth <- make.method(SOURCE_SUBSET)
#make sure that binary data corresponds with logreg method
meth

#reset meth object for a particular feature
#meth["XXX"] <- 'logreg'

#predictorMatrix
pred <- make.predictorMatrix(SOURCE_SUBSET)
pred

#fluxplot
#Variables with higher outflux are (potentially) the more powerful.
fx <- fluxplot(SOURCE_SUBSET)
fx

#additional manipulation predictormatrix
# to do

#imp_data <- mice(SOURCE_SUBSET,method = "logreg", pred=pred,m=5,maxit=10,seed=500, print=F)

imp_data <- mice(SOURCE_SUBSET,meth=meth, pred=pred,m=5,maxit=10,seed=500, print=F)
#inspect the convergence 
summary(imp_data)
plot(imp_data)


#do 30 additional iterations lead to more convergence than maxit 10?
imp40 <- mice.mids(imp_data, maxit=30, print=F)
plot(imp40)

#apply to SOURCE_SUBSET
SOURCE_SUBSET <- complete(imp_data)

#stats on missing values (post-imputation)
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))


mdf <- data.matrix(SOURCE_SUBSET) # convert to numeric matrix for correlation calculation 
cor(mdf)

#-------------------------------------------------------------------------------
# Re-attach respondent id

SOURCE_SUBSET <- cbind(SEQ,SOURCE_SUBSET)
SOURCE_SUBSET <- remove_rownames(SOURCE_SUBSET)
SOURCE_SUBSET <- column_to_rownames(SOURCE_SUBSET, var = "volgnummer")

head(SOURCE_SUBSET,2)
#complete cases
n<-nrow(SOURCE_SUBSET) 
n


#-------------------------------------------------------------------------------
# Outliers

#Outlier detection for high-dimensional data (Mahalanobis)
#Calculate Mahalanobis predictor variables
#Mahalanobis squared distances using the (robust) minimum covariance determinant (MCD)
mah_df <- SOURCE_SUBSET
m_dist <- mahalanobis(mah_df, colMeans(mah_df), cov(mah_df),method="mcd")
SOURCE_SUBSET$MD <- round(m_dist, 1)

md <- SOURCE_SUBSET %>%
  mutate(md_bin = binning(SOURCE_SUBSET$MD)
  )

md$md_bin

#Plot outlier distribution by bin 
plot.title = paste0('Outlier distribution * bin')

outlier_dis <- ggplot(md, aes(x = md_bin)) +
  ggtitle(plot.title) +
  labs(x = "outlier bin") +
  geom_bar()
outlier_dis

plot.nme = paste0('Outliers_distribution.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


# Binary outlier variable
# threshold determined by lower boundery of last-2 bin 
threshold_zw <- 29.8 # Threshold 
SOURCE_SUBSET$outlier <- "No"
SOURCE_SUBSET$outlier[SOURCE_SUBSET$MD > threshold_zw] <- "Yes"  

SOURCE_SUBSET$outlier_fac <- 0
SOURCE_SUBSET$outlier_fac[SOURCE_SUBSET$MD > threshold_zw] <- 1 
hist(SOURCE_SUBSET$outlier_fac)



#remove outlier indicators
ANALYSIS_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$MD < threshold_zw), ]
rm(SOURCE_SUBSET)
ANALYSIS_SUBSET[ ,c('MD', 'outlier', 'outlier_fac')] <- list(NULL)
head(ANALYSIS_SUBSET,2)
dim(ANALYSIS_SUBSET)

save(ANALYSIS_SUBSET,file=paste0(data.loc,"ANALYSIS_SUBSET",".Rda"))

#save analysis subset to csv
cols.srce.nme <- paste0(data.loc, "cols-source-merged-",dest.nme.var, ".csv")
write.csv(ANALYSIS_SUBSET, file=cols.srce.nme)

#Bartlettas test of sphericity (test for homogeneity of variances) : check if data reduction is possible
#significance level must reside below of 0.05
bartlett.test(ANALYSIS_SUBSET)
#levine normale verdling 


#-------------------------------------------------------------------------------
# Distance matrix (similarity of cases)

#Gower distance
#gower_dist <- daisy(ANALYSIS_SUBSET,metric = "gower",type = list(logratio = 3))

# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

#summary(gower_dist)

#gower_mat <- as.matrix(gower_dist)

# Most similar pair
#ANALYSIS_SUBSET[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),arr.ind = TRUE)[1, ], ]


#-------------------------------------------------------------------------------
# I. Dimensionality reduction (DR)


#-------------------------------------------------------------------------------
# I.1 T-Distributed Stochastic Neighbor Embedding (TSNE)

tsne_model = Rtsne(ANALYSIS_SUBSET, check_duplicates=FALSE, pca=TRUE, perplexity=perplex, theta=0.5, dims=2)

#re-attach row id
tsne_with_ID = cbind.data.frame(rownames(ANALYSIS_SUBSET),tsne_model$Y)
colnames(tsne_with_ID)[0] <- paste(colnames(ANALYSIS_SUBSET)[0])

d_tsne = as.data.frame(tsne_with_ID)   
remove_rownames(d_tsne)
d_tsne <- column_to_rownames(d_tsne, var = "rownames(ANALYSIS_SUBSET)")

dim(d_tsne)
colnames(d_tsne)[1] <- "V1"
colnames(d_tsne)[2] <- "V2"
head(d_tsne,2)

plot.title = paste0('TSNE raw cloud ',dest.nme.var, ' ')

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
plot.nme = paste0('tsne_raw_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


## keeping original tsne data
tsne_original=d_tsne
#head(tsne_original)


#-------------------------------------------------------------------------------
# II. Dimension Reduction (DR) > Clustering (CL)


#-------------------------------------------------------------------------------
# Clustering tendency

#if you wonder whether the cases will cluster, run the part below
#Hopkins statistic: If the value of Hopkins statistic is close to zero (far below 0.5), then dataset is significantly clusterable. 

#gradient_col = list(low = "steelblue", high = "white")
#get_clust_tendency(d_tsne, n = 50, gradient = gradient_col)

#res <- get_clust_tendency(ANALYSIS_SUBSET, n = nrow(ANALYSIS_SUBSET)-1, graph = FALSE)
#res$hopkins_stat

#-------------------------------------------------------------------------------
# Optimal number of clusters

#very important to determine the optimal number of clusters!
#three methods: elbow, silhouette and GAP. We choose GAP here.
plot.nme = paste0('clusters_n_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)

# Elbow method
#fviz_nbclust(d_tsne, kmeans, method = "wss") +
#  geom_vline(xintercept = 4, linetype = 2)+
#  labs(subtitle = "Elbow method")

# Silhouette method
#fviz_nbclust(d_tsne, kmeans, method = "silhouette") +
#  labs(subtitle = "Silhouette method")

#gap statistic 
fviz_nbclust(d_tsne, kmeans, method = "gap_stat")+
  labs(subtitle = "GAP method")

#heavy-duty version (when not converging)
#CusKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=50))
#fviz_nbclust(d_tsne, FUNcluster=CusKmeansFUN, method="gap_stat") +
#labs(subtitle = "GAP method - heavy duty")

ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


#-------------------------------------------------------------------------------
# II.1 TSNE (DR) > kmeans (CL)


## Creating k-means clustering model
fit_cluster_kmeans=kmeans(scale(d_tsne), k,iter.max = 1000,algorithm = c("Forgy"))  

#cluster membership distribution
fit_cluster_kmeans$size	

#cluster membership
#fit_cluster_kmeans$cluster
fit_cluster_kmeans$cluster <- factor(fit_cluster_kmeans$cluster)

#add clustermemberschip to Tsne dataframe
tsne_original$cl_kmeans <- fit_cluster_kmeans$cluster

tsne_original$cl_kmeans <- as.factor(tsne_original$cl_kmeans)
tsne_original <- rename(tsne_original,pos_x=V1,pos_y=V2)

head(tsne_original,2)


## plotting the results with Kmeans clustering
plot.title = paste0('TSNE > Kmeans of ',dest.nme.var, ' k=',k,' perplexity=',perplex)
ggplot(tsne_original, aes(pos_x, pos_y, color = cl_kmeans)) +
        geom_point()   + 
        ggtitle(plot.title) +
#geom_text(aes(label=row.names(X)),color="#ababab") +
scale_colour_manual(values=colors_cust) + 
geom_text(aes(label = ""), size = 3, vjust = 1, color = "black")
plot.nme = paste0('tsne_kmeans_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)



#-------------------------------------------------------------------------------
# II.2 TSNE (DR) > K-medoids clustering  / partition around mediods PAM (CL)
# less prone to outliers
pam_res <- pam(d_tsne, k)

plot.nme = paste0('tsne_pam_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
title = paste0('TSNE > PAM of ',dest.nme.var, ' k=',k)
fviz_cluster(pam_res, geom = "point", ellipse.type = "convex") + 
labs(title = title)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio)


pam_res$clustering <- factor(pam_res$clustering)

#add clustermemberschip to Tsne dataframe
tsne_original$cl_pam <- pam_res$clustering


#-------------------------------------------------------------------------------
# II.3 TSNE (DR) > hierarchical clustering (CL)
## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne

#We use the Euclidean distance as distance metrics, and use 
#Ward's minimum variance method to perform agglomerative clustering.

plot.nme = paste0('tsne_hca_colors_',dest.nme.var,'_k',k,'.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store)

fit_hca_tsne=hclust(dist(scale(d_tsne), method="euclidean"), method="ward.D2")
head(fit_hca_tsne)

dend <- as.dendrogram(fit_hca_tsne)

# order it the closest we can to the order of the observations:
#dend <- rotate(dend, 1:150)
# Color the branches based on the clusters:
dend <- color_branches(dend, k = k) 

# plot dendrogram

plot(dend, type = "rectangle", ylab = "Height", main = paste("TSNE > HCA of " , dest.nme.var , " k=",k,' perplexity=',perplex),height = graph_height , width = graph_height * aspect_ratio)

rect.hclust(fit_hca_tsne, k = k, border = 2:4) 
dev.off()

# Agglomerative Nesting (Hierarchical Clustering)
#agnes(gower_dist, metric = "euclidean", stand = FALSE, method = "average")

# DIvisive ANAlysis Clustering
#diana(gower_dist, metric = "euclidean", stand = FALSE)


## setting k clusters as output
tsne_original$cl_hierarchical = factor(cutree(fit_hca_tsne, k=k)) 
#as.data.frame(tsne_original)

tsne_original$cl_hierarchical <-as.factor(tsne_original$cl_hierarchical)

head(tsne_original,2)


#-------------------------------------------------------------------------------
# II.4 TSNE (DR) > DBSCAN (CL) (optional)
#Density-based spatial clustering of applications with noise

#d_tsne_mat <- as.matrix(d_tsne)

#kNNdistplot(d_tsne_mat, k=4)
#abline(h=0.4, col="red")

#db <- dbscan(d_tsne_mat,eps = .4, MinPts = 4)
#db

#hullplot(d_tsne_mat, db$cluster)
#table(tsne_original$cl_kmeans,db$cluster)

#pairs(d_tsne, col = db$cluster + 1L)

# Local outlier factor 
#lof <- lof(d_tsne, k = 4)
#pairs(d_tsne, cex = lof)

#OPTICS
#opt <- optics(d_tsne, eps = 1, minPts = 4)
#opt

#opt <- extractDBSCAN(opt, eps_cl = .4)
#plot(opt)

#opt <- extractXi(opt, xi = .05)
#opt
#plot(opt)

#hdb <- hdbscan(d_tsne, minPts = 4)

#plot(hdb, show_flat = T)

#colors <- mapply(function(col, i) adjustcolor(col, alpha.f = hdb$membership_prob[i]), 
#                 palette()[hdb$cluster+1], seq_along(hdb$cluster))
#plot(d_tsne, col=colors, pch=20)

#tsne_original$cl_hdbscan <-as.factor(hdb$cluster)


#-------------------------------------------------------------------------------
# II.5 GOWER > PAM (CL) 

# please note that Gower in combination with clustering method PAM is also an interesting
# method of clustering cases with a large number of features  


#-------------------------------------------------------------------------------
# Compare cluster methods

#within.cluster.ss measurement shows how closely related objects are in 
#clusters; the smaller the value, the more closely related objects are within the cluster

#avg.silwidth is a measurement that considers how closely related objects 
#are within the cluster and how clusters are separated from each other.

#Kmeans 
cs1 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_original$cl_kmeans))
cs1[c("within.cluster.ss","avg.silwidth")]

#PAM 
cs2 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_original$cl_pam))
cs2[c("within.cluster.ss","avg.silwidth")]

#HCA 
cs3 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_original$cl_hierarchical))
cs3[c("within.cluster.ss","avg.silwidth")]

#hdbscan
#cs3 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_original$cl_hdbscan))
#cs3[c("within.cluster.ss","avg.silwidth")]

#we choose Kmeans 

#-------------------------------------------------------------------------------
# Writing cluster membership to csv

tsne_original_export <- tsne_original

cluster_membership_name <- paste0(data.loc,"cluster-membership-",dest.nme.var,"-k",k,"-p",perplex,".csv")
#clustermembership of both kmeans (on PCA comp)  and hierarchical clustering
write.csv(tsne_original_export, file=cluster_membership_name,row.names=TRUE)


#-------------------------------------------------------------------------------
# merging and writing all data
#do remove GEO dataframe if GEO / location parameters are included in the main dataset
#total population
z <- multimerge( list (SOURCE_ENRICHED, GEO, zw, tsne_original) )
z <- as.data.frame(z)
head(z,2)

#dichotomize clustermembership Kmeans per clusters 
for (cluster in 1:k){
  col = sprintf("clus%d", cluster)
  z[col] = as.integer(as.logical(z$cl_kmeans == cluster))
}

#dichotomize vulnerability
z$vulnerable <- as.integer(as.logical(as.integer(z$cl_kmeans)>0))
z$vulnerable[is.na(z$vulnerable)] <- 0


#clean NA notations
#z %>% replace(., is.na(.), "") %>% stringr::str_replace_all("[0-9]", "")

final_ds_name <- paste0(data.loc,"Xfinal-",dest.nme.var,"-k",k,"-p",perplex)
final_csv <- paste0(final_ds_name,".csv")
final_sav <- paste0(final_ds_name,".sav")

#z <- as.data.frame(z)
head(z)
dim(z)

as.numeric(z$volgnummer)
write.csv(z, file=final_csv,row.names=FALSE)
write_sav(z, final_sav)

FINAL_DF <- z

save(FINAL_DF,file=paste0(data.loc,"FINAL_DF",".Rda"))
