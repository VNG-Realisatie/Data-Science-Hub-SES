#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 1/4
# scope : dimension reduction and clustering
# tecniques: imputation (IM), outlier analysis (OU), dimensional reduction (DR), clustering (CL), 
# approach: unsupervised
# requirements: R Statistics version  (3.60=<)
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders (GGD HvB), Ester de Jonge (GGD ZHZ)
# DataScienceHub @ JADS, GGD Hart voor Brabant, GGD Zuid-Holland Zuid
# lud 2019-12-18
#------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#packages
packages <- c("devtools","tools","here","tidyverse","naniar", "haven", "stats", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot", "weights", "RColorBrewer","skimr", "corrplot",  
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","dlookr", "aod", "janitor", "descr", "forcats", "sqldf")
#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE,quietly = TRUE)
#review packages loaded
sessionInfo()

#-------------------------------------------------------------------------------
# Global settings

#root location of this procedure (working directory)
root <- getwd()
root

#GGD dep.
#ggd <- 'HVB' #Hart voor Brabant
ggd <- 'ZHZ' #Zuid-Holland Zuid
#ggd <- 'UTR' #Utrecht

#source varariables manipulation location
src.dir <- paste0(root,'/SRC/VARS.R')

#set graphs location
plots.dir <- paste0(root,'/PLOTS/')
plots.loc <- paste0(root,'/PLOTS/',ggd,'/')

#set data location 
data.dir <- paste0(root,'/DATA/')
data.loc <- paste0(root,'/DATA/',ggd,'/')

locations <- c(plots.dir, data.dir, plots.loc, data.loc)

lapply(locations, function(x) {
  if (!dir.exists(x)) {dir.create(x)}
})

#config
set.seed(123)  # for reproducibility
options(digits=15)

#respondent id = "volgnummer"

#number of clusters (Kmeans, Hierarchical Cluster Analysis) 
#always (re)check the optimal number of clusters
#see DIMENSION.R section II. 'Optimal number of clusters'
#determine the optimal number of clusters first before running the entire script
#run the script to the point (including) section II first
#the Silhoutte or GAP plot will indicate the optimal number of clusters
#adjust 'k' accordingly below. 
k <- 8

#perplexity (Tsne)
#In Tsne, the perplexity may be viewed as a knob that sets the number of 
#effective nearest neighbors. It is comparable with the number of nearest neighbors k 
#value between 30 and 50 is usually fine
perplex <- 35

#dimension and quality plots
graph_height <- 9
png_height <- 600
aspect_ratio <- 2
dpi <- 320 #retina

#qualitative color scheme from ColorBrewer
ncolors <- k+1 #number of clusters plus group of outliers
colors_cust <- brewer.pal(ncolors, "Paired")

#scope
#title- and file name string 
dest.nme.var <- paste0("kwetsbaar", "_",ggd)

#-------------------------------------------------------------------------------
# LOAD DATA 
# 3 options:

#1. load Spss dataset from location DATA
srce.loc <- paste0(data.loc,"GGD-MONITOR-2016.sav")
SOURCE_RAW <- read_spss(srce.loc)

#2. load Spss dataset via modal window
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
skim(SOURCE_RAW)

#load TFI Spss dataset from location DATA (ZHZ only)
#tfi.loc <- paste0(data.loc,"TFI.sav")
#TFI <- read_spss(tfi.loc)

dim(SOURCE_RAW)

#-------------------------------------------------------------------------------
# IDENTIFIER 

#unique identifiers?
sqldf("SELECT count(distinct(volgnummer)) FROM SOURCE_RAW")

#define respondent id
SOURCE_RAW$respondent_id <- as.character(SOURCE_RAW$volgnummer) 

#attach respondent id to index of dataframe
SOURCE_RAW$respondent_id <- paste0("x",SOURCE_RAW$respondent_id)
has_rownames(SOURCE_RAW)
SOURCE_RAW <- remove_rownames(SOURCE_RAW)
SOURCE_RAW <- column_to_rownames(SOURCE_RAW, var = "respondent_id")

#re-set respondent_id
SOURCE_RAW$respondent_id <- as.factor(SOURCE_RAW$volgnummer) 
head(SOURCE_RAW)

#save source dataframe as Rdata set
save(SOURCE_RAW,file=paste0(data.loc,"SOURCE_RAW",".Rda"))

#-------------------------------------------------------------------------------
#DATA PREPARATION

#HIGH-ORDER OUTCOME VARIABLES (as proxy for vulnerability) : inclusion criteria
#
#GGEES203 : (zeer) ernstig eenzaam (dichotoom)
#GGRLS202 : onvoldoende regie over eigen leven (dichotoom)
#GGADS201 : (mid-)Matig of hoog risico op angststoornis of depressie (dichotoom)
#KLGGA207 ; (zeer) slechte gezondheid (dichotoom)
#score_zw : hoge samenloop (op onderstaande items, 4<) (will be created on-the-fly) 


#FEATURES 

#zinvol bestaan / betekenisvol sociaal netwerk 
#MMWSA205, MMWSA211 : werk / opleiding 
#MMVWB201 : vrijwilligerswerk 
#MCMZGS203 : mantelzorger 

#eenzaamheid 
#GGEEB201 : !Kan praten over dagelijkse problemen 
#GGEEB203 : Ervaar leegte 
#GGEEB204 : !mensen om op terug te vallen bij narigheid 
#GGEEB207 : !Veel mensen om op te vertrouwen 
#GGEEB208 : !Voldoende mensen waarmee verbondenheid is 

#GGEEB210 : Voel me vaak in de steekgelaten REMOVED
#GGEEB211 : !Kan bij vrienden terecht wanneer behoefte is REMOVED

#gezondheid en beperkingen
#CALGA260 : Heeft langdurige ziekte(n) of aandoening(en) 
#CALGA261 : Is (ernstig) beperkt in activiteiten vanwege gezondheid 
#MMIKB201 : moeite met rondkomen 

#LGBPS209 : Heeft minimaal een beperking met horen, zien of mobiliteit (ofwel minimaal grote moeite met 1 vd 7 OECD items) REMOVE
#LGBPS203 : beperking horen
#LGBPS204 : beperking zien
#LGBPS205 : beperking mobiliteit

#lifestyle/gedrag
#AGGWS205 : Obesitas, ofwel een BMI van 30 of hoger
#LFALA213 : zware drinker
#LFRKA205 : roker
# beweegnorm / fitnorm ADD? 

#angst en depressie
#GGADB201 : Vaak vermoeid zonder duidelijke reden 
#GGADB202 : Vaak zenuwachtig 
#GGADB204 : Vaak hopeloos 
#GGADB207 : Vaak somber of depressief
#GGADB210 : Vaak afkeurenswaardig, minderwaardig of waardeloos

#regie op het leven
#GGRLB201 : Weinig controle over dingen die mij overkomen
#GGRLB202 : Sommige van mijn problemen kan ik met geen mogelijkheid oplossen
#GGRLB204 : Ik voel me vaak hulpeloos bij omgaan problemen van het leven
#GGRLB206 : Wat er in de toekomst met me gebeurt hangt voor grootste deel van mezelf af
#MCMZOS304 : mantelzorg ontvangen

#vitaliteit en veerkracht ADD?
#levenstevredenheid en geluk ADD?

#variable manipulation (see directory SRC > VARS.R)
source(src.dir)

SOURCE_ENRICHED <- SOURCE_RAW

#remove source raw file
rm(SOURCE_RAW)

save(SOURCE_ENRICHED,file=paste0(data.loc,"SOURCE_ENRICHED",".Rda"))


#-------------------------------------------------------------------------------
# Filtering

SOURCE_SUBSET <- SOURCE_ENRICHED
#municipality filter
#SOURCE_SUBSET <- SOURCE_ENRICHED[SOURCE_ENRICHED$Gemeentecode>1,]


#-------------------------------------------------------------------------------
# Subsetting

#features : first four are higher-order outcome dimensions
#if you add features, please do so add the end of the list
#only add dichotomized features (which indicicate experienced issues/vulnerability, vitality or resilience)

cols <- c("GGEES203","GGRLS202","GGADS201","KLGGA207", # <- outcome level
          "werkopleiding_dich", "vrijwilligerswerk_dich", "mantelzorg_dich", # <- zinvol bestaan
          # perception in outcome features -> 
          #eenzaamheid
          "GGEEB201_dich","GGEEB203_dich","GGEEB204_dich","GGEEB207_dich","GGEEB208_dich",
          #"GGEEB210", "GGEEB211" # in de tseek gelaten, kan bij vrienden terecht
          #gezondheid en beperkingen
          "CALGA260","CALGA261","LGBPS209","MMIKB201_dich",
          #angst en depressie
          "GGRLB201_dich","GGRLB202_dich","GGRLB204_dich","GGADB207_dich","GGADB210_dich",
          #regie op het leven
          "GGADB201_dich","GGADB202_dich","GGADB204_dich","GGRLB206_dich", "MCMZOS304", 
          #lifestyle/gedrag
          "LFALA213", "LFRKA205", "AGGWS205")
          
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = cols)

#append '_dich' to variable name of the remaining variables not containing '_dich'
colnames(SOURCE_SUBSET) <- sub("^(?!.*_dich)(.*)", "\\1_dich", colnames(SOURCE_SUBSET), perl=TRUE)

#proxy for level of issues: 'hoge samenloop / multi-problematiek' start column range after outcome level and zinvol bestaan indicators
score_zw <- SOURCE_SUBSET %>%
  mutate (score_zw = rowSums(SOURCE_SUBSET[,8:ncol(SOURCE_SUBSET)],na.rm=TRUE))

score_zw <- score_zw[,c("score_zw")]
SOURCE_SUBSET <- cbind(SOURCE_SUBSET,score_zw)


#determine base level of 'samenloop' (score_sw) and 'angst en depressie' (GGADS201_dich) by (natural) binning
#based on population
bin_outcome <- SOURCE_SUBSET %>%
  mutate(zw_bin = binning(SOURCE_SUBSET$score_zw),
         dep_bin = binning(SOURCE_SUBSET$GGADS201_dich),
         reg_bin = binning(SOURCE_SUBSET$GGRLS202_dich)
  )

#regie en samenloop
plot.title = paste0('Gebrek aan regie * samenloop')
bp1 <- ggplot(bin_outcome, aes(y = GGRLS202_dich, x = zw_bin, fill=zw_bin)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  # geom_point(data=a,aes(x=cl_kmeans,y=samenloop_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() +
  xlab("level of samenloop") +
  ylab("gebrek aan regie") +
  guides(fill=guide_legend(title="Samenloop")) +
  ggtitle(plot.title)
bp1
plot.nme = paste0(ggd,'_explore_regie_samenloop.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)

#samenloop en gebrek aan regie
plot.title = paste0('Samenloop * gebrek aan regie')
bp2 <- ggplot(bin_outcome, aes(x = reg_bin, y = score_zw, fill=reg_bin)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  # geom_point(data=a,aes(x=cl_kmeans,y=samenloop_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  ylab("level of samenloop") +
  xlab("level of gebrek aan regie") +
  guides(fill=guide_legend(title="Level of regie")) +
  ggtitle(plot.title)
bp2
plot.nme = paste0(ggd,'_explore_samenloop_gebrekregie.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)

#samenloop en angst & depressie
plot.title = paste0('Samenloop * angst en depressie')
bp3 <- ggplot(bin_outcome, aes(x = dep_bin, y = score_zw, fill=dep_bin)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  # geom_point(data=a,aes(x=cl_kmeans,y=samenloop_w),shape = 23, size = 3, fill ="red",inherit.aes=FALSE) +
  theme_minimal() + 
  ylab("level of samenloop") +
  xlab("level of angst en depressie") +
  guides(fill=guide_legend(title="Level of A&D")) +
  ggtitle(plot.title)
bp3
plot.nme = paste0(ggd,'_explore_samenloop_angstendepressie.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)


#inclusion criteria
#kwetsbare of (dreigende) zorgwekkende gevallen op het vlak van 
#eenzaamheid en / of gebrek regie op het leven, en / of angststoornis/depressie en / of
#ervaren gezondheid en / of hoge samenloop van problematiek
#adjust threshold gebrek aan regie en samenloop based on binning and boxplot results (see above)


SOURCE_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$GGEES203_dich>8 #eenzaamheid 
                                      | SOURCE_SUBSET$GGRLS202_dich<23 #regie op het leven 
                                      | SOURCE_SUBSET$GGADS201_dich>20 #angststoornis of depressie
                                      | SOURCE_SUBSET$KLGGA207_dich==3 #ervaren gezondheid 
                                      | SOURCE_SUBSET$score_zw>4) #samenloop
                                , ]

#remove outcome variables, keep relevant variables for the fingerprint
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = -c(GGEES203_dich,GGRLS202_dich,GGADS201_dich,KLGGA207_dich,score_zw))

#number of dichotomous features (df)
pred_df <- ncol(SOURCE_SUBSET)

#TAG : attach df-value to plot name and -title
dest.nme.var <- paste0(dest.nme.var,"_df",pred_df)

#draw sample (for evaluation purposes) or limitations computing set-up/size population
#sample_size <- 100000
#SOURCE_SUBSET <- SOURCE_SUBSET[sample(nrow(SOURCE), sample_size), 1:pred_df]

head(SOURCE_SUBSET)
dim(SOURCE_SUBSET)


#-------------------------------------------------------------------------------
#keep track of respondent id in subject group

respondent_id  <- row.names(SOURCE_SUBSET)
SEQ <- as.data.frame(respondent_id)

colnames(SEQ)[colnames(SEQ)=="respondent_id"] <- "volgnummer"
SEQ$respondent_id <- SEQ$volgnummer
#set index / rownames
SEQ <- column_to_rownames(SEQ, var = "respondent_id")
head(SEQ,5)


#-------------------------------------------------------------------------------
# Missing values and imputatation

#stats on missing values (pre-imputation)
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))

#plot missing data pattern
md.pattern(SOURCE_SUBSET,plot = T)

#remove cases with missing values (don't do this unless there are very good reasons for it)
#SOURCE_SUBSET <- na.omit(SOURCE_SUBSET)

#missing data imputation
#method : Multivariate Imputation via Chained Equations, logreg (Logistic Regression) for binary features

#fluxplot
#Variables with higher outflux are (potentially) the more powerful.
plot.nme = paste0(ggd,'_fluxplot_pattern.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
fplot <- fluxplot(SOURCE_SUBSET)
fplot
dev.off()

#initial run to auto-determine powerful predictors for imputation
ini <- mice(SOURCE_SUBSET,pred=quickpred(SOURCE_SUBSET, mincor=.35),seed=500, print=F)
#predictor matrix
(pred <- ini$pred)

save(pred, file = "prediction_matrix_features.RData")

#pred <- load("prediction_matrix_features.RData")

#first run (low number of iterations)
imp_data <- mice(SOURCE_SUBSET,method = "logreg", pred=pred,m=5,maxit=10,seed=500, print=T)

summary(imp_data)

#convergence
#it is important that convergence is taking effect towards the end of the iteration process

plot.nme = paste0(ggd,'_convergence_imputation_iterations_first_run.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
cplot_1 <- plot(imp_data)
cplot_1
dev.off()

#second run
#do additional iterations lead to better convergence than in first run?
imp_ext <- mice.mids(imp_data, maxit=15, print=F)

plot.nme = paste0(ggd,'_convergence_imputation_iterations_second_run.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
cplot_2 <- plot(imp_ext)
cplot_2
dev.off()

#if so, use the extended version (otherwize adjust maxit in mice.mids)
imp_data <- imp_ext

#densityplot : imputation versus oberservation (proportions are important here)
plot.nme = paste0(ggd,'_imputation_pattern.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
dplot <- densityplot(imp_data)
dplot
dev.off()

#apply imputated values to SOURCE_SUBSET
SOURCE_SUBSET <- complete(imp_data)

#stats on missing values (post-imputation). All gone!
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))
#head(SOURCE_SUBSET)

#-------------------------------------------------------------------------------
# Re-attach respondent id

SOURCE_SUBSET <- cbind(SEQ,SOURCE_SUBSET)
SOURCE_SUBSET <- remove_rownames(SOURCE_SUBSET)
SOURCE_SUBSET <- column_to_rownames(SOURCE_SUBSET, var = "volgnummer")

#in-scope : subject group
SOURCE_SUBSET$vulnerable_suspect <- 1

head(SOURCE_SUBSET,2)
#complete cases
(n<-nrow(SOURCE_SUBSET)) 


#-------------------------------------------------------------------------------
# Collinearity
#low correlation is good
#plot.nme = paste0(ggd,'_correlation_features.png')
#plot.store <-paste0(plots.loc,plot.nme)
#png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
#mdf <- data.matrix(SOURCE_SUBSET) # convert to numeric matrix for correlation calculation 
#corplot <- corrplot(cor(mdf), method="color",type="upper")
#(corplot)
#dev.off()


#-------------------------------------------------------------------------------
# Outliers

#Outlier detection for high-dimensional data (Mahalanobis)
#Calculate Mahalanobis predictor variables
#Mahalanobis squared distances using the (robust) minimum covariance determinant (MCD)
mah_df <- SOURCE_SUBSET[,-which(names(SOURCE_SUBSET) %in% c("vulnerable_suspect"))] #list columns which are not features
m_dist <- mahalanobis(mah_df, colMeans(mah_df), cov(mah_df),method="mcd")
SOURCE_SUBSET$MD <- round(m_dist, 1)

#outliers binned
md <- SOURCE_SUBSET %>%
  mutate(md_bin = binning(SOURCE_SUBSET$MD)
  )
md$md_bin

#Plot outlier distribution by bin 
plot.title = paste0('Outlier distribution * bin')
outlier_dis <- ggplot(md, aes(x = md_bin)) +
  ggtitle(plot.title) +
  guides(fill=guide_legend(title="Level of outlier")) +
  labs(x = "outlier bin") +
  theme_minimal() + 
  geom_bar(aes(fill = md_bin)) 

outlier_dis
plot.nme = paste0(ggd,'_outliers_distribution.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

# Binary outlier variable
# threshold determined by lower boundary of last valid bin, see outlier distribution plot 
threshold_zw <- 48.9 # Threshold UTR: 51.5, ZHZ : 54.2 , HvB : 35.1 (higher value means more outliers in scope)
SOURCE_SUBSET$outlier <- "No"
SOURCE_SUBSET$outlier[SOURCE_SUBSET$MD > threshold_zw] <- "Yes"  

SOURCE_SUBSET$outlier_fac <- 0
SOURCE_SUBSET$outlier_fac[SOURCE_SUBSET$MD > threshold_zw] <- 1 

plot.title = paste0('Outliers')
plot.nme = paste0(ggd,'_outliers.png')
plot.store <-paste0(plots.loc,plot.nme)

outlier_dich <- ggplot(md, aes(x = SOURCE_SUBSET$outlier_fac)) +
  ggtitle(plot.title) +
  labs(x = "outliers") +
  guides(fill=guide_legend(title="Level of outlier")) +
  theme_minimal() + 
  geom_bar(aes(fill = md_bin)) +
  geom_text(aes(label=..count..),stat="count")

outlier_dich
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

save(SOURCE_SUBSET,file=paste0(data.loc,"SOURCE_SUBSET_INC_OUTLIERS",".Rda"))

#store outlier qualifier for later use (after dimension reduction, before clustering)
outlier_vec <- (SOURCE_SUBSET$outlier=="Yes")

#prepare for analysis
ANALYSIS_SUBSET <- SOURCE_SUBSET
ANALYSIS_SUBSET[ ,c('MD', 'outlier', 'outlier_fac','vulnerable_suspect')] <- list(NULL)
head(ANALYSIS_SUBSET,2)
#dimensions should represent research subject group and relevant features only!
dim(ANALYSIS_SUBSET)

#save analysis subset
save(ANALYSIS_SUBSET,file=paste0(data.loc,"ANALYSIS_SUBSET",".Rda"))
cols.srce.nme <- paste0(data.loc, "analysis-subset-",dest.nme.var, ".csv")
write.csv(ANALYSIS_SUBSET, file=cols.srce.nme)

#Bartlettas test of sphericity (test for homogeneity of variances) : check if data reduction is possible
#significance level must reside well below of 0.05
bartlett.test(ANALYSIS_SUBSET)


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

#reduce features to two dimensions with TSNE
tsne_model = Rtsne(ANALYSIS_SUBSET, check_duplicates=FALSE, pca=TRUE, perplexity=perplex, theta=0.5, dims=2)

#re-attach row id
tsne = cbind.data.frame(rownames(ANALYSIS_SUBSET),tsne_model$Y)
colnames(tsne)[0] <- paste(colnames(ANALYSIS_SUBSET)[0])
remove_rownames(tsne)
tsne <- column_to_rownames(tsne, var = "rownames(ANALYSIS_SUBSET)")

colnames(tsne)[1] <- "V1"
colnames(tsne)[2] <- "V2"
head(tsne,2)

#plot subject group in a two-dimensional space
plot.title = paste0('TSNE cloud ',dest.nme.var)
ggplot(tsne, aes(x=V1, y=V2)) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=15))) +
  xlab("") + ylab("") +
  ggtitle(plot.title) +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  theme_void() +
  scale_colour_brewer(palette = "Set2")
plot.nme = paste0('tsne_raw_',dest.nme.var,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)


## TSNE data subject group
tsne_sub <- tsne
## registration of subject group INCLUDING membership of various clustering methods
tsne_store <- tsne

#Preparation for Kmeans clustering 
#TSNE of focal group (without outliers / noise)
#exclude outliers from clustering in case of Kmeans
#Kmeans is sensitive to (strong) outliers
outlier_df <- as.data.frame(outlier_vec)
tsne_core <- cbind(tsne_sub,outlier_df)


tsne_foc <- tsne_core[outlier_vec==FALSE,]
tsne_foc <- subset(tsne_foc, select=c(V1,V2))
rm(tsne_core)

#recap: tsne_sub is the research population (with outliers), tsne_foc is
#the focal group without the outliers (for Kmeans)

#-------------------------------------------------------------------------------
# II. Dimension Reduction (DR) > Clustering (CL)


#-------------------------------------------------------------------------------
# Clustering tendency

#Hopkins statistic
#gradient_col = list(low = "steelblue", high = "white")
#get_clust_tendency(tsne, n = 50, gradient = gradient_col)

#res <- get_clust_tendency(ANALYSIS_SUBSET, n = nrow(ANALYSIS_SUBSET)-1, graph = FALSE)
#res$hopkins_stat

#-------------------------------------------------------------------------------
# Optimal number of clusters

#very important to determine the optimal number of clusters!
#three methods: elbow, silhouette and GAP. We choose GAP here.
plot.nme = paste0(ggd, 'optimal_clusters_n_',dest.nme.var,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)

# 3 methods
# Elbow method
#fviz_nbclust(tsne_sub, kmeans, method = "wss") +
#  geom_vline(xintercept = 4, linetype = 2)+
#  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(tsne_sub, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# GAP method (regular, 10 iterations)
#fviz_nbclust(tsne_sub, kmeans, method = "gap_stat")+
#  labs(subtitle = "GAP method")

# GAP heavy-duty version (when regular not converging)
#CusKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=50))
#fviz_nbclust(tsne_sub, FUNcluster=CusKmeansFUN, method="gap_stat") +
#labs(subtitle = "GAP method - heavy duty")

ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

#reset k?



#-------------------------------------------------------------------------------
# II.1 TSNE (DR) > kmeans (CL)

#NB: as kmeans is prone to outliers we use the focal group here (cases excluding outliers). 
#we cluster the focal group cases and plot the outliers as 0
#later we associate outliers to it's position to the dominant cluster

#the trick is to accommodate as much outliers as possible untill the point that it gets too
#difficult for Kmeans to converge: adjust outlier threshold accordingly !!
#the group of outliers that cannot be associated with a K-means cluster may need special attention
#in policy and intervention efforts.
#especially when outliers are near (or 'in') multi-problem or highly vulnerable clusters 

#working tsne for kmeans (focus group)
tsne_km <- tsne_foc

## Creating k-means clustering model
fit_cluster_kmeans=kmeans(scale(tsne_km), k,iter.max = 1000,algorithm = c("Forgy"))  

#cluster membership distribution
fit_cluster_kmeans$size	

#add clustermemberschip to Tsne (focal) dataframe
tsne_km$cl_kmeans <- as.factor(fit_cluster_kmeans$cluster)

#tsne_sub$repondent_id <- row.names(tsne_sub) 
#merge focal group with research group
tsne_km_ext <- merge(tsne_sub, tsne_km, left_index=True, right_index=True,all=TRUE)

#extended version
#tsne_km_ext <- fr[,3:5]
#rm(fr)

tsne_km_ext$cl_kmeans <- fct_explicit_na(tsne_km_ext$cl_kmeans, "0")
head(tsne_km_ext,2)

#just in case
tsne_km_ext$V1 <- as.numeric(as.character(tsne_km_ext$V1))
tsne_km_ext$V2 <- as.numeric(as.character(tsne_km_ext$V2))

## plotting the results with Kmeans clustering
plot.title = paste0('TSNE > Kmeans of ',dest.nme.var, ' k=',k,' perplexity=',perplex)
ggplot(tsne_km_ext, aes(V1, V2, color = cl_kmeans)) +
  geom_point() + 
  ggtitle(plot.title) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  #geom_text(aes(label=row.names(X)),color="#ababab") +
  scale_colour_manual(name = "cluster",values=colors_cust) + 
  geom_text(aes(label = ""), size = 3, vjust = 1, color = "black")
plot.nme = paste0('tsne_kmeans_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

km_cl <- merge(tsne_store, tsne_km, by=0, all.x=T, keep=F)
row.names(km_cl) <- km_cl[,1]
#km_cl <- km_cl[,c(2,3,6)]

#names(km_cl)[names(km_cl) == 'V1.x'] <- 'V1'
#names(km_cl)[names(km_cl) == 'V2.x'] <- 'V2'

#add clustermemberschip to Tsne dataframe
tsne_store$cl_kmeans <- km_cl$cl_kmeans 
rm(km_cl)

#-------------------------------------------------------------------------------
# II.2 TSNE (DR) > K-medoids clustering  / partition around mediods PAM (CL)

# less prone to outliers, therefore we use the subject group (incl outliers)

pam_res <- pam(tsne_sub, k)

plot.nme = paste0('tsne_pam_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
title = paste0('TSNE > K-mediods (PAM) of ',dest.nme.var, ' k=',k)
fviz_cluster(pam_res, geom = "point", ellipse.type = "convex") +
  labs(title = title)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

#add clustermemberschip to Tsne dataframe
tsne_store$cl_pam <- pam_res$clustering

#-------------------------------------------------------------------------------
# II.3 TSNE (DR) > hierarchical clustering (CL)
## Creating hierarchical cluster model, and assigning the result to the data used to create the tsne

#We use the Euclidean distance as distance metrics, and use 
#Ward's minimum variance method to perform agglomerative clustering.

plot.nme = paste0('tsne_hca_colors_',dest.nme.var,'_k',k,'.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store)

fit_hca_tsne=hclust(dist(scale(tsne_sub), method="euclidean"), method="ward.D2")
head(fit_hca_tsne)

dend <- as.dendrogram(fit_hca_tsne)

# order it the closest we can to the order of the observations:
#dend <- rotate(dend, 1:150)
# Color the branches based on the clusters:
dend <- color_branches(dend, k = k) 
# plot dendrogram

plot(dend, type = "rectangle", ylab = "Height", main = paste("TSNE > HCA of " , dest.nme.var , " k=",k,' perplexity=',perplex),height = graph_height , width = graph_height * aspect_ratio,dpi = 300)
rect.hclust(fit_hca_tsne, k = k, border = 2:4) 
dev.off()

# Agglomerative Nesting (Hierarchical Clustering)
#agnes(gower_dist, metric = "euclidean", stand = FALSE, method = "average")

# DIvisive ANAlysis Clustering
#diana(gower_dist, metric = "euclidean", stand = FALSE)

#add clustermemberschip to Tsne dataframe
tsne_store$cl_hierarchical = as.factor(factor(cutree(fit_hca_tsne, k=k))) 
#as.data.frame(tsne_original)

head(tsne_store,2)


#-------------------------------------------------------------------------------
# POST PROCESSING

#mark outliers
tsne_store$outlier <- 0 
tsne_store$outlier[is.na(tsne_store$cl_kmeans)] <-1

#reattach index to dataframe
tsne_store$respondent_id <- as.factor(row.names(tsne_store)) 
tsne_store$volgnummer <- gsub("x","",as.character(tsne_store$respondent_id))
head(tsne_store)

#-------------------------------------------------------------------------------
# Compare cluster methods

#within.cluster.ss measurement shows how closely related objects are in 
#clusters; the smaller the value, the more closely related are observations within the cluster

#avg.silwidth is a measurement that considers how closely related objects 
#are within the cluster and how clusters are separated from each other.

#Kmeans 
ANALYSIS_SUBSET_km <- cbind(ANALYSIS_SUBSET,outlier_df)
ANALYSIS_SUBSET_km <- ANALYSIS_SUBSET_km[outlier_vec==FALSE,]

cs1 = cluster.stats(dist(ANALYSIS_SUBSET_km),as.numeric(tsne_km$cl_kmeans))
silwidth_kmeans <- cs1[c("within.cluster.ss","avg.silwidth")]
silwidth_kmeans

#PAM
cs2 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_store$cl_pam))
silwidth_pam <- cs2[c("within.cluster.ss","avg.silwidth")]
silwidth_pam

#HCA 
cs3 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_store$cl_hierarchical))
silwidth_hca <- cs3[c("within.cluster.ss","avg.silwidth")]
silwidth_hca

silwidth <- rbind(silwidth_kmeans,silwidth_pam,silwidth_hca)

silwidth <- as.data.frame(silwidth)
silwidth

silwidth$within.cluster.ss <- as.numeric(silwidth$within.cluster.ss) 
silwidth$methods <- row.names(silwidth)
silwidth$methods <- as.factor(silwidth$methods)  
#Plot outlier distribution by bin 
plot.title = paste0('Measurement of consistency observations within cluster')
plot.nme = paste0(ggd,'_within_cluster_ss.png')
plot.store <-paste0(plots.loc,plot.nme)
cluster_ss <- ggplot(silwidth, aes(x=methods, y = within.cluster.ss)) +
  ggtitle(plot.title) +
  labs(x = "Clustering method", y = "Within.cluster.ss (lower is better)") +
  geom_boxplot() 
cluster_ss
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)



#clusterassignaties vergelijken Fowlkes-Mallows
#cl_kmeans versus cl_pam
rep <- 10000
FM_index_H0 <- replicate(rep, FM_index_permutation(tsne_store$cl_pam, tsne_store$cl_kmeans)) 
plot(density(FM_index_H0), main = "FM Index distribution under H0\n (10000 permutation)")
abline(v = mean(FM_index_H0), col = 1, lty = 2)

skew(FM_index_H0) 
kurtosi(FM_index_H0)




#-------------------------------------------------------------------------------
# Writing cluster membership to csv

cluster_membership_name <- paste0(data.loc,"cluster-membership-",dest.nme.var,"-k",k,"-p",perplex,".csv")
write.csv(tsne_store, file=cluster_membership_name,row.names=TRUE)

#-------------------------------------------------------------------------------
# merging and writing all data
#do remove GEO dataframe if GEO / location parameters are included in the main dataset
#total population

ZW <- data.frame(score_zw)
ZW$volgnummer <- row.names(ZW)
ZW$volgnummer <- gsub("x","",ZW$volgnummer)
z<- NULL
y <- merge(SOURCE_ENRICHED, tsne_store,all=T, by='volgnummer')
z <- merge(y,ZW,all=T, by='volgnummer')
rm(y)

#dichotomize clustermembership Kmeans per clusters 
for (cluster in 1:k){
  col = sprintf("kmeans_clus%d", cluster)
  z[col] = as.integer(as.logical(z$cl_kmeans == cluster))
}

#dichotomize vulnerability
z$vulnerable <- as.integer(as.logical(as.integer(z$cl_pam)>0))
z$vulnerable[is.na(z$vulnerable)] <- 0

#clean NA notations
#z %>% replace(., is.na(.), "") %>% stringr::str_replace_all("[0-9]", "")

final_ds_name <- paste0(data.loc,"Xfinal-",dest.nme.var,"-k",k,"-p",perplex)
final_csv <- paste0(final_ds_name,".csv")
final_sav <- paste0(final_ds_name,".sav")

#z <- as.data.frame(z)
head(z,2)
dim(z)

as.numeric(z$volgnummer)
write.csv(z, file=final_csv,row.names=FALSE)
write_sav(z, final_sav)

FINAL_DF <- z

save(FINAL_DF,file=paste0(data.loc,"FINAL_DF",".Rda"))

