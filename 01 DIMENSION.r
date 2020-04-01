#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 1/4
# scope : dimension reduction and clustering
# tecniques: imputation (IM), outlier analysis (OU), dimensional reduction (DR), clustering (CL)
# methods: mice, TSNE, UMAP, HCA, KMEANS, PAM, DBSCAN
# approach: unsupervised
# requirements: R Statistics version  (3.62=<)
# author : Mark Henry Gremmen (VNG/Data Science Hub), 
# in cooperation with Gemma Smulders (GGD HvB), Ester de Jonge (GGD ZHZ)
# DataScienceHub @ JADS, GGD Hart voor Brabant, GGD Zuid-Holland Zuid
# lud 2020-04-01
#------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#containerized packages
#if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/packrat")
packrat::init()

#CRAN packages
packages <- c("tools","here","tidyverse","naniar", "haven", "stats", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "umap", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot", "weights", "RColorBrewer","skimr", "corrplot",  
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","rgl","dlookr", "aod", "janitor", "descr", "forcats", "sqldf","broom")
#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE,quietly = TRUE)
#review packages loaded (important!)
sessionInfo()

#-------------------------------------------------------------------------------
# Global settings

#root location of this procedure (working directory)
(root <- getwd())

#GGD department (organization)
#ggd <- 'HVB' #Hart voor Brabant
ggd <- 'ZHZ' #Zuid-Holland Zuid
#ggd <- 'UTR' #Utrecht

#project name (in one word)
proj <- 'kwetsbaar'

#location feature-preparation
src.dir <- paste0(root,'/SRC/VARS.R')

#location graphs
plots.dir <- paste0(root,'/PLOTS/')
plots.loc <- paste0(root,'/PLOTS/',ggd,'/')

#location data
data.dir <- paste0(root,'/DATA/')
data.loc <- paste0(root,'/DATA/',ggd,'/')

locations <- c(plots.dir, data.dir, plots.loc, data.loc)

#create locations if not exist
lapply(locations, function(x) {
  if (!dir.exists(x)) {dir.create(x)}
})

#config
set.seed(123)  # for reproducibility
options(digits=15)

#respondent id = "volgnummer"

#number of clusters for partitioning methods (K-means, PAM, etc.)
#always (re)check the optimal number of clusters
#see DIMENSION.R section II. 'Optimal number of clusters'
#determine the optimal number of clusters first before running the entire script
#run the script to the point (including) section II first
#the Silhoutte or GAP plot will indicate the optimal number of clusters
#adjust 'k' accordingly below. 
#please note that there can be good reasons to choose a slightly different number of clusters
#clustering is an explorative technique. There is no single "true" clustering
k <- 9

#dimension and quality plots
graph_height <- 9
png_height <- 600
aspect_ratio <- 2
dpi <- 320 #retina

#qualitative color scheme from ColorBrewer
ncolors <- k+1 #number of clusters plus group of outliers
colors_cust <- brewer.pal(ncolors, "Paired")

#scope
#title- and file name string (combining project name and organization)
dest.nme.var <- paste0(proj, "_",ggd)

#-------------------------------------------------------------------------------
# LOAD DATA 

#-------------------------------------------------------------------------------
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
dim(SOURCE_RAW)
skim(SOURCE_RAW)

#-------------------------------------------------------------------------------
# IDENTIFIER 

#unique identifiers
#sqldf("SELECT count(distinct(volgnummer)) FROM SOURCE_RAW")

#define respondent id
SOURCE_RAW$respondent_id <- as.character(SOURCE_RAW$volgnummer) 

#attach respondent id to index of dataframe
SOURCE_RAW$respondent_id <- paste0("x",SOURCE_RAW$respondent_id)
if (has_rownames(SOURCE_RAW)) {SOURCE_RAW <- remove_rownames(SOURCE_RAW)}

SOURCE_RAW <- column_to_rownames(SOURCE_RAW, var = "respondent_id")

#re-set respondent_id
SOURCE_RAW$respondent_id <- as.factor(SOURCE_RAW$volgnummer) 
head(SOURCE_RAW)

#save source dataframe as Rdata set
save(SOURCE_RAW,file=paste0(data.loc,"SOURCE_RAW",".Rda"))

#-------------------------------------------------------------------------------
#FEATURE PREPARATION

#-------------------------------------------------------------------------------

#HIGH-ORDER 'OUTCOME' VARIABLES (as proxy for vulnerability) : inclusion criteria
#
#GGEES203 : (zeer) ernstig eenzaam (dichotoom)
#GGRLS202 : onvoldoende regie over eigen leven (dichotoom)
#GGADS201 : (mid-)Matig of hoog risico op angststoornis of depressie (dichotoom)
#KLGGA207 ; (zeer) slechte gezondheid (dichotoom)
#score_zw : hoge samenloop (op onderstaande items, 4<) (will be created on-the-fly) 


#FEATURES 

#zinvolle dagbesteding / betekenisvol sociaal netwerk 
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

#feature preparation (see directory SRC > VARS.R)
source(src.dir)

SOURCE_ENRICHED <- SOURCE_RAW
rm(SOURCE_RAW)

save(SOURCE_ENRICHED,file=paste0(data.loc,"SOURCE_ENRICHED",".Rda"))


#-------------------------------------------------------------------------------
# Filtering

#no filtering (default)
SOURCE_SUBSET <- SOURCE_ENRICHED

#filter by year, municipalities, age etc.
# SOURCE_SUBSET <- SOURCE_SUBSET %>% 
# meting (jaar)
#  filter(AGOJB201==2016) %>% 
# leeftijd
#  filter(LFT0109>64) 
# gemeenten
#   %>% filter(Gemeentecode %in% c("...", "...")) 


#-------------------------------------------------------------------------------
# Subsetting

#first line contains higher-order 'outcome' dimensions
#additional lines contain the features. if you add features, please do so add the end of the list
#only add dichotomized features (which indicicate experienced issues/vulnerability, vitality or resilience)

cols <- c("GGEES203","GGRLS202","GGADS201","KLGGA207", # <- outcome level
          "werkopleiding_dich", "vrijwilligerswerk_dich", "mantelzorg_dich", # <- zinvolle dagbesteding
          # perception in outcome features -> 
          #eenzaamheid
          "GGEEB201_dich","GGEEB203_dich","GGEEB204_dich","GGEEB207_dich","GGEEB208_dich",
          #"GGEEB210", "GGEEB211" # in de steek gelaten, kan bij vrienden terecht
          #gezondheid en beperkingen
          "CALGA260","CALGA261","LGBPS209","MMIKB201_dich",
          #angst en depressie
          "GGRLB201_dich","GGRLB202_dich","GGRLB204_dich","GGADB207_dich","GGADB210_dich",
          #regie op het leven
          "GGADB201_dich","GGADB202_dich","GGADB204_dich","GGRLB206_dich", "MCMZOS304" 
          #lifestyle/gedrag
          #,"LFALA213", "LFRKA205", "AGGWS205"
          )
          
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = cols)

#append '_dich' to variable name of the remaining variables not already recoded and -containing '_dich' to prevent contamination
colnames(SOURCE_SUBSET) <- sub("^(?!.*_dich)(.*)", "\\1_dich", colnames(SOURCE_SUBSET), perl=TRUE)

#proxy for level of issues: 'hoge samenloop / multi-problematiek' start column range after outcome level and zinvolle dagbesteding indicators
score_zw <- SOURCE_SUBSET %>%
  mutate (score_zw = rowSums(SOURCE_SUBSET[,8:ncol(SOURCE_SUBSET)],na.rm=TRUE))

score_zw <- score_zw[,c("score_zw")]
SOURCE_SUBSET <- cbind(SOURCE_SUBSET,score_zw)

#determine critical level of 'samenloop' (score_sw) and 'angst en depressie' (GGADS201_dich) by (natural) binning
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
 #geom_jitter(aes(color='observations'),alpha=0.02) +
  theme_minimal() +
  xlab("level of samenloop") +
  ylab("gebrek aan regie") +
  guides(fill=guide_legend(title="Samenloop")) +
  ggtitle(plot.title)
bp1
plot.nme = paste0(ggd,'_explore_regie_samenloop.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)

#samenloop en regie
plot.title = paste0('Samenloop * gebrek aan regie')
bp2 <- ggplot(bin_outcome, aes(x = reg_bin, y = score_zw, fill=reg_bin)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) +
  #geom_jitter(aes(color='observations'),alpha=0.02) +
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
  #geom_jitter(aes(color='observations'),alpha=0.05) +
  theme_minimal() + 
  ylab("level of samenloop") +
  xlab("level of angst en depressie") +
  guides(fill=guide_legend(title="Level of A&D")) +
  ggtitle(plot.title)
bp3
plot.nme = paste0(ggd,'_explore_samenloop_angstendepressie.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)


#-------------------------------------------------------------------------------
#inclusion criteria

#kwetsbare of (dreigende) zorgwekkende gevallen op het vlak van 
#eenzaamheid en / of gebrek regie op het leven, en / of angststoornis/depressie en / of
#ervaren gezondheid en / of hoge samenloop van problematiek
#adjust threshold level 'gebrek aan regie' en 'samenloop' based on binning and boxplot results (see above)

SOURCE_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$GGEES203_dich>8 #eenzaamheid 
                                      | SOURCE_SUBSET$GGRLS202_dich<23 #regie op het leven 
                                      | SOURCE_SUBSET$GGADS201_dich>20 #angststoornis of depressie
                                      | SOURCE_SUBSET$KLGGA207_dich==3 #ervaren gezondheid 
                                      | SOURCE_SUBSET$score_zw>4) #samenloop
                                , ]

#remove 'outcome' variables, keep relevant features for the fingerprint
SOURCE_SUBSET <- subset(SOURCE_SUBSET, select = -c(GGEES203_dich,GGRLS202_dich,GGADS201_dich,KLGGA207_dich,score_zw))

#number of dichotomous features 
features_n <- ncol(SOURCE_SUBSET)

#TAG : attach number of features to plot name and -title
dest.nme.var <- paste0(dest.nme.var,"_f",features_n)

#draw sample (for evaluation purposes) or limitations computing set-up 
#sample_size <- 100000
#SOURCE_SUBSET <- SOURCE_SUBSET[sample(nrow(SOURCE_SUBSET), sample_size), 1:features_n]

head(SOURCE_SUBSET)
dim(SOURCE_SUBSET)


#-------------------------------------------------------------------------------
#keep track of respondent id within subject group

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
#md.pattern(SOURCE_SUBSET,plot = T)

#remove cases with missing values (don't do this unless there are very good reasons for it)
#SOURCE_SUBSET <- na.omit(SOURCE_SUBSET)

#missing data imputation
#method : Multivariate Imputation via Chained Equations (MICE), logreg (Logistic Regression) for binary features
#More info : https://stefvanbuuren.name/mice/

#see methods available
#methods(mice)

#fluxplot
#Featuress with higher outflux are (potentially) the more powerful.
plot.nme = paste0(ggd,'_fluxplot_pattern.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
fplot <- fluxplot(SOURCE_SUBSET)
fplot
dev.off()

#initial run to auto-determine powerful predictors for imputation, tweak correlation value accordingly
ini <- mice(SOURCE_SUBSET,pred=quickpred(SOURCE_SUBSET, mincor=.35),seed=500, print=F)
#predictor matrix
(pred <- ini$pred)
#save prediction matrix
save(pred, file = "prediction_matrix_features.RData")

#or load a predefined prediction matrix
#pred <- load("prediction_matrix_features.RData")

#first run (low number of iterations)
#method: logistic regression for binary features 
imp_data <- mice(SOURCE_SUBSET,method = "logreg", pred=pred,m=5,maxit=10,seed=500, print=T)
summary(imp_data)

#convergence
#The mice function implements an iterative Markov Chain Monte Carlo type of algorithm.
#it is important that convergence takes effect towards the end of the iteration process
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
#high correlation is not desirable (inflated attribution)
#plot.nme = paste0(ggd,'_correlation_features.png')
#plot.store <-paste0(plots.loc,plot.nme)
#png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
#mdf <- data.matrix(SOURCE_SUBSET) # convert to numeric matrix for correlation calculation 
#corplot <- corrplot(cor(mdf), method="color",type="upper")
#(corplot)
#dev.off()


#-------------------------------------------------------------------------------
# Outliers

#Why are we interested in outliers? (Extreme) Outliers ('unique' situations) tend 
#to distort the detection of clear (natural) formations of similar situations.
#some clustering techniques are more sensitive to extreme outliers than others
#comparing sensitive and non-sensitive methods provides additional insight

#Outlier detection for high-dimensional data (Mahalanobis)
#Mahalanobis squared distances using the (robust) minimum covariance determinant (MCD)
mah_df <- SOURCE_SUBSET[,-which(names(SOURCE_SUBSET) %in% c("vulnerable_suspect"))] #list columns which are not features
m_dist <- mahalanobis(mah_df, colMeans(mah_df), cov(mah_df),method="mcd")
SOURCE_SUBSET$MD <- round(m_dist, 1)

#density plot Mahalanobis
plot.title = paste0('Mahalanobis squared distance (outlier detection)')
mplot <- ggplot(data = SOURCE_SUBSET, mapping = aes(x=MD)) + 
  geom_histogram(aes(y=..density..),fill="darkblue",color="white",alpha=0.4) + 
  geom_density() +
  geom_rug() +
  ggtitle(plot.title) +
  labs(x='Mahalanobis distance ') +
  theme_minimal()
(mplot)
plot.nme = paste0(ggd,'_mahalanobis_distance.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

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
plot.nme = paste0(ggd,'_mahalanobis_outliers_distribution.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

# Binary outlier qualification
# threshold determined by lower boundary of last valid bin, see outlier distribution plot 
threshold_zw <- 49.3 # Threshold UTR: 51.5, ZHZ : 54.2 , HvB : 35.1 (higher value means more outliers in scope)
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
  geom_text(aes(label=..count..),stat="count",position=position_dodge(width=0.9), vjust=-0.25)
outlier_dich
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

save(SOURCE_SUBSET,file=paste0(data.loc,"SOURCE_SUBSET_INC_OUTLIERS",".Rda"))

#store outlier qualifier for later use (after dimension reduction, before clustering)
outlier_vec <- (SOURCE_SUBSET$outlier=="Yes")
outlier_df <- as.data.frame(outlier_vec)

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

#Bartlettas test of sphericity (test for homogeneity of variances) 
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
#TSNE is a non-linear technique for dimensionality reduction 
#well-suited for the visualization of high-dimensional data

#TSNE preserves local structure but not always global structure nor density structure. 
#Doing density- (or distance-)based clustering after t-SNE is not advisable

#reduce features to two dimensions with TSNE
#hyper - paramaters :
#perplexity : local/global structure trade-off
#more info: https://distill.pub/2016/misread-tsne/
perplex <- 40
#pca : dimensional reduction as initial step
# set 'pca' and 'pca_scale' to TRUE to speed-up (only when needed) 
#theta : speed/accuracy trade-off (decrease for more accuracy), default 0.5
th <- 0.3

#TSNE model with 2 dimensions
tsne_model = Rtsne(ANALYSIS_SUBSET, check_duplicates=FALSE, pca=FALSE, perplexity=perplex, theta=th, dims=2, max_iter=1000)

#re-attach row id
tsne = cbind.data.frame(rownames(ANALYSIS_SUBSET),tsne_model$Y)
colnames(tsne)[0] <- paste(colnames(ANALYSIS_SUBSET)[0])
if (has_rownames(tsne)) {remove_rownames(tsne)}
tsne <- column_to_rownames(tsne, var = "rownames(ANALYSIS_SUBSET)")

colnames(tsne)[1] <- "V1"
colnames(tsne)[2] <- "V2"
head(tsne,2)

#plot entire subject group in a two-dimensional space
plot.title = paste0('TSNE cloud ',dest.nme.var)
plot.subtitle = paste0('Hyperparameters: perplexity=',perplex, ', theta=',th)

ggplot(tsne, aes(x=V1, y=V2)) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=15))) +
  xlab("") + ylab("") +
  ggtitle(label=plot.title,subtitle=plot.subtitle) +
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
## registration of subject group for future cluster membership registration
tsne_store <- tsne

#Preparation for Kmeans clustering 
#TSNE of focal group (without extreme outliers / noise)
#exclude extreme outliers from clustering in case of Kmeans
#Kmeans is sensitive to (strong) outliers

tsne_core <- cbind(tsne_sub,outlier_df)
#dataframe with just coordinates
tsne_foc <- tsne_core[outlier_vec==FALSE,]
tsne_foc <- subset(tsne_foc, select=c(V1,V2))
rm(tsne_core)

#recap: tsne_sub is the research population (with outliers), tsne_foc is
#the focal group without the outliers (for Kmeans)


#-------------------------------------------------------------------------------
# I.2 Uniform Manifold Approximation and Projection (UMAP)
#UMAP is also a non linear dimensionality reduction algorithm in the same family as TSNE.
#UMAP’s output results in more compact, separated clusters compared to TSNE.
#UMAP claims to preserve both local and most of the global structure in the data
#UMAP is therefore better than TSNE to compare the relative position of clusters

umap_model <- umap::umap(ANALYSIS_SUBSET)

umap_model
head(umap_model$layout, 3)

umap <-as.data.frame(umap_model$layout)
colnames(umap)[1] <- "V1"
colnames(umap)[2] <- "V2"
head(umap,2)
#umap <- as.data.frame(umap)

plot.title = paste0('UMAP cloud ',dest.nme.var)
ggplot(umap, aes(x=V1, y=V2)) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=15))) +
  xlab("") + ylab("") +
  ggtitle(plot.title) +
  theme_light(base_size=20) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  theme_void() +
  scale_colour_brewer(palette = "Set2")
plot.nme = paste0('umap_raw_',dest.nme.var,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

## UMAP data subject group
umap_sub <- umap
## registration of subject group for future cluster membership registration
umap_store <- umap

#Preparation for Kmeans clustering 
#UMAP of focal group (without extreme outliers)
#exclude extreme outliers from clustering in case of Kmeans
#Kmeans is sensitive to (strong) outliers

umap_core <- cbind(umap_sub,outlier_df)
umap_foc <- umap_core[outlier_vec==FALSE,]
#dataframe with just coordinates
umap_foc <- subset(umap_foc, select=c(V1,V2))
rm(umap_core)


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

#very important to determine the optimal number of clusters in order to justify 
#the number of clusters chosen!
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

#ALERT!!!
#reset k?


#-------------------------------------------------------------------------------
# II.1.1 TSNE (DR) > Kmeans (CL)

#Kmeans is a partitioning method that assigns observations to k clusters in which each
#observation belongs to the cluster with the nearest mean. The mean is (often) not a real observation.

#Please note that the algoritm fails when data is not spherical (check the raw cloud first)

#As kmeans is prone to outliers we use the focal group here (cases excluding extreme outliers). 
#we cluster the focal group cases and plot the outliers as cases with qualidier '0'

#the trick is to accommodate as much outliers as possible untill the point that it gets too
#difficult for the Kmeans algorithm to converge: adjust outlier threshold accordingly !!
#the group of extreme outliers that are very difficult to associate with a K-means cluster may need 
#special attention in policy and intervention efforts.
#especially when outliers are near (or 'in') multi-problem or highly vulnerable clusters 

#working version of TSNE for kmeans (focus group)
tsne_km <- tsne_foc

## Creating k-means clustering model
fit_tsne_kmeans=kmeans(scale(tsne_km), k,iter.max = 1000,algorithm = c("Forgy"))  

#summary of model-level statistics
glance(fit_tsne_kmeans) %>% glimpse()

#cluster membership distribution
fit_tsne_kmeans$size	

#add clustermemberschip to TSNE (focal group) dataframe
tsne_km$tsne_cl_kmeans <- as.factor(fit_tsne_kmeans$cluster)

#tsne_sub$repondent_id <- row.names(tsne_sub) 
#merge focal group with research group
tsne_km_ext <- merge(tsne_sub, tsne_km, left_index=True, right_index=True,all=TRUE)

tsne_km_ext$tsne_cl_kmeans <- fct_explicit_na(tsne_km_ext$tsne_cl_kmeans, "0")
head(tsne_km_ext,2)

#dichotomize outliers
tsne_km_ext$outlier <-0
tsne_km_ext$outlier[tsne_km_ext$tsne_cl_kmeans==0] <-1
tsne_km_ext$outlier<-as.factor(tsne_km_ext$outlier)

## plotting the results with Kmeans clustering
plot.title = paste0('TSNE > Kmeans of ',dest.nme.var)
plot.subtitle = paste0('Hyperparameters: k=',k,', perplexity=',perplex, ', theta=',th)

tsne_plot <- ggplot(tsne_km_ext, aes(V1, V2, color = tsne_cl_kmeans, shape=outlier)) +
  geom_point() +
  ggtitle(label=plot.title, subtitle=plot.subtitle) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  #geom_text(aes(label=row.names(X)),color="#ababab") +
  scale_colour_manual(name = "cluster",values=colors_cust) + 
  geom_text(aes(label = ""), size = 3, vjust = 1, color = "black")

tsne_plot + scale_shape_discrete(name = "outlier",   breaks=c("0", "1"), labels = c("no", "yes")) 
  
plot.nme = paste0('tsne_kmeans_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

tsne_km_cl <- merge(tsne_store, tsne_km, by=0, all.x=T, keep=F)
row.names(tsne_km_cl) <- tsne_km_cl[,1]

#add clustermemberschip to TSNE dataframe
tsne_store$tsne_cl_kmeans <- tsne_km_cl$tsne_cl_kmeans 
rm(tsne_km_cl)

#-------------------------------------------------------------------------------
# II.1.2 TSNE (DR) > K-medoids clustering  / partition around mediods PAM (CL)

#PAM is a partitioning method where mediods -a real observation- have the lowest dissimilarities with
#all other observations in a cluster

#PAM is less prone to outliers, therefore we use the entire subject group (incl extreme outliers)

pam_res <- pam(tsne_sub, k)

plot.nme = paste0('tsne_pam_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)
title = paste0('TSNE > K-mediods (PAM) of ',dest.nme.var, ' k=',k)
fviz_cluster(pam_res, geom = "point", ellipse.type = "convex") +
  labs(title = title)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

#add clustermemberschip to TSNE subject group dataframe
tsne_store$tsne_cl_pam <- as.factor(pam_res$clustering)

#-------------------------------------------------------------------------------
# II.1.3 TSNE (DR) > hierarchical clustering (CL)

#Hierarchical clustering forms a tree-type structure based on the hierarchy 

#Euclidean distance and Ward's minimum variance method to perform agglomerative clustering

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

# DIvisive ANAlysis Clustering (DIANA)
#diana(gower_dist, metric = "euclidean", stand = FALSE)

#add clustermemberschip to Tsne dataframe
tsne_store$tsne_cl_hierarchical = as.factor(factor(cutree(fit_hca_tsne, k=k))) 
#as.data.frame(tsne_original)

#done with all our clustering methods on TSNE!
head(tsne_store,2)

#Doing density- (or distance-)based clustering after t-SNE is not advisable

#-------------------------------------------------------------------------------
# II.2.1 UMAP (DR) > kmeans (CL)

#UMAP cloud as the basis for clustering methods
#NB: as kmeans is prone to outliers we use the focal group here (cases excluding extreme outliers). 
#we cluster the focal group cases and plot the outliers as cases with qualidier '0'

#working UMAP for kmeans (focus group)
umap_km <- umap_foc

## Creating k-means clustering model
fit_umap_kmeans=kmeans(scale(umap_km), k,iter.max = 1000,algorithm = c("Forgy"))  

#summary of model-level statistics
glance(fit_umap_kmeans) %>% glimpse()

#cluster membership distribution
fit_umap_kmeans$size	

#add clustermemberschip to UMAP (focal) dataframe
umap_km$umap_cl_kmeans <- as.factor(fit_umap_kmeans$cluster)

#merge focal group with research group
umap_km_ext <- merge(umap_sub, umap_km, left_index=True, right_index=True,all=TRUE)


umap_km_ext$umap_cl_kmeans <- fct_explicit_na(umap_km_ext$umap_cl_kmeans, "0")
head(umap_km_ext,2)

umap_km_ext$outlier <-0
umap_km_ext$outlier[umap_km_ext$umap_cl_kmeans==0] <-1
umap_km_ext$outlier<-as.factor(umap_km_ext$outlier)


## plotting the results with Kmeans clustering
plot.title = paste0('UMAP > Kmeans of ',dest.nme.var, ' k=',k)
umap_plot <- ggplot(umap_km_ext, aes(V1, V2, color = umap_cl_kmeans, shape=outlier)) +
  geom_point() + 
  ggtitle(plot.title) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  #geom_text(aes(label=row.names(X)),color="#ababab") +
  scale_colour_manual(name = "cluster",values=colors_cust) + 
  geom_text(aes(label = ""), size = 3, vjust = 1, color = "black")

umap_plot + scale_shape_discrete(name = "outlier",   breaks=c("0", "1"), labels = c("no", "yes"))

plot.nme = paste0('umap_kmeans_',dest.nme.var,'_k.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)


umap_km_cl <- merge(umap_store, umap_km, by=0, all.x=T, keep=F)
row.names(umap_km_cl) <- umap_km_cl[,1]

#add clustermemberschip to UMAP dataframe
umap_store$umap_cl_kmeans <- as.factor(umap_km_cl$umap_cl_kmeans) 




#-------------------------------------------------------------------------------
# II.2.2 UMAP (DR) > DBSCAN (CL)

#Density-based spatial clustering of applications with noise (DBSCAN)
#Clusters in this method have a higher density than the remainder of the dataset. 
#DBSCAN is better in handling nonconvex clusters and noise

#Number of clusters and outliers are determined by DBSCAN 

umap_sub_mat <- as.matrix(umap_sub)

# determine optimal e(ps) value 
kNNdistplot(umap_sub_mat, k=k)
abline(h=0.4, col="red")

#hyper-parameters: eps defines the neighborhood around an observation. MinPts the number of neighbors
db <- dbscan(umap_sub_mat,eps = .4, MinPts = 4)
db

#hullplot
#black points are outliers (also located within polygon)
hullplot(umap_sub_mat, db$cluster)

#db$cluster
table(db$cluster)

umap_sub$umap_cl_dbscan <- as.factor(db$cluster) 


#set color scheme for large number of clusters
dbclus_cnt = length(unique(umap_sub$umap_cl_dbscan))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))



#dichotomize outliers
umap_sub$outlier <-0
umap_sub$outlier[umap_sub$umap_cl_dbscan==0] <-1
umap_sub$outlier<-as.factor(umap_sub$outlier)

## plotting the results with DBSCAN clustering
#group 0 (<>cluster!) are outliers (as determined by DBSCAN)
plot.title = paste0('UMAP > DBSCAN of ',dest.nme.var,' k=',dbclus_cnt)
umap_plot <- ggplot(umap_sub, aes(V1, V2, color =umap_cl_dbscan,shape=outlier)) +
  geom_point() + 
  ggtitle(plot.title) +
  theme_minimal() +
  xlab('') +
  ylab('') +
  #geom_text(aes(label=row.names(X)),color="#ababab") +
  #scale_colour_manual(name = "cluster",values=colors_cust) + 
  scale_colour_manual(name = "cluster",values = getPalette(dbclus_cnt)) +
  geom_text(aes(label = ""), size = 3, vjust = 1, color = "black")

umap_plot + scale_shape_discrete(name = "outlier",   breaks=c("0", "1"), labels = c("no", "yes")) 
plot.nme = paste0('umap_dbscan_',dest.nme.var,'_k.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

#add clustermemberschip to UMAP dataframe
umap_store$umap_cl_dbscan <- as.factor(db$cluster) 
umap_store$umap_outlier <- umap_sub$outlier



#-------------------------------------------------------------------------------
# POST PROCESSING

#TSNE dataframe
#reattach index to dataframe
tsne_store$respondent_id <- as.factor(row.names(tsne_store)) 

names(tsne_store)[names(tsne_store) == "V1"] <- "tsne_coord_x1"
names(tsne_store)[names(tsne_store) == "V2"] <- "tsne_coord_x2"
head(tsne_store)


#UMAP dataframe
#reattach index to dataframe
umap_store$respondent_id <- as.factor(row.names(umap_store)) 

names(umap_store)[names(umap_store) == "V1"] <- "umap_coord_x1"
names(umap_store)[names(umap_store) == "V2"] <- "umap_coord_x2"
head(umap_store)


final_store <- merge(tsne_store,umap_store,by="respondent_id")
final_store$volgnummer <- gsub("x","",as.character(final_store$respondent_id))

#mark outliers
final_store$outlier <- 0 
final_store$outlier[is.na(final_store$tsne_cl_kmeans)] <-1



#-------------------------------------------------------------------------------
# Compare cluster methods (for TSNE and UMAP)

#-------------------------------------------------------------------------------

#within.cluster.ss measurement shows how closely related observations are in 
#clusters; the smaller the value, the more closely related are observations within the cluster

#avg.silwidth is a measurement that considers how closely related objects 
#are within the cluster and how clusters are separated from each other.

#prepare focal group for Kmeans measurement
ANALYSIS_SUBSET_km <- cbind(ANALYSIS_SUBSET,outlier_df)
ANALYSIS_SUBSET_km <- ANALYSIS_SUBSET_km[outlier_vec==FALSE,]

#Kmeans on TSNE (focal group)
#Please note that we enhanced consistency of the clusters by removing the extreme outliers
cs1_1 = cluster.stats(dist(ANALYSIS_SUBSET_km),as.numeric(tsne_km$tsne_cl_kmeans))
(silwidth_kmeans_tsne <- cs1_1[c("within.cluster.ss","avg.silwidth")])

#PAM on TSNE (subject group)
cs2 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_store$tsne_cl_pam))
(silwidth_pam_tsne <- cs2[c("within.cluster.ss","avg.silwidth")])

#HCA on TSNE (subject group)
cs3 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_store$tsne_cl_hierarchical))
(silwidth_hca_tsne <- cs3[c("within.cluster.ss","avg.silwidth")])

#Kmeans on UMAP (focal group)
#Please note that we enhanced consistency of the clusters by removing the extreme outliers
cs1_2 = cluster.stats(dist(ANALYSIS_SUBSET_km),as.numeric(umap_km$umap_cl_kmeans))
(silwidth_kmeans_umap <- cs1_2[c("within.cluster.ss","avg.silwidth")])

#DBSCAN on UMAP (subject group)
#please note that cluster '0' (outliers as defined by dbscan) is not a consistent in the sense
# of the word cluster (it is just a group). Therefore the silwidth tends to be a bit higher
cs4 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(umap_store$umap_cl_dbscan))
(silwidth_dbscan_umap <- cs4[c("within.cluster.ss","avg.silwidth")])

(silwidth <- as.data.frame(rbind(silwidth_kmeans_tsne,silwidth_pam_tsne,silwidth_hca_tsne,silwidth_kmeans_umap,silwidth_dbscan_umap)))

silwidth$within.cluster.ss <- as.numeric(silwidth$within.cluster.ss) 
silwidth$methods <- row.names(silwidth)
silwidth$methods <- as.factor(silwidth$methods)  

#Plot consistency of observations within cluster 
plot.title = paste0('Measurement of consistency observations within cluster')
plot.nme = paste0(ggd,'_within_cluster_ss.png')
plot.store <-paste0(plots.loc,plot.nme)
cluster_ss <- ggplot(silwidth, aes(x=methods, y = within.cluster.ss)) +
  ggtitle(plot.title) +
  labs(x = "Clustering method", y = "Within.cluster.ss (lower is better)") +
  geom_boxplot()
cluster_ss
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

#evaluation of the similarity between two clusterings
#cl_kmeans versus cl_pam
#rep <- 10000
#FM_index_H0 <- replicate(rep, FM_index_permutation(tsne_store$tsne_cl_pam, tsne_store$tsne_cl_kmeans)) 
#plot(density(FM_index_H0), main = "FM Index distribution under H0\n (10000 permutation), PAM vs Kmeans")
#abline(v = mean(FM_index_H0), col = 1, lty = 2)

#skew(FM_index_H0) 
#kurtosi(FM_index_H0)


#-------------------------------------------------------------------------------
# Writing cluster membership to csv

cluster_membership_name <- paste0(data.loc,"cluster-membership-",dest.nme.var,"-k",k,"-p",perplex,".csv")
write.csv(final_store, file=cluster_membership_name,row.names=TRUE)

#-------------------------------------------------------------------------------
# merging and writing all data
#do remove GEO dataframe if GEO / location parameters are included in the main dataset
#total population

ZW <- data.frame(score_zw)
ZW$volgnummer <- row.names(ZW)
ZW$volgnummer <- gsub("x","",ZW$volgnummer)
z<- NULL
y <- merge(SOURCE_ENRICHED, final_store,all=T, by='volgnummer')
z <- merge(y,ZW,all=T, by='volgnummer')
rm(y)

#dichotomize clustermembership Kmeans (resp. TSNE,UMAP) per clusters into new variables
for (cluster in 1:k){
  tsne_km_var = sprintf("tsne_kmeans_clus%d", cluster)
  z[tsne_km_var] = as.integer(as.logical(z$tsne_cl_kmeans == cluster))
  
  uamp_km_var = sprintf("umap_kmeans_clus%d", cluster)
  z[uamp_km_var] = as.integer(as.logical(z$umap_cl_kmeans == cluster))
}

#dichotomize vulnerability
z$vulnerable <- as.integer(as.logical(as.integer(z$tsne_cl_pam)>0))
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


#-------------------------------------------------------------------------------
#EXTRA: for educational purposes
#TSNE model with 3 dimensions (to show multi-dimensional concept in Shiny) icw Kmeans clustering
#tsne_model_3d = Rtsne(ANALYSIS_SUBSET, check_duplicates=FALSE, pca=FALSE, perplexity=perplex, theta=th, dims=3, max_iter=1000)
#plot3d(tsne_model_3d$Y)
#tsne_3d_shiny = as.data.frame(tsne_model_3d$Y)

#fit_tsne_3d_kmeans=kmeans(scale(tsne_3d_shiny), k,iter.max = 1000,algorithm = c("Forgy"))  
#tsne_3d_shiny$cluster<- as.factor(fit_tsne_3d_kmeans$cluster)
#saveRDS(tsne_3d_shiny, file = "tsne_3d_shiny.rds")