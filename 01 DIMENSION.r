#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 1/4
# scope : dimension reduction and clustering
# tecniques: imputation (IM), outlier analysis (OU), dimensional reduction (DR), clustering (CL), 
# approach: unsupervised
# requirements: R Statistics version  (3.60=<)
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders, Ester de Jonge
# DataScienceHub @ JADS, GGD Hart voor Brabant, GGD Zuid-Holland Zuid
# lud 2019-11-05
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#packages
packages <- c("devtools","tools","here","tidyverse","naniar", "haven", "stats", "mice","VIM", "corrplot", "car", "nFactors", "psych", "caret", 
              "Rtsne", "cluster","dbscan", "dendextend", "fpc", "factoextra", "rpart", "rpart.plot", "weights", "RColorBrewer","skimr",  
              "ggplot2", "ggthemes", "qgraph", "gridExtra","randomForest","tidyr","dlookr", "aod", "janitor", "descr", "forcats", "sqldf")
#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE,quietly = TRUE)
#review packages loaded
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
plots.dir <- paste0(root,'/PLOTS/')
if (!dir.exists(plots.dir)) {dir.create(plots.dir)}

plots.loc <- paste0(root,'/PLOTS/',ggd,'/')
if (!dir.exists(plots.loc)) {dir.create(plots.loc)}

#set data location 
data.dir <- paste0(root,'/DATA/')
if (!dir.exists(data.dir)) {dir.create(data.dir)}

data.loc <- paste0(root,'/DATA/',ggd,'/')
if (!dir.exists(data.loc)) {dir.create(data.loc)}

#config
set.seed(123)  # for reproducibility
options(digits=3)

#respondent id = "volgnummer"

#number of clusters (Kmeans, Hierarchical Cluster Analysis) 
#always (re)check the optimal number of clusters
#see DIMENSION.R section II. 'Optimal number of clusters'
#determine the optimal number of clusters first before running the entire script
#run the script to the point (including) section II first
#the GAP plot will indicate the optimal number of clusters
#adjust 'k' accordingly below. 
k <- 10

#perplexity (Tsne)
#In Tsne, the perplexity may be viewed as a knob that sets the number of 
#effective nearest neighbors. It is comparable with the number of nearest neighbors k 
#value between 30 and 50 is usually fine
perplex <- 35

#dimension plots
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
sqldf("select count(distinct(volgnummer)) from SOURCE_RAW")

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

#GEO data was supplied seperately (due to AVG/GDPR)
#if included in the main dataframe please comment the following block
#... and the merge of the GEO dataframe near the end of the procedure
geo.loc <- paste0(data.loc,"GGD-MONITOR-2016-GEO.sav")
GEO <- as.data.frame(read_spss(geo.loc))
GEO$respondent_id<- as.factor(GEO$volgnummer) 
GEO$respondent_id <- paste0("x",GEO$volgnummer)
GEO <- remove_rownames(GEO)
GEO <- column_to_rownames(GEO, var = "respondent_id")
#GGD region
GEO$GGD <- ggd
head(GEO)

#-------------------------------------------------------------------------------
#DATA PREPARATION

#HIGH-ORDER OUTCOME VARIABLES (as proxy for vulnerability) : inclusion criteria
#
#eenzaamheid_dich : (zeer) ernstig eenzaam (dichotoom)
#regie_dich : onvoldoende regie over eigen leven (dichotoom)
#GGADS201_dich: (mid-)Matig of hoog risico op angststoornis of depressie (dichotoom)
#ervarengezondheid_dich ; (zeer) slechte gezondheid (dichotoom)
#score_zw : hoge samenloop (op onderstaande items, 5<) (will be created on-the-fly) 

#FEATURES (20 vars)

#zinvolle dagbesteding - 1 var
#dagactiviteit : !betaald werk, vrijwilligerswerk, student (dichotoom)

#proxy eenzaamheid - 5 vars
#GGEEB201 : !Kan praten over dagelijkse problemen (nog dischotomiseren)
#GGEEB203 : Ervaar leegte (nog dischitomiseren)
#GGEEB204 : !mensen om op terug te vallen bij narigheid (nog dischotomiseren)
#GGEEB207 : !Veel mensen om op te vertrouwen (nog dischitomiseren)
#GGEEB208 : !Voldoende mensen waarmee verbondenheid is (nog dischotomiseren)

#gezondheid en beperkingen - 5 vars 
#CALGA260 : Heeft langdurige ziekte(n) of aandoening(en) (dichotoom)
#CALGA261 : Is (ernstig) beperkt in activiteiten vanwege gezondheid (dichotoom)
#LGBPS209 : Heeft minimaal een beperking met horen, zien of mobiliteit (ofwel minimaal grote moeite met 1 vd 7 OECD items)
#AGGWS205 : Obesitas, ofwel een BMI van 30 of hoger (dichotoom)
#MMIKB201 : moeite met rondkomen (nog dichotomiseren)

#angst en depressie (categorical) - 5 vars
#GGADB201 : Hoe vaak vermoeid zonder duidelijke reden? (nog dischotomiseren)
#GGADB202 : Hoe vaak zenuwachtig? (nog dischotomiseren)
#GGADB204 : Hoe vaak hopeloos? (nog dischotomiseren)
#GGADB207 : Hoe vaak somber of depressief? (nog dischotomiseren)
#GGADB210 : Hoe vaak afkeurenswaardig, minderwaardig of waardeloos? (nog dischotomiseren)

#proxy regie op het leven - 4 vars
#GGRLB201 : Weinig controle over dingen die mij overkomen
#GGRLB202 : Sommige van mijn problemen kan ik met geen mogelijkheid oplossen
#GGRLB204 : Ik voel me vaak hulpeloos bij omgaan problemen van het leven
#GGRLB206 : Wat er in de toekomst met me gebeurt hangt voor grootste deel van mezelf af

#create new recoded variables
#ervarengezondheid_dich 1 '(zeer) slecht' 0 'matig tot (zeer) goed)'
SOURCE_RAW$ervarengezondheid_dich = recode(SOURCE_RAW$KLGGA207, "3=1; 1=0; 2=0; 9=NA")

#regie_dich 1 'onvoldoende regie' 0 'matig of veel regie'
SOURCE_RAW$regie_dich = recode(SOURCE_RAW$GGRLS203, "0=1; 1=0; 9=NA")

#angstdepressie_dich 1 '(zeer) hoog' 0 'niet of nauwelijks'
SOURCE_RAW$angstdepressie_dich <-0
SOURCE_RAW$angstdepressie_dich[SOURCE_RAW$GGADS201>22] <-1
SOURCE_RAW$angstdepressie_dich[SOURCE_RAW$GGADS201==9] <- NA

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
#SOURCE_RAW$inkomenzeerlaag_dich = recode(SOURCE_RAW$inkkwin_2016, "1=1; 2=0; 3=0; 4=0; 5=0; 9=NA")
SOURCE_RAW$inkomenzeerlaag_dich = recode(SOURCE_RAW$KwintielInk, "1=1; 2=0; 3=0; 4=0; 5=0; 9=NA")

#inkomenlaag_dich 1 'laag, max 21.300 euro' 0 'hoger'
#SOURCE_RAW$inkomenlaag_dich = recode(SOURCE_RAW$inkkwin_2016, "1=1; 2=1; 3=0; 4=0; 5=0; 9=NA")
SOURCE_RAW$inkomenlaag_dich = recode(SOURCE_RAW$KwintielInk, "1=1; 2=1; 3=0; 4=0; 5=0; 9=NA")

#leeftijdcat6  'leeftijd in 6 categorieen obv geboortedatum' 
SOURCE_RAW$leeftijdcat6 = recode(SOURCE_RAW$Lftklassen, "1=1; 2:4=2; 5:7=3; 8:9=4; 10:11=5; 12:14=6; 99=NA")

#leeftijd70eo 'leeftijd 70 jaar of ouder'
SOURCE_RAW$leeftijd70eo = recode(SOURCE_RAW$Lftklassen, "11=1; 12=1; 13=1; 14=1; 99=NA; else=0")

#opl_lm opleiding laag midden 1
#SOURCE_RAW$opl_lm = recode(SOURCE_RAW$opl_dichVM, "0=0; 1=1; 9=NA")
SOURCE_RAW$opl_lm = recode(SOURCE_RAW$Opleiding_samind, "1=1; 2=1; 3=0; 4=0; 9=NA")

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

#SOURCE_ENRICHED$respondent_id <- paste0("x",SOURCE_ENRICHED$volgnummer)

#SOURCE_ENRICHED <- remove_rownames(SOURCE_ENRICHED)
#SOURCE_ENRICHED <- column_to_rownames(SOURCE_ENRICHED, var = "respondent_id")

save(SOURCE_ENRICHED,file=paste0(data.loc,"SOURCE_ENRICHED",".Rda"))


#-------------------------------------------------------------------------------
# Subsetting

#features : first four are higher-order outcome dimensions
#if you add features, please do so add the end of the list
#only add dichotomized features (which indicicate experienced issues/vulnerability, vitality or resilience)
cols <- c("GGEES203","GGRLS202","GGADS201","KLGGA207", # <- outcome level
          "MMIKB201_dich","CALGA260","CALGA261","LGBPS209","AGGWS205","GGEEB201_dich","GGEEB203_dich","GGEEB204_dich","GGEEB207_dich",
          "GGEEB208_dich","GGRLB201_dich","GGRLB202_dich","GGRLB204_dich","GGRLB206_dich","GGADB201_dich","GGADB202_dich","GGADB204_dich",
          "GGADB207_dich","GGADB210_dich","dagactiviteit_dich") 
SOURCE_SUBSET <- subset(SOURCE_ENRICHED, select = cols)

#append '_dich' to variable name of the remaining variables not containing '_dich'
colnames(SOURCE_SUBSET) <- sub("^(?!.*_dich)(.*)", "\\1_dich", colnames(SOURCE_SUBSET), perl=TRUE)

#proxy for level of issues: 'hoge samenloop / multi-problematiek' start column range after outcome level indicators
score_zw <- SOURCE_SUBSET %>%
  mutate (score_zw = rowSums(SOURCE_SUBSET[,5:ncol(SOURCE_SUBSET)],na.rm=TRUE))

score_zw <- score_zw[,c("score_zw")]
SOURCE_SUBSET <- cbind(SOURCE_SUBSET,score_zw)


#determine base level of 'samenloop' (score_sw) and 'angst en depressie' (GGADS201_dich) by (natural) binning
#based on population
bin_outcome <- SOURCE_SUBSET %>%
  mutate(zw_bin = binning(SOURCE_SUBSET$score_zw),
         dep_bin = binning(SOURCE_SUBSET$GGADS201_dich)
         )

#regie en samenloop
plot.nme = paste0(ggd,'_explore_regie_samenloop.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)

bp1 <- boxplot(GGRLS202_dich~zw_bin,data=bin_outcome, main="Gebrek aan regie * samenloop",
        xlab="level of samenloop", ylab="gebrek aan regie", staplewex = 1) 
bp1
dev.off()

#samenloop en angst & depressie
plot.nme = paste0(ggd,'_explore_samenloop_angstdepressie.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
bp2 <- boxplot(score_zw~dep_bin,data=bin_outcome, main="Samenloop * angst en depressie",
              xlab="level of angst en depressie", ylab="samenloop", staplewex = 1) 
bp2
dev.off()

#inclusion criteria
#kwetsbare of (dreigende) zorgwekkende gevallen op het vlak van 
#eenzaamheid en / of gebrek regie op het leven, en / of angststoornis/depressie en / of
#ervaren gezondheid is slechts en / of hoge samenloop van problematiek
#adjust threshold gebrek aan regie en samenloop based on binning and boxplot results (see above)

SOURCE_SUBSET <- SOURCE_SUBSET[ which(SOURCE_SUBSET$GGEES203_dich>8 #eenzaamheid
                                      | SOURCE_SUBSET$GGRLS202_dich<20 #regie op het leven 
                                      | SOURCE_SUBSET$GGADS201_dich>22 #angststoornis of depressie
                                      | SOURCE_SUBSET$KLGGA207_dich==3 #ervaren gezondheid 
                                      | SOURCE_SUBSET$score_zw>5) #samenloop
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
ini <- mice(SOURCE_SUBSET,pred=quickpred(SOURCE_SUBSET, mincor=.3),seed=500, print=F)
#predictor matrix
(pred <- ini$pred)

save(pred, file = "prediction_matrix_features.RData")

#pred <- load("prediction_matrix_features.RData")

#final run
imp_data <- mice(SOURCE_SUBSET,method = "logreg", pred=pred,m=5,maxit=10,seed=500, print=T)

summary(imp_data)

#convergence
#it is important that convergence is taking effect towards the end of the iteration process
plot(imp_data)

#do additional iterations lead to more convergence than maxit 10?
imp_ext <- mice.mids(imp_data, maxit=20, print=F)

plot.nme = paste0(ggd,'_convergence_imputation_iterations.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
cplot <- plot(imp_ext)
cplot
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
head(SOURCE_SUBSET)

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
plot.nme = paste0(ggd,'_correlation_features.png')
plot.store <-paste0(plots.loc,plot.nme)
png(filename=plot.store,height = png_height,width = png_height * aspect_ratio)
mdf <- data.matrix(SOURCE_SUBSET) # convert to numeric matrix for correlation calculation 
corplot <- corrplot(cor(mdf), method="color")
corplot
dev.off()


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
plot.nme = paste0(ggd,'_outliers_distribution.png')
plot.store <-paste0(plots.loc,plot.nme)

outlier_dis <- ggplot(md, aes(x = md_bin)) +
  ggtitle(plot.title) +
  labs(x = "outlier bin") +
  geom_bar()
outlier_dis
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio,dpi = dpi)

# Binary outlier variable
# threshold determined by lower boundery of last valid bin, see outlier distribution plot 
threshold_zw <- 41.7 # Threshold ZHZ : 36.3 , HvB : 35.1
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
  geom_bar() +
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
#significance level must reside below of 0.05
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
plot.nme = paste0('tsne_raw_',dest.nme.var,'_k',k,'_p',perplex,'.png')
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
plot.nme = paste0(ggd, 'optimal_clusters_n_',dest.nme.var,'_k',k,'_p',perplex,'.png')
plot.store <-paste0(plots.loc,plot.nme)

# 3 methods
# Elbow method
#fviz_nbclust(tsne_sub, kmeans, method = "wss") +
#  geom_vline(xintercept = 4, linetype = 2)+
#  labs(subtitle = "Elbow method")

# Silhouette method
#fviz_nbclust(tsne_sub, kmeans, method = "silhouette") +
#  labs(subtitle = "Silhouette method")

# GAP method (regular, 10 iterations)
fviz_nbclust(tsne_sub, kmeans, method = "gap_stat")+
  labs(subtitle = "GAP method")

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

#working tsne for kmeans (focus group)
tsne_km <- tsne_foc

## Creating k-means clustering model
fit_cluster_kmeans=kmeans(scale(tsne_km), k,iter.max = 1000,algorithm = c("Forgy"))  

#cluster membership distribution
fit_cluster_kmeans$size	

#cluster membership
#fit_cluster_kmeans$cluster <- factor(fit_cluster_kmeans$cluster)

#add clustermemberschip to Tsne dataframe
tsne_km$cl_kmeans <- as.factor(fit_cluster_kmeans$cluster)

#merge focal group with research group
tsne_km_ext <- merge(tsne_km, tsne_sub, left_index=True, right_index=True,all=TRUE)

#extended version
#tsne_km_ext <- fr[,3:5]
#rm(fr)

tsne_km_ext$cl_kmeans <- fct_explicit_na(tsne_km_ext$cl_kmeans, "0")
head(tsne_km_ext,2)

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

#add clustermemberschip to Tsne dataframe
tsne_store$cl_kmeans <- tsne_km_ext$cl_kmeans 


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
# II.4 TSNE (DR) > DBSCAN (CL) (optional)
#Density-based spatial clustering of applications with noise

#d_tsne_mat <- as.matrix(tsne_sub)

#kNNdistplot(d_tsne_mat, k=4)
#abline(h=0.4, col="red")

#db <- dbscan(d_tsne_mat,eps = .4, MinPts = 4)
#db

#hullplot(d_tsne_mat, db$cluster)
#table(tsne_store$cl_kmeans,db$cluster)

#pairs(tsne_sub, col = db$cluster + 1L)

# Local outlier factor 
#lof <- lof(tsne_sub, k = 4)
#pairs(tsne_sub, cex = lof)

#OPTICS
#opt <- optics(tsne_sub, eps = 1, minPts = 4)
#opt

#opt <- extractDBSCAN(opt, eps_cl = .4)
#plot(opt)

#opt <- extractXi(opt, xi = .05)
#opt
#plot(opt)

#hdb <- hdbscan(tsne_sub, minPts = 4)

#plot(hdb, show_flat = T)

#colors <- mapply(function(col, i) adjustcolor(col, alpha.f = hdb$membership_prob[i]), 
#                 palette()[hdb$cluster+1], seq_along(hdb$cluster))
#plot(d_tsne, col=colors, pch=20)

#tsne_original$cl_hdbscan <-as.factor(hdb$cluster)


#-------------------------------------------------------------------------------
# POST PROCESSING

#mark outliers
tsne_store$outlier <- 0 
tsne_store$outlier[tsne_store$cl_kmeans==0] <-1

#reset cluster 0 (outliers) in cl_kmeans to NA
tsne_store$cl_kmeans[tsne_store$cl_kmeans==0] <- NA

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

#hdbscan
#cs3 = cluster.stats(dist(ANALYSIS_SUBSET),as.numeric(tsne_original$cl_hdbscan))
#cs3[c("within.cluster.ss","avg.silwidth")]

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

