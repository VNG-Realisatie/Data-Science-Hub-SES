#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 4/4
# Predict cluster membership
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders
# DataScienceHub @ JADS, GGD Hart voor Brabant
# lud 2019-07-12
#-------------------------------------------------------------------------------

#packages
packages <- c("tools","here","tidyverse","caTools", "psych", "mice",
              "ggplot2","ggfortify","tidyr", "nnet", "caret","randomForest")
#install packages which are not available
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE)
#review
sessionInfo()


#-------------------------------------------------------------------------------
# Global settings (change if/where needed)

#root location of this procedure (working directory)
root <- getwd()
root

#options
set.seed(123)  # for reproducibility
options(digits=3)

#set graphs location (create directory first)
plots.loc <- paste0(root,'/PLOTS/')

#set data location (create directory first)
data.loc <- paste0(root,'/DATA/')

#set library location (create directory first)
lib.loc <- paste0(root,'/LIB/')

#source file name 
source.file <- paste0(data.loc,'Xfinal-zorgwekkend_df20-k8-p40.csv')

SOURCE <- read.csv(source.file, T, ",")

str(SOURCE)

#original variables (not rcoded) from the Gezondheidsmonitor
cols <- c("GGADS201","KLGGB201",
          "MMIKB201_dich","CALGA260","CALGA261","LGBPS205","GGEEB201","GGEEB203","GGEEB204","GGEEB207",
          "GGEEB208","GGRLB201","GGRLB202","GGRLB204","GGRLB206","GGADB201","GGADB202","GGADB204",
          "GGADB207","GGADB210") 
SOURCE_SUBSET <- subset(SOURCE, select = cols)
dim(SOURCE_SUBSET)

#stats on missing values (pre-imputation)
sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))
#remove cases with missing values
#SOURCE_SUBSET <- na.omit(SOURCE_SUBSET)

#missing data imputation
#method : Multivariate Imputation via Chained Equations, random forest (rf) method
imp_data <- mice(SOURCE_SUBSET,m=5,maxit=50,meth='rf',seed=500)
summary(imp_data)
#apply to SOURCE_SUBSET
SOURCE_SUBSET <- complete(imp_data,1)

sapply(SOURCE_SUBSET, function(x) sum(is.na(x)))

#use only complete records with cl_kmeans membership for modelling
data <- na.omit(SOURCE_SUBSET)

#load model
model_ses <- readRDS("model_rf_ses.RDS")

# prediction with the randomForest 


pred.testR <- predict(model_ses, newdata = data)
confusionMatrix(pred.testR, data$NSP)



#or trace-back to best-fit plot position by similarity (gower)
