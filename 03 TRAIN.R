#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 3/4
# scope : Train classification model
# techniques : Classification
# methods : PrincipalComponentAnalysis, RandomForest, SupportVectorMachine
# requirements: R Statistics version  (3.62=<)
# author : Mark Henry Gremmen (VNG DataScienceHub), in cooperation with Gemma Smulders (GGD HvB), Ester de Jonge (GGD ZHZ)
# VNG DataScienceHub, GGD Hart voor Brabant, GGD Zuid-Holland Zuid
# lud 2020-03-30
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

packrat::init()

#CRAN packages
packages <- c("tools","here","tidyverse","caTools", "psych", "RColorBrewer", "e1071", "xgboost",
              "ggplot2","ggfortify","tidyr", "caret","randomForest", "gbm", "car",
              "stringr")
#nnet causes problems with R 3.6.3

#install packages which are not available on the computing setup
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

#source file name (csv-format)
#make sure the filename corresponds with the output from procedure '01 DIMENSION'
source.file <- paste0(data.loc,'Xfinal-kwetsbaar_ZHZ_df22-k9-p35.csv')

SOURCE <- read.csv(source.file, T, ",")

str(SOURCE)

#set cluster membership of non-vulnerbale observations to zero 
SOURCE$tsne_cl_pam[is.na(SOURCE$tsne_cl_pam)] <- 0

head(SOURCE)

#original variables (not recoded, different likert scales) from the GGD Gezondheidsmonitor incl cluster membership cl_pam. 
#cluster membership must reside at the last position if the list 

cols <- c("GGEES203","GGRLS202","GGADS201","KLGGA207", # <- outcome level
          "MMIKB201","CALGA260","CALGA261","LGBPS209","AGGWS205","GGEEB201","GGEEB203","GGEEB204","GGEEB207",
          "GGEEB208","GGRLB201","GGRLB202","GGRLB204","GGRLB206","GGADB201","GGADB202","GGADB204",
          "GGADB207","GGADB210","GGEEB210","GGEEB211", "MCMZOS304","tsne_cl_pam") 

SOURCE_SUBSET <- subset(SOURCE, select = cols)

#number of features
df_tr <- ncol(SOURCE_SUBSET)
df_tr

#use only complete records for building the model
data <- na.omit(SOURCE_SUBSET)

# spliting the data into train and test
#SplitRatio : 60%:40% 
inTrain= sample.split(data,SplitRatio = 0.6)

#subsetting into train data
train =subset(data,inTrain==TRUE)

#subsetting into test data
test =subset(data,inTrain==FALSE)


#-------------------------------------------------------------------------------
# DIMENSION REDUCTION

#-------------------------------------------------------------------------------

#latent dimensions from PCA are used to train a classification model (to reduce noise)

# important set scale to TRUE 
pca = prcomp(train[,-df_tr], scale=TRUE, center=TRUE)

#biplot
biplot(pca, cex=.7)
	
#screeplot	
screeplot(pca, type="line", main="Scree plot")
abline(h=1)

# checking attributes available PCA
attributes(pca)
pca$center

# analysing the PCA model
print(pca)
summary(pca)

#autoplot(prcomp(train), loadings= T,
#         loadings.colour = "blue", loadings.label = T, loadings.label.size=3,
#         frame= T)

# predicting training data with pca
pca.train <- predict(pca, train)
pca.train <- as.data.frame(pca.train)

# adding our target variable which was not included during the PCA training
# NSP is cluster membership
pca.train <- cbind(pca.train, NSP= train[,"tsne_cl_pam"])

#table(observed=, predicted=)

# predicting test data with pca
pca.test <- predict(pca, test)
pca.test <- as.data.frame(pca.test)

pca.test <- cbind(pca.test, NSP= test[,"tsne_cl_pam"])

#model with multinomial logistic regression

# changing NSP variable into factor variable for our model
pca.train$NSP <- as.factor(pca.train$NSP)
pca.test$NSP <- as.factor(pca.test$NSP)

pca.train$NSP <- relevel(pca.train$NSP, ref = "1")


#-------------------------------------------------------------------------------
# BUILD MODEL

#-------------------------------------------------------------------------------

# selecting first 10 components, together they are responsible for more than 85% of the variability of the data
# the number of dimensions effects the accuracy rate of the model.
# more dimensions is not always better
# the PCA-method and the selective approach in the number of dimensions is a way to get lose noise

#multinomial logistic regression (MLR)
#model_mnlr <- multinom(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
#                  data = pca.train)


#randomForest (RF)
model_rf <- randomForest(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
                       data = pca.train)



#support vector machine (SVM)
#kernelc”radial” for multi-class classification
model_svm <- svm(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10,
            data = pca.train, 
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)

#write classification models
#multinomial logistic regression
#saveRDS(model_mnlr , "model_mnlr_ses.RDS")
#randomForest (prefered for SES project)
saveRDS(model_rf , "model_rf_ses.RDS")
#support vector machine
saveRDS(model_rf , "model_svm_ses.RDS")

#variable importance plot RandonmForest for predictors
varImpPlot(model_rf, n.var = 10, cex=.6)

# prediction with the multinomial logistic regression
#train dataset
#pred.train <- predict(model_mnlr, newdata = pca.train)
#confusionMatrix(pred.train, pca.train$NSP)

#test dataset
#pred.test.mnlr <- predict(model_mnlr, newdata = pca.test)
#confusionMatrix(pred.test.mnlr, pca.test$NSP)

# prediction with the randomForest 
#pred.trainR <- predict(model_rf, newdata = pca.train)
#confusionMatrix(pred.trainR, pca.train$NSP)

pred.test.rf <- predict(model_rf, newdata = pca.test)
confusionMatrix(pred.test.rf, pca.test$NSP)


pred.test.svm <- predict(model_svm, newdata = pca.test)
confusionMatrix(pred.test.svm, pca.test$NSP)


#accuracy rate in the confusionmatrices indicate that randomForest is 
#the best prediction model
