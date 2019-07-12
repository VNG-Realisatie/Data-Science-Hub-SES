#-------------------------------------------------------------------------------
# Roadmap Positive Health
# vulnerable citizens
# script 3/4
# Train model
# author : Mark Henry Gremmen, in cooperation with Gemma Smulders
# DataScienceHub @ JADS, GGD Hart voor Brabant
# lud 2019-07-12
#-------------------------------------------------------------------------------

#clear environment
rm(list=ls())

#clear console
cat("\014")  

#packages
packages <- c("tools","here","tidyverse","caTools", "psych", "RColorBrewer",
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

#set graphs location (if na, create directory first)
plots.loc <- paste0(root,'/PLOTS/')

#set data location (if na, create directory first)
data.loc <- paste0(root,'/DATA/')

#set library location (if na, create directory first)
lib.loc <- paste0(root,'/LIB/')

#source file name (csv-format)
source.file <- paste0(data.loc,'Xfinal-zorgwekkend_df20-k8-p40.csv')

SOURCE <- read.csv(source.file, T, ",")

str(SOURCE)

#original variables (not recoded) from the GGD Gezondheidsmonitor incl cluster membership cl_kmeans. 
#cluster membership must reside at the last position if the list 
cols <- c("GGADS201","KLGGB201","MMIKB201","CALGA260","CALGA261","LGBPS205","GGEEB201","GGEEB203",
          "GGEEB204","GGEEB207","GGEEB208","GGRLB201","GGRLB202","GGRLB204","GGRLB206","GGADB201",
          "GGADB202","GGADB204","GGADB207","GGADB210","cl_kmeans") 
SOURCE_SUBSET <- subset(SOURCE, select = cols)
dim(SOURCE_SUBSET)
df_tr <- ncol(SOURCE_SUBSET)
df_tr

#use only complete records (with cl_kmeans membership) for modelling
data <- na.omit(SOURCE_SUBSET)

# spliting the data into train and test
#SplitRatio : 60%:40% 
data1= sample.split(data,SplitRatio = 0.4)

#subsetting into train data
train =subset(data,data1==TRUE)

#subsetting into test data
test =subset(data,data1==FALSE)


# cheacking the corralation of the data

#data2 <- subset(data, select = -df_tr)
#pairs.panels(train[,data2],gap= 0,pch= 22)

# training the data with pca
# important scale data TRUE 
pca <- prcomp(train[,-df_tr],
              scale. = T,
              center = T)

# checking attributes available pca
attributes(pca)
pca$center

# analysing the pca model
print(pca)
summary(pca)

autoplot(prcomp(train), loadings= T,
         loadings.colour = "blue", loadings.label = T, loadings.label.size=3,
         frame= T)

# predicting traing data with pca
pca.train <- predict(pca, train)
pca.train <- as.data.frame(pca.train)

# adding our target variable which was not included during the pca training
pca.train <- cbind(pca.train, NSP= train[,"cl_kmeans"])


# predicting test data with pca
pca.test <- predict(pca, test)
pca.test <- as.data.frame(pca.test)

pca.test <- cbind(pca.test, NSP= test[,"cl_kmeans"])

#model with multinomial logistic regression

# changing NSP variable into factor variable for our model
pca.train$NSP <- as.factor(pca.train$NSP)
pca.test$NSP <- as.factor(pca.test$NSP)

pca.train$NSP <- relevel(pca.train$NSP, ref = "1")

# selecting first 14 variables, together they are responsible for more than 95% of the variability of the data

#multinomial logistic regression
model_mnlr <- multinom(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14,
                  data = pca.train)

#randomForest
model_rf <- randomForest(NSP ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14,
                       data = pca.train)


# prediction with the multinomial logistic regression
pred.train <- predict(model_mnlr, newdata = pca.train)
confusionMatrix(pred.train, pca.train$NSP)

pred.test <- predict(model_mnlr, newdata = pca.test)
confusionMatrix(pred.test, pca.test$NSP)

# prediction with the randomForest 
pred.trainR <- predict(model_rf, newdata = pca.train)
confusionMatrix(pred.trainR, pca.train$NSP)

pred.testR <- predict(model_rf, newdata = pca.test)
confusionMatrix(pred.testR, pca.test$NSP)

#write models
saveRDS(model_mnlr , "model_mnlr_ses.RDS")
saveRDS(model_rf , "model_rf_ses.RDS")
