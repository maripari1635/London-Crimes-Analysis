#install and load libraries
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(readr)) install.packages('readr')
library(readr)
if (!require(lattice)) install.packages('lattice')
library(lattice)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(randomForest)) install.packages('randomForest')
library(randomForest)
if (!require(rpart)) install.packages('rpart')
library(rpart)
if (!require(parallel)) install.packages('parallel')
library(parallel)
if (!require(knitr)) install.packages('knitr')
library(knitr)

##INCREASES MEMORY LIMIT WITHIN R
memory.limit(size=100000) 


#london crimes dataset
#https://www.kaggle.com/jboysen/london-crime
#from 2008 to 2016

#Overall objective of project
#prediction model to predict crimes given variables
#leading towards prediction of crimes as occurrences
#looking at data from 2010 onward

#------------------------------------------------------------------#
#WRANLGING AND SETUP OF DATA#

#
  #NOTE: Wrangling done here done to dataset obtained from
#https://www.kaggle.com/jboysen/london-crime

  #Done to cretae file size smaller than 25MB limit for Githhub
  #And for analysis
  #in terms of report however the loading of the london_crimes.csv
  #will suffice, simply to show how used csv was created.
#
# #load via relative path dataset into London crimes
# london_crimes <- read.csv("./london.csv")
# 
# #WRANGLING
# #looking at values from 2012 onward
# london_crimes <- london_crimes %>% filter(year > 2012)
# 
# #add a column to indicate if a crime occured or not
# #henceforth crime_occurred
# london_crimes <- london_crimes %>% mutate(crime_occurred = ifelse(london_crimes$value == 0,"No","Yes"))
# 
# #change months from numeric to month names
# london_crimes <- london_crimes %>% mutate(month = month.abb[as.numeric(month)])
# 
# 
# #sample 1% of the data for use
# london_crimes <- sample_n(london_crimes, 30000)
# 
# #write to a new csv file
# 
# write.csv(london_crimes,'london_crime_curated.csv')

#get created data
london_crimes <- read.csv("./london_crime_curated.csv")

#------------------------------------------------------#

#set aside 10% of data for final validation and a set for training
#set seed to ensure replicability
set.seed(2021, sample.kind="Rounding")

#split for a final test set and observation set, henceforth london
test_index <- createDataPartition(y = london_crimes$value, times = 1, p = 0.1, list = FALSE)
london <- london_crimes[-test_index,]
temp <- london_crimes[test_index,]

# Make sure following are found in both final test and london
#bourough, major_cat, lsoa_code
final_test <- temp %>% 
  semi_join(london, by = "borough")%>%
  semi_join(london, by = "major_category")%>%
  semi_join(london, by = "lsoa_code")%>%
  semi_join(london, by = "minor_category")%>%
  semi_join(london, by = "year")%>%
  semi_join(london, by = "month")%>%
  semi_join(london, by = "value")%>%
  semi_join(london, by = "crime_occurred")


# Add rows removed from final set back into london
removed <- anti_join(temp, final_test)
london <- rbind(london, removed)

##cleanup
rm(removed, temp, test_index, london_crimes)
#------------------------------------------------------------------#

#EXPLORING DATA

#head of data for quick overview
head(london, 5)

#expect year and month to be 3 and 12 respectively
n_distinct(london$year)
n_distinct(london$month)

#now check distinct values for specific cat data
# lsoa code, borough, major cat, minor cat
n_distinct(london$lsoa_code)

#districts distinct and names thereof
n_distinct(london$borough)
head(distinct(london[2]),5)

#now breakdown of major cat
n_distinct(london$major_category)
head(distinct(london[3]),5)

#minor cat breakdown
n_distinct(london$minor_category)
head(distinct(london[4]),5)

#see spread of values, defined previous as count per month of a crime
crime_month_values <- table(london$value)
print(crime_month_values, width=100)

#quick table of value count
london %>% group_by(value)%>%
  ggplot(aes(value))+
  geom_bar() +
  labs(title = "Crime Occurrences")+
  scale_y_continuous(name="Count", labels = comma)+
  scale_x_continuous(name="Crimes Per Month")

#more in-depth view 
london %>% group_by(value)%>%
  filter(value > 0 && value < 10) %>%
  ggplot(aes(value))+
  geom_bar() +
  labs(title = "Crime Occurrences")+
  scale_y_continuous(name="Count", labels = comma)+
  scale_x_continuous(name = "Crimes Per Month", breaks = c(0,1,2,3,4,5,6,7,8,9,10))

#more meaningful as summarize
london %>% group_by(value) %>%
  summarise(n=n()) %>% head(10)

#now see as incidence
london %>% group_by(crime_occurred)%>%
  ggplot(aes(crime_occurred))+
  geom_bar()+  
  scale_y_continuous(name="Count", labels = comma)+
  labs(title = "Whether Crime Occurs")

london %>% group_by(crime_occurred)%>%
  summarise(n=n())

#note, need to look at values greater than 0 when summarizing or via crime occurred
#0 here means nothing counted for specific crime in given month

#demonstrate below, comparing borough crime occurences per month
#note, just seeing if crime occured, not amount,
london %>%
  group_by(borough)%>%
  ggplot(aes(borough))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Occurence", labels = comma)+
  labs(title="Crimes Occurence Within Boroughs")


london %>%
  filter(crime_occurred=="Yes") %>%
  group_by(borough)%>%
  ggplot(aes(borough))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Occurence", labels = comma)+
  labs(title="Crime Occurence Within Boroughs")


#here can see the amount of crime per city from 2010 to 2016, with prev comparison unneeded
#as adding zero will not change amount, only count
london %>%
  group_by(borough)%>%
  ggplot(aes(borough, value))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Amount", labels = comma)+
  labs(title="Crime Amount Within Boroughs")


#repeat with major cat, no need demonstrate
london %>%
  filter(value > 0) %>%
  group_by(major_category)%>%
  ggplot(aes(major_category))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Occurrence", labels = comma)+
  labs(title="Instances of Crime Per Type")


#here can see the amount of crime per city from 2010 to 2016, with prev comparison unneeded
#as adding zero will not change amount, only count
london %>%
  filter(value > 0) %>%
  group_by(major_category)%>%
  ggplot(aes(major_category, value))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Amount", labels = comma)+
  labs(title = "Crime Amount Per Type")


#minor cat, since what makes major, can be seen as stack for major
#set colors for greater readability
groupColors <- c("#696969", "#800000", "#006400", "#808000", "#483d8b", "#008b8b",
                 "#d2691e", "#9acd32", "#00008b", "#daa520", "#7f007f", "#8fbc8f",
                 "#b03060", "#ff4500", "#ffff00", "#40e0d0", "#7fff00", "#8a2be2",
                 "#00ff7f", "#dc143c", "#00bfff", "#0000ff", "#ff00ff", "#1e90ff",
                 "#fa8072", "#b0e0e6", "#90ee90", "#ff1493", "#7b68ee", "#f5deb3",
                 "#ee82ee", "#ffb6c1")

london %>%
  ggplot(aes(x = major_category, y=value))+
  geom_col(aes(fill=minor_category))+
  scale_fill_manual(values=groupColors)+
  scale_y_continuous(name="Crime Amount", labels = comma)+
  labs(title="Crime Amount Per Type by Subtypes")+
  theme(axis.text.x = element_text(angle = 90))



#now check lsao codes
london %>%
  filter(value > 0) %>%
  group_by(lsoa_code)%>%
  ggplot(aes(lsoa_code))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Occurences", labels = comma) %>%
  labs(title="LSOA CODE Crime Occurrences")


#better to see as summary perhaps
lsoa_Table <- london %>% group_by(lsoa_code) %>%
  summarise(n=n())

#more distinct values
top_n(as.data.frame(lsoa_Table), 3)
top_n(as.data.frame(lsoa_Table), -3)
sample_n(lsoa_Table,5)

#------------------------------#
#subset into lsoa codes
#add count of lsoa codes to each borough to show how diff boroughs have diff number of codes
qplot(london$borough,london$lsoa_code)

#now a heat map to show more nuiances
london %>%
  ggplot(aes(x=borough, y=lsoa_code))+
  geom_bin2d()

#summarize due to ill look of plot
london %>% group_by(lsoa_code, borough) %>%
  summarise(n=n()) %>% head(10)

#get index of lsoa codes as char vector for filtering
set.seed(2021, sample.kind="Rounding")
lsoa_index <- as.vector(sample(london$lsoa_code,50))

#sample low amount to show readable lsoa codes
london%>%
  filter(lsoa_code %in% lsoa_index)%>%
  ggplot(aes(x=borough, y=lsoa_code))+
  geom_bin2d() +  
  scale_fill_gradient(low="blue",high="orange",trans="log10")+
  labs(title = "Boroughs by Curated Codes")+
  theme(axis.text.x = element_text(angle = 90))

#subset end
#------------------------------#

#check year and month var
london %>%
  filter(value > 0) %>%
  group_by(year)%>%
  ggplot(aes(year))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Occurences", labels = comma)

#here can see the amount of crime per year
london %>%
  group_by(year)%>%
  ggplot(aes(year, value))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Amount", labels = comma)


london %>% mutate(month= fct_relevel(month, "Jan","Feb","Mar","Apr","May",
                                     "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))%>%
  filter(value > 0) %>%
  group_by(month)%>%
  ggplot(aes(month))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Occurences", labels = comma)

#here can see the amount of crime per year
london %>% mutate(month= fct_relevel(month, "Jan","Feb","Mar","Apr","May",
                                     "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))%>%
  filter(value > 0) %>%
  group_by(month)%>%
  ggplot(aes(month, value))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(name="Crime Amount", labels = comma)


#clean up variables for next step, the Analysis
rm(crime_month_values, lsoa_Table, lsoa_index)

#------------------------------------------------------------------#
#Model Presetup


#make rmse function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


set.seed(2021, sample.kind="Rounding")

#since modeling for final test must separate the training set into two, with 10% as training test set
test_index <- createDataPartition(y = london$value, times = 1, p = 0.1, 
                                  list = FALSE)
train_set <- london[-test_index,]
test_set <- london[test_index,]


##ensure for samples data in test set has same borough, major, and code as train set sample
test_set <- test_set %>% 
  semi_join(train_set, by = "borough")%>%
  semi_join(train_set, by = "major_category")%>%
  semi_join(train_set, by = "lsoa_code")%>%
  semi_join(train_set, by = "minor_category")%>%
  semi_join(train_set, by = "year")%>%
  semi_join(train_set, by = "month")%>%
  semi_join(train_set, by = "value")%>%
  semi_join(train_set, by = "crime_occurred")

#---------------------------------------------------------------------------#
#Model 1 Averages of effects

##simple way to predict,
##using only mu, the average of random variance
set.seed(2021, sample.kind="Rounding")
mu_hat <- mean(train_set$value)
mu_hat
naive_rmse <- RMSE(test_set$value, mu_hat)
naive_rmse

##put result into table for later use
rmse_results <- tibble(method = "Assumed Average", RMSE = naive_rmse)

#now predict based on all effects accounted for
#these are year, month, lsao code, borough, major cat, minor cat,
set.seed(2021, sample.kind="Rounding")

mu <- mean(train_set$value) 

code_avg <- train_set %>% 
  group_by(lsoa_code) %>% 
  summarize(b_c = mean(value - mu))

borough_avg <- train_set %>%
  left_join(code_avg, by="lsoa_code") %>%
  group_by(borough) %>%
  summarize(b_b = mean(value - mu - b_c))

major_avg <- train_set %>%
  left_join(code_avg, by="lsoa_code") %>%
  left_join(borough_avg, by="borough")%>%
  group_by(major_category) %>%
  summarize(b_ma = mean(value - mu - b_c - b_b))

minor_avg <- train_set %>%
  left_join(code_avg, by="lsoa_code") %>%
  left_join(borough_avg, by="borough")%>%
  left_join(major_avg, by="major_category")%>%
  group_by(minor_category)%>%
  summarize(b_mi = mean(value - mu - b_c - b_b - b_ma))

year_avg <- train_set %>%
  left_join(code_avg, by="lsoa_code") %>%
  left_join(borough_avg, by="borough")%>%
  left_join(major_avg, by="major_category")%>%
  left_join(minor_avg, by="minor_category")%>%
  group_by(year)%>%
  summarize(b_y = mean(value - mu - b_c - b_b - b_ma - b_mi))

month_avg <- train_set %>%
  left_join(code_avg, by="lsoa_code") %>%
  left_join(borough_avg, by="borough")%>%
  left_join(major_avg, by="major_category")%>%
  left_join(minor_avg, by="minor_category")%>%
  left_join(year_avg, by="year")%>%
  group_by(month)%>%
  summarize(b_m = mean(value - mu - b_c - b_b - b_ma - b_mi - b_y))

predicted_values <- test_set %>% 
  left_join(code_avg, by="lsoa_code") %>%
  left_join(borough_avg, by="borough")%>%
  left_join(major_avg, by="major_category")%>%
  left_join(minor_avg, by="minor_category")%>%
  left_join(year_avg, by="year")%>%
  left_join(month_avg, by="month")%>%
  mutate(pred = mu + b_c + b_b + b_ma + b_mi + b_y +b_m) %>%
  pull(pred)

#now here are the effects accounted for
all_effects <- RMSE(predicted_values, test_set$value)
all_effects

#now the effects rounded downward
all_effects_rounded <- RMSE(floor(predicted_values), test_set$value)
all_effects_rounded


#effects but all negative numbers rounded to zero
predicted_values_no_negative <- ifelse(predicted_values <= 0, 0, predicted_values)

all_effects_added_no_negatives <- RMSE(predicted_values_no_negative, test_set$value)
all_effects_added_no_negatives

#-----------------------------------------------------#
#Regularization

set.seed(2021, sample.kind="Rounding")
#now utilize regularization for effects
lambdas <- seq(0, 10, 1)

set.seed(2021, sample.kind="Rounding")
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$value)
  
  b_c <- train_set %>% 
    group_by(lsoa_code) %>%
    summarize(b_c = sum(value - mu)/(n()+l))
  
  b_b <- train_set %>% 
    left_join(b_c, by="lsoa_code") %>%
    group_by(borough) %>%
    summarize(b_b = sum(value - b_c - mu)/(n()+l))
  
  b_ma <- train_set %>%
    left_join(b_c, by="lsoa_code")%>%
    left_join(b_b, by="borough") %>%
    group_by(major_category) %>%
    summarize(b_ma = sum(value - b_c - b_b - mu)/(n()+l))
  
  b_mi <- train_set%>%
    left_join(b_c, by="lsoa_code")%>%
    left_join(b_b, by="borough") %>%
    left_join(b_ma, by="major_category") %>%
    group_by(minor_category) %>%
    summarize(b_mi = sum(value - b_c - b_b - b_ma - mu)/(n()+l))
  
  b_y <- train_set%>%
    left_join(b_c, by="lsoa_code")%>%
    left_join(b_b, by="borough") %>%
    left_join(b_ma, by="major_category") %>%
    left_join(b_mi, by='minor_category') %>%
    group_by(year) %>%
    summarize(b_y = sum(value - b_c - b_b - b_ma - b_mi - mu)/(n()+l))
  
  b_m <- train_set%>%
    left_join(b_c, by="lsoa_code")%>%
    left_join(b_b, by="borough") %>%
    left_join(b_ma, by="major_category") %>%
    left_join(b_mi, by='minor_category') %>%
    left_join(b_y, by='year') %>%
    group_by(month) %>%
    summarize(b_m = sum(value - b_c - b_b - b_ma - b_mi - b_y - mu)/(n()+l))
  
  predicted_values <- 
    test_set %>% 
    left_join(b_c, by="lsoa_code")%>%
    left_join(b_b, by="borough") %>%
    left_join(b_ma, by="major_category") %>%
    left_join(b_mi, by='minor_category') %>%
    left_join(b_y, by='year') %>%
    left_join(b_m, by='month') %>%
    mutate(pred = mu + b_c + b_b + b_ma + b_mi + b_y + b_m) %>%
    pull(pred)
  
  predicted_values <- ifelse(predicted_values <= 0, 0, predicted_values)
  
  return(RMSE(predicted_values, test_set$value))
  
})
qplot(lambdas, rmses) 


#obtain best lambda
best_lambda <- lambdas[which.min(rmses)]

#now apply to the model above
set.seed(2021, sample.kind="Rounding")
mu <- mean(train_set$value)

b_c <- train_set %>% 
  group_by(lsoa_code) %>%
  summarize(b_c = sum(value - mu)/(n()+best_lambda))

b_b <- train_set %>% 
  left_join(b_c, by="lsoa_code") %>%
  group_by(borough) %>%
  summarize(b_b = sum(value - b_c - mu)/(n()+best_lambda))

b_ma <- train_set %>%
  left_join(b_c, by="lsoa_code")%>%
  left_join(b_b, by="borough") %>%
  group_by(major_category) %>%
  summarize(b_ma = sum(value - b_c - b_b - mu)/(n()+best_lambda))

b_mi <- train_set%>%
  left_join(b_c, by="lsoa_code")%>%
  left_join(b_b, by="borough") %>%
  left_join(b_ma, by="major_category") %>%
  group_by(minor_category) %>%
  summarize(b_mi = sum(value - b_c - b_b - b_ma - mu)/(n()+best_lambda))

b_y <- train_set%>%
  left_join(b_c, by="lsoa_code")%>%
  left_join(b_b, by="borough") %>%
  left_join(b_ma, by="major_category") %>%
  left_join(b_mi, by='minor_category') %>%
  group_by(year) %>%
  summarize(b_y = sum(value - b_c - b_b - b_ma - b_mi - mu)/(n()+best_lambda))

b_m <- train_set%>%
  left_join(b_c, by="lsoa_code")%>%
  left_join(b_b, by="borough") %>%
  left_join(b_ma, by="major_category") %>%
  left_join(b_mi, by='minor_category') %>%
  left_join(b_y, by='year') %>%
  group_by(month) %>%
  summarize(b_m = sum(value - b_c - b_b - b_ma - b_mi - b_y - mu)/(n()+best_lambda))

predicted_values <- 
  test_set %>% 
  left_join(b_c, by="lsoa_code")%>%
  left_join(b_b, by="borough") %>%
  left_join(b_ma, by="major_category") %>%
  left_join(b_mi, by='minor_category') %>%
  left_join(b_y, by='year') %>%
  left_join(b_m, by='month') %>%
  mutate(pred = mu + b_c + b_b + b_ma + b_mi + b_y + b_m) %>%
  pull(pred)

#now set predicted values for negative to be zero and predict
predicted_values <- ifelse(predicted_values <= 0, 0, predicted_values)
values_regularized_effects <- RMSE(predicted_values, test_set$value)

rmse_results <- rmse_results %>% add_row(method = "Regularized All Effects", 
                                         RMSE = values_regularized_effects)

rmse_results %>% knitr::kable()


#---------------------------------------------------------------------#
#for training purposes only
#sample for faster usage
train_set_sample <- sample_frac(train_set, 0.1)
test_set_sample <- sample_frac(test_set, 0.1)

test_set_sample <- test_set_sample %>% 
  semi_join(train_set_sample, by = "borough")%>%
  semi_join(train_set_sample, by = "major_category")%>%
  semi_join(train_set_sample, by = "lsoa_code")%>%
  semi_join(train_set_sample, by = "minor_category")%>%
  semi_join(train_set_sample, by = "year")%>%
  semi_join(train_set_sample, by = "month")%>%
  semi_join(train_set_sample, by = "value")%>%
  semi_join(train_set_sample, by = "crime_occurred")

#----------------------------------------------------------------------#
#look at other methods
#look more as cateogrical, note in particular crime_occurred
#unused kNN, slightly more accurate than below but poor performance

# set.seed(2021, sample.kind="Rounding")
# 
# ###knn, nearest neighbor
# train_knn <- train(crime_occurred ~ lsoa_code + borough + major_category + minor_category + year + month,
#                    method = "knn",
#                    data = train_set,
#                    tuneGrid = data.frame(k = seq(1, 20, 2)))
# #see best tuning k parameter
# train_knn$bestTune
# 
# #plot out neighnor k parameters for their accuracies
# ggplot(train_knn, highlight = TRUE)
# 
# #prediction
# knn_cv_preds <- predict(train_knn, test_set[c(1:4,6:7)])
# mean(knn_cv_preds == test_set$crime_occurred)

#----------------------------------------------------#
#used knn via cv to increase performance
#now edit cross validation for performance, 

#due to computation time, this will be used within the markdown
train_knn_cv <- train(crime_occurred ~ lsoa_code + borough + major_category + 
                        minor_category + year + month,
                      method = "knn",
                      data = train_set_sample,
                      tuneGrid = data.frame(k = seq(1, 15, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = .9))

#see best fit as number and within plot
ggplot(train_knn_cv, highlight = TRUE)
train_knn_cv$bestTune
best_k <- train_knn_cv$bestTune

#prediction
knn_cv_preds <- predict(train_knn_cv, test_set_sample[c(1:4,6:7)])
knn_prediction <- mean(knn_cv_preds == test_set_sample$crime_occurred)

#copy prediction to table for future comparisons
prediction_results <- tibble(method = "kNN Method", Prediction = knn_prediction)

#see results
prediction_results %>% knitr::kable()

#--------------------------------------------#
#rpart training 


#train via rpart
train_rpart <- train(crime_occurred ~ lsoa_code + borough + major_category + minor_category + year + month, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.2, 0.05)),
                     data = train_set_sample)

#see rpart tune and plot and 
train_rpart$bestTune
ggplot(train_rpart, highlight = TRUE)

#set best cp
best_cp <- train_rpart$bestTune

#prediction
rpart_preds <- predict(train_rpart, test_set_sample[c(1:4,6:7)])
rpart_prediction <- mean(rpart_preds == test_set_sample$crime_occurred)

#add to table
prediction_results <- prediction_results %>% add_row(method = "RPart Method", 
                                                     Prediction = rpart_prediction)

#see results
prediction_results %>% knitr::kable()
#--------------------------------------------------#
#random forest

train_rf <- train(crime_occurred ~ lsoa_code + borough + major_category + minor_category + year + month,
                  data = train_set_sample,
                  method = "rf",
                  ntree = 125,
                  nodesize = 300,
                  tuneGrid = data.frame(mtry = seq(1:3)))

#see best tuning and set
train_rf$bestTune
best_mtry <- train_rf$bestTune

#create prediction
rf_preds <- predict(train_rf, test_set_sample[c(1:4,6:7)])
randForest_prediction <- mean(rf_preds == test_set_sample$crime_occurred)



#add to table
prediction_results <- prediction_results %>% add_row(method = "Random Forest Method", 
                                                     Prediction = randForest_prediction)
#see important variables
varImp(train_rf)

#see results
prediction_results %>% knitr::kable()
#-------------------------------------------------------------------------#

#------------------------------------------------------------------------#
#cleanup

rm(ensemble, ensembles, ensemble_preds, RMSE, rpart_preds, rf_preds, p, knn_cv_preds,
   train_set, train_set_sample, train_rpart, train_rf, train_knn_cv, train_knn,
   test_index, test_set, test_set_sample, randomForest_prediction,
   rpart_prediction, knn_prediction, ensemble_pred, degree, best_mtry,
   randForest_prediction)

#-----------------------------------------------------------------------#
#using best model with london and final test sets
#best model is knn

#--------------------------------------------------------#
#use best model below

final_knn <- train(crime_occurred ~ lsoa_code + borough + major_category + 
                        minor_category + year + month,
                      method = "knn",
                      data = london,
                      tuneGrid = data.frame(k = best_k),
                      trControl = trainControl(method = "cv", number = 10, p = .9))

#create prediction for final model
final_preds <- predict(final_knn, final_test[c(1:4,6:7)])
final_prediction <- mean(final_preds == final_test$crime_occurred)


#add to table
prediction_results <- prediction_results %>% add_row(method = "Final Model: kNN", 
                                                     Prediction = final_prediction)

#see results
prediction_results %>% knitr::kable()


