knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(tidymodels)
library(readxl)
library(neuralnet)
library(knitr)

#read from the excel data set and assign it to datatable variable
forecast_datatable <- read_excel("C:\\Users\\sujeewae\\Documents\\load_updated.xlsx") %>%
  janitor::clean_names() %>%
  mutate( date_in_ymd=ymd(dates)) %>%
  select( -1) %>%
  select( date_in_ymd, everything())

# Summary view of the original data set
summary(forecast_datatable)

# Create multiple sets with variations and store them all within a single
#data frame to preserve for testing and training
#data augmantation 

forecast_datatable_all_features = forecast_datatable %>%
  mutate(previous1DaySet_A = lag(forecast_datatable$x11_00,1),
         previous1DaySet_B = lag(forecast_datatable$x11_00,1),
         previous2DaysSet_B = lag(forecast_datatable$x11_00,2),
         previous1DaySet_C = lag(forecast_datatable$x11_00,1),
         previous2DaysSet_C = lag(forecast_datatable$x11_00,2),
         previous3DaysSet_C = lag(forecast_datatable$x11_00,3),
         previous1DaySet_D = lag(forecast_datatable$x11_00,1),
         previous2DaysSet_D = lag(forecast_datatable$x11_00,2),
         rolling5Days = rollmean(x11_00,5, fill = NA),
         rolling10Days = rollmean(x11_00,10, fill = NA)) %>%
  drop_na()

summary(forecast_datatable_all_features)

# Plot set A input variables
forecast_datatable_all_features %>%
  pivot_longer(cols =3, names_to= "kind", values_to="rate") %>%
  ggplot( aes( date_in_ymd,rate,color= kind))+geom_line() +
  facet_wrap(~kind
  )+theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set A of input variables")+theme(
    legend.position="none")

# Plot set B input variables
forecast_datatable_all_features %>%
  pivot_longer(cols =c(4,5),names_to="kind",values_to="rate" ) %>%
  ggplot( aes( date_in_ymd, rate,color=kind))+geom_line() +
  facet_wrap( ~kind) + theme( axis.text.x =
                                element_text(angle=45,vjust=0.5,hjust=1))+
  labs(x ="",title="Set B of input Variables")+theme(
    legend.position="none")

# Plot set C input variables
forecast_datatable_all_features %>%
  pivot_longer(cols=6:8, names_to="kind", values_to="rate") %>%
  ggplot( aes( date_in_ymd, rate,color=kind))+geom_line() +
  facet_wrap( ~kind)+theme( axis.text.x=element_text(angle=45,vjust
                                                     =0.5,hjust=1)) +
  labs(x ="",title="Set C of input variables")+theme(
    legend.position="none")

# Plot set D input variables
forecast_datatable_all_features %>%
  pivot_longer(cols=9:12, names_to="kind", values_to="rate") %>%
  ggplot( aes( date_in_ymd, rate,color=kind))+geom_line() +
  facet_wrap( ~kind)+theme(axis.text.x=element_text(angle=45,vjust=
                                                      0.5,hjust=1)) +
  labs(x= "", title="Set D of input variables")+theme(
    legend.position="none")

# Following function is to normalize data between 1 & 0
normalizeData <- function(x) {
  return ((x-min(x))/(max(x) -min(x)))
}

# Normalize all columns of forecast_datatable_all_features
normalized_forecast_datatable = forecast_datatable_all_features %>%
  mutate(across( 2:12, ~normalizeData(.x)))

# View data that has been normalized
summary(normalized_forecast_datatable)

# set.seed sets the starting number used to generate a sequence of random numbers
# it ensures that you get the same result each time you run the same process.
set.seed(124)


# First 400 rows will be train set
forecast_train_set <- normalized_forecast_datatable[1:400,]
# Beyond first 400 will be test set
forecast_test_set <- normalized_forecast_datatable[401:491,]

# We can create a function to unnormalize the data=
unnormalizeData<-function( x,min,max) {
  return(( max-min) *x+min)
}


# Get the max and min values of the original training column
forecastMinTrainVal<-min(forecast_datatable_all_features[1:400,2])
forecastMaxTrainVal<-max(forecast_datatable_all_features[1:400,2])

# Get the max and min values of the original testing column
forecastMinTestVal<-min(forecast_datatable_all_features[401:491,2])
forecastMaxTestVal<-max(forecast_datatable_all_features[401:491,2])


# Check the min and max of the training and test dataset
forecastMinTrainVal
forecastMaxTrainVal
forecastMinTestVal
forecastMaxTestVal

#Compare actual and predicted values for model type
comparePreditionStat<-function(realVal,predictedVal, mdlType) {
  rbind((tibble(truth=realVal,prediction=predictedVal) %>%
           metrics(truth, prediction) %>%
           mutate(type=mdlType)),(tibble(truth=realVal,
                                         prediction=predictedVal) %>%
                                    mape(truth, prediction) %>%
                                    mutate(type=mdlType)))
}

# set.seed sets the starting number used to generate a sequence of random numbers
# it ensures that you get the same result each time you run the same process.
set.seed(12346)
# Create 2 layer models
mdlTwoHiddenLayers = function(hiddenNode1,hiddenNode2, setType) {
  intermediaryData <- ""
  if (setType=="A") {
    intermediaryData <- x11_00 ~ previous1DaySet_A
  } else if (setType=="B") {
    intermediaryData <- x11_00 ~ previous1DaySet_B + previous2DaysSet_B
  } else if (setType=="C") {
    intermediaryData <- x11_00 ~ previous1DaySet_C + previous2DaysSet_C +
      previous3DaysSet_C
  } else if (setType=="D") {
    intermediaryData <- x11_00 ~ previous1DaySet_D + previous2DaysSet_D
 #   + rolling5Days + rolling10Days
  }
  
  #explain neuralnet
  nnModelTrue=neuralnet(intermediaryData, data=forecast_train_set,hidden=c(hiddenNode1,hiddenNode2),linear.output=TRUE)
  resultsOfTraining =compute(nnModelTrue,forecast_test_set[,2:12])
  truth_Column=forecast_datatable_all_features[401:491,4]$x11_00
  print(truth_Column)
  
  pred_column=unnormalizeData(resultsOfTraining$net.result,forecastMinTrainVal,forecastMaxTrainVal)[,1]
  comparePreditionStat(truth_Column,pred_column,
                       "Two Hidden Layers") %>%
    mutate(hiddel_layers=paste0(hiddenNode1," and ",hiddenNode2),
           input_set=setType) %>%
    filter(.metric != "rsq")
}



# Creation of various models for Set A
setATwoHiddenLayers=bind_rows(
  lapply(1:10,function(n) {
    bind_rows(
      lapply(1:5,function(m) {
        mdlTwoHiddenLayers(n,m, "A")
      })
    )
  })) %>%
  janitor::clean_names()

# Creation of various models for Set B
setBTwoHiddenLayers=bind_rows(
  lapply(1:10,function(n) {
    bind_rows(
      lapply(1:5,function(m) {
        mdlTwoHiddenLayers(n,m, "B")
      })
    )
  })) %>%
  
  janitor::clean_names()

#Creation of various models for Set C
setCTwoHiddenLayers=bind_rows(
  lapply(1:10,function(n) {
    bind_rows(
      lapply(1:5,function(m) {
        mdlTwoHiddenLayers(n,m, "C")
      })
    )
  })) %>%
  janitor::clean_names()

#Creation of various models for Set D
setDTwoHiddenLayers=bind_rows(
  lapply(1:10,function(n) {
    bind_rows(
      lapply(1:5,function(m) {
        mdlTwoHiddenLayers(n,m, "D")
      })
    )
  })) %>%
  janitor::clean_names()
# Bind all observations together
twHiddenLayerModels=rbind(setBTwoHiddenLayers, setBTwoHiddenLayers,
                          setCTwoHiddenLayers, setDTwoHiddenLayers)

#Get top performing models according to RMSE
twoHiddenLayerSortedRMSE=twHiddenLayerModels %>%
  select(-estimator) %>%
  pivot_wider(names_from=metric,values_from=estimate) %>%
  arrange(rmse)
kable(twoHiddenLayerSortedRMSE[1:5,])

# Get top performing models according to MAE
twoHiddenLayerSortedMAE=twHiddenLayerModels %>%
  select(-estimator) %>%
  pivot_wider(names_from=metric,values_from=estimate) %>%
  arrange(mae)
kable(twoHiddenLayerSortedMAE[1:5,])

#Get top performing models according to MAPE
twoHiddenLayerSortedMAPE=twHiddenLayerModels %>%
  select(-estimator) %>%
  pivot_wider(names_from=metric,values_from=estimate) %>%
  arrange(mape)
kable(twoHiddenLayerSortedMAPE[1:5,])


# Define optimal network: set B with 5 nodes in hidden layer 1 & 1 layer in
#hidden layer 2
optimalNeuralNetwork=neuralnet(x11_00 ~ previous1DaySet_B
                               +previous2DaysSet_B + rolling5Days + rolling10Days , data=forecast_train_set,
                               hidden=c(
                                 5,1), linear.output=TRUE)
plot(optimalNeuralNetwork)

