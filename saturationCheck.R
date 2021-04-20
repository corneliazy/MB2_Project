# MB2 Project (training data saturation)
# Cornelia Zygar, 2582296

###################################################################
# This is a script for the assessment training data saturation    #
# for different satellite images for a supervised  random forest  #
# classification within the RStoolbox.                            #
# LIMITATION:                                                     #
# So far, code is only working for classifications with 3 classes # 
###################################################################

# loading relevant packages
library(RStoolbox)
library(ggplot2)
library(sf)
library(raster)
library(rgdal)
library(RCurl)
library(mosaic)
library(tidyr)
library(viridis)
library(here)

# for self starting nonlinear regression model
library(drc)
library(stats)
#library(Deriv)


###################################################################
# function definitions                                            # 
###################################################################

# running the supervised classification function from RSToolbox package
# relevant parameters:
# nSamples = Number of samples per land cover class 
# predict = logical. Produce a map (TRUE) or only fit and validate the model (FALSE)
# model = "rf" (random forest default)


# DO NOT RUN THIS FUNCTION INDEPENDENTLY!  
# function to iterate over list and estimate sensitivity, specificity, overallAccuracy
# for different nSamples.
# @param sampleValNumber defines number of training samples for classification (nSamples)
# @param image rasterbrick to perform the supervised classification on
# @param training set of training data (polygons)
# @param validation set of validation data (polygons)
# @return list of classwise sensitivity and specificity values and overall accuracy
helpSaturationCheck <- function(sampleValNumber, image, training, validation){
  caretResult <- getValidation(
    superClass(
      image, 
      training, 
      valData = validation, 
      responseCol = "class", 
      predict=FALSE, 
      nSamples = sampleValNumber
      ), 
    metrics="caret"
    )
  
  # documentation for outputs:
  # https://www.rdocumentation.org/packages/caret/versions/6.0-86/topics/confusionMatrix
  
  # classwise accuracies
  sensitivity <- caretResult$byClass[,"Sensitivity"]
  specificity <- caretResult$byClass[,"Specificity"]
  
  # overall accuracy
  overallAcc <- caretResult$overall["Accuracy"]
  
  return (list("sensitivity" = sensitivity, "specificity" = specificity, "accuracy" = overallAcc))
}

  
# This function runs the helpSaturationCheck()-function
# Depending on set number of iterations and sampleValList_raw, this can take veeeeeeeery long!
# But it is working, no worries!
# @param iterations number of repetitions for supervised classification with same nSamples
# @param sampleValList_raw list of integers that indicate the different nSample values over which will be iterated
# @param image rasterbrick to perform the supervised classification on
# @param training training set of training data (polygons)
# @param validation set of validation data (polygons)
# @return dataframe containing information on classification accuracy depending on nSamples
saturationCheck <- function(iterations, sampleValList_raw, image, training, validation){
  
  # defining list to which helpSaturationcheck can be applied on.
  # The elements of the list are defined as follows:
  # each value in sampleValList_raw is repeated "iterations" times.
  # Each number is one list element. So the list has a total length of iterations * sampleValList_raw
  sampleValList <- (rep(sampleValList_raw, each = iterations))

  # applying helpSaturationCheck on each element of sampleValList
  finalList <- lapply(sampleValList,helpSaturationCheck, image = image, training=training, validation = validation)
  
  # formatting the list to a dataframe (long format)
  df <- stack(unlist(finalList))
  
  # additional column containing nSamples
  # repeating each element 7 times because of 
  # 3*specificity + 3*sensitivity + 1*accuracy values in table
  nSamples <- (rep(sampleValList, each = 7))
  
  # updating dataframe
  df$nSamples <- unlist(nSamples)
  
  # splitting "ind" column to 
  # "attribute" (sensitivity/ specificity/ accuracy) and "class"
  df <- separate(data = df, col=ind, into = c("attribute", "Class"), sep="\\.")

return (df)
}

# Plotting function
# three different options for plotting the data
# 1: plotting overall accuracy
# 2: plotting classwise sensitivity
# 3: plotting classwise specificity
# @param accuaryDf dataframe created by saturationCheck() function
# @param attribute attribute whose accuracy will be plotted ("accuracy"/"specificity"/"sensitivity")
# @return boxplot visualizing connection between nSamples and accuracy characteristics
saturationPlot <- function(accuracyDf, attribute){
  
  # select all rows where attribute == attribute
  accuracyDf <- accuracyDf[accuracyDf$attribute == attribute,]
  
  # create ggplot of data
  # point plot (one color per class)
  # plot <- ggplot(data = accuracyDf, aes(x=nSamples, y = values))+
  #   geom_point(aes(x=nSamples, y=values, color = Class))
  
  # box plot (with points and groups)
  plot <- ggplot(data = accuracyDf, aes(x=as.factor(nSamples), y=values, fill = as.factor(nSamples), group=as.factor(nSamples)))+
    geom_boxplot()+
    scale_x_discrete(labels= waiver())+
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) + 
    geom_jitter(color = "black", size = 0.4, alpha=0.9)+
    ggtitle("accuracy per samplesize")+
    facet_wrap(~Class)
  
  return(plot)
}


# this function uses the drm() function from the drc package to model an asymptotic regression model
# for the datapoints provided by saturationCheck()
# @param df data.frame object like the ones created by saturationCheck()
# @return asymptotic regression model
estimateCurve <- function(df){
  
  # filtering all rows for rows with attribute == accuracy
  #saturationfiltered <- saturationImage1[saturationImage1$attribute == "accuracy",]
  saturationfiltered <- df[df$attribute == "accuracy",]
  
  # extracting the median overall accuracy value per nSamples iteration
  saturationfiltered_agg <- aggregate(values~nSamples, saturationfiltered, median)
  
  # assumption:
  # saturation functions can be modeled using asymptotic curves
  
  # used for this non linear regression:
  # drm() function and AR.3 function (self starting) for asymptotic regression model 
  # provided by drc package
  # AR.3 : f(x) = c+(d-c)(1-exp(-x/e))
  
  # definition of input values
  nSamples_1 <- saturationfiltered_agg$nSamples
  values_1 <- saturationfiltered_agg$values
  
  # drm fit
  model <- drm(values_1 ~ nSamples_1, fct = AR.3())
  plot(model, 
       log="", 
       main = "Trainingdata saturation"#, 
       #xlim = c(0,6000)
  )
  return (model)
}

# function to estimate the nSamples for which the overall accuracy of classification result seems to be saturated.
# @param model asymptotic regression model exactly like the ones provided by estimateCurve()
# @param satslope slope of a curve at which it is defined as saturated
# @return nSamples for which overall accuracy of classification saturates
sampleSaturation <- function(model, satslope){
  
  # extracting coefficient values from model
  coef_c <- as.list(model$coefficients)$c
  coef_d <- as.list(model$coefficients)$d
  coef_e <- as.list(model$coefficients)$e
  
  # insert coefficient values into function
  f <- function(x) (coef_c+(coef_d-coef_c)*(1-exp(-x/coef_e)))
  # plot f
  curve(f,0,3000)
  
  # calculating derivation (with respect to x) of f to get its slope
  g <- function(x){}
  body(g) <- D(body(f), "x")
  
  # plot g
  curve(g, 0, 3000)
  
  # derivation function:
  # function (x) (coef_d - coef_c) * (exp(-x/coef_e) * (1/coef_e))
  
  # rearranging it and add satslope as y
  g_rearraged <- function(x) (coef_d - coef_c)*(exp(-x/coef_e)*(1/coef_e))-satslope
  x_sat <- uniroot(g_rearraged, lower=0, upper=3000)$root
  # print(x_sat)
  
  return (x_sat)
  
}
###################################################################
# loading the data                                                # 
###################################################################
# Code has to be located in the same directory as the "validation_data", 
# "training_data" and "img_data" folders!

# only selecting rgb (not NIR)
rgb_2019_bavaria_1 <- dropLayer(brick(here("img_data/S2Stack_20190704_bayern_1_small.tif")),4)
rgb_2019_bavaria_2 <- dropLayer(brick(here("img_data/S2Stack_20190704_bayern_2_small.tif")),4)

# importing Training data
training_bavaria_1 <- readOGR(here("training_data/training_bayern_1_small.shp"))
training_bavaria_2 <- readOGR(here("training_data/training_bayern_2_small.shp"))

# importing validation data
validation_bavaria_1 <- readOGR(here("validation_data/validation_bayern_1_small.shp"))
validation_bavaria_2 <- readOGR(here("validation_data/validation_bayern_1_small.shp"))

###################################################################
# running the previously defined functions                        # 
###################################################################

# this function call will take long! Depending on the chosen number of iterations and the number 
# and size of samples in sampleValList_raw it might take very (!) long (20-30 min).
# To test it, I suggest using something like 
# iterations = 2, 
# sampleValList_raw = list(50,100,1000,2000)
# the result will not be as clear!
saturationDf_bayern_1 <-  saturationCheck(
  iterations = 4, 
  sampleValList_raw = list(50,100,500,1000,2000,4000), 
  image = rgb_2019_bavaria_1, 
  training = training_bavaria_1, 
  validation = validation_bavaria_1
  )

saturationDf_bayern_1

# exporting dataframe for further usage
# write.table(saturationDf, file=here("outputs/df_bayern_1.txt"), sep=";")

# plotting saturationDf results using the saturationPlot() function
plotSaturationDf_bayern_1 <- saturationPlot(
  accuracyDf = saturationDf_bayern_1, 
  attribute = "accuracy"
  )

plotSaturationImage1

# running estimateCurve() function
curve_bayern_1 <- estimateCurve(saturationDf_bayern_1)
curve_bayern_1


# running sampleSaturation() function
sampleSaturation(curve_bayern_1, 0.000001)

###################################################################
# the  end                                                        # 
###################################################################