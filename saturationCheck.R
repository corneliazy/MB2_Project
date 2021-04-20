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
# function to iterate over list and calculate sensitivity, specificity, overallAccuracy
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
# running the functions                                           # 
###################################################################


# definition of nSamples iteration steps
# sampleValList <- list(100, 300, 500, 700 ,1000, 2000, 3000, 4000, 6000)
# sampleValList <- list(100,500, 1000,1500 ,2000, 3000,4000)
# function needs to be applied on a list of SpatialPolygonDataframes


#saturationCheck(iterations, sampleValList_raw, image, training, validation)
saturationImage1 <-  saturationCheck(
  iterations = 4, 
  sampleValList_raw = list(50,100,500,1000,2000,4000), 
  image = rgb_2019_bavaria_1, 
  training = training_bavaria_1, 
  validation = validation_bavaria_1
  )

saturationImage1

# exporting dataframe for further usage
# getwd()
# write.table(saturationImage1, file="outputs/df_bayern_1.txt", sep=";")

# plotting saturationImage1 results using the saturationPlot() function
plotSaturationImage1 <- saturationPlot(
  accuracyDf = saturationImage1, 
  attribute = "accuracy"
  )

plotSaturationImage1


###################################################################
# experimental bonus functions                                    # 
###################################################################


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

# running the above function

ggg <- estimateCurve(saturationImage1)
ggg

# function to extract point of trainingdata saturation
# satslope = slope of a curve at which it is defined as saturated
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
  
  # rearranging it
  g_rearraged <- function(x) (coef_d - coef_c)*(exp(-x/coef_e)*(1/coef_e))-satslope
  x_sat <- uniroot(g_rearraged, lower=0, upper=3000)$root
  # print(x_sat)
  
  return (x_sat)

}

# testing function
sampleSaturation(ggg, 0.000001)

###################################################################
# the  end                                                        # 
###################################################################
f3 <- function(x){5~3+x}
solve(f3)

# plotting the result:
ggplot(data = accuracydf, aes(x=nSamples, y = accuracy))+
  geom_point(aes(x=nSamples, y=accuracy))+
  stat_smooth(method="lm")

ggplot(data = accuracydf, aes(x=nSamples, y = accuracy))+
  geom_point(aes(x=nSamples, y=accuracy))+
  stat_smooth(method="nls", formula = "y ~ A+B*exp(k*x)", method.args = list(start= c(A=0.93, B=-0.93, k=-0.6)))



# adjusting a curve to the datapoints
# equation: accuracy ~ A+B*exp(k*nSamples)
# guesses: A=1; b=-1; k=ln(0.5)
f <- fitModel(accuracy ~ A+B*exp(k*nSamples), 
              data= accuracydf, 
              start = list(A = 0.99, B = -0.99, k = log(0.5)/0.5)
              )

# looking at the actual classification
SC <- superClass(rgb_2019_bavaria_1, training_bavaria_1, trainPartition = 0.7, responseCol = "class", nSamples = 100)
SC
x11()
plot(SC$map)