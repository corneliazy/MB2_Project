# MB2 Project (sensitivity analysis)
# author: Cornelia Zygar, 2582296

###################################################################
# This is a script for the assessment training data saturation    #
# for different satellite images for a supervised classification  #
# within the RStoolbox.                                           #
# LIMITATION:                                                     #
# So far, code is only working for Classifications with 3 classes # 
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

# for self starting nonlinear regression model
library(drc)
library(stats)



###################################################################
# function definitions                                            # 
###################################################################

# running the supervised classification function from RSToolbox package
# relevant parameters:
# nSamples = Number of samples per land cover class 
# predict = logical. Produce a map (TRUE) or only fit and validate the model (FALSE)
# model = "rf" (random forest default)

# definition of nSamples iteration steps
# sampleValList <- list(100, 300, 500, 700 ,1000, 2000, 3000, 4000, 6000)
# sampleValList <- list(100,500, 1000,1500 ,2000, 3000,4000)
# function needs to be applied on a list of SpatialPolygonDataframes


# DO NOT RUN INDEPENDENTLY!  
# function to iterate over list and calculate sensitivity, specificity, overallAccuracy
# for different nSamples.
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

  
# This function runs the helpSaturationCheck-function
# Depending on set iterations and sampleValList_raw, this can take veeeeeeeery long!
# But it is working, no worries!
  
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

# TODO evtl. option zum auswaehlen verschiedener plotarten?!

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
# TODO: this must be set to the path within GitHub!
setwd("C:/Users/Cornelia/Documents/Studium/EAGLE/1.Semester/MB2/Project")

# only selecting rgb (not NIR)
rgb_2019_bavaria_1 <- dropLayer(brick("S2Stack_20190704_bayern_1_small.tif"),4)

# importing Training data
training_bavaria_1 <- readOGR("training_bayern_1_small.shp")

# importing validation data
validation_bavaria_1 <- readOGR("validation_bayern_1_small.shp")

###################################################################
# running the functions                                           # 
###################################################################


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
  curve(f,0,3000)
  
  # calculating derivation (with respect to x) of f to get its slope
  g <- function(x){}
  body(g) <- D(body(f), "x")
  # f <- y ~ coef_c+(coef_d-coef_c)*(1-exp(-x/coef_c))
  #f <- expression(coef_c+(coef_d-coef_c)*(1-exp(-x/coef_e)))
  #f <- coef_c+(coef_d-coef_c)*(1-exp(-x/coef_e))
  # curve(g, 0, 3000)
  #result_hoffentlich <- x[which(g(x)==0.01)]
  
  # get x of first derivation where y== satslope
  #sat_x <- uniroot((g$y)-0.05, interval = c(0,6000))$root
  
  #return(sat_x)
  #return(result_hoffentlich)
  return (g)
  
  # calculating derivation 
  #first_derivation <- D(f,"x")
  
  #uniroot(first_derivation, )
  
  #result1 <- eval(first_derivation, envir = list(coef_c = coef_c, coef_d = coef_d, coef_e = coef_e))
  #return (result1)
  
  #res <- first_derivation(coef_c = coef_c, coef_d = coef_d, coef_e = coef_e, x=1000)
  #eval(first_derivation)
  #class(first_derivation)
  #g <- y~first_derivation
  
 #return(res)
  #return(first_derivation)
}

# testing function
sampleSaturation(ggg, "0.5")

###################################################################
# the  end                                                        # 
###################################################################



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