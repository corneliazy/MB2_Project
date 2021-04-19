# Function to assess the influence of the distribution of trainingsdata on the accuracy of a classification
# Based on the superClass-function within the RSToolbox
# author: Cornelia Zygar

# This function can be tested with the training data provided in the XXX folder

# loading packages
library("ggplot2")
library("rgdal")
library("RStoolbox")

# superClass() function has parameter nSamples.
# from the documentation of superClass():
# nSamples: "Integer. Number of samples per land cover class."
# "Sample training coordinates. If trainData(and valData if present) are SpatialPolygons-
# DataFrames superClass will calculate the area per polygon and sample nSamples locations 
# per class within these polygons.  The number of samples per individual polygon scales
# with the polygon area, i.e. the bigger the polygon, the more samples."

# function takes a list (?) of multiple shapefiles containing different numbers of 
# Polygons. The more Polygons within the shapefile, the more distributed the trainingdata
# will be.



# function needs to be applied on a list of SpatialPolygonDataframes
distrAssessment <- function(trainingDataset, img, validationData, numSamples){
  result <- getValidation(superClass(img, trainingDataset, valData=validationData, responseCol = "class", predict=FALSE, nSamples = numSamples), metrics="caret")
 
  # classwise accuracies
  sensitivity <- result$byClass[,"Sensitivity"]
  specificity <- result$byClass[,"Specificity"]
  
  # overall accuracy
  overallAcc <- result$overall["Accuracy"]

  return (list(sensitivity, specificity, overallAcc))
}



# data
# trainingDataList = List of SpatialPolygonDataframes
trainingList <- list(
  readOGR("training_distrAssessment/training_bayern_1_small_1.shp"),
  readOGR("training_distrAssessment/training_bayern_1_small_2.shp"),
  readOGR("training_distrAssessment/training_bayern_1_small_4.shp"),
  readOGR("training_distrAssessment/training_bayern_1_small_8.shp"),
  readOGR("training_distrAssessment/training_bayern_1_small_16.shp")
  )
valData <- readOGR("validation_bayern_1_small.shp")
image <- brick("S2Stack_20190704_bayern_1_small.tif")

# running the function
test <- lapply(trainingList, distrAssessment, img = image, numSamples=10, validationData=valData)
test


# parameter feature: feature that should be plotted
plotDistrAssessment <- function(feature, resultList){
  helpf <- function(x,n){
    return(x[[n]])
  }
  numList <- list(1,2,4,8,16)
  
  if (feature == "sensitivity"){
    data <- lapply(resultList, helpf, n=1)
    df <- do.call(rbind, Map(data.frame, nPolygons = numList, accuracy = data))
    df$class <- c(rep(1:3, times = length(numList)))
  } else if (feature == "specificity"){
    data <- lapply(resultList, helpf, n=2)
    df <- do.call(rbind, Map(data.frame, nPolygons = numList, accuracy = data))
    df$class <- c(rep(1:3, times = length(numList)))
  } else if (feature == "overall"){
    data <- lapply(resultList, helpf, n=3)
    df <- do.call(rbind, Map(data.frame, nPolygons = numList, accuracy = data))
  } else{
    print("please provide valid feature argument!")
    return (NULL)
  }

  if("class" %in% colnames(df)){
    plot <- ggplot(data = df, aes(x=nPolygons, y=accuracy, group = class, col=class))+
      geom_line()
      #  geom_point(aes(x=nPolygons,y=accuracy))
  } else{
    plot <- ggplot(data = df, aes(x=nPolygons, y=accuracy))+
      geom_line()
    
  }
  
  return (plot)
}
print("#################")
test3 <- plotDistrAssessment("overall", test)
test3

test2 <- plotDistrAssessment("sensitivity", test)
test2
class(test2)
class(test2[[1]])

test1 <- plotDistrAssessment("specificity", test)
test1
