# Training data saturation analysis
**Important note:** In the code, some functions will be run. Especially the saturationCheck() function can take very (>20 minutes) long (depending on selected parameter values).
For testing, I suggest to reduce the amount iterations and sampleValList_raw values. Nevertheless, this will most probably reduce the quality of the result. <br>
Therefore, In the following I present some results I already calculated.
## Site overview
![overview image](https://raw.githubusercontent.com/corneliazy/MB2_Project/main/readme_data/OverviewSmallSitesNummeriert.png) <br>
<br>
1 = img_data/S2Stack_20190704_bayern_1_small.tif <br>
2 = img_data/S2Stack_20190704_bayern_2_small.tif <br>
3 = img_data/S2Stack_20190704_bayern_3_small.tif <br>
4 = img_data/S2Stack_20190704_bayern_4_small.tif <br>

## Class overview
In all example data files the classes are assigned as follows:<br>
 <table>
  <tr>
    <td>Class 1</td>
    <td>forest</td>
  </tr>
  <tr>
    <td>Class 2</td>
    <td>agri</td>
  </tr>
  <tr>
    <td>Class 3</td>
    <td>urban</td>
  </tr>
</table> 

## saturationCheck.R
### saturationCheck()
This function is the base as it estimates the accuracy values for different numbers of samples.

### saturationPlot()
With this function, classwise sensitivity and specificity values can be plotted as well as overall accuracy values. <br>
It serves as a tool for a visual (more subjective) saturation estimation and also shows the behaviour of individual classes.

#### results for bayern_1:
<p float="left">
  <img src="https://raw.githubusercontent.com/corneliazy/MB2_Project/main/readme_data/bayern_1_accuracy.png" width="33%" />
  <img src="https://raw.githubusercontent.com/corneliazy/MB2_Project/main/readme_data/bayern_1_sensitivity.png" width="33%" /> 
  <img src="https://raw.githubusercontent.com/corneliazy/MB2_Project/main/readme_data/bayern_1_specificity.png" width="33%" />
</p>


### estimateCurve() 
Here, an asymptotic regression model is adjusted to the accuracy data frame (provided by saturationCheck()). <br>
![saturationcurve bayern_1](https://raw.githubusercontent.com/corneliazy/MB2_Project/main/readme_data/curve_bayern_1.png)

### sampleSaturation()
This function returns the number of nSamples at which a saturation of the data can be expected. <br>
The calculation is based on the regression curve (estimateCurve()) and a slope (provided by the user) at which the curve is defined as saturated.
