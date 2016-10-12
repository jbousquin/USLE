# R_USLE
Universal Soil Loss Equation on spatial inputs

##About USLE Method
The USLE for estimating average annual soil erosion is:

**A = RKLSCP** 

+ **A** = average annual soil loss in t/a (tons per acre)
+ **R** = rainfall erosivity index
+ **K** = soil erodibility factor
+ **LS** = topographic factor - L is for slope length & S is for slope
+ **C** = cropping factor
+ **P** = conservation practice factor

###R - the rainfall erosivity index
The erosivity index is a statistic calculated from the annual summation of rainfall energy in every storm (correlates with raindrop size) times its maximum 30 - minute intensity. As expected, it varies geographically.

##Datasources
**R** rainfall erosivity index queiried from USDA RUSLE database

**K** ?Surgo?

**LS** slope length and slope determined from elevations using elevatR

**C** is a user specified landuse and lookup-table table for C values 

**P** is user specified
