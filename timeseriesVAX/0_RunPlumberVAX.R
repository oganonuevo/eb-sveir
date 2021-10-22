setwd("C:/Users/ogano/Desktop/FASSSTER/backend-projections/timeseriesVAX/")
r = plumber::plumb("1_SetupVAX.R") 
r$run(port=7177,swagger = TRUE)
getwd()
