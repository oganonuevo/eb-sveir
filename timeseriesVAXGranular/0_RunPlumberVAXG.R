setwd("~/backend-projections/timeseriesVAXGranular/")
r = plumber::plumb("1_SetupVAXG.R") 
r$run(port=8866,swagger = TRUE)
getwd()
