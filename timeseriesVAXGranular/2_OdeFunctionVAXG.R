library(deSolve)
library(GenSA)
library(Metrics)
library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
library(filesstrings)
library(tools)
library(taRifx)
library(ff)
library(stringr)
library(stats)
library(magrittr)

# or put this in the main file?
# filterPSGCs = c("410", "421", "434")

summaries = read.csv("./inputs/parametersvaxg.csv") #the file with the summaries

# confirmedCases.df = read.csv(unzip("linelist/ConfirmedCases.zip", files = "ConfirmedCases.csv"), stringsAsFactors = FALSE)
source("../linelist/read_linelist.R")
source("./3_ProcessActiveCaseswithVAXG.R")

fitParamToUse = "Cumulative"

if(fitParamToUse == "Cumulative"){
  confirmedCases.df <- get_linelist() %>% 
    dplyr::mutate_at(vars(contains('Date_') | contains("_Date")), as.Date)%>%
    dplyr::mutate(Report_Date = as.Date(Report_Date))%>%
    dplyr::mutate(Report_Date = ifelse(Report_Date < as.Date("2021-07-01"), as.Date("2021-07-01"), Report_Date))
  confirmedCases.df <- confirmedCases.df %>% 
    dplyr::mutate(Report_Date = as.Date(Report_Date, origin="1970-01-01"))
  confirmedCases.df$nationalPSGC = "180000000"
  source("3_ProcessActiveCaseswithVAXG.R")
} else{ #get active instead
  confirmedCases.df <- writeActiveCases() %>% 
    dplyr::filter(linelist_date >= as.Date("2021-07-01"))
  maxDate = confirmedCases.df$linelist_date[1]
  source("3_ProcessActiveCaseswithVAXG.R")
}



getPsgcDetails = function(psgcInput) {
  dfToReturn = summaries[which(as.numeric(summaries$area) == as.numeric(psgcInput)),]
  return(dfToReturn)
}


RunProjections = function(lambdaDateInput, lambdaValuesInput,
                          psgcInput, startHcDate, HcLevel, projectionDuration, 
                          scaleLevel, fitParamToUse, interventionStartDate,
                          interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment,
                          v1Value,v2Value,JValue,filterPSGCs){
  
  dateAdjustmentSearchSpace = 10
  lambdaDateInput = as.Date(strsplit(lambdaDateInput, ";")[[1]])
  lambdaValuesInput = as.numeric(strsplit(lambdaValuesInput, ";")[[1]])
  
  interventionStartDate = as.Date(interventionStartDate)
  interventionEndDate = as.Date(interventionEndDate)
  interventionPercentage = as.numeric(interventionPercentage)
  interventionCompliance = as.numeric(interventionCompliance)
  
  startHcDate = as.Date(startHcDate)
  HcLevel = as.numeric(HcLevel)
  filterPSGCs = strsplit(filterPSGCs, ";")[[1]]
  
  origProjectDuration = as.numeric(projectionDuration)
  projectionDuration = as.numeric(projectionDuration) + dateAdjustmentSearchSpace + 1
  scaleLevel = as.numeric(scaleLevel)
  dateAdjustment = as.logical(dateAdjustment)
  
  parGen_estimateLambda2 = function(){ #tinanggal yung engineer = TRUE
    parnames = c("tau", "beta", "c", "c1","c2","omega", "thetaA", "thetaS","r", 
                 "deltaA", "deltaA1","deltaA2","epsT","epsI","epsIH", 
                 "E1", "E2", "E3", "E4", "E5", "k1", "k2","mu")#"J"
    pargenDetails = psgcInput %>% as.numeric() %>% getPsgcDetails() %>% filter(FittedTo == fitParamToUse)
    
    deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
    
    result = c(
      pargenDetails['tau'], pargenDetails['beta'],# transmission
      pargenDetails['c'], pargenDetails['c1'], pargenDetails['c2'], pargenDetails['omega'], # transitions
      pargenDetails['thetaA'],pargenDetails['thetaS'], pargenDetails['r'], # recoveries
      0.25*deltaS.l[1], 0.1*deltaS.l[1], 0.1*deltaS.l[1], # deltaA
      pargenDetails['epsT'], pargenDetails['epsI'],pargenDetails['epsIH'],
      pargenDetails['E1'], pargenDetails['E2'], pargenDetails['E3'], pargenDetails['E4'], pargenDetails['E5'],
      pargenDetails['k1'],pargenDetails['k2'],
      pargenDetails['mu']#, pargenDetails['J'], pargenDetails['v1'], pargenDetails['v2'],
    )
    
    names(result) = parnames
    
    return(list(parameters = result, deltaS.l = deltaS.l))
  }
  
  pargenDetails <- getPsgcDetails(psgcInput) %>% filter(FittedTo == fitParamToUse)
  
  deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v1.l <- v1Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v2.l <- v2Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  J.l <- JValue %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  
  Covid19Model = function(t, state, parameters){
    with(as.list(c(state,parameters)),{
      if (t >= intervStartDate & t <= intervEndDate) {
        interventionEffect = (interventionPercentage * interventionCompliance)
      } else {
        interventionEffect = 0
      }
      
      if (t <= tail(equar.l, n = 1)) {
        reallam = (lambdaValuesInput[min(which(t <= equar.l))])
      } else {
        reallam = 0
      }
      
      
      startYear <- 2021
      startMonth <- 7
      
      computeYear <- format(first_date + t - 1, "%Y") %>% as.numeric()
      computeMonth <- format(first_date + t - 1, "%m") %>% as.numeric()
      
      parameter_index <- 12*computeYear + computeMonth - (12*startYear + startMonth) + 1
      
      deltaS <- ifelse(parameter_index >= length(deltaS.l),
                       deltaS.l[length(deltaS.l)],
                       deltaS.l[parameter_index])
      
      tv1 <- ifelse(parameter_index >= length(v1.l),
                    v1.l[length(v1.l)],
                    v1.l[parameter_index])
      
      tv2 <- ifelse(parameter_index >= length(v2.l),
                    v2.l[length(v2.l)],
                    v2.l[parameter_index])
      
      J <- ifelse(parameter_index >= length(J.l),
                  J.l[length(J.l)],
                  J.l[parameter_index])
      
      if(t >= setdate){
        deltaS = setdelt
      }
      

      deltaS1 <- deltaS
      deltaS2 <- deltaS
      deltaA <- 0.25*deltaS
      deltaA1 <- 0.1*deltaS
      deltaA2 <- 0.1*deltaS

      dS= A - (1-reallam)*beta*S*(IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 +E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu*S) - tv1 - J
      dE = (1-reallam)*beta*S*(IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu + 1/tau)*E
      dIA = c*E/tau - (mu + omega + deltaA + thetaA)*IA
      dIS = (1-c)*E/tau + omega*IA - (mu + epsI + deltaS + thetaS)*IS
      
      dV1 = tv1 - k1*(1-reallam)*beta*V1*(IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu*V1) -tv2
      dEv1 = k1*(1-reallam)*beta*V1*( IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R)-(mu + 1/tau)*Ev1
      dIAv1 = c1*Ev1/tau - (mu + omega + deltaA1 + thetaA)*IAv1
      dISv1 = (1-c1)*Ev1/tau + omega*IAv1 - (mu + epsIH + deltaS1 + thetaS )*ISv1
      
      dV2 = tv2 - k2*(1-reallam)*beta*V2*( IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 +E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R)- mu*V2 + J
      dEv2 = k2*(1-reallam)*beta*V2*( IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu + 1/tau)*Ev2
      dIAv2 = c2*Ev2/tau - (mu + omega + deltaA2 + thetaA)*IAv2
      dISv2 = (1-c2)*Ev2/tau + omega*IAv2 - (mu + deltaS2 + thetaS)*ISv2
      
      dQ = deltaA*IA + deltaS*IS + deltaA1*IAv1 + deltaS1*ISv1 + deltaA2*IAv2 + deltaS2*ISv2 - (mu + epsT + r)*Q
      dC = deltaA*IA + deltaS*IS + deltaA1*IAv1 + deltaS1*ISv1 + deltaA2*IAv2 + deltaS2*ISv2
      dR = r*Q + thetaA*(IA+IAv1+IAv2) + thetaS*(IS+ISv1+ISv2) - mu*R
      
      list(c(dS, dE, dIA, dIS, dV1, dEv1, dIAv1, dISv1, dV2, dEv2, dIAv2, dISv2, dQ, dC, dR))
    })
  }
  
  scen = which(as.numeric(summaries$area) == as.numeric(psgcInput) & summaries$FittedTo == fitParamToUse)
  
  first_date = as.Date(as.character(summaries[scen, 'first_date']))
  
  inputs = summaries[scen, ]
  inputs = suppressWarnings(as.numeric(inputs))
  names(inputs) = colnames(summaries)[1:ncol(summaries)]
  
  N = inputs['pop']
  A = N*0.020177/365
  
  equar.l = as.numeric((lambdaDateInput - first_date + 1))

  pars = parGen_estimateLambda2()$parameters
  
  setdate = as.numeric(startHcDate - first_date) + 1
  setdelt = HcLevel

  
  intervStartDate = as.numeric(interventionStartDate - first_date) + 1
  intervEndDate = as.numeric(interventionEndDate - first_date) + 1
  

     istates <- c(
      S = as.numeric(inputs['S0']),#inputs['E0']
      E = as.numeric(inputs['E0']),
      IA = as.numeric(inputs['IA0']),
      IS = as.numeric(inputs['IS0']),
      V1 = as.numeric(inputs['V10']),
      Ev1 = as.numeric(inputs['Ev10']),
      IAv1 = as.numeric(inputs['IAv10']),
      ISv1 = as.numeric(inputs['ISv10']),
      V2 = as.numeric(inputs['V20']),
      Ev2 = as.numeric(inputs['Ev20']),
      IAv2 = as.numeric(inputs['IAv20']),
      ISv2 = as.numeric(inputs['ISv20']),
      Q = as.numeric(inputs['Q0']),
      C = as.numeric(inputs['C0']),
      R = as.numeric(inputs['R0']))


# ###################  
  projtime = seq(1, projectionDuration, by = 1)
  check = ode(y=istates,time=projtime,func=Covid19Model,parms=pars)
  
  
  check = as.data.frame(check[,2:ncol(check)])
  
  dailyDeaths = c(0,diff(check$D))
  dailyRecovered =  c(0,diff(check$R))
  dailyCritical = check$Q*.06
  dailySevere = check$Q*.14
  dailyMild = check$Q*.80
  
  cumulativeCritical = check$C*.06
  cumulativeSevere = check$C*.14
  cumulativeMild = check$C*.80
  
  totalInfectious = check$IA + check$IS
  
  datesSequence = data.frame(Date = seq(first_date, 
                                        first_date + (nrow(check)-1), 
                                        1,))
  
  checkReturn = cbind(format(datesSequence, "%Y-%m-%d"), check, dailyDeaths, dailyRecovered,
                      dailyCritical, dailySevere, dailyMild,
                      cumulativeCritical, cumulativeSevere, cumulativeMild,
                      totalInfectious)
  

  # # Retain the else statement after all fit to cumulative are based on report date
  # if (deltaS.l[1] == 0.111028684 & fitParamToUse == "Cumulative") {
  #   actualIncidenceCounts = getActiveCases(psgcInput, "Active")
  # } else{
  #   actualIncidenceCounts = getActiveCases(psgcInput, fitParamToUse)
  # }
  
  actualIncidenceCounts = getActiveCases(psgcInput, fitParamToUse, filterPSGCs)
  
  
  if(dateAdjustment){
    maxDateIndex = which(as.character(checkReturn$Date) == maxDate)
    
    #Movement to match latest data and projections
    if (fitParamToUse == "Cumulative") {
      maxCumulative = tail(actualIncidenceCounts$CumulativeSumAdmitted, n=1)
      rangeOfCheck = checkReturn$C[(maxDateIndex-dateAdjustmentSearchSpace):(maxDateIndex+dateAdjustmentSearchSpace)]
      dateMovement = which(abs(rangeOfCheck-maxCumulative) == min(abs(rangeOfCheck-maxCumulative))) - (dateAdjustmentSearchSpace+1)
    } else {
      maxActive = tail(actualIncidenceCounts$ActiveCases, n=1)
      rangeOfCheck = checkReturn$Q[(maxDateIndex-dateAdjustmentSearchSpace):(maxDateIndex+dateAdjustmentSearchSpace)]
      dateMovement = which(abs(rangeOfCheck-maxActive) == min(abs(rangeOfCheck-maxActive))) - (dateAdjustmentSearchSpace+1)
    }
    
    datesSequence = data.frame(Date = seq(first_date - dateMovement, 
                                          first_date + (nrow(check)-1) - dateMovement, 
                                          1,))
    
    checkReturn = cbind(format(datesSequence, "%Y-%m-%d"),checkReturn[,c(2:(ncol(checkReturn)))])
  } else {
    dateMovement = 0
  }
  
  checkReturn = checkReturn %>% 
    dplyr::mutate(Date = as.character(Date))
  checkReturn = merge(x = checkReturn, y = actualIncidenceCounts, by = "Date", all.x = TRUE)
  
  if (scaleLevel != 0) {
    checkReturn = as.data.frame((checkReturn[,2:ncol(checkReturn)]/(inputs['pop']/inputs['popMultiplier']))*scaleLevel)
    checkReturn = cbind(datesSequence,checkReturn)
  }
  checkReturn$Date = as.Date(as.character(checkReturn$Date))
  psgcDetails = getPsgcDetails(psgcInput)
  firstDate = as.Date(psgcDetails[which(psgcDetails$FittedTo == fitParamToUse), "first_date"])
  lastDate = firstDate + origProjectDuration - 1
  checkReturn = checkReturn[which(checkReturn$Date >= firstDate & checkReturn$Date <= lastDate),]
  
  
  ########### OTHER DETAILS ###########
  
  peakIndex = which(checkReturn$Q == max(checkReturn$Q))
  peakDate = checkReturn[peakIndex,"Date"]
  peakConfirmed = checkReturn[peakIndex,"Q"]
  peakMortality = checkReturn[peakIndex, "dailyDeaths"]
  peakSevere = peakConfirmed*.14
  peakCritical = peakConfirmed*.06
  
  epidemicEndIndex = suppressWarnings(min(which(
    checkReturn$IA < 1 &
      checkReturn$IS < 1 &
      checkReturn$Q < 1
  )))
  
  epidemicEndDate = checkReturn[epidemicEndIndex,"Date"]
  
  otherInfo = data.frame(peakDate = as.character(peakDate),
                         epidemicEndDate = as.character(epidemicEndDate),
                         peakConfirmed = peakConfirmed,
                         peakMortality = peakMortality,
                         peakSevere = peakSevere,
                         peakCritical = peakCritical,
                         dateAdjustment = dateMovement)
  
  ##############
  
  returnValues = list(OdeResult = checkReturn, OtherDetails = otherInfo)
  return(returnValues)
}

# ##### TEST RUN -------------
# if (TRUE){
  # HcLevel = "0.159519" # removed feature 2020-12-16
  # dateAdjustment = "FALSE"
  # fitParamToUse = "Cumulative"
  # interventionCompliance = "0"
  # interventionEndDate = "2025-01-01"
  # interventionPercentage = "0"
  # interventionStartDate = "2021-12-01"
  # lambdaDateInput = "2021-07-31;2021-08-31"
  # lambdaValuesInput = "0.696074072033914;0"
  # projectionDuration = "100"
  # psgcInput = "130000000"
  # scaleLevel = "0"
  # startHcDate = "2021-08-01" # removed feature 2020-12-16
  # vax1 = "36537;115189" #37754,45833 65574,45833
  # vax2 = "67602;37511"
  # jvax = "5905;6230"
  # filterPSGCs = "137602;137401;137403" #if no filters just input ;
  # output = RunProjections(lambdaDateInput,
  #                         lambdaValuesInput,
  #                         psgcInput,
  #                         startHcDate,
  #                         HcLevel,
  #                         projectionDuration,
  #                         scaleLevel,
  #                         fitParamToUse,
  #                         interventionStartDate,
  #                         interventionEndDate,
  #                         interventionPercentage,
  #                         interventionCompliance,
  #                         dateAdjustment,vax1,vax2,jvax,filterPSGCs)
  # oderes = output$OdeResult
# 
#   library(ggplot2)
#   library(reshape2)
#   if (fitParamToUse == "Cumulative"){
#     dfToGraph <- oderes %>%
#       dplyr::select(Date, C, CumulativeSumAdmitted)
#   } else {
#     dfToGraph <- oderes %>%
#       dplyr::select(Date, Q, ActiveCases)
#   }
# 
#   dfToGraph %>%
#     dplyr::filter(Date < Sys.Date()) %>%
#     melt(id = "Date") %>%
#     ggplot(aes(x = Date, y = value, color = variable)) +
#     geom_line(stat = "identity", position = "identity", size = 1.2)
# }
