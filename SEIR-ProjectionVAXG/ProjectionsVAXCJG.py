import requests
import redis
import pandas as pd
import numpy as np
import math
import os
import csv
import glob
import plotly
import plotly.graph_objs as go
from matplotlib import pyplot as plt
from datetime import datetime
import shutil
import copy
from HSC import *
from datetime import date
import xlsxwriter



def allcompartments(dfOutput,dfOdeResult, colName,startCIndex,end_date):
    print("All Compartments")
    
    colNameS = "S-" + colName
    colNameE = "E-" + colName
    colNameR = "R-" + colName
    colNameIS = "IS-" + colName
    colNameIA = "IA-" + colName
    #VAX1
    colNameV1 = "V1-" + colName 
    colNameEv1 = "Ev1-" + colName 
    colNameISv1 = "ISv1-" + colName 
    colNameIAv1 = "IAv1-" + colName 
    #VAX2
    colNameV2 = "V2-" + colName 
    colNameEv2 = "Ev2-" + colName 
    colNameISv2 = "ISv2-" + colName 
    colNameIAv2 = "IAv2-" + colName 

    dfOutput[colNameS] = dfOdeResult['S'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameE] = dfOdeResult['E'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameIS] = dfOdeResult['IS'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameIA] = dfOdeResult['IA'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameR] = dfOdeResult['R'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    #VAX1
    dfOutput[colNameV1] = dfOdeResult['V1'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameEv1] = dfOdeResult['Ev1'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameISv1] = dfOdeResult['ISv1'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameIAv1] = dfOdeResult['IAv1'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    #VAX2
    dfOutput[colNameV2] = dfOdeResult['V2'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameEv2] = dfOdeResult['Ev2'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameISv2] = dfOdeResult['ISv2'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfOutput[colNameIAv2] = dfOdeResult['IAv2'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    
    # dfOutput['dailyDeaths'] = dfOdeResult['dailyDeaths'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['dailyRecovered'] = dfOdeResult['dailyRecovered'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['dailyCritical'] = dfOdeResult['dailyCritical'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['dailySevere'] = dfOdeResult['dailySevere'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['dailyMildR'] = dfOdeResult['dailyMild'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['cumulativeCritical'] = dfOdeResult['cumulativeCritical'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['cumulativeSevere'] = dfOdeResult['cumulativeSevere'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['cumulativeMild'] = dfOdeResult['cumulativeMild'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    # dfOutput['totalInfectious'] = dfOdeResult['totalInfectious'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    return dfOutput

def csvRequirements(path,dfOutput,colName,dfOdeResult,dfHospital,startCIndex,end_date,data_dfPsgcEconDetails,treatlevel,data_geo,fitness,dateAdjValue,ic,ip,title):
    print("Projected Requirements")
    dfHospital['IS'] = dfOdeResult['IS'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfHospital['IA'] = dfOdeResult['IA'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfHospital['dailyMild'] = dfOdeResult['dailyMild'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfHospital['dailySevere'] = dfOdeResult['dailySevere'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfHospital['dailyCritical'] = dfOdeResult['dailyCritical'][startCIndex:startCIndex+end_date].reset_index(drop=True)
                                
    dfHospital['S'] = dfOdeResult['S'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfHospital['R'] = dfOdeResult['R'][startCIndex:startCIndex+end_date].reset_index(drop=True)
    dfHospital['E'] = dfOdeResult['E'][startCIndex:startCIndex+end_date].reset_index(drop=True)

    dates = dfOutput["Date"].to_list()
                        
    dailySevere = dfHospital['dailySevere'].to_list()
    dailyCritical = dfHospital['dailyCritical'].to_list()

    total_confirmed = dfOutput[colName].to_list()
    symptomatic = dfHospital['IS'].to_list()

    asymptomatic = dfHospital['IA'].to_list()
    dailyMild = dfHospital['dailyMild'].to_list()

    susceptible = dfHospital['S'].to_list()
    exposed = dfHospital['E'].to_list()
    cumulative_recovered = dfHospital['R'].to_list()

    hc_parameter = {"data": { "Critical Confirmed": total_confirmed, "Severe Confirmed": dailySevere } } 
    hc_hospital_data = compute_hospital_parametersV2(hc_parameter)
                                
    testing_diagnostic_parameter = {"data": { "Symptomatic": symptomatic, "Confirmed": total_confirmed } }
    testing_diagnostic_data = compute_testing_diagnostic_parameters(testing_diagnostic_parameter)
                                                    
    clc_parameterV2 = {'data':  {'Symptomatic': symptomatic,
                                                        'Asymptomatic': asymptomatic,
                                                        'Mild': dailyMild }
                                                }

    hc_ligtascenter_data = compute_clc_parametersV2(clc_parameterV2)
                                
    try: 
        if data_dfPsgcEconDetails:       
            econ_parameter = {'data':  { 'S' :susceptible, 'E':exposed, 'Ia':asymptomatic,
            'R' :cumulative_recovered,'C': total_confirmed,'Is': symptomatic,
            'H1': data_dfPsgcEconDetails[0]['H1'],'H2': data_dfPsgcEconDetails[0]['H2'],
            'L1': data_dfPsgcEconDetails[0]['L1'],'L2': data_dfPsgcEconDetails[0]['L2'],
            'ECQ': data_dfPsgcEconDetails[0]['ECQ'],'MECQ': data_dfPsgcEconDetails[0]['MECQ'],
            'GCQ': data_dfPsgcEconDetails[0]['GCQ'],'MGCQ': data_dfPsgcEconDetails[0]['MGCQ'],
            'ECQ2': data_dfPsgcEconDetails[0]['ECQ2'],'MECQ2': data_dfPsgcEconDetails[0]['MECQ2'],
            'GCQ2': data_dfPsgcEconDetails[0]['GCQ2'],'MGCQ2': data_dfPsgcEconDetails[0]['MGCQ2'],
            'ECQ3': data_dfPsgcEconDetails[0]['ECQ3'],'MECQ3': data_dfPsgcEconDetails[0]['MECQ3'],
            'GCQ3': data_dfPsgcEconDetails[0]['GCQ3'],'MGCQ3': data_dfPsgcEconDetails[0]['MGCQ3'],
            'CQ_Classification':data_dfPsgcEconDetails[0]['CQ.Classification'],'CQ_Dates': data_dfPsgcEconDetails[0]['CQ.Dates'],
            'CQ_Values': data_dfPsgcEconDetails[0]['CQ.Values']} }
   
 
    except Exception as e:
        print(e)

    hc_hospital_data['date'] = dates
    testing_diagnostic_data['date'] = dates
    hc_ligtascenter_data['date'] = dates

    parent_dir = '/home/lenard.tamayo/backend-projections/SEIR-ProjectionVAXG/Projections/'+ str(title) + '/' + str(data_geo) #+ '/' + str(dateAdjustment) + '/' 
    path = os.path.join(parent_dir) 

    if not os.path.exists(path):
        os.makedirs(path,mode=0o777)
        
    hc_hospital_data.to_csv(path_or_buf=os.path.join(path, "{}%HSC-{}-[{}][{}][{}][{}]-Hospital.csv".format((treatlevel*100),data_geo, str(fitness), str(dateAdjValue),(ic*100),(ip*100))))#,(ic*100),(ip*100))))
    testing_diagnostic_data.to_csv(path_or_buf=os.path.join(path, "{}%HSC-{}-[{}][{}][{}][{}]-Testing.csv".format((treatlevel*100),data_geo, str(fitness), str(dateAdjValue),(ic*100),(ip*100))))#,(ic*100),(ip*100))))
    hc_ligtascenter_data.to_csv(path_or_buf=os.path.join(path, "{}%HSC-{}-[{}][{}][{}][{}]-CommunityCenter.csv".format((treatlevel*100),data_geo, str(fitness), str(dateAdjValue),(ic*100),(ip*100))))#),(ic*100),(ip*100))))                                
                                # econ_data.to_csv(path_or_buf=os.path.join(path, "{}%HSC-{}-[{}][{}][{}][{}]-Econ.csv".format((treatlevel*100),data[0]['Geographical.Place'], str(fitness), str(dateAdjValue[a]),(ic*100),(ip*100))))#),(ic*100),(ip*100))))                                
    return dfOutput

def massGen(title,locations,scale,fitTo,lambda1DateInput,lambdaValueInput1,startHcDate,treatlevels, projectionDuration,
interventionCompliance,interventionPercentage, interventionStartDate,interventionEndDate, dateAdjustment, start_date,end_date, dateAdjValue, allCompartments,csvOn,vax1,vax2,jvax,filterPSGC):#,vaxDate
    
    TIMESERIES_URL = "http://localhost:8866"
    #TIMESERIES_URL = "http://fassster.ehealth.ph/timeseries"

    locdate = []
    a = 0
    # 
    # dirtitle = str(title) + "-" + str(date.today())
    # directory = dirtitle #+ '/' + str(fitTo) #+ '/' + str(dateAdjustment)
    # 
   
    dirtitle = str(locations[0]) + "-" + str(date.today())+"-"+fitTo
    directory = dirtitle #+ '/' + str(fitTo) #+ '/' + str(dateAdjustment)
    
    parent_dir = "/home/lenard.tamayo/backend-projections/SEIR-ProjectionVAXG/Projections/"
    path = os.path.join(parent_dir, directory) 

    if not os.path.exists(path):
        os.makedirs(path,mode=0o777)
        
    print(title)

    filename_log = "logs--{0}.txt".format(datetime.now().strftime("%Y-%m-%d")) #"param_log.txt" 
    logfit = "{0} -- {1} ".format(title ,fitTo)
    
    logParameters(logfit, filename_log)

    for location in locations:
        print(location)
        location = str(location)
        urlPsgcDetails = "{0}/getPsgcDetails?psgcInput={1}".format(TIMESERIES_URL,int(location))
        urlPsgcEconDetails = "{0}/getPsgcEconDetails?psgcInput={1}".format(TIMESERIES_URL,int(location))

        getPsgcDetails = requests.get(urlPsgcDetails)
        dataPsgcDetails = getPsgcDetails.json()
        dfPsgcDetails = pd.DataFrame.from_dict(dataPsgcDetails)
        data = dfPsgcDetails.to_dict('records')
        temp = dfPsgcDetails.set_index('FittedTo').to_dict("index")
        data = [temp[fitTo]] if fitTo in temp.keys() else []

        getPsgcEconDetails = requests.get(urlPsgcEconDetails)
        dataPsgcEconDetails = getPsgcEconDetails.json()
        dfPsgcEconDetails = pd.DataFrame.from_dict(dataPsgcEconDetails)
        data_dfPsgcEconDetails = dfPsgcEconDetails.to_dict('records')
        
        if data_dfPsgcEconDetails: #if not null
            print("Economics Variables")# pprint.pprint(data_dfPsgcEconDetails)
        econ_output = {}
        econ_data = []
        
        if lambda1DateInput is None:
            lambdaDateInput = data[0]['equar_date']
            lambdaValuesInput = data[0]['lambdaValues']
            vax1 = data[0]['tv1']
            vax2 = data[0]['tv2']
            jvax = data[0]['J']
            deltaSInput = data[0]['deltaS'].split(";")
            treatlevels = deltaSInput[-1:]
            
            #if isa pa lang si deltaS from parametersVAX.csv
            # deltaSInput = data[0]['deltaS']#.split(";")
            # treatlevels = deltaSInput#[-1:]
            #print(treatlevels)

        else:  
            # lambdaDates = data[0]['equar_date'].split(';')
            # lambdaValues = data[0]['lambdaValues'].split(';')
            
            # lambdaDates = lambdaDates[0] #0 July [0:1]
            # lambdaValues = lambdaValues[0] #0

            # lambda_dates = []
            # lambda_values = []

            # lambda_dates.append(lambdaDates)
            # lambda_values.append(lambdaValues)
            
            #if isa pa lang si deltaS from parametersVAX.csv
            # deltaSInput = data[0]['deltaS']
            # treatlevels = []
            # treatlevels.append(deltaSInput)

            if lambda1DateInput: 
                
                lambda1DateInput = lambda1DateInput.split(';')
                lambda_dates =  lambda1DateInput #+ lambdaDates
                
                lambdaValueInput1 = lambdaValueInput1.split(';')
                lambda_values = lambdaValueInput1 #+ lambdaValues
                
                if len(treatlevels) == 0:
                    deltaSInput = data[0]['deltaS'].split(";")
                    treatlevels = deltaSInput[-1:]
                
                print(treatlevels)

            
            lambdaDateInput = ";".join(map(str, lambda_dates))
            lambdaValuesInput = ";".join(map(str, lambda_values))
            # logParameters(lambda_dates, filename_log)
            # logParameters(lambda_values, filename_log)

        fitness = data[0]["fitness"]
        
        fitness = fitness * 100
        v1 = vax1# * 100
        v2 = vax2# * 100
        
        if len(data) > 0:
            dfOutput = pd.DataFrame()
            dfHospital = pd.DataFrame()

            for treatlevel in treatlevels:
                for ic in interventionCompliance:
                    for ip in interventionPercentage:
                        
                        params = {
                            "psgcInput": location, "projectionDuration": projectionDuration, "lambdaDateInput": lambdaDateInput,
                            "lambdaValuesInput": lambdaValuesInput, "HcLevel": treatlevel, "startHcDate": startHcDate,
                            "scaleLevel": scale, "fitParamToUse" : fitTo, "interventionCompliance" : ic, "interventionPercentage": ip,
                            "interventionEndDate" : interventionEndDate, "interventionStartDate" : interventionStartDate, "dateAdjustment": dateAdjustment,
                            "v1Value": vax1,"v2Value": vax2, "jValue": jvax, "filterPSGCs": filterPSGC #"vaxDate": vaxDate,
                        }
                        urlProjections = "{0}/getProjections".format(TIMESERIES_URL)
                        getProjections = requests.get(urlProjections,params=params)
                        
                        param_settings = urlProjections,params

                        logParameters("deltaS {0} -- {1}".format(treatlevel,startHcDate), filename_log)
                        # loglambdaDates = lambdaDateInput.split(";")
                        # loglambdaValues = lambdaValuesInput.split(";")
                        # 
                        # loglambdaDates = loglambdaDates[-4:]
                        # loglambdaValues = loglambdaValues[-4:]

                        # logParameters(loglambdaDates, filename_log)
                        # logParameters(loglambdaValues, filename_log)
                        #loglambdaDates, loglambdaValues LambdaDate: {1}\nLambdaValues: {2}\n
                        print_params = "PSGC: {0}\nDeltaS: {1}\nStartHSC: {2}\nFit: {3}\n".format(location, treatlevel, startHcDate, fitTo)
                        logParameters(print_params, filename_log)
                        logParameters(param_settings, filename_log)
                        
                        print(print_params)
                        print(param_settings)

                        dataProjections = getProjections.json()
                        OdeResult = dataProjections['OdeResult']
                        dfOdeResult = pd.DataFrame.from_dict(OdeResult)
                        
                        OtherDetails= dataProjections['OtherDetails']
                        dfOtherDetails = pd.DataFrame.from_dict(OtherDetails)
                        data_OtherDetails = dfOtherDetails.to_dict('records')
                        dateAd = data_OtherDetails[0]['dateAdjustment']

                        # # matic get the date
                        if start_date == '2021-07-01':
                            data_dOdeResult = dfOdeResult.to_dict('records')
                            start_date = data_dOdeResult[0]['Date']
                        tlvl = float(treatlevel)*100
                        colName = "{0} HSC {1}%".format(title,tlvl)#[{2}][{3}] ,v1,v2
    
                        if fitTo == 'Cumulative':
                            startDateIndex = dfOdeResult.index[dfOdeResult['Date'] == start_date][0]
                            startCIndex = startDateIndex + dateAdjValue[a] #date shift adjust value

                            dfOutput["Date"] = dfOdeResult['Date'][startDateIndex:startDateIndex+end_date].reset_index(drop=True)
                            dfOutput['CumulativeSumAdmitted'] = dfOdeResult['CumulativeSumAdmitted'][startDateIndex:startDateIndex+end_date].reset_index(drop=True)
                            dfOutput[colName] = dfOdeResult['C'][startCIndex:startCIndex+end_date].reset_index(drop=True)

                            if allCompartments == 1:
                                allcompartments(dfOutput,dfOdeResult,colName,startCIndex,end_date)

                        else: #else if ACTIVE
                            startDateIndex = dfOdeResult.index[dfOdeResult['Date'] == start_date][0]
                            startCIndex = startDateIndex + dateAdjValue[a]

                            dfOutput["Date"] = dfOdeResult['Date'][startDateIndex:startDateIndex+end_date].reset_index(drop=True)
                            dfOutput['ActiveCases'] = dfOdeResult['ActiveCases'][startDateIndex:startDateIndex+end_date].reset_index(drop=True)
                            dfOutput[colName] = dfOdeResult['Q'][startCIndex:startCIndex+end_date].reset_index(drop=True)
                            
                            if allCompartments == 1:
                                allcompartments(dfOutput,dfOdeResult,colName,startCIndex,end_date)
                            
                            #uncomment this to produce hospital,testing and community center csv
                            if csvOn == 1:
                                csvRequirements(path,dfOutput,colName,dfOdeResult,dfHospital,startCIndex,end_date,data_dfPsgcEconDetails,treatlevel,data[0]['Geographical.Place'],fitness,dateAdjValue[a],ic,ip,dirtitle)


        parent_dir = '/home/lenard.tamayo/backend-projections/SEIR-ProjectionVAXG/Projections/'+ str(dirtitle) #+ '/' + str(dirtitle+fitTo)#+ '/' + str(data[0]['Geographical.Place']) #+ '/' + str(dateAdjustment) + '/' 
        path = os.path.join(parent_dir) 
        
        if not os.path.exists(path):
            os.makedirs(path,mode=0o777)
        
        if dateAdjustment == "TRUE":
            #dfOutput.to_csv(path_or_buf=os.path.join(path,"{}-{}-{}[{}][{}]v1[{}]v2[{}].csv".format(data[0]['Geographical.Place'],str(title),str(fitTo),str(fitness),str(dateAd),str(v1),str(v2),index=False,quoting=csv.QUOTE_NONNUMERIC)))
            # dfOutput.to_csv(path_or_buf=os.path.join(path,"{}-{}-{}[{}]v1[{}]v2[{}].csv".format(str(dirtitle),str(location),str(fitTo),str(fitness),str(v1),str(v2),index=False,quoting=csv.QUOTE_NONNUMERIC)))
            dfOutput.to_csv(path_or_buf=os.path.join(path,"{}-{}-{}[{}]v1[{}]v2[{}].csv".format(str(dirtitle),str(location),str(fitTo),str(fitness),str(v1),str(v2),index=False,quoting=csv.QUOTE_NONNUMERIC)))

            # uploadToGdrive()
        else:
            #dfOutput.to_csv(path_or_buf=os.path.join(path,"{}-{}-{}[{}][{}]v1[{}]v2[{}].csv".format(data[0]['Geographical.Place'],str(title),str(fitTo),str(fitness),str(dateAdjValue[a]),str(v1),str(v2),index=False,quoting=csv.QUOTE_NONNUMERIC)))
            # dfOutput.to_csv(path_or_buf=os.path.join(path,"{}-{}-{}[{}]v1[{}]v2[{}].csv".format(str(location),str(title),str(fitTo),str(fitness),str(v1),str(v2),index=False,quoting=csv.QUOTE_NONNUMERIC)))
            dfOutput.to_csv(path_or_buf=os.path.join(path,"{}.csv".format(str(title),index=False,quoting=csv.QUOTE_NONNUMERIC)))
            # uploadToGdrive()
       
        locdate.append(dateAd)
        a = a+1
        
    return locdate


def logParameters(param, filename):
    path = "/home/lenard.tamayo/backend-projections/SEIR-ProjectionVAXG/Projections/"+filename
    f = open(path, "a")
    f.write("{0} -- {1}\n".format(datetime.now().strftime("%Y-%m-%d %H:%M"), param))
    f.close()

def listToString(s):      
    # return string   
    return (";".join(s)) 
    
def uploadToGdrive():
    print("Sample")



    
        
