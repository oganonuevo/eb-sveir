from ProjectionsVAX import *

def combineToExcel(location,fitTo):
    import pandas as pd
    import xlsxwriter
    import glob
    import os

    title = str(location[0]) + "-" + str(date.today()) +"-"+ fitTo
    filename = "C:/Users/ogano/Desktop/FASSSTER/backend-projections/SEIR-ProjectionVAX/Projections/{0}/Summary-{1}.xlsx".format(title,title)
    path = "C:/Users/ogano/Desktop/FASSSTER/backend-projections/SEIR-ProjectionVAX/Projections/{0}/".format(title)
    all_files = glob.glob(os.path.join(path, "*.csv"))

    writer = pd.ExcelWriter(filename, engine='xlsxwriter')
    
    for f in all_files:
        df = pd.read_csv(f)
        df.to_excel(writer, sheet_name=os.path.basename(f)[:31])
    
    writer.save()

df = pd.read_csv("C:/Users/ogano/Desktop/FASSSTER/backend-projections/SEIR-ProjectionVAX/scenario-files/Oct20.csv")#Name of Scenario file
for index, row in df.iterrows():
    folder_title = row['scenariotitle']
    location = [int(row['psgcInput'])]
    fitTo = row['fitTo']
    lambdaDate = row['lambdaDates']
    lambdaValue = row['lambdaValues']
    #treatlvl = []  #deltaS 0.159519
    #treatlvl = str(row['hsc']).split()
    treatlvl = [row['hsc']]
    #startHC = str(datetime.strptime(row['starthsc'], '%m/%d/%y').date())
    startHC = str(datetime.strptime(row['starthsc'], '%Y-%m-%d').date())
    # vaxDate = row['vaxDate']
    vax1 = row['vax1']
    vax2 = row['vax2']
    jvax = row['jvax']
    #intervention variables
    ic = [0] #compliance rate
    ip = [0] #kappa
    istart = "2020-12-01" #compliance start
    iend = "2025-01-01" #compliance end

    duration = "365" #Projection duration
    scale = 0

    #csv settings
    adjust = "FALSE" #TRUE - w/ date adjustment or FALSE w/o (Adjust values)
    #projStart = str(datetime.strptime(row['projstart'], '%m/%d/%y').date()) # output start of projection
    projStart = str(datetime.strptime(row['projstart'], '%Y-%m-%d').date()) # output start of projection
    projEnd = 180 # output end of projection

    allCompartments = row['allcompartments'] #1-Show All Compartments(S, E, IA, IS, R)
    csvOn = 0 #1 to produce Hospital, Testing and TTMF csv

    # #if adjust equals to TRUE will put values to this list
    dateADJ = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

    dateAdjust  = massGen(folder_title, location, scale, fitTo, lambdaDate,lambdaValue, startHC, treatlvl, duration,
                    ic, ip, istart, iend, adjust ,projStart, projEnd, dateADJ, allCompartments, csvOn,vax1,vax2,jvax)#vaxDate
    
combine = 0
 
if combine == 1:
    combineToExcel(location,fitTo)

