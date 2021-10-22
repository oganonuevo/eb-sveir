import requests
import redis

import pandas as pd
import numpy as np
import math
import os
import csv
import pprint
from datetime import datetime


def compute_hospital_parameters(data):
    '''
    Convert df data to health logistic requirements for hospitals.
    ---
    Parameters:
    JSON format - {'data': {'Critical Confirmed': [1,2],
                            'Severe Confirmed': [1,2]}}
    ---
    Output:
    df_output - dict/json, requirements contents
    '''
    df = pd.DataFrame(data['data'])


    df_output = pd.DataFrame(index = df.index)
    df_output['ICU Beds'] = df['Critical Confirmed']/2
    df_output['Regular Beds'] = (df['Critical Confirmed']/2 +
                                df['Severe Confirmed'])
    df_output['Ventilators'] = df_output['ICU Beds']
    df_output['Suction Machines'] = df_output['ICU Beds']
    df_output['ICU Doctors'] = (df_output['ICU Beds']/4)*3
    df_output['ICU Nurse'] = (df_output['ICU Beds']/2)*3
    df_output['Reg Doctors'] = (df_output['Regular Beds']/10)*3
    df_output['Reg Nurse'] = (df_output['Regular Beds']/7)*3

    df_output['Medtech - venous'] = (df_output['ICU Beds']/20 +
                                    df_output['Regular Beds']/140)
    df_output['Medtech - arterial'] = df_output['ICU Beds']/80

    df_output['Respiratory Therapists'] = df_output['Regular Beds']/6
    df_output['Hospital Full PPE'] = df_output[['ICU Doctors', 'ICU Nurse', 'Reg Doctors',
                            'Reg Nurse', 'Medtech - venous', 'Medtech - arterial',
                            'Respiratory Therapists']].sum(axis=1)*2
    df_output['N95 respirators'] = 2*(df_output[['ICU Doctors', 'ICU Nurse', 'Medtech - arterial']].sum(axis=1) + df_output['ICU Beds']/20) #ICU Beds/20 == Medtech - venous in ICU
    df_output['Hospital - Patient Masks'] = df_output[['ICU Beds', 'Regular Beds']].sum(axis=1)*3

    return df_output

def compute_clc_parameters(data):
    '''
    Convert df data to health logistic requirements for Covid Ligtas Centers.
    ---
    Parameters:
    JSON format - {'data': {'Total Confirmed': [1,2],
                                'Symptomatic Not Confirmed': [1,2],
                                'Mild Confirmed': [1,2],
                                'Total Susceptible': [1,2],
                                'Recovery': [1,2]
                            },
                    'positive_test_ratio': 0.17,
                    'new_removed_ratio': 1,
                    'isolate_possibility_ratio': 0}
    positive_test_ratio - float, value of positive tests / total tests. To be divided on the total confirmed and symptomatic to compute number of PUIs.
    isolate_possibility_ratio: percentage of PUIs that can self-isolate (not living with an elderly, with separate rooms and adequate air flow). 0 means every PUI stays in a Covid Ligtas Center.
    new_removed_ratio: ratio of new PUIs / removed PUIs. A value of 1 means that no PUIs are removed. To be multiplied to the difference of PUIs per day to calculate number of new PUIs for tests.
        Notes on removed patients: They get removed through negative tests, getting better, being moved to a hospital, or having died.
        - Target test result time is 2 days.
        - Patients who get better gets removed after 7-14 days from their last symptom.
    ---
    Output:
    df_output - dict/json, requirements contents
    '''
    df = pd.DataFrame(data['data'])
    positive_test_ratio = data['positive_test_ratio']
    new_removed_ratio = data['new_removed_ratio']
    isolate_possibility_ratio = data['isolate_possibility_ratio']

    df_output = pd.DataFrame(index = df.index)
    df_output['PUI Beds'] = pd.DataFrame(
        [df[['Total Confirmed',
            'Symptomatic Not Confirmed']].sum(axis=1)/positive_test_ratio * (
            1 - isolate_possibility_ratio),
        df['Total Susceptible']]).T.min(axis=1)
    df_output['CLC Confirmed Beds'] = df['Mild Confirmed']
    df_output['CLC Doctors'] = df_output['PUI Beds']/30
    df_output['CLC Nurses/Midwives'] = df_output['PUI Beds']/30 * 3 * 2
    df_output['CLC Full PPE'] = df_output['CLC Doctors'] + 2*df_output['CLC Nurses/Midwives']
    df_output['CLC Patient Masks'] = (df_output['PUI Beds'] + df_output['CLC Confirmed Beds']) * 3
    df_output['Test Kits - Infection'] = [max(x, y) for x, y in
            list(zip((df_output['PUI Beds'].diff() * new_removed_ratio).values,
                                        [0]*len(df_output.index)))]
    df_output['Test Kits - Recovery'] = df['Recovery'] * 2
    df_output['Test Swabs'] = (df_output['Test Kits - Infection'] +
                            df_output['Test Kits - Recovery'])*2

    return df_output


#new formulas on Hospital Capacity Req Updated as of July 9, 2020
#should be renamed into Hospital and Testing Capacity July 23, 2020
def compute_hospital_parametersV2(data):
    '''
    Convert df data to health logistic requirements for hospitals.
    ---
    Parameters:
    JSON format - {'data': {'Critical Confirmed': [1,2],
                            'Severe Confirmed': [1,2]}}
    ---
    Output:
    df_output - dict/json, requirements contents
    '''
    df = pd.DataFrame(data['data'])

    
    df_output = pd.DataFrame(index = df.index)

    #Hospital capacity
    #Bed Capacity
    df_output['ICU Beds'] = df['Critical Confirmed']*.01  #updates 2021
    df_output['Regular Beds'] = df['Severe Confirmed'] #(df['Critical Confirmed']/2 + ) #same as prev
    df_output['ER Beds'] = df['Severe Confirmed']/8 #ER Beds (for Covid-related cases, assuming 8-hr stay)

    #ICU Staff Capacity
    df_output['ICU Nurse'] = (df_output['ICU Beds']/2)*3 #same as prev
    df_output['ICU Doctors'] = (df_output['ICU Beds']/4)*3 #same as prev
    df_output['Proning Personnel'] = (df_output['ICU Beds']*1*5) #assumes  max (5 per adult, 2-3 per pedia), proning every 16 hrs (minimum) 

    #Emergency staff
    df_output['ER Nurse'] = (df_output['ER Beds']/6)*3 #(1 nurse per 6 ER beds, 3 shifts)
    df_output['ER Doctors'] = (df_output['ER Beds']/6)*3 #(1 doctor per 4 ER beds, 3 shifts)

    #Equipment
    df_output['Ventilators'] = df_output['ICU Beds'] #same as prev
    df_output['Suction Machines'] = df_output['ICU Beds'] #same as prev
    
    #HCW for severe & critical (non-ICU, inpatient regular)
    df_output['Reg Doctors'] = (df_output['Regular Beds']/10)*3 #same as prev
    df_output['Reg Nurse'] = (df_output['Regular Beds']/5)*3 #Nurses=(Regular Beds/7)*3 changed 7 into 5

    #Other medical staff
    #(assumptions:  ICU: 1 extraction per ICU patient per day, each Medtech can do 10 extractions in an 8-hr shift,
    #Regular Beds: only 50% of Regular Bed patients need extraction, each Medtech can do 20 extractions in an 8-hr shift)
    #old formula - Medtech - venous = ICU Beds/20 + Regular Beds/140
    df_output['Medtech - venous'] = (df_output['ICU Beds']/10 + (df_output['Regular Beds']/2)/20)
    
    #(assumes 3 extractions per day per ICU patient, each Medtech can do 12 extractions in an 8-hr shift)
    df_output['Medtech - arterial'] = (df_output['ICU Beds']*3)/12 #old formula -Medtech - arterial = ICU Beds/80

    #(assumptions:  ICU: 1 visit per ICU patient per day, each therapist can handle 6 ICU patients in an 8-hr shift,
    #Regular Bed:  only 50% of Regular Bed patients will be visited per day)
    #old formula - Respiratory Therapists = Regular Beds / 6
    df_output['Respiratory Therapists'] = df_output['ICU Beds']/6 + (df_output['Regular Beds']/2)/6
    
    #included Proning Personnel, ER Nurse and ER Doctors
    df_output['Hospital Full PPE'] = df_output[['ICU Doctors', 'ICU Nurse', 'Proning Personnel','ER Nurse',
                            'ER Doctors', 'Reg Doctors','Reg Nurse', 'Medtech - venous', 'Medtech - arterial',
                            'Respiratory Therapists']].sum(axis=1)*2
    
    ## Removed - July 23 based on the suggestion on July, 21
    # df_output['N95 respirators'] = 2*(df_output[['ICU Doctors', 'ICU Nurse',
    #                                     'Medtech - arterial']].sum(axis=1) +
    #                                     df_output['ICU Beds']/20) #ICU Beds/20 == Medtech - venous in ICU
    # df_output['Hospital - Patient Masks'] = df_output[['ICU Beds',
    #                                         'Regular Beds']].sum(axis=1)*3

    return df_output

#new formulas on Testing/ Diagnostic Capacity Updated as of July 10, 2020
def compute_testing_diagnostic_parameters(data):
    '''
    Convert df data to health logistic requirements for Testing/ Diagnostic Capacity.
    ---
    Parameters: IS & delta_S (Confirmed)
    JSON format - {"data": { "Symptomatic": symptomatic, "Confirmed": total_confirmed } }
    ---
    Output:
    df_output - dict/json, requirements contents
    '''
    df = pd.DataFrame(data['data'])
    df_output = pd.DataFrame(index = df.index)
   ##added Testing Capacity formulas July 23
    #Testing (RT PCR) (for infection, not recovery)
    #May take positivity ratio = 10%
    positivityratio = .10
    delta_S = .19
    df_output['Testing'] = df['Symptomatic']  *  (delta_S)/positivityratio
    
    #HCW = Test kits/ (8*3)
    df_output['Healthcare worker'] = df_output['Testing'] / (8*3)

    #Diagnostic PPE set = HCW * 2
    df_output['Diagnostic PPE set'] = df_output['Healthcare worker'] * 2

    #(Outer) gloves = Test kits
    df_output['Outer gloves'] = df_output['Testing']

    return df_output


#Community capacity requirements
def compute_clc_parametersV2(data):
    '''
    Convert df data to health logistic requirements for Community capacity requirements.
    ---
    Parameters:
    JSON format - 'data': { 
                            'Symptomatic': symptomatic,
                             'Asymptomatic': asymptomatic,
                              'Mild': mild_confirmed
                                    }
                                }

    Mild = Confirmed * 80% 
    (assume that mild = mild + asymptomatic + moderate confirmed; mild with no comorbidities and with ages 18-60)

    Suspect/probable = (infectious asymptomatic + infectious symptomatic)/ (positivity rate)
    (assume that positivity rate = 9%; infectious asymptomatic is included in the computation based on the patient flow diagram, note: patients will stay in TTMF for 14 days)  
    ---
    Output:
    df_output - dict/json, requirements contents
    '''
    df = pd.DataFrame(data['data'])
    df_output = pd.DataFrame(index = df.index)
    
    positivityratio = .09
    # df['Mild'] = df['Confirmed'] * .80
    # (df['Symptomatic'] + df['Asymptomatic']) / positivityratio - old revised Aug 9
    df['Suspect or Probable'] = (df['Symptomatic'] + df['Asymptomatic']) * 3.5

    # Cohorted to TTMF beds for confirmed mild cases = Mild
    df_output['TTMF beds'] = df['Mild'] *.15
    
    # Isolation beds  for probable/suspect = Suspect/probable
    # df_output['Isolation beds'] = df['Suspect or Probable']
    
    # Facility physician (either physically present or available remotely thru telehealth 24/7, 
    # at least 1 per shift) = (Cohorted beds + Isolation beds)/20*3
    df_output['Facility physician'] = (df_output['TTMF beds']) / 20 * 3
    
    # Nurse/midwife (at least 2 per shift, 1:12) = (Cohorted beds + Isolation beds)/12*3
    df_output['Nurse or midwife'] =(df_output['TTMF beds']) / 12 * 3
    
    # Barangay health worker or community health volunteer (at least 2 per shift, 1:25) = (Cohorted beds + Isolation beds)/25*3
    df_output['BHW or CHW'] = (df_output['TTMF beds']) / 25 * 3
    
    # Nutritionist-dietitian (1:50) = (Cohorted beds + Isolation beds)/50
    df_output['Nutritionist dietitian'] = (df_output['TTMF beds']) / 50
    
    # Pharmacist (1:50, at least 1 per shift) = (Cohorted beds + Isolation beds)/50*3
    df_output['Pharmacist'] = (df_output['TTMF beds']) / 50 * 3
    
    # Food handler (1:10) = (Cohorted beds + Isolation beds)/10
    df_output['Food handler'] = (df_output['TTMF beds']) / 10
    
    # HCW PPEs (Surgical masks, Gowns, Goggles/face shields, N95 respirators) = (Facility physician + Nurse/midwife +BHW/CHV)
    df_output['HCW PPEs'] = df_output['Facility physician'] +  df_output['Nurse or midwife'] +  df_output['BHW or CHW']

    df_output['Patient Surgical Mask'] = df_output['TTMF beds']

    return df_output

def compute_economics(data, dates, dateInputs=None, cqClassifications=None):
    '''
    Convert df data to health logistic requirements for Community capacity requirements.
    ---
    Parameters:
    JSON format -       econ_parameter = {
                                'data':  {
                                    'S' :susceptible,
                                    'E':exposed,
                                    'Ia':asymptomatic,
                                    'R' :cumulative_recovered,
                                    'C': total_confirmed,
                                    'Is': symptomatic,
                                    'H1': data_dfPsgcEconDetails[0]['H1'],
                                    'H2': data_dfPsgcEconDetails[0]['H2'],
                                    'L1': data_dfPsgcEconDetails[0]['L1'],
                                    'L2': data_dfPsgcEconDetails[0]['L2'],
                                    'ECQ': 0,
                                    'MECQ': 0,
                                    'GCQ': data_dfPsgcEconDetails[0]['GCQ'],
                                    'MGCQ': data_dfPsgcEconDetails[0]['MGCQ'],
                                    'CQ_Classification':data_dfPsgcEconDetails[0]['CQ.Classification'],
                                    'CQ_Dates': data_dfPsgcEconDetails[0]['CQ.Dates']
                                }
                            }---
    Output:
    df_output - dict/json, requirements contents
    '''

    if (cqClassifications == None and dateInputs == None):
        cq_classifications = data['data']['CQ_Classification'].split(';')
        cq_vals = data['data']['CQ_Values'].split(';')
        cq_date_range = data['data']['CQ_Dates'].split(';')
    elif(cqClassifications == None and dateInputs != None):
        return []
    else:
        cq_classifications = data['data']['CQ_Classification'].split(';') + cqClassifications
        cq_date_range = data['data']['CQ_Dates'].split(';') + dateInputs
    df = pd.DataFrame(data['data'])

    df.L1 = df.L1.astype(float)
    df.L2 = df.L2.astype(float)
    df['dates'] = dates
    cq_values = list(range(0, len(dates)))
    cq_index = 0
    cq_date_index = 0

    for index, row in df.iterrows():
        cq_value = row[cq_classifications[cq_index]]
        cq_date = cq_date_range[cq_date_index]
        if datetime.strptime(row['dates'], "%Y-%m-%d") <= datetime.strptime(cq_date, "%Y-%m-%d"):
            cq_values[index] = cq_value
        else:
            cq_index += 1
            cq_date_index += 1
            cq_values[index] = row[cq_classifications[cq_index]]
    
    df['CQ'] = cq_values
    df_output = pd.DataFrame(index = df.index)
    df_output['dates'] = dates
    df_output['e7'] =  (df['H1'] * df['C']) + (df['H2'] * df['Is']) #constant for all regions
    df_output['e8'] = (df['L1'] * df['Is'] ) + (df['L1'] * df['C']) ## per region L1 == L2
    df_output['e9CQ'] = df['CQ'] * (df['S'] + df['E'] + df['Ia'] + df['R'])

    return df_output