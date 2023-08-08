# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# This module is developed based on the the SAFE Toolbox by F. Pianosi, F. Sarrazin and T. Wagener at Bristol University (2015).
# For detail, please see https://www.safetoolbox.info
# Sensitivity analysis: Elementary Effects Test (Saltelli, 2008)
# Saltelli, A., Tarantola, S. and Chan, K.P.S. (1999), A Quantitative Model-Independent Method for Global Sensitivity Analysis of Model Output, 
#Technometrics, 41(1), 39-56.
# Modify all links if change the current folder tree
# 03/03/2020 fix errors related to the 3rd soil layer
# ------------------------------------------------------------------------------------------------------------------------------------------------

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import csv
import multiprocessing
import datetime
from SAFEpython.lhcube import lhcube
from SAFEpython.EET import EET_indices
from indices import NSE,TRMSE
from functions import viccall
import math
import time

# ------------------------------------------------------------------------------------------------------------------------------------------------
# HERE IS THE MAIN CODE
# ------------------------------------------------------------------------------------------------------------------------------------------------

# PART 1: GENERATING sampling file
start = datetime.datetime.now()
# Soil parameters and uniform distribution - note: the default value of N is calculated based on Saltelli et al. (1999)
# Open operation file
os.chdir('../../RoutingSetup')
text_file = open('reservoirEET.txt','r')
lines = text_file.read().split('\n')
number_of_days = int(lines[2].split('\t')[0])					# Number of simulation days in VIC
spinning_period = int(lines[3].split('\t')[0])					# Ignore the a number of days in VIC simulation for the warm-up period
number_of_cores = int(lines[4].split('\t')[0])					# number of computer cores
rEET = int(lines[5].split('\t')[0])								# Number of sampling points
VIC_fitness = [0 for x in range(2)]
for i in range(2):												# 2 fitness functions considered
    VIC_fitness[i] = int(lines[i+20].split('\t')[0])
M = 0															# Number of VIC parameters considered
maximum_no_reservoirs = len(lines[18].split(' '))
VIC_vars = [0 for x in range(10+maximum_no_reservoirs*4)]		# Modify when needed (>10)
for i in range(10):												# 10 parameters considered
    VIC_vars[i] = int(lines[i+7].split('\t')[0])
    if (VIC_vars[i]>0):
        M+=1
reservoirs = [[0 for x in range(4)] for y in range(maximum_no_reservoirs)]
if (maximum_no_reservoirs>0):
    for i in range(maximum_no_reservoirs):
        try:
            reservoirs[i][0] = int(lines[18].split(' ')[i])
        except:
            print(".. ")										# ignore if there is no reservoir considered
text_file.close()
os.chdir('../Reservoirs')
if (maximum_no_reservoirs>0):
    for i in range(maximum_no_reservoirs):
        try:
            text_file = open('res'+str(reservoirs[i][0])+'.txt','r')
            lines = text_file.read().split('\n')
            hmax = float(lines[1].split('\t')[0])
            hmin = float(lines[1].split('\t')[1])
            opt = int(lines[5].split('\t')[0])
            vcap = float(lines[1].split('\t')[2])
            vd = float(lines[1].split('\t')[3])
            text_file.close()
            reservoirs[i][1] = opt
            if ((opt==1) or (opt==2)):
                reservoirs[i][2] = hmax
                reservoirs[i][3] = hmin
                if (opt==1):
                    M+=4
                elif (opt==2):
                    M+=12
            elif (opt==3):
                reservoirs[i][2] = vcap
                reservoirs[i][3] = vd
                M+=4
            elif (opt==5):
                reservoirs[i][2] = vcap
                reservoirs[i][3] = vd
                M+=48
        except:
            print("CANNOT FILE RESERVOIR INFORMATION OR FILE READING ERROR ...")

# Generate samping set
X = [[0 for x in range(M)] for y in range(rEET*(M+1))]
xmin=[]
xmax=[]
np.random.seed(10)												# Change here if needed
for i in range(M):
    X[0][i] = np.random.random()
for i in range(1,rEET*(M+1)):
    for j in range(M):
        X[i][j] = X[i-1][j]
    ord = np.random.randint(0,M)
    ran = np.random.random()
    X[i][ord] = ran
X = np.asarray(X)
# Consider all parameters - generating VIC parameter files
rowcount = 0
for i in range(10):
    if (VIC_vars[i]>0):
        if (i==3):
            X[:,rowcount]*=30									# Dmax (0-30)
            xmin.append(0)
            xmax.append(30)
        elif (i==1):
            X[:,rowcount]*=0.9									# binifil (0-0.9)
            xmin.append(0)
            xmax.append(0.3)
        elif (i==4):
            X[:,rowcount]= X[:,rowcount]*(3-1) + 1				# C (1-3)
            xmin.append(1)
            xmax.append(3)
        elif (i==5):
            X[:,rowcount]= X[:,rowcount]*(0.25-0.05) + 0.05		# d1 (0.05-0.25)
            xmin.append(0.05)
            xmax.append(0.25)
        elif (i==6 or i==7):
            X[:,rowcount]= X[:,rowcount]*(1.5-0.3) + 0.3		# d2,d3 (0.3-1.5)
            xmin.append(0.3)
            xmax.append(1.5)
        elif (i==8):
            X[:,rowcount]= X[:,rowcount]*(5-0.5) + 0.5			# velocity (0.5-5)
            xmin.append(0.5)
            xmax.append(5)
        elif (i==9):
            X[:,rowcount]= X[:,rowcount]*(4000-200) + 200		# diffusivity (200-4000)
            xmin.append(200)
            xmax.append(4000)
        else:
            X[:,rowcount]*=0.9									# Ds (0-1); Ws (0-1)
            xmin.append(0)
            xmax.append(1)
        rowcount+=1
if (maximum_no_reservoirs>0):
    for i in range(maximum_no_reservoirs):
        if (reservoirs[i][1]==1):
            X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]		# H1
            xmin.append(reservoirs[i][3])
            xmax.append(reservoirs[i][2])
            rowcount+=1
            X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]		# H2
            xmin.append(reservoirs[i][3])
            xmax.append(reservoirs[i][2])
            rowcount+=1
            X[:,rowcount]*=X[:,rowcount]*(365-1) + 1			# T1
            xmin.append(1)
            xmax.append(365)
            rowcount+=1
            X[:,rowcount]*=X[:,rowcount]*(365-1) + 1			# T2
            xmin.append(1)
            xmax.append(365)
            rowcount+=1
        elif (reservoirs[i][1]==2):
            for j in range(12):
                X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]	# Hi i = 1 to 12
                xmin.append(reservoirs[i][3])
                xmax.append(reservoirs[i][2])
                rowcount+=1
        elif (reservoirs[i][1]==3):
            X[:,rowcount]*=math.pi/2																# x1
            xmin.append(0)
            xmax.append(math.pi/2)
            rowcount+=1
            X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]		# x2
            xmin.append(reservoirs[i][3])
            xmax.append(reservoirs[i][2])
            rowcount+=1
            X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]		# x3
            xmin.append(reservoirs[i][3])
            xmax.append(reservoirs[i][2])
            rowcount+=1
            X[:,rowcount]*=math.pi/2																# x4
            xmin.append(0)
            xmax.append(math.pi/2)
            rowcount+=1
        elif (reservoirs[i][1]==5):
            for j in range(12):
                X[:,rowcount]*=math.pi/2															# x1
                xmin.append(0)
                xmax.append(math.pi/2)
                rowcount+=1
                X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]	# x2
                xmin.append(reservoirs[i][3])
                xmax.append(reservoirs[i][2])
                rowcount+=1
                X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]	# x3
                xmin.append(reservoirs[i][3])
                xmax.append(reservoirs[i][2])
                rowcount+=1
                X[:,rowcount]*=math.pi/2															# x4
                xmin.append(0)
                xmax.append(math.pi/2)
                rowcount+=1
os.chdir('../RoutingSetup')
# save the array X to a text file in python called VICparameters.txt
np.savetxt("EETparameters.txt",X,fmt="%s")

# PART 2: RUNNING VIC-Res model
# Read model parameters generated by the Latin hyper cube sampling technique
start = datetime.datetime.now()
text_file = open('EETparameters.txt','r')						# Save to file and open file so that the three parts can be implemented separately
lines = text_file.read().split('\n')
soildata = [[0 for x in range(M)] for y in range(len(lines))]
countno = 0
for line in lines:
    for i in range(M):
        try:
            soildata[countno][i] = float(line.split(' ')[i])
        except:
            print("...")										# the last line is a string
    countno+=1
text_file.close()
# Run the parallelized experiments
os.chdir('../Sensitivity')
os.system('rm *.txt')
number_of_samples = len(X)
procs = []
p = [0 for x in range(number_of_cores)]
for i in range(int(number_of_samples/number_of_cores)):
    try: 
        for j in range(number_of_cores):
            if (soildata[i*number_of_cores+j][2]!=0):
                p[j] = multiprocessing.Process(target=viccall(soildata,j+1,i*number_of_cores+j,number_of_days,VIC_vars,reservoirs,maximum_no_reservoirs))
                procs.append(p[j])
                p[j].start()
#       for j in range(number_of_cores):						# active these lines in case of parallel modelling
#           if (soildata[i*number_of_cores+j][2]!=0):
#               p[j].join()
    except:
            print("....")										# to eliminate the error if the number of samples cannot be divided by the number of cores
try: 
    for proc in procs:
        proc.terminate()
except:
    print("...")												# to eliminate the error if there is only one core used


# PART 3: RUNNING EET analysis
# Change parameters here
path = '../Sensitivity' 										# VIC model
files =[]
modelleddischarge = [[0] for x in range(number_of_days) for y in range(number_of_samples)]
for r,d,f in os.walk(path):
    for file in f:
            if '.txt' in file:
                files.append(os.path.join(r,file))
for f in files:
    with open(f) as file:
        array = file.read().splitlines() 
        array = list(map(float,array))
        modelleddischarge[int(os.path.splitext(os.path.basename(file.name))[0])]=array

# Read measured values
text_file = open('../RoutingSetup/Observeddischarge.csv','r')	# Observed data file
lines = text_file.read().split('\n')
countdata = 0
gaudata = [0 for x in range(number_of_days)]
for line in lines:
    try:
        gaudata[countdata] = float(line)
        countdata+= 1
    except:
        print("...")											# Avoid blank line
text_file.close()

# Calculate NSE and TRMSE
NSE_array = [0] * number_of_samples
TRMSE_array = [0] * number_of_samples
for j in range(number_of_samples):
    NSE_array[j] = NSE(gaudata[spinning_period:number_of_days],modelleddischarge[j][spinning_period:number_of_days])
    TRMSE_array[j] = TRMSE(gaudata[spinning_period:number_of_days],modelleddischarge[j][spinning_period:number_of_days])
# Sensitivity analysis with EET
if (VIC_fitness[0]>0):
    Y = np.asarray(NSE_array)
    Y = pd.to_numeric(Y,errors='coerce')
    miNSE, sigma, EENSE = EET_indices(rEET, xmin, xmax, X,Y,'trajectory',Nboot=0)				# Options: 'trajectory', 'radial'
    np.savetxt("../Results/miNSE.txt",miNSE,fmt="%s")
if (VIC_fitness[1]>0):
    Y = np.asarray(TRMSE_array)
    Y = pd.to_numeric(Y,errors='coerce')
    miTRMSE, sigma, EETRMSE = EET_indices(rEET, xmin, xmax, X,Y,'trajectory',Nboot=0)
    np.savetxt("../Results/miTRMSE.txt",miTRMSE,fmt="%s")
end = datetime.datetime.now()
print('Finish runing experiments')
print("Start",start)
print("End",end)
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------