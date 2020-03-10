# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# This module is developed based on the the SAFE Toolbox by F. Pianosi, F. Sarrazin and T. Wagener at Bristol University (2015).
# For detail, please see https://www.safetoolbox.info
# Sensitivity analysis method: extended Fourier Amplitutde Sensitivity Test (eFAST) (Cukier et al., 1978; Saltelli et al., 1999)
# Cukier, R.I., Levine, H.B., and Shuler, K.E. (1978), Nonlinear
#    Sensitivity Analyis of Multiparameter Model SYstems, Journal of
#    Computational Physics, 16, 1-42.
# Saltelli, A., Tarantola, S. and Chan, K.P.S. (1999), A Quantitative
#    Model-Independent Method for Global Sensitivty Analysis of Model Output,
#    Technometrics, 41(1), 39-56.
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
from SAFEpython.FAST import FAST_sampling_unif
from SAFEpython.FAST import FAST_indices
from indices import NSE,TRMSE
from functions import viccall
import math
import time

# ------------------------------------------------------------------------------------------------------------------------------------------------
# MAIN CODE
# A FAST sensitivity includes three steps: (1) generating samples; (2) calculate fitness functions; (3) calculate sensitivity indices
# ------------------------------------------------------------------------------------------------------------------------------------------------

start = datetime.datetime.now()
# PART 1: GENERATING sampling file using FAST method
# Uniform distribution - note: the default value of N (minimum number of samples) is calculated based on Saltelli et al. (1999)
# Open operation file
os.chdir('../../RoutingSetup')
text_file = open('reservoireFAST.txt','r')
lines = text_file.read().split('\n')
number_of_days = int(lines[2].split('\t')[0])						# Number of simulation days in VIC
spinning_period = int(lines[3].split('\t')[0])						# Ignore the a number of days in VIC simulation for the warm-up period	
number_of_cores = int(lines[4].split('\t')[0])						# number of computer cores
VIC_vars = [0 for x in range(10)]
maximum_no_reservoirs = len(lines[17].split(' '))
VIC_fitness = [0 for x in range(2)]
for i in range(2):													# 2 fitness functions considered (NSE and TRMSE)
    VIC_fitness[i] = int(lines[i+19].split('\t')[0])
M = 0																# Number of VIC parameters considered
for i in range(10):													# 10 parameters considered
    VIC_vars[i] = int(lines[i+6].split('\t')[0])
    if (VIC_vars[i]>0):
        M+=1
reservoirs = [[0 for x in range(4)] for y in range(maximum_no_reservoirs)]
if (maximum_no_reservoirs>0):
    for i in range(maximum_no_reservoirs):
        try:
            reservoirs[i][0] = int(lines[17].split(' ')[i])
        except:
            print(".. ")											# ignore if there is no reservoir considered
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
                reservoirs[i][2] = discharge
                M+=4
            elif (opt==5):
                reservoirs[i][2] = vcap
                reservoirs[i][3] = vd
                M+=48
        except:
            print("CANNOT FILE RESERVOIR INFORMATION OR FILE READING ERROR ...")

X, s = FAST_sampling_unif(M, N=[], Nharm=4, omega=[])				# N is the minimum number of samples
# Consider all parameters - generating VIC parameter files
rowcount = 0
for i in range(10):
    if (VIC_vars[i]>0):
        if (i==3):
            X[:,rowcount]*=30										# Dmax (0-30)
        elif (i==1):
            X[:,rowcount]*=0.9										# binifil (0-0.9)
        elif (i==4):
            X[:,rowcount]= X[:,rowcount]*(3-1) + 1 					# C (1-3)
        elif (i==5):
            X[:,rowcount]= X[:,rowcount]*(0.25-0.05) + 0.05			# d1 (0.05-0.25)
        elif (i==6 or i==7):
            X[:,rowcount]= X[:,rowcount]*(1.5-0.3) + 0.3			# d2,d3 (0.3-1.5)
        elif (i==8):
            X[:,rowcount]= X[:,rowcount]*(5-0.5) + 0.5				# velocity (0.5-5)
        elif (i==9):
            X[:,rowcount]= X[:,rowcount]*(4000-200) + 200			# diffusivity (200-4000)
        else:
            X[:,rowcount]*=0.9										# Ds (0-1); Ws (0-1)
        rowcount+=1
if (maximum_no_reservoirs>0):
    for i in range(maximum_no_reservoirs):
        if (reservoirs[i][1]==1):
            X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]		# H1
            rowcount+=1
            X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]		# H2
            rowcount+=1
            X[:,rowcount]*=X[:,rowcount]*(365-1) + 1				# T1
            rowcount+=1
            X[:,rowcount]*=X[:,rowcount]*(365-1) + 1				# T2
            rowcount+=1
        elif (reservoirs[i][1]==2):
            for j in range(12):
                X[:,rowcount]=(reservoirs[i][2]-reservoirs[i][3])*X[:,rowcount]+reservoirs[i][3]	# Hi i = 1 to 12
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
# Save the array X to a text file called eFASTparameters.txt
np.savetxt("../RoutingSetup/eFASTparameters.txt",X,fmt="%s")

# PART 2: RUNNING VIC-Res model to calculate the fitness functions
# Read model parameters generated by the FAST sampling technique
os.chdir('../RoutingSetup')
start = datetime.datetime.now()										# Read from file (in case of using a remote computer; the three parts may have to be implemented separately)
text_file = open('eFASTparameters.txt','r')
lines = text_file.read().split('\n')
soildata = [[0 for x in range(M)] for y in range(len(lines))]
countno = 0
for line in lines:
    for i in range(M):
        try:
            soildata[countno][i] = float(line.split(' ')[i])		# Read from file (in case of using a remote computer; the three parts may have to be implemented separately)
        except:
            print("...")											# the last line may be a string
    countno+=1
text_file.close()
# Run the parallelized experiments
os.chdir('../Sensitivity')											# Run VIC simulations and save into a folder called Sensitivity
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
#       for j in range(number_of_cores):							# active these lines in case of parallel modelling
#           if (soildata[i*number_of_cores+j][2]!=0):
#               p[j].join()
    except:
            print("...")											# to eliminate the error if the number of samples cannot be divided by the number of cores
try: 
    for proc in procs:
        proc.terminate()
except:
    print("...")													# to eliminate the error if there is only one core used

# PART 3: RUNNING eFAST analysis
path = '../Sensitivity'
files =[]
modelleddischarge = [[0] * number_of_days] * number_of_samples
for r,d,f in os.walk(path):											# Read all VIC simulation results (files 0.txt to M.txt)
    for file in f:
            if '.txt' in file:
                files.append(os.path.join(r,file))
for f in files:
    with open(f) as file:
        array = file.read().splitlines() 
        array = list(map(float,array))
        modelleddischarge[int(os.path.splitext(os.path.basename(file.name))[0])]=array
    
# Read measured values
text_file = open('../RoutingSetup/Observeddischarge.csv','r')		# Observed data file
lines = text_file.read().split('\n')
countdata = 0
gaudata = [0 for x in range(number_of_days)]
for line in lines:
    try:
        gaudata[countdata] = float(line)
        countdata+= 1
    except:
        print("FINISH READING GAUGED FILE ...")
text_file.close()

# Calculate NSE and TRMSE
NSE_array = [0] * number_of_samples
TRMSE_array = [0] * number_of_samples
for j in range(number_of_samples):
    NSE_array[j] = NSE(gaudata[spinning_period:number_of_days],modelleddischarge[j][spinning_period:number_of_days])
    TRMSE_array[j] = TRMSE(gaudata[spinning_period:number_of_days],modelleddischarge[j][spinning_period:number_of_days])
# Sensitivity analysis with eFAST
if (VIC_fitness[0]>0):
    Y = np.asarray(NSE_array)
    Y = pd.to_numeric(Y,errors='coerce')
    try:
        SiNSE, V, A, B, Vi = FAST_indices(Y, M, Nharm=4, omega=[])		# SiNSE is the sensitivity index for the NSE fitness function
        np.savetxt("../Results/SiNSE.txt",SiNSE,fmt="%s")
    except:
        print("...")
if (VIC_fitness[0]>0):
    Y = np.asarray(TRMSE_array)
    Y = pd.to_numeric(Y,errors='coerce')
    try:
        SiTRMSE, V, A, B, Vi = FAST_indices(Y, M, Nharm=4, omega=[]) 	# SiTRMSE is the sensitivity index for the TRMSE fitness function
        np.savetxt("../Results/SiTRMSE.txt",SiTRMSE,fmt="%s")
    except:
        print("...")
end = datetime.datetime.now()
print('Finish runing experiments')
print("Start",start)
print("End",end)
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------