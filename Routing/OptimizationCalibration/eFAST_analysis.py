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
import math
import time

# ------------------------------------------------------------------------------------------------------------------------------------------------
# THE FOLLOWING CODE CALLS VIC-Res TO IMPLEMENT MODELLING WITH DIFFERENT MODEL PARAMETERS

def viccall(soildata,rank,order,number_of_days):
    # Modify VIC parameters
    global maximum_no_reservoirs
    global reservoirs
    os.chdir('../Rainfall-runoffSetup/')
    text_file = open('soil.txt','r')
    lines = text_file.read().split('\n')
    global VIC_vars
    no_of_row = len(lines[0])
    countno = 0
    soilparam = [[0 for x in range(no_of_row)] for y in range(len(lines))]
    for line in lines:
        for i in range(no_of_row):
             try:
                soilparam[countno][i] = float(line.split('\t')[i])
             except:
                print(line)
        countno+=1
    text_file.close()
    with open('soil.txt','w') as my_csv:
        for sl in soilparam:
            if (sl[2]!=0):														#Avoid add more row if the last row is blank
                count_no = 0
                for k in range(10):
                    if (VIC_vars[k]>0):
                        if (k==0):
                             sl[4] = soildata[order][count_no]					#binfil
                        elif (k==1):
                             sl[5] = soildata[order][count_no]					#Ds
                        elif (k==2):
                             sl[6] = soildata[order][count_no]					#Dmax
                        elif (k==3):
                             sl[7] = soildata[order][count_no]					#Ws
                        elif (k==4):
                             sl[8] = soildata[order][count_no]					#c
                        elif (k==5):
                             sl[18] = soildata[order][count_no]					#d1
                        elif (k==6):
                             sl[19] = soildata[order][count_no]					#d2
                        elif ((k==7) and (no_of_row==42)):
                             sl[20] = soildata[order][count_no]					#d3 (in case the VIC model has more than 3 layers; modify this Python code accordingly)
                        count_no+=1
                if (no_of_row ==42):
                    my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
                    %(sl[0],sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9],sl[10],sl[11],sl[12],sl[13],sl[14],sl[15],sl[16],sl[17],sl[18],sl[19],sl[20],sl[21],sl[22],sl[23],sl[24],sl[25],sl[26],sl[27],sl[28],sl[29],sl[30],sl[31],sl[32],
                    sl[33],sl[34],sl[35],sl[36],sl[37],sl[38],sl[39],sl[40],sl[41]))         # 3 layers
                else:
                    my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
                    %(sl[0],sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9],sl[10],sl[11],sl[12],sl[13],sl[14],sl[15],sl[16],sl[17],sl[18],sl[19],sl[20],sl[21],sl[22],sl[23],sl[24],sl[25],sl[26],sl[27],sl[28],sl[29],sl[30],sl[31],sl[32],
                    sl[33],sl[34],sl[35],sl[36],sl[37],sl[38],sl[39],sl[40])) 		  		 # 2 layers
    os.chdir('../RoutingSetup')
    text_file = open('configuration.txt','r')									# Modify flow routing file
    lines = text_file.read().splitlines()    									# 1st running, lines in the configuration file does not contain \n
    if (len(lines)<5):
        lines = text_file.read().split('\n')
    with open('configuration.txt','w') as my_csv:								# Modify when needed
        for i in range(31):
            if ((i==5) and (VIC_vars[8]>0)):
                my_csv.write("%f\n"%(soildata[order][count_no]))				#velocity
                count_no+=1
            elif ((i==9) and (VIC_vars[9]>0)):
                my_csv.write("%f\n"%(soildata[order][count_no]))				#diffusivity
                count_no+=1
            else:
                my_csv.write("%s\n"%(lines[i]))
    text_file.close()
    os.chdir('../Reservoirs')
    D5demand = [0.0 for x in range(12)]
    for i in range(maximum_no_reservoirs):
        text_file = open('res'+str(reservoirs[i][0])+'.txt','r')
        lines = text_file.read().split('\n')
        hmax = float(lines[1].split('\t')[0])
        hmin = float(lines[1].split('\t')[1])
        volume = float(lines[1].split('\t')[2])
        dvolume = float(lines[1].split('\t')[3])
        height = float(lines[1].split('\t')[4])
        discharge = float(lines[1].split('\t')[5])
        yearopt = int(lines[1].split('\t')[6])
        initialvolume = float(lines[1].split('\t')[7])
        resname = lines[1].split('\t')[8]
        seepage = float(lines[3].split('\t')[0])
        infil = float(lines[3].split('\t')[1])
        opt = int(lines[5].split('\t')[0])
        if (opt==3):
            D4demand = float(lines[6].split('\t')[0])
        text_file.close()
        with open('res'+str(reservoirs[i][0])+'.txt','w') as my_csv:
            my_csv.write("Hmax(M)	Hmin(M)	Scap(1000M3)	Sd(1000M3)	Hturbine(M)	Qdesign (M3/s)	Year	Sinitial(1000M3)	Name\n")
            my_csv.write("%f\t%f\t%f\t%f\t%f\t%f\t%i\t%f\t%s\n"%(hmax,hmin,volume,dvolume,height,discharge,yearopt,initialvolume,resname))
            my_csv.write("SEEPAGE	INFILTRATION\n")
            my_csv.write("%f\t%f\n"%(seepage,infil))
            my_csv.write("OPERATION STRATEGY\n")
            my_csv.write(str(opt)+"\n")
            if (opt==1):
                my_csv.write("%f\t%f\t%i\t%i\n"%(soildata[order][count_no],soildata[order][count_no+1],soildata[order][count_no+2],soildata[order][count_no+3]))
                count_no+=4
            elif (opt==2):
                my_csv.write("%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n"%(soildata[order][count_no],soildata[order][count_no+1],soildata[order][count_no+2],
                soildata[order][count_no+3],soildata[order][count_no+4],soildata[order][count_no+5],soildata[order][count_no+6],soildata[order][count_no+7],
                soildata[order][count_no+8],soildata[order][count_no+9],soildata[order][count_no+10],soildata[order][count_no+11]))
                count_no+=12
            elif (opt==3):
                x1 = soildata[order][count_no]
                x2 = soildata[order][count_no+1]
                x3 = soildata[order][count_no+2]
                x4 = soildata[order][count_no+3]
                if (x2>x3):
                    temp = x2
                    x2 = x3
                    x3 = temp
                my_csv.write("%f\t%f\t%f\t%f\t%f\n"%(D4demand,x1,x2,x3,x4))
                count_no+=4
            elif (opt==5):
                for j in range(12):
                    x1 = soildata[order][count_no]
                    x2 = soildata[order][count_no+1]
                    x3 = soildata[order][count_no+2]
                    x4 = soildata[order][count_no+3]
                    if (x2>x3):
                        temp = x2
                        x2 = x3
                        x3 = temp
                    my_csv.write("%f\t%f\t%f\t%f\t%f\n"%(D5demand[j],x1,x2,x3,x4))
                    count_no+=4
    # Run rainfall-runoff model
    os.chdir('../Rainfall-runoffSetup/Results')
    os.system('rm fluxes*.*')
    os.system('rm snow*.*')
    os.chdir('../../RoutingSetup/input')
    os.system('rm fluxes*.*')
    os.chdir('../../Rainfall-runoff')
    os.system('./vicNl -g ../Rainfall-runoffSetup/globalparam.txt')
    # Change the order of X and Y in the file name
    files = [f for f in os.listdir("../Rainfall-runoffSetup/Results/") if os.path.isfile(os.path.join("../Rainfall-runoffSetup/Results/",f))]
    for file in files:
        if (len(file)>=22):											# default VIC rainfall-runoff file: 22-23 characters
            if (len(file)==22):
                coorX = file[7:14]
                coorY = file[15:22]
            else:
                coorX = file[7:15]
                coorY = file[16:23]
            newfile = "../RoutingSetup/input/fluxes_"+coorY+"_"+coorX
            file = "../Rainfall-runoffSetup/Results/" + file
            os.rename(file,newfile)
    # Run routing model
    os.chdir('../Routing/SourceCode')
    os.system('rm *.uh_s')
    os.system('./rout ../../RoutingSetup/configuration.txt')
    os.chdir('../../Results')
    text_file = open('OUTLE.day','r')
    lines = text_file.read().split('\n')
    countno = 0
    # Read modeling results
    discharge = [x for x in range(number_of_days)]
    for line in lines:
        try:
           year,month,day,flows = filter(None,line.split(' '))
           discharge[countno] = float(flows)
           countno+=1
        except:
            print("FINISH READING MODELED FILE ...")
    text_file.close()
    # Save the results to a file for further Sensitivity analysis
    os.chdir('../Sensitivity')
    np.savetxt(str(order)+".txt",discharge,fmt="%s")				# Note: this can save additional data due to the length of the array, but this is fine
    return discharge

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
for i in range(maximum_no_reservoirs):
    reservoirs[i][0] = int(lines[17].split(' ')[i])
text_file.close()
os.chdir('../Reservoirs')
for i in range(maximum_no_reservoirs):
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

X, s = FAST_sampling_unif(M, N=[], Nharm=4, omega=[])				# N is the minimum number of samples
# Consider all parameters - generating VIC parameter files
rowcount = 0
for i in range(10):
    if (VIC_vars[i]>0):
        if (i==3):
            X[:,rowcount]*=30										# Dmax (0-30)
        elif (i==1):
            X[:,rowcount]*=0.9   									# binifil (0-0.9)
        elif (i==4):
            X[:,rowcount]= X[:,rowcount]*(3-1) + 1 					# C (1-3)
        elif (i==5):
            X[:,rowcount]= X[:,rowcount]*(0.25-0.05) + 0.05   		# d1 (0.05-0.25)
        elif (i==6 or i==7):
            X[:,rowcount]= X[:,rowcount]*(1.5-0.3) + 0.3   			# d2,d3 (0.3-1.5)
        elif (i==8):
            X[:,rowcount]= X[:,rowcount]*(5-0.5) + 0.5   			# velocity (0.5-5)
        elif (i==9):
            X[:,rowcount]= X[:,rowcount]*(4000-200) + 200  			# diffusivity (200-4000)
        else:
            X[:,rowcount]*=0.9   									# Ds (0-1); Ws (0-1)
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
                p[j] = multiprocessing.Process(target=viccall(soildata,j+1,i*number_of_cores+j,number_of_days))
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
text_file = open('../RoutingSetup/Observeddischarge.csv','r')  		# Observed data file
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
NSE = [0] * number_of_samples
TRMSE = [0] * number_of_samples
for j in range(number_of_samples):
    trmse = 0
    numerator = 0
    meangau = 0
    summdl = 0
    sumobs = 0
    for i in range(spinning_period,number_of_days):					# Only calculate NSE and TRMSE after the spinning period
        zmdl = (pow((abs(modelleddischarge[j][i])+1),0.3)-1)/0.3
        zobs = (pow((gaudata[i]+1),0.3)-1)/0.3
        trmse+=pow(zmdl-zobs,2)
        numerator+=pow(abs(modelleddischarge[j][i])-gaudata[i],2)
        meangau+=abs(gaudata[i])									# Not consider two-direction flow (need a hydrodynamic modle)
        summdl+=abs(modelleddischarge[j][i])
        sumobs+=gaudata[i]
    meangau = meangau / (number_of_days-spinning_period)
    denominator = 0
    for i in range(spinning_period,number_of_days):
        denominator+=pow(gaudata[i]-meangau,2)
        zobs = (pow((gaudata[i]+1),0.3)-1)/0.3
    nash = 1 - numerator/denominator
    trmse = pow(trmse,0.5)
    NSE[j] = nash
    TRMSE[j] = trmse
# Sensitivity analysis with eFAST
if (VIC_fitness[0]>0):
    Y = np.asarray(NSE)
    Y = pd.to_numeric(Y,errors='coerce')
    try:
        SiNSE, V, A, B, Vi = FAST_indices(Y, M, Nharm=4, omega=[])		# SiNSE is the sensitivity index for the NSE fitness function
        np.savetxt("../Results/SiNSE.txt",SiNSE,fmt="%s")
    except:
        print("...")
if (VIC_fitness[0]>0):
    Y = np.asarray(TRMSE)
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