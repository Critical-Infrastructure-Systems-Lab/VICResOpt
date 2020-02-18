# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# Visualization: extended Fourier Amplitutde Sensitivity Test (eFAST) sensitivity analysis
# ------------------------------------------------------------------------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt
import os
import csv

# ------------------------------------------------------------------------------------------------------------------------------------------------
# MODIFY THIS PART WHERE APPROPRIATE
link1 = '../RoutingSetup/reservoireFAST.txt'						# Link to the eFAST configuration file to read parameters
link2 = '../Results/SiNSE.txt'										# Link to the result file: SiNSE.txt
link3 = '../Results/SiTRMSE.txt'									# Link to the result file: SiTRMSE.txt
link4 = '../Reservoirs/'											# Link to reservoir data
# DO NOT MODIFY THE CODE BELOW
# ------------------------------------------------------------------------------------------------------------------------------------------------

# Open operation file
text_file = open(link1,'r')
lines = text_file.read().split('\n')
no_var = 0
no_fitness = 0
my_xticks = []
VIC_vars = [0 for x in range(10)]
VIC_fitness = [0 for x in range(2)]
for i in range(2):													# 2 fitness functions considered
    if (int(lines[i+19].split('\t')[0])>0):
        no_fitness+=1												# Count the number of fitness functions considered
        VIC_fitness[i]=int(lines[i+19].split('\t')[0])
maximum_no_reservoirs = len(lines[17].split(' '))
reservoirs = [0 for i in range(maximum_no_reservoirs)]
for i in range(maximum_no_reservoirs):
    reservoirs[i] = int(lines[17].split(' ')[i])
for i in range(10):													# 10 parameters considered
    if (int(lines[i+6].split('\t')[0])>0):
        no_var+=1													# Count the number of parameters considered	    
        if (i==0):
             my_xticks.append('b')
        elif (i==1):
             my_xticks.append('Ds')
        elif (i==2):
             my_xticks.append('Dmax')
        elif (i==3):
             my_xticks.append('Ws')
        elif (i==4):
             my_xticks.append('c')
        elif (i==5):
             my_xticks.append('d1')
        elif (i==6):
             my_xticks.append('d2')
        elif (i==7):
             my_xticks.append('d3')
        elif (i==8):
             my_xticks.append('Velo')
        elif (i==9):
             my_xticks.append('Diff')
text_file.close()
for i in range(maximum_no_reservoirs):
    text_file = open(link4+'res'+str(reservoirs[i])+'.txt','r')
    lines = text_file.read().split('\n')
    opt = int(lines[5].split('\t')[0])
    if (opt==1):
       my_xticks.append('Res'+str(reservoirs[i])+'-hmax')
       my_xticks.append('Res'+str(reservoirs[i])+'-hmin')
       my_xticks.append('Res'+str(reservoirs[i])+'-tmax')
       my_xticks.append('Res'+str(reservoirs[i])+'-tmin')
       no_var+=4
    elif (opt==2):
       for j in range(12):
            my_xticks.append('Res'+str(reservoirs[i])+'-h'+str(j+1))
       no_var+=12
    elif (opt==3):
       my_xticks.append('Res'+str(reservoirs[i])+'-x1')
       my_xticks.append('Res'+str(reservoirs[i])+'-x2')
       my_xticks.append('Res'+str(reservoirs[i])+'-x3')
       my_xticks.append('Res'+str(reservoirs[i])+'-x4')
       no_var+=4
text_file.close()

# Read result data
if (VIC_fitness[0]>0):												# This could be shortened by using two FOR loops, but not neccessary
    path1 = open(link2,'r')
    lines = path1.read().split('\n')
    countdata = 0
    SiNSE = [0 for x in range(no_var)]								# Note: SiNSE file contains Sensitivity indices for the NSE function
    for line in lines:
        try:
             SiNSE[countdata] = float(line.split(' ')[0])
             countdata+=1
        except:
             print("...")											# the last line may be a string
    path1.close()

if (VIC_fitness[1]>0):
    path2 = open(link3,'r')
    lines = path2.read().split('\n')
    countdata = 0
    SiTRMSE = [0 for x in range(no_var)]							# Note: SiTRMSE file contains Sensitivity indices for the TRMSE function
    for line in lines:
        try:
             SiTRMSE[countdata] = float(line.split(' ')[0])
             countdata+=1
        except:
             print("...")
    path2.close()

# Setup plot
x = np.arange(0, no_var, 1)
if (no_fitness>1):	
    plt.figure(figsize=(16, 5))
else:
    plt.figure(figsize=(8, 5))

# Plot Sensitivity indices	
if (no_fitness>1):													# This could be shortened by using two FOR loops, but not neccessary
    ax = plt.subplot(121)											# If there are two fitness functions considered, use multi-plots
    ax.bar(x, SiNSE, width=0.4, color='r', align='center')
if (VIC_fitness[0]>0):
    plt.bar(x, SiNSE, width=0.4, color='r', align='center')
    plt.xlabel('Variables',fontsize=16)
    plt.ylabel('Main effect (first-order) sensitivity index',fontsize=14)
    plt.title("NSE",fontsize=18)
    plt.xticks(x, my_xticks)
plt.xticks(rotation=90)
if (no_fitness>1):
    ax = plt.subplot(122)
    ax.bar(x, SiTRMSE, width=0.4, color='r', align='center')
if (VIC_fitness[1]>0):
    plt.bar(x, SiTRMSE, width=0.4, color='r', align='center')
    plt.xlabel('Variables',fontsize=16)
    plt.ylabel('Main effect (first-order) sensitivity index',fontsize=14)
    plt.title("TRMSE",fontsize=18)
    plt.xticks(x, my_xticks)
plt.xticks(rotation=90)
plt.show()
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------