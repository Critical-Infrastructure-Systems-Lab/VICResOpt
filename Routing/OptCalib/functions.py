# ------------------------------------------------------------------------------------------------------------------------------------------------
# 10 Mar 2020
# THE FOLLOWING CODE CALLS VIC-Res TO IMPLEMENT MODELLING WITH DIFFERENT MODEL PARAMETERS
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import csv
import multiprocessing
import datetime
import math
import time

def viccall(soildata,rank,order,number_of_days,VIC_vars,reservoirs,maximum_no_reservoirs):			# Modify when needed
    # Modify VIC parameters
    os.chdir('../Rainfall-runoffSetup/')
    text_file = open('soil.txt','r')
    lines = text_file.read().split('\n')
    no_of_col = len(lines[0].split('\t'))
    countno = 0
    soilparam = [[0 for x in range(no_of_col)] for y in range(len(lines))]
    for line in lines:
        for i in range(no_of_col):
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
                            if (no_of_col==53): 
                                sl[22] = soildata[order][count_no]				#d1
                            else:
                                sl[18] = soildata[order][count_no]				#d1
                        elif (k==6):
                            if (no_of_col==53): 
                                sl[23] = soildata[order][count_no]				#d2
                            else:
                                sl[19] = soildata[order][count_no]				#d2
                        elif ((k==7) and (no_of_col==53)):
                             sl[24] = soildata[order][count_no]					#d3 (in case the VIC model has more than 3 layers; modify this Python code accordingly)
                        count_no+=1
                if (no_of_col==53):												#3-layer soil - not test yet, if there are any errors, modify Lines 71
                    my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
                    %(sl[0],sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9],sl[10],
                    sl[11],sl[12],sl[13],sl[14],sl[15],sl[16],sl[17],sl[18],sl[19],sl[20],
                    sl[21],sl[22],sl[23],sl[24],sl[25],sl[26],sl[27],sl[28],sl[29],sl[30],
                    sl[31],sl[32],sl[33],sl[34],sl[35],sl[36],sl[37],sl[38],sl[39],sl[40],
                    sl[41],sl[42],sl[43],sl[44],sl[45],sl[46],sl[47],sl[48],sl[49],sl[50],sl[51],sl[52]))		# 3 layers
                else:
                    my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
                    %(sl[0],sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9],sl[10],
                    sl[11],sl[12],sl[13],sl[14],sl[15],sl[16],sl[17],sl[18],sl[19],sl[20],
                    sl[21],sl[22],sl[23],sl[24],sl[25],sl[26],sl[27],sl[28],sl[29],sl[30],
                    sl[31],sl[32],sl[33],sl[34],sl[35],sl[36],sl[37],sl[38],sl[39],sl[40]))						# 2 layers
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
        try:
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
            irrigation = int(lines[5].split('\t')[0])
            irripath = lines[6].split('\t')[0]
            opt = int(lines[8].split('\t')[0])
            if (opt==3):
                D4demand = float(lines[6].split('\t')[0])
            elif (opt==5):
                for j in range(12):
                    D5demand[j] = float(lines[6+j].split('\t')[0])
            text_file.close()
            if (opt!=4):
                with open('res'+str(reservoirs[i][0])+'.txt','w') as my_csv:
                    my_csv.write("Hmax(M)	Hmin(M)	Scap(1000M3)	Sd(1000M3)	Hturbine(M)	Qdesign (M3/s)	Year	Sinitial(1000M3)	Name\n")
                    my_csv.write("%f\t%f\t%f\t%f\t%f\t%f\t%i\t%f\t%s\n"%(hmax,hmin,volume,dvolume,height,discharge,yearopt,initialvolume,resname))
                    my_csv.write("SEEPAGE	INFILTRATION\n")
                    my_csv.write("%f\t%f\n"%(seepage,infil))
                    my_csv.write("IRRIGATION")
                    my_csv.write("%i\n"%(irrigation))
                    my_csv.write("%s\n"%(irripath))
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
        except:
            print("..")															# ignored no reservoir considered
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
        newfile = "../RoutingSetup/input/" + file
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