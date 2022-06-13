# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# Configuration file: reservoircalibration.txt
# Result files: calibration_objectives.txt and calibration_variables.txt
# Version: 1.0 23 Oct 2019
# Change links and filenames where appropriate
# Require the following libs
# 03/03/2020 fix errors related to the 3rd soil layer
# 12/06/2022 fix error related to lat - lon
from platypus import EpsNSGAII, Problem, Real, ProcessPoolEvaluator, Hypervolume, nondominated
import os
import csv
import numpy as np
import multiprocessing
import datetime
import time
from indices import NSE,TRMSE,MSDE,ROCE

# Function to call VIC
def viccall(vars):
    # Declare decision variables (VIC)
    global no_of_var																# number of decision variables
    global VIC_vars																	# decision variable list
    global VIC_objs																	# objective function list
    global number_of_days															# number of simulation days

    # Starting the process - locating resources based on CPU (parallel computing - distributed sources)
    rank = multiprocessing.current_process()._identity[0]
    print("-----------------------------------------------------------------------------------")
    print("Thread no: ",rank," out of ",multiprocessing.cpu_count())

    # Modify VIC parameters
    os.chdir('../Rainfall-runoffSetup/')											# Modify when needed, for parallel modelling, create Folder called Core+str(rank)
    text_file = open('soil.txt','r')												# Modify when needed
    lines = text_file.read().split('\n')
    count_no = 0
    no_of_col = len(lines[0].split('\t'))

    soilparam = [[0 for x in range(no_of_col)] for y in range(len(lines))]
    for line in lines:
        for i in range(0,no_of_col):
             try:
                soilparam[count_no][i] = float(line.split('\t')[i])
             except:
                print("...")														# Ignore lines with errors
        count_no+=1
    text_file.close()

    with open('soil.txt','w') as my_csv:											# Modify soil file
        for sl in soilparam:
            if (sl[2]!=0):															#Avoid add more row if the last row is blank
                count_no = 0
                for k in range(10): 
                    if (VIC_vars[k]>0):
                        if (k==0):
                            sl[4] = vars[count_no]									#binfil
                            count_no+=1
                        elif (k==1):
                            sl[5] = vars[count_no]									#Ds
                            count_no+=1
                        elif (k==2):
                            sl[6] = vars[count_no]									#Dmax
                            count_no+=1
                        elif (k==3):
                            sl[7] = vars[count_no]									#Ws
                            count_no+=1
                        elif (k==4):
                            sl[8] = vars[count_no]									#c
                            count_no+=1
                        elif (k==5):
                            if (no_of_col==53): 
                                sl[22] = vars[count_no]								#d1
                            else:
                                sl[18] = vars[count_no]								#d1
                            count_no+=1
                        elif (k==6):
                            if (no_of_col==53): 
                                sl[23] = vars[count_no]								#d2
                            else:
                                sl[19] = vars[count_no]								#d2
                            count_no+=1
                        elif ((k==7) and (no_of_col==53)):
                            sl[24] = vars[count_no]									#d3 (in case the VIC model has more than 3 layers; modify this Python code accordingly)
                            count_no+=1												# 2-layer soil = 41; 3-layer soi; = 53
                #print('No of line:',no_of_col)										# to check when neccessary
                #time.sleep(10000)
                if (no_of_col==53):													#3-layer soil - not test yet, if there are any errors, modify Lines 77
                    my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
                    %(sl[0],sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9],sl[10],
                    sl[11],sl[12],sl[13],sl[14],sl[15],sl[16],sl[17],sl[18],sl[19],sl[20],
                    sl[21],sl[22],sl[23],sl[24],sl[25],sl[26],sl[27],sl[28],sl[29],sl[30],
                    sl[31],sl[32],sl[33],sl[34],sl[35],sl[36],sl[37],sl[38],sl[39],sl[40],
                    sl[41],sl[42],sl[43],sl[44],sl[45],sl[46],sl[47],sl[48],sl[49],sl[50],sl[51],sl[52]))		# 3 layers
                else:																#2-layer soil
                    my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
                    %(sl[0],sl[1],sl[2],sl[3],sl[4],sl[5],sl[6],sl[7],sl[8],sl[9],sl[10],
                    sl[11],sl[12],sl[13],sl[14],sl[15],sl[16],sl[17],sl[18],sl[19],sl[20],
                    sl[21],sl[22],sl[23],sl[24],sl[25],sl[26],sl[27],sl[28],sl[29],sl[30],
                    sl[31],sl[32],sl[33],sl[34],sl[35],sl[36],sl[37],sl[38],sl[39],sl[40]))						# 2 layers
    os.chdir('../RoutingSetup')

    text_file = open('configuration.txt','r')										# Modify flow routing file
    lines = text_file.read().splitlines()											# 1st running, lines in the configuration file does not contain \n
    if (len(lines)<5):
        lines = text_file.read().split('\n')
    text_file.close()
    with open('configuration.txt','w') as my_csv:									# Modify when needed
        for i in range(31):
            if ((i==5) and (VIC_vars[8]>0)):
                my_csv.write("%f\n"%(vars[count_no]))								#velocity
                count_no+=1
            elif ((i==8) and (VIC_vars[9]>0)):
                my_csv.write("%f\n"%(vars[count_no]))								#diffusivity
                count_no+=1
            else:
                my_csv.write("%s\n"%(lines[i]))

    # Run rainfall-runoff model and change filename
    os.chdir('../Rainfall-runoffSetup/Results')										# Modify when needed
    os.system('rm fluxes*.*')
    os.system('rm snow*.*')
    os.chdir('../../RoutingSetup/input')											# Modify when needed
    os.system('rm fluxes*.*')
    os.chdir('../../Rainfall-runoff')												# Modify when needed
    os.system('./vicNl -g ../Rainfall-runoffSetup/globalparam.txt') 	 			# Modify when needed
    files = [f for f in os.listdir("../Rainfall-runoffSetup/Results") if os.path.isfile(os.path.join("../Rainfall-runoffSetup/Results",f))]
    for file in files:             													# Change the name of VIC file (convert from X - Y to Y - X)
        newfile = "../RoutingSetup/input/" + file				                    # Modify when needed
        file = "../Rainfall-runoffSetup/Results/" + file						    # Modify when needed (this is only correct if you use the provided VIC version)
        os.rename(file,newfile)

    # Run routing model																# Modify when needed
    os.chdir('../Routing/SourceCode')
    os.system('rm *.uh_s')
    os.system('./rout ../../RoutingSetup/configuration.txt')						# Modify when needed

    # Read measured values
    os.chdir('../../RoutingSetup')													# Modify when needed
    text_file = open('Observeddischarge.csv','r')									# Modify when needed
    lines = text_file.read().split('\n')
    count_no = 0
    gaudata = [0 for x in range(number_of_days)]
    for line in lines:
        try:
            gaudata[count_no] = float(line)
            count_no+= 1
        except:
            print("...")															# Ignore lines with errors
    text_file.close()

    # Read modeling results
    text_file = open('../Results/OUTLE.day','r')									# Modify when needed
    lines = text_file.read().split('\n')
    count_no = 0
    VIC_time = [[0 for x in range(3)] for y in range(number_of_days)]
    VIC_release = [0 for x in range(number_of_days)]
    for line in lines:
        try:
            year,month,day,flows = filter(None,line.split(' '))
            VIC_time[count_no][0] = year
            VIC_time[count_no][1] = month
            VIC_time[count_no][2] = day
            VIC_release[count_no] = float(flows)
            if (VIC_release[count_no]<0):
                VIC_release[count_no] = -1											# If there is a negative flow, set to -1
            count_no+=1
        except:
            if (count_no<number_of_days):
                VIC_release[count_no][3] = -1 										# Ignore lines with errors
    text_file.close()

    # Calculate objective functions (NSE, TRMSE, MSDE, ROSE)
    nortrmse = 0
    normsde = 0
    meangautrmse = 0
    for i in range(spinning_period,number_of_days):
        meangautrmse+=gaudata[i]
        normsde+=pow((gaudata[i]-gaudata[i-1])*2,2)
    meangautrmse = meangautrmse /(number_of_days-spinning_period)
    for i in range(spinning_period,number_of_days):
        zobs = (pow((abs(gaudata[i])+1),0.3)-1)/0.3
        nortrmse+=pow(zobs-meangautrmse,2)
    nash = NSE(gaudata[spinning_period:number_of_days],VIC_release[spinning_period:number_of_days])
    trmse = TRMSE(gaudata[spinning_period:number_of_days],VIC_release[spinning_period:number_of_days])
    msde = MSDE(gaudata[spinning_period:number_of_days],VIC_release[spinning_period:number_of_days])
    roce= ROCE(gaudata[spinning_period:number_of_days],VIC_release[spinning_period:number_of_days])

#   Normalized NSE
    if nash<=0:
        standnash = 1
    else:
        standnash = 1 - nash
#   Normalized TRMSE
    standtrmse = trmse/pow(nortrmse/(number_of_days-spinning_period),0.5)
    if (standtrmse>1):
        standtrmse = 1
    elif (standtrmse<0):
        standtrmse = 0
#   Normalized MSDE
    standmsde = msde / normsde * (number_of_days-spinning_period)
    if (standmsde>1):
        standmsde = 1
    elif (standmsde<0):
        standmsde = 0
#   Normalized ROCE
    standroce = 0
    if (roce > 1):
        standroce = 1
    elif (roce<0):
        standroce = 0
    calibrationresults = []
    if (VIC_objs[0]>0):
        calibrationresults.append(standnash)
        print("NSE = ",nash)
    if (VIC_objs[1]>0):
        calibrationresults.append(standtrmse)
        print("TRMSE = ",trmse)
    if (VIC_objs[2]>0):
        calibrationresults.append(standmsde)
        print("MSDE = ",msde)
    if (VIC_objs[3]>0):
        calibrationresults.append(standroce)
        print("ROCE = ",roce)
    return calibrationresults

# MAIN PROGRAM
# Declare optimization problem(Decision variable, number of objective functions) 
# -----------------------------------------------------------------------------------------------------------------------------------
# Read operation file (default: reservoircalibration.txt)
start = datetime.datetime.now()
os.chdir('../../RoutingSetup')
text_file = open('reservoircalibration.txt','r')
lines = text_file.read().split('\n')
number_of_days  = int(lines[2].split('\t')[0])
spinning_period = int(lines[3].split('\t')[0])
number_functions = int(lines[4].split('\t')[0])
pop_size = int(lines[5].split('\t')[0])
number_of_core = int(lines[6].split('\t')[0])
text_file.close()
VIC_vars = [0 for x in range(10)]
VIC_objs = [0 for x in range(4)]
no_of_var = 0
no_of_obj = 0
VIC_types = []
for i in range(10):																			# 10 decision variables considered
    VIC_vars[i] = int(lines[i+8].split('\t')[0])
    if (VIC_vars[i]>0):
        no_of_var+=1
        if (i==0):
            VIC_types.append(Real(0.00001, 0.899999))										#binfil
        elif (i==1):
            VIC_types.append(Real(0.00001, 0.99999))										#Ds
        elif (i==2):
            VIC_types.append(Real(0.00001, 30))												#Dmax
        elif (i==3):
            VIC_types.append(Real(0.00001, 0.99999))										#Ws
        elif (i==4):
            VIC_types.append(Real(1, 3))													#C
        elif (i==5):
            VIC_types.append(Real(0.05, 0.25))												#d1
        elif (i==6):
            VIC_types.append(Real(0.3, 1.5))												#d2
        elif (i==7):
            VIC_types.append(Real(0.3, 1.5))												#d3
        elif (i==8):
            VIC_types.append(Real(0.5, 5))													#Velocity
        elif (i==9):
            VIC_types.append(Real(200, 4000))												#Diffusivity
minobj = []
maxobj = []
eps = []
for i in range(4):																			# 4 objective functions considered
    VIC_objs[i] = int(lines[i+19].split('\t')[0])
    if (VIC_objs[i]>0):
        no_of_obj+=1
        minobj.append(0)																	# Normalize objective functions to the range of 0-1
        maxobj.append(1)
        eps.append(0.001)

# Setup optimization parameters
problem = Problem(no_of_var, no_of_obj)
problem.types[:] = VIC_types
problem.function = viccall
hyp = Hypervolume(minimum=minobj, maximum=maxobj)
x = []

# Start simulations
with ProcessPoolEvaluator(number_of_core) as evaluator:
    algorithm = EpsNSGAII(problem, eps, population_size=pop_size, evaluator = evaluator)
    while algorithm.nfe<=number_functions:
        algorithm.step()
        y = hyp.calculate(algorithm.result)
        x.append(y)
end = datetime.datetime.now()
nondominated_solutions = nondominated(algorithm.result)
os.chdir('../Results')

# Finish and save results to files
print("Start",start)
print("End",end)
print("Finish running simulations! See opt_objectives.txt and opt_variables.txt for results.")
np.savetxt("calibration_objectives.txt",[s.objectives[:] for s in nondominated_solutions],fmt="%s")
np.savetxt("calibration_variables.txt",[s.variables[:] for s in nondominated_solutions],fmt="%s")
print("Hypervolume indicator:")
print(x)
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------ 