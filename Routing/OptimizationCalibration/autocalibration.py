# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# Configuration file: reservoircalibration.txt
# Result files: calibration_objectives.txt and calibration_variables.txt
# Version: 1.0 23 Oct 2019
# Change links and filenames where appropriate
# Require the following libs
from platypus import EpsNSGAII, Problem, Real, ProcessPoolEvaluator, Hypervolume, nondominated
import os
import csv
import numpy as np
import multiprocessing
import datetime

# Function to call VIC
def viccall(vars):
    # Declare decision variables (VIC)
    global no_of_var   																# number of decision variables
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
    no_of_row = len(lines[0])
    soilparam = [[0 for x in range(no_of_row)] for y in range(len(lines))]
    for line in lines:
        for i in range(0,no_of_row):
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
                            sl[18] = vars[count_no]									#d1
                            count_no+=1
                        elif (k==6):
                            sl[19] = vars[count_no]									#d2
                            count_no+=1
                        elif ((k==7) and (no_of_row==43)):
                            sl[20] = vars[count_no]									#d3 (in case the VIC model has more than 3 layers; modify this Python code accordingly)
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
    text_file = open('configuration.txt','r')										# Modify flow routing file
    lines = text_file.read().splitlines()    										# 1st running, lines in the configuration file does not contain \n
    if (len(lines)<5):
        lines = text_file.read().split('\n')
    with open('configuration.txt','w') as my_csv:									# Modify when needed
        for i in range(31):
            if ((i==5) and (VIC_vars[8]>0)):
                my_csv.write("%f\n"%(vars[count_no]))								#velocity
                count_no+=1
            elif ((i==9) and (VIC_vars[9]>0)):
                my_csv.write("%f\n"%(vars[count_no]))								#diffusivity
                count_no+=1
            else:
                my_csv.write("%s\n"%(lines[i]))
    text_file.close() 

    # Run rainfall-runoff model and change filename
    os.chdir('../Rainfall-runoffSetup/Results')										# Modify when needed
    os.system('rm fluxes*.*')
    os.system('rm snow*.*')
    os.chdir('../../RoutingSetup/input')											# Modify when needed
    os.system('rm fluxes*.*')
    os.chdir('../../Rainfall-runoff')												# Modify when needed
    os.system('./vicNl -g ../Rainfall-runoffSetup/globalparam.txt') 	 			# Modify when needed
    files = [f for f in os.listdir("../Rainfall-runoffSetup/Results") if os.path.isfile(os.path.join("../Rainfall-runoffSetup/Results",f))]
    for file in files:             													# Change the name of VIC file (for VIC 4.2)
        if (len(file)>=22):
            if (len(file)==22):
                coorX = file[7:14]
                coorY = file[15:22]
            else:
                coorX = file[7:15]
                coorY = file[16:23]
            newfile = "../RoutingSetup/input/fluxes_"+coorY+"_"+coorX       		# Modify when needed
            file = "../Rainfall-runoffSetup/Results/" + file            			# Modify when needed
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
    VIC_data = [[0 for x in range(4)] for y in range(number_of_days)]
    for line in lines:
        try:
           year,month,day,flows = filter(None,line.split(' '))
           VIC_data[count_no][0] = year
           VIC_data[count_no][1] = month
           VIC_data[count_no][2] = day
           VIC_data[count_no][3] = float(flows)
           count_no+=1
        except:
            print("...")															# Ignore lines with errors
    text_file.close()

    # Calculate objective functions (NSE, TRMSE, MSDE, ROSE)
    trmse = 0
    qmean = 0
    numerator = 0
    denominator = 0
    summdl = 0
    sumobs = 0
    nortrmse = 0
    normsde = 0
    meangau = 0
    msde = 0
    meangautrmse = 0
    roce = 0
    for i in range(spinning_period,number_of_days):
        zmdl = (pow((abs(VIC_data[i][3])+1),0.3)-1)/0.3
        zobs = (pow((abs(gaudata[i])+1),0.3)-1)/0.3
        trmse+=pow(zmdl-zobs,2)
        meangautrmse+=gaudata[i]
        numerator+=pow(abs(VIC_data[i][3])-gaudata[i],2)
        meangau+=gaudata[i]
        summdl+=VIC_data[i][3]
        sumobs+=gaudata[i]
        msde+=pow(((gaudata[i]-gaudata[i-1])-(VIC_data[i][3]-VIC_data[i-1][3])),2)
        normsde+=pow((gaudata[i]-gaudata[i-1])*2,2)
    meangau = meangau/(number_of_days-spinning_period)
    meangautrmse = meangautrmse /(number_of_days-spinning_period)
    for i in range(spinning_period,number_of_days):
        denominator+=pow(gaudata[i]-meangau,2)
        zobs = (pow((abs(gaudata[i])+1),0.3)-1)/0.3 
        nortrmse+=pow(zobs-meangautrmse,2)
    nash = 1 - numerator/denominator
    roce= abs(summdl-sumobs)/sumobs
    if nash<=0:
        standnash = 1
    else:
        standnash = 1 - nash
    trmse = pow(trmse/count_no,0.5)
    msde = msde/(count_no-1)
#   Normalized TRMSE
    trmse = trmse/pow(nortrmse/count_no,0.5)
    if (trmse>1):
        trmse = 1
    elif (trmse<0):
        trmse = 0
#   Normalized MDSE
    msde = msde / normsde * (count_no - 1)
    if (msde>1):
        msde = 1
    elif (msde<0):
        msde = 0
#   Normalized ROCE
    if (roce > 1):
        roce = 1 
    elif (roce<0):
        roce = 0
    calibrationresults = []
    if (VIC_objs[0]>0):
        calibrationresults.append(standnash)
        print("NSE = ",standnash)
    if (VIC_objs[1]>0):
        calibrationresults.append(trmse)
        print("TRMSE = ",trmse)
    if (VIC_objs[2]>0):
        calibrationresults.append(msde)
        print("MSDE = ",msde)
    if (VIC_objs[3]>0):
        calibrationresults.append(roce)
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
print(x)