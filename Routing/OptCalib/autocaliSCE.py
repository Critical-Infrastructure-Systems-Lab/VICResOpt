# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# Version: 1.0 28 Mar 2020
# Change links and filenames where appropriate
# This is an additional calibration tool based on SCE-UA. In the current version, we only use NSE as an indicator.
# Note: this code is under development and not verified yet.
# Require the following libs
import spotpy
import csv
import os
import numpy
import matplotlib.pyplot as plt
import multiprocessing
import datetime
import pathos
from mpi4py import MPI												# for paralel computing only (MPI)

# This function calls VIC and return the objective function
class spotpy_setup(object):
	rank = 0
	def __init__(self):
		self.params = [spotpy.parameter.Uniform('Ds',low = 0.00001, high = 0.9, optguess = 0.01),
						spotpy.parameter.Uniform('Dsmax', low = 0, high = 30, optguess = 1),
						spotpy.parameter.Uniform('Ws', low = 0.1, high = 0.99, optguess = 0.1),
						spotpy.parameter.Uniform('Binfil', low = 0, high = 0.4, optguess = 0.05),
						spotpy.parameter.Uniform('cexp', low = 1, high = 3, optguess = 0.02),
						spotpy.parameter.Uniform('depth1', low = 0.05, high = 0.25, optguess = 0.01),
						spotpy.parameter.Uniform('depth2', low = 0.3, high = 3.5, optguess = 0.1)
						]
	def parameters(self):
		return spotpy.parameter.generate(self.params)
	def simulation(self,vector):
		# Starting the process
		Ds = vector[0]
		Dsmax = vector[1]
		Ws = vector[2]
		Binfil = vector[3]
		cexp = vector[4]
		depth1 = vector[5]
		depth2 = vector[6]
		global rank
		global number_of_days										# number of simulation days
		global spinning_period
		comm = MPI.COMM_WORLD
		rank = comm.rank + 1
		print("-----------------------------------------------------------------------------------")
		# print("Thread no: ",rank," out of ",multiprocessing.cpu_count())
		# Modify VIC parameters
		os.chdir('Rainfall-runoffSetup')
		text_file = open('soil.txt','r')
		lines = text_file.read().split('\n')
		count_no = 0
		no_of_col = len(lines[0].split('\t'))
		soilparam = [[0 for x in range(no_of_col)] for y in range(len(lines))]
		for line in lines:
			for i in range(0,no_of_col):
				try:
					soilparam[count_no][i] = float(line.split('\t')[i])
				except:
					print(line)
			count_no+=1
		text_file.close()
		with open('soil.txt','w') as my_csv:
			for i in soilparam:
				my_csv.write("%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\t%i\t%i\t%i\t%i\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%i\r\n"
				%(i[0],i[1],i[2],i[3],Binfil,Ds,Dsmax,Ws,cexp,i[9],i[10],i[11],i[12],i[13],i[14],i[15],i[16],i[17],depth1,depth2,i[20],i[21],i[22],i[23],i[24],i[25],i[26],i[27],i[28],i[29],i[30],i[31],i[32],
				i[33],i[34],i[35],i[36],i[37],i[38],i[39],i[40]))
		# Run rainfall-runoff model
		os.chdir('../Results')
		os.system('rm fluxes*.*')
		os.system('rm snow*.*')
		os.chdir('../RoutingSetup/input')
		os.system('rm fluxes*.*')
		os.chdir('../../Rainfall-runoff')
		os.system('./vicNl -g ../Rainfall-runoffSetup/globalparam.txt')
		files = [f for f in os.listdir("../Rainfall-runoffSetup/Results") if os.path.isfile(os.path.join("../Rainfall-runoffSetup/Results",f))]
		for file in files:
			newfile = "../RoutingSetup/input/" + file
			file = "../Rainfall-runoffSetup/Results/" + file
			os.rename(file,newfile)
		# Run routing model
		os.chdir('../Routing/SourceCode')
		os.system('rm *.uh_s')
		os.system('./rout ../RoutingSetup/configuration.txt')
		text_file = open('../../Results/OUTLE.day','r')
		lines = text_file.read().split('\n')
		count_no = 0
		# Read modeling results
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
				print("FINISH READING MODELED FILE ...")
		text_file.close()
		os.chdir('../../')
		return VIC_data[spinning_period:number_of_days][3]

	# Read observed data
	def evaluation(self):
		# Read measured values (activate the global rank line if using MPI)
		# global rank
		# rank = multiprocessing.current_process()._identity[0]
		rank = 1
		comm = MPI.COMM_WORLD
		os.chdir('RoutingSetup')
		global number_of_days										# number of simulation days
		global spinning_period
		#rank = comm.rank + 1
		text_file = open('Observeddischarge.csv','r')
		lines = text_file.read().split('\n')
		count_no = 0
		gaudata = [0 for x in range(number_of_days)]
		for line in lines:
			try:
				gaudata[count_no] = float(line)
				count_no+= 1
			except:
				print("FINISH READING GAUGED FILE ...")
		text_file.close()
		os.chdir('..')
		return gaudata[spinning_period:number_of_days]

	# Calculate objective functions
	def objectivefunction(self,simulation,evaluation):
		objectivefunction = spotpy.objectivefunction.nash(evaluation,simulation)
		return objectivefunction

# Declare optimization problem
os.chdir('../../')
number_of_days = 2130
spinning_period = 365
numpy.random.seed(10)
#vicrun = spotpy.algorithms.sceua(spotpy_setup(), parallel='mpi') << run this line for paralization
vicrun = spotpy.algorithms.sceua(spotpy_setup())
vicrun.sample(5)
results = sampler.getdata()
spotpy.analyser.plot_parametertrace(results)
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------ 