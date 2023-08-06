# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# Visualization: optimization for reservoir operations 
# ------------------------------------------------------------------------------------------------------------------------------------------------
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import pandas as pd

# ------------------------------------------------------------------------------------------------------------------------------------------------
# MODIFY THIS PART WHERE APPROPRIATE
link1 = '../Results/optimization_objectives.txt'							# Link to the result file: calibration_variables.txt
link2 = '../RoutingSetup/reservoiroptimization.txt'							# Link to the configuration file: reservoircalibration.txt
# ------------------------------------------------------------------------------------------------------------------------------------------------

# Open operation file
text_file = open(link2,'r')
lines = text_file.read().split('\n')
totalobs = 0
objs = [0 for x in range(6)]
labels = []
countdata = 0
for i in range(6):
    objs[i] = int(lines[9].split(' ')[i])
    if (objs[i]>=1):
        totalobs+=1
    if (i==0): 
        labels.append('Annual water deficit (m3)')
    elif (i==1): 
        labels.append('Annual hydropower production (GWh)')
    elif (i==2): 
        labels.append('Firm hydropower production (GWh)')
    elif (i==3): 
        labels.append('Peak flow (m3/s)')
    elif (i==4): 
        labels.append('Number of days above predefined flood threshold (days)')
    elif (i==5): 
        labels.append('Deviation from pre-defined water level (m)')

# Read result data
path = open(link1,'r')
lines = path.read().split('\n')
countdata = 0
VIC_obs = [[0 for x in range(len(lines[0].split(' ')))] for y in range(len(lines)-1)]
for line in lines:
    for i in range(len(line.split(' '))):
        try:
            VIC_obs[countdata][i] = float(line.split(' ')[i])
        except:
            print("...")													# the last line is blank
    countdata+=1
path.close()

# Plot data 2D
if (totalobs>=2):
    for i in range(countdata-1):
        for j in range(i+1,countdata):
            x = np.array(VIC_obs)[:,i]
            y = np.array(VIC_obs)[:,j]
            plt.subplots(figsize=(10, 10))
            plt.grid(alpha=0.8, c="gray")
            plt.scatter(x, y, s=100, c="g", alpha=0.6667, edgecolors='black', linewidths=0.6667)
            plt.xlabel(labels[i], size=18, labelpad=5)
            plt.ylabel(labels[j], size=18, labelpad=5)
            plt.tick_params(axis='x', labelsize=18, pad=5)
            plt.tick_params(axis='y', labelsize=18, pad=5)
            plt.show()

# Plot data 3D
if (totalobs>=3):
     fig = plt.figure(figsize=(20, 20))
     ax = fig.add_subplot(111, projection='3d')
     for i in range(countdata-2):
         for j in range(i+1,countdata-1):
              for k in range(i+2,countdata):
                  x = np.array(VIC_obs)[:,i]
                  y = np.array(VIC_obs)[:,j]
                  z = np.array(VIC_obs)[:,k]
                  ax.scatter(x, y, z, s=100, c="g", alpha=0.6667, edgecolors='black', linewidths=0.6667)
                  ax.view_init(20, 20)
                  ax.tick_params(axis='x', labelsize=18, pad=5)
                  ax.tick_params(axis='y', labelsize=18, pad=5)
                  ax.tick_params(axis='z', labelsize=18, pad=5) 
                  ax.set_xlabel(labels[i], size=18, labelpad=20)
                  ax.set_ylabel(labels[j], size=18, labelpad=20)
                  ax.set_zlabel(labels[k], size=18, labelpad=20)
                  plt.show()
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------