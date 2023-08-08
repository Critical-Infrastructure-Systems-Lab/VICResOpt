# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# Visualization: automatic calibration for VIC
# This module plots a parallel coordinate plot for VIC parameters
# ------------------------------------------------------------------------------------------------------------------------------------------------

import plotly.graph_objects as go
import pandas as pd
import numpy as np

# ------------------------------------------------------------------------------------------------------------------------------------------------
# MODIFY THIS PART WHERE APPROPRIATE
link1 = '../Results/calibration_variables.txt'								# Link to the result file: calibration_variables.txt
link2 = '../RoutingSetup/reservoircalibration.txt'							# Link to the configuration file: reservoircalibration.txt
# ------------------------------------------------------------------------------------------------------------------------------------------------

# Open operation file
text_file = open(link2,'r')
lines = text_file.read().split('\n')
no_var = 0
no_fitness = 0
my_xticks = []
VIC_vars_ord = [0 for x in range(10)]
VIC_vars_visible = [0 for x in range(10)]
countdata = 0
for i in range(10):															# 10 parameters considered
    if (int(lines[i+8].split('\t')[0])>0):
        VIC_vars_visible[i]=True
        VIC_vars_ord[i]=countdata
        countdata+=1
    else:
        VIC_vars_visible[i]=False
        VIC_vars_ord[i] = 0

# Read result data
path = open(link1,'r')
lines = path.read().split('\n')
countdata = 0
VIC_vars = [[0 for x in range(len(lines[0].split(' ')))] for y in range(len(lines)-1)]
for line in lines:
    for i in range(len(line.split(' '))):
        try:
            VIC_vars[countdata][i] = float(line.split(' ')[i])
        except:
            print("...")													# the last line is blank
    countdata+=1
path.close()
VIC_vars = np.asarray(VIC_vars)
fig = go.Figure(data=
    go.Parcoords(
        line = dict(colorscale = 'Electric',
                   showscale = True),
        dimensions = list([
            dict(range = [0,1],
                 visible = VIC_vars_visible[0],
                 label = "b", values = VIC_vars[:,VIC_vars_ord[0]]),		# binfil
            dict(range = [0,1],
                 visible = VIC_vars_visible[1],
                 label = 'Ds', values = VIC_vars[:,VIC_vars_ord[1]]),		# Ds
            dict(range = [0,30],
                 visible = VIC_vars_visible[2],
                 label = 'Dmax', values = VIC_vars[:,VIC_vars_ord[2]]),		# Dmax
            dict(range = [0,1],
                 visible = VIC_vars_visible[3],
                 label = 'C', values = VIC_vars[:,VIC_vars_ord[3]]),		# C
            dict(range = [1,3],
                 visible = VIC_vars_visible[4],
                 label = 'Ws', values = VIC_vars[:,VIC_vars_ord[4]]),		# Ws
            dict(range = [0.05,0.25],
                 visible = VIC_vars_visible[5],
                 label = 'd1', values = VIC_vars[:,VIC_vars_ord[5]]),		# d1
            dict(range = [0.3,1.5],
                 visible = VIC_vars_visible[6],
                 label = 'd2', values = VIC_vars[:,VIC_vars_ord[6]]),		# d2
            dict(range = [0.3,1.5],
                 visible = VIC_vars_visible[7],
                 label = 'd3', values = VIC_vars[:,VIC_vars_ord[7]]),		# d3
            dict(range = [0.5,5],
                 visible = VIC_vars_visible[8],
                 label = 'Velocity', values = VIC_vars[:,VIC_vars_ord[8]]),	# Velocity
            dict(range = [200,4000],
                 visible = VIC_vars_visible[9],
                 label = 'Diffusivity', values = VIC_vars[:,VIC_vars_ord[9]])])	# Diffusivity
    )
)
fig.show()
# END OF FILE
# ------------------------------------------------------------------------------------------------------------------------------------------------