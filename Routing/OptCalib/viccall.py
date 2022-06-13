# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# This short script is to run the rainfall-runoff and routing modules together
# Version: 1.0 28 Mar 2020
# Corrected Jun 12 2022
import os, sys
import numpy
# Run rainfall-runoff model
os.chdir('../../Rainfall-runoffSetup/Results')
os.system('rm fluxes*.*')
os.system('rm snow*.*')
os.chdir('../../RoutingSetup/input')
os.system('rm fluxes*.*')
os.chdir('../../Rainfall-runoff')
os.system('./vicNl -g ../Rainfall-runoffSetup/globalparam.txt')
# The following code is to move fluxes files to routing modulea 
files = [f for f in os.listdir("../Rainfall-runoffSetup/Results") if os.path.isfile(os.path.join("../Rainfall-runoffSetup/Results",f))]
for file in files:
    newfile = "../RoutingSetup/input/" + file
    file = "../Rainfall-runoffSetup/Results/" + file
    os.rename(file,newfile)
# Run routing model
os.chdir('../Routing/SourceCode')
os.system('rm *.uh_s')
os.system('./rout ../../RoutingSetup/configuration.txt')