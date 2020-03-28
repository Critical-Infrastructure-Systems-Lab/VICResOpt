# coding: utf-8
# Code: Resilient Water Systems Group / Singapore University of Technology and Design (https://people.sutd.edu.sg/~stefano_galelli/)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# This short script is to run the rainfall-runoff and routing modules together
# Version: 1.0 28 Mar 2020
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
# The following code is to rename the VIC output and move to routing module 
# (Only need when you run VIC-Res). Otherwise, remove Lines 18-29.
files = [f for f in os.listdir("../Rainfall-runoffSetup/Results") if os.path.isfile(os.path.join("../Rainfall-runoffSetup/Results",f))]
for file in files:
        if (len(file)>=22):
            if (len(file)==22):
                toadox = file[7:14]
                toadoy = file[15:22]
            else:
                toadox = file[7:15]
                toadoy = file[16:23]
            newfile = "../RoutingSetup/input/fluxes_"+toadoy+"_"+toadox
            file = "../Rainfall-runoffSetup/Results/" + file
            os.rename(file,newfile)
# Run routing model
os.chdir('../Routing/SourceCode')
os.system('rm *.uh_s')
os.system('./rout ../../RoutingSetup/configuration.txt')