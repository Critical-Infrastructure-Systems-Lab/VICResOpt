# VIC-ResOpt: Optimizing water reservoir Operations in the Variable Infiltration Capacity model

The Variable Infiltration Capacity (VIC) model is a popular large-scale hydrological model developed and maintained by the University of Washington, US. VIC has been used extensively to study and model river basins around the world. The model simulates key hydrological processes occurring in a river basin (e.g., water and energy balances, streamflow routing), but does not account for the operations of artificial water reservoirs. Since the environmental impact of reservoirs comes under ever-increasing scrutiny, we developed a variant of VIC that allows to: 

* Account for the presence of artificial water reservoirs;
* Optimize their operations; 
* Explore the sensitivity of the model output w.r.t. the parameters of the operating rules;
* Automatically calibrate the model parameters.

### Prerequisites

VIC-ResOpt is based on VIC 4.2 -— the last release of the VIC Version 4 development track, which has a large number of active users. The key difference between VIC 4.2 and VIC-ResOpt stands in the routing module (written in FORTRAN), which has been modified to account for the presence and operations of water reservoirs. The two models share the same rainfall-runoff module (written in C). Both VIC and VIC-ResOpt have been developed for Linux and Unix platforms, and they require the GNU and G77 compilers. 

The optimization of the rule curves/operating rules, model sensitivity analysis, and automatic calibration of the model parameters can be executed on both Python 2.7x and 3.x. The three functionalities require the following Python packages: (i) Numpy, (ii) os, (iii) multiprocessing, (iv) scipy, (v) numba, (vi) Matplotlib (optional for plotting), and (vii) plotly (optional for parallel coordinate plots). 

The optimization-related components also require [Platypus](https://github.com/Project-Platypus/Platypus) - a platform for Multi-Objective Evolutionary Algorithms (MOEAs) in Python.

The sensitivity analysis requires the Sensitivity Analysis for Everybody ([SAFE](https://www.safetoolbox.info/info-and-documentation/)) toolbox (Python version), developed by Pianosi et al. (2015).

VIC-ResOpt has been tested on Linux Ubuntu 14.04 LTS (Trusty Tahr) and 16.04 (Xenial Xerus) operating systems.

### Major Update

* 12 Jun 2022: fix lat - lon bug for example files
* 28 Dec 2020: fix dead storage bug; add water withdrawal (permanent loss) due to irrigation activities.
* 17 May 2020: add multicalieNSGAII.py for multi-site calibration.
* 28 Mar 2020: add viccall.py to call VIC and Rout together; add autocaliSCE.py for automatic calibration with SCE-UA.
* 10 Mar 2020: rename autocalibration.py to autocalieNSGAII.py; add indices.py and functions.py; fix 3L-soil layer bug.

### How to run

**A. How to run the reservoir component of VIC-ResOpt**

This task can be carried out through the following steps:

1 Download/compile the source code (by using GNU and G77/GFORTRAN)

2 Prepare the input files:

* Climate forcings (e.g. precipitation, maximum/minimum temperature, wind, etc.)
* Land use
* Soil parameters
* Flow direction
* Location of basin outlet
* Location of reservoirs (new)
* Reservoir parameters (new)

3 Configure the output files

More information on how to run the original version of VIC can be found here:
```
https://vic.readthedocs.io/en/master/
```
The sequence of syntax to run VIC-ResOpt is similar to the one adopted by VIC.

**B. How to run Optimization for rule curves/operating rules**

The Python code and configuration file for this function are *optimization.py* and *reservoiroptimization.txt*. There are four required tiers of information:

* Simulation parameters: length of simulation and spinning periods, number of function evaluations, population size, and number of computer cores.
* Objective functions: there are six options, which can be activated using the numbers 0 (not to consider) or 1 (consider).
	* Option 1: Minimize the average annual deficit of downstream water supply (m3) 
	* Option 2: Maximize the average annual hydropower production (MWh)
	* Option 3: Maximize the firm hydropower production (MWh)
	* Option 4: Minimize the peak flow (m3/s)
	* Option 5: Minimize the the number of days above a pre-defined flood threshold (days)
	* Option 6: Minimize the average annual deviation from a pre-defined water (m)
* List of reservoirs considered in the optimization problem.
* Additional data: path to water demand file (for Option 1), observed peak flow (m3/s; for Option 4), flooding threshold (m3/s; for Option 5), and path for the pre-defined reservoir water levels (365 values in m; for Option 6).

This function creates *optimization_objectives.txt* and *optimization_variables.txt* files as a result.

**C. How to run eFAST and EET analyses**

Users can call *eFAST_analysis.py* or *EET_analysis.py*. These two functions fist create two sampling sets, which are saved  in the files called *eFASTparameters.txt* and *EETparameters.txt*. VIC-ResOpt is then fed by the sampling sets. eFAST or EET provide either sensitivity indices (eFAST; in files *SiNSE.txt* and/or *SiTRMSE.txt*) or mean indices (EET; in files *miNSE.txt* and/or *miTRMSE.txt*).

In the configuration file, uers need to provide the following information:

* Simulation parameters: length of the simulation and spinning periods, and number of computer cores.
* VIC model parameters: 0 (not to consider) and 1 (consider)
	* Option 1: Variable infiltration curve parameter, b
	* Option 2: Fraction of Dmax where non-linear baseflow begins, Ds
	* Option 3: Maximum velocity of baseflow, Dmax (mm/day)
	* Option 4: Fraction of maximum soil moisture where non-linear baseflow occurs, Ws
	* Option 5: Exponent of the nonlinear part of the baseflow curve, c
	* Option 6: Thickness of top soil moisture layer, Depth 1 (m)
	* Option 7: Thickness of 2nd soil layer, Depth 2 (m)
	* Option 8: Thickness of 3rd soil layer, Depth 3 (m)
	* Option 9: Wave velocity in the channel routing (m/s)
	* Option 10: Diff diffusivity in the channel routing (m2/s)
* The list of reservoirs considered in the sensitivity analysis.
* Fitness functions: there are two options (NSE and TRMSE).

**D. How to run Automatic calibration**

The function Automatic calibration is stored in *autocalibration.py*; its configuration file is named *reservoircalibration.txt*. The results are stored in *calibration_objectives.txt* and *calibration_variables.txt*.

The configuration is organized similarly to the sensitivity analysis configuration file. However, the parameters of the reservoir operating rules are not taken into account in this case.

As for the objective functions, users can choose among the Nash-Sutcliffe Efficiency (NSE), which measures the model accuracy primarily during high-flow periods; Box-Cox Transformed Root Mean Squared Error (TRMSE), which accounts for low-flow periods; Runoff COefficient Error (ROCE), which accounts for the long-term water balance; and Mean Squared Derivative Error (MSDE), an indicator of the fit of the hydrograph shape.

### How to customize

**A. How to customize the reservoir component of VIC-ResOpt**

Representation of water reservoirs:
The reservoir location is stored in the file *reservoirlocation.txt*, which has the same number of rows and columns of the flow direction matrix. This file contains integer numbers:

* '1' - '9998' represent the ID of reservoirs/dams
* '9999' represents open water surface (reservoir cells)
* '0' refers to other land use

Note: avoid using No. (n+1) for reservoir name (n is the number of reservoirs) because this is used for the basin outlet.

Operation of water reservoirs:
For each reservoir (e.g., ID = 1), there is a reservoir configuration file (e.g., *res1.txt*), containing specifications on the maximum water level (*m*), minimum water level (*m*), storage capacity (*1000 m3*), hydraulic head (*m*; optional for the calculation of hydropower production), design discharge (*m3/s*), year of commission, initial water volume (*1,000 m3*), reservoir name, seepage rate (*m3/s*), infiltration rate (*m3/s*), and operation strategy. There are four operating strategies adopted in this software package

* Option 1: Simplified rule curve. There are four parameters required, including predefined maximum and minimum water levels (H1 and H2) at the time T1 and T2 at which these levels are reached.
* Option 2: Rule curve. There are monthly target water levels.
* Option 3: Operating rule. The rule curve is defined via four parameters (x1, x2, x3 and x4).
* Option 4: Predefined release time series data. Users must provide a link to a time series, containing daily release data over the simulation horizon. Although the time series data are provided, the real amount of water released is dependent on physical contraints of the reservoirs (e.g. maximum design discharge) as well as the available water.

**B. How to customize other components (optimization, sensitivity analysis, and automatic calibration)**

Each of these functions has a configuration file as (mentioned above). Users only need to modify this configuration file, so they do not need to change the current file/folder arrangement. The current version of VIC-ResOpt is available with the eps-NSGA-II algorithm, so in case another MOEA algorithm is chosen, please change the algorithm's parameters accordingly.

**C. How to implement parallel modelling**

Users can parallelize modelling steps in optimization, sensitivity analyses, and automatic calibration via the two following steps:

* Modify *the number of cores* parameter in configuration files.
* Modify the link to the folder where modelling results are stored in Python codes, so there is no conflict in data storage.

### Examples

VIC-ResOpt is demonstrated via two case studies

- A ‘toy’ case study, that is, a small catchment with dimension of 5 x 5 (cell) (see *Rainfall-runoffSetup* and *RoutingSetup* folders). The representation of water reservoirs is exemplified in the file *reservoirlocation.txt*, which contains three reservoirs (Reservoir 1, Reservoir 2, and Reservoir 3; IDs 1, 2 and 3). The files containing all specifications of these reservoirs are named *res1.txt* (rule curve),*res2.txt* (operating rule), and *res3.txt* (pre-defined release).

- The case study of the Upper Chao Phraya river basin in Thailand (see *Rainfall-runoffSetupChaoPhraya* and *RoutingSetupChaoPhraya*). The basin, which has a drainage area of 110,000 km2, is modelled by using VIC-ResOpt with 4,426 cells (0.0625 x 0.0625 degree). Rainfall time series data are from the Asian Precipitation Highly Resolved Observational Data Integration Towards Evaluation of Water Resources (APHRODITE) product, which is developed based on multiple-gauging stations. Other climatic forcings, including maximum/minimum temperature and wind speeds, are derived from the NCEP Climate Forecast System Reanalysis (CFSR). These datasets are available at different spatial resolutions and are interpolated to the resolution of the adopted VIC cell size by the bilinear interpolation method. This method calculates a grid-based value based on its four neighbouring values, considering distance weighting. Land cover data which are obtained from the Global Land Cover Project (GLCP) database, and soil characteristics are obtained from the Harmonized World Soil Database (HWSD). These datasets have a spatial resolution of 30 arcseconds (around 1 km), so they are rescaled to the resolution of VIC using the majority interpolation method. This method assigns the most frequent values of the pixels inside the search space for the pixel with the unknown value. Due to the large size of climate forcing data and limited available storage of this repository, we do not upload the forcing input. However, these data are freely available on the website of the providers or available from us (please refer to the contacts listed below). They can be also replaced by other products. 

## Acknowledgments

VIC-ResOpt development is supported by Singapore's Ministry of Education (MoE) via the Tier 2 project "Linking water availability to hydropower supply-an engineering system approach" (Award No. MOE2017-T2-1-143).

### Contact

For questions and feedback related to VIC-ResOpt—and requests to fix possible bugs—please send an email to ddt21@usf.edu (Thanh Dang). Alternatively, you may reach out to stefano_galelli@sutd.edu.sg (Stefano Galelli).

### Citation

If you use VIC-Res, please cite the following papers:

*Dang, T.D., Chowdhury, AFMK, Galelli, S. (2020) On the representation of water reservoir storage and operations in large-scale hydrological models: implications on model parameterization and climate change impact assessments. Hydrology and Earth System Sciences, 24, 397–416. [Link](https://hess.copernicus.org/articles/24/397/2020/hess-24-397-2020.html)

*Dang, T.D., Vu, T.D. Chowdhury, K., Galelli, S. (2020) A software package for the representation and optimization of water reservoir operations in the VIC hydrologic model. Environmental Modelling & Software, 126, 104673. [Link](https://www.sciencedirect.com/science/article/abs/pii/S1364815219310291?via%3Dihub)
