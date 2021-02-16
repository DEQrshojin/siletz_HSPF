# CONTROL SCRIPT FOR SILETZ CALIBRATION IN PYTHON
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

###############################################################################
# WARNING - DO NOT EDIT THE PYTHON FILE (.py), EDIT THE SCRIPT TEMPLATE (.tmpl)
###############################################################################

import pandas as pd
import numpy as np
import csv, os, datetime, numpy, pickle, time
from pyhspf import HSPFModel, WDMUtil

# SET MODEL NAMES
newNme = 'base'

modelNames = ['siletz.hspf', newNme + '.hspf']

# Source the functions modify_parameters, run_model, and proc_results
pyPath = 'c:/siletz_tmdl/04_scripts/01_hspf/01_python/'
exec(open(pyPath + 'fnct_mod_pars.py').read())
exec(open(pyPath + 'fnct_run_hspf.py').read())
exec(open(pyPath + 'fnct_proc_res_hspf.py').read())

# Run the functions (model)
modify_parameters(modelNames[0], modelNames[1], 'c:/siletz_tmdl/01_inputs/01_hspf/h_pars/' + newNme)

run_siletz_model(modelNames[1], 'runoff')

proc_hspf_results(newNme, 'runoff')

run_siletz_model(modelNames[1], 'reach_flow')

proc_hspf_results(newNme, 'reach_flow')

