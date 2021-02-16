# Main control file for building HSPF model files

import os, csv, pickle, datetime, math
import pandas as pd
import shapefile
from pyhspf import Subbasin, Watershed, HSPFModel, WDMUtil
from array import *

# Source the functions for processing and builing model
exec(open('c:/siletz_tmdl/04_scripts/01_hspf/01_python/fnct_build_hspf.py').read())

# Specify the land use file and path
hruF = 'C:/siletz_tmdl/01_inputs/01_hspf/lulc/hru_2011.csv'
# Specify the precipitation file and path
pcpF = 'C:/siletz_tmdl/01_inputs/01_hspf/pcp/pcp_base.csv'
# Specify the PET file and path
petF = 'C:/siletz_tmdl/01_inputs/01_hspf/pet/pet_base.csv'
# Specify the model file and path (where to write the model)
modF = 'C:/siletz_tmdl/03_models/01_hspf/siletz.hspf'

# BUILD THE MODEL
build_hspf_model(hruF, pcpF, petF, modF)
