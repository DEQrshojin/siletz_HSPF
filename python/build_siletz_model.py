# CREATE SILETZ HSPF MODEL
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# This script creates the initial instance of the HSPF model for the
# Siletz River. Once executed, the model does not need to be recreated

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

import os, csv, pickle, datetime, math
import pandas as pd
import shapefile
from pyhspf import Subbasin, Watershed, HSPFModel, WDMUtil
from array import *

start = datetime.datetime(2004, 1, 1)

end = datetime.datetime(2018, 4, 1)

tstep = 60

lc_codes = ['FORLO', 'FORHI', 'DEVLO', 'DEVHI', 'GRSLO',
            'GRSHI', 'CULLO', 'CULHI', 'IMPRV']

wdmFile = 'D:/siletz/siletz_in.wdm'

mssgpath = 'D:/siletz/hspfmsg.wdm'

# Source functions
exec(open('D:/siletz/scripts/python/build_watershed_functions.py').read())

exec(open('D:/siletz/scripts/python/wdm_functions.py').read())

# READ IN LAND COVER DATA
with open('D:/siletz/inputs/hru.csv') as hruFile:
    
    readTables = csv.reader(hruFile, delimiter = ',')
    
    hru_df = []
    
    for hruRow in readTables:

        hru_df.append(hruRow)

# Convert the items in each row-wise list to float
lists = 0

for i in hru_df:

    hru_df[lists] = [float(hru) for hru in hru_df[lists]]

    lists += 1

# fraction of developed land that is impervious (assume all impervious land)
ifraction = 1.

# Read and input the FTables
fTables = read_ftables('D:/siletz/inputs/ftab.csv')

# READ IN BASIN, REACH AND OUTLET SHAPEFILES
basinShp = shapefile.Reader('D:/siletz/inputs/shp/basins.shp')

reachShp = shapefile.Reader('D:/siletz/inputs/shp/reaches.shp')

basinRecords = basinShp.records()

reachRecords = reachShp.records()

# Send the import data to the pre-build processors
subbasins = create_subbasins(basinRecords,
                             reachRecords,
                             2011,
                             lc_codes,
                             hru_df,
                             fTables)

flow_network = create_flownetwork(basinRecords)

# CREATE HSPF MODEL
watershedSiletz = Watershed("Siletz", subbasins)

watershedSiletz.add_mass_linkage(flow_network)

for basin in range(0, len(basinRecords)):
    
    if basinRecords[basin][6] == 0:
        
        watershedSiletz.add_outlet(str(basin + 1)) # Assumes basin numbers     
        
    x = 1 # Don't need this but the loop wants to include 'hspfmodel...'

# Build the model
hspfmodel = HSPFModel(units = 'Metric')

filename = 'siletz'

outfile = filename + '.out'

wdmoutfile = filename + '_out.wdm'

hspfmodel.build_from_watershed(watershedSiletz,
                               'siletz',
                               ifraction = ifraction,
                               tstep = tstep,
                               print_file = outfile)

watershedSiletz.plot_mass_flow(output = 'siletz_network')

# ADD TIME SERIES DATA: PRECIP, PET, and FLOW TO THE WDM FILE
pcpData = pd.read_csv('D:/siletz/inputs/pcp.csv')

petData = pd.read_csv('D:/siletz/inputs/pet.csv')
    
ts_to_wdmFile(wdmFile = wdmFile, pcpData = pcpData, petData = petData)

# Read the data from the WDM file
wdm = WDMUtil(verbose = True, messagepath = mssgpath)

# ADD BASIN TIMESERIES FROM THE WDM TO HSPFMODEL
# open the wdm for read access
wdm.open(wdmFile, 'r')

start, end = wdm.get_dates(wdmFile, 101)

x = 1 # Counter for the input data DSNs - 101 = precip, 201 = PET

# Add specific basin met data
for basin in range(0, len(basinRecords)):

    # The DSNs f0r input data - 101 = precip, 201 = PET
    prcp = wdm.get_data(wdmFile, 100 + x)

    evap = wdm.get_data(wdmFile, 200 + x)

    # Add and assign timeseries data
    hspfmodel.add_timeseries('precipitation', ('prcp_' + str(x)), start, prcp,
                             tstep = tstep)

    hspfmodel.add_timeseries('evaporation', ('evap_' + str(x)), start, evap,
                             tstep = tstep)
    
    # Assign to specific basin
    hspfmodel.assign_subbasin_timeseries('precipitation', str(basin + 1),
                                         ('prcp_' + str(x)))

    hspfmodel.assign_subbasin_timeseries('evaporation', str(basin + 1),
                                         ('evap_' + str(x)))

    x += 1

wdm.close(wdmFile)

# COMPLETE AND EXECUTE MODEL
hspfmodel.add_hydrology()

hspfmodel.add_sediment()

with open('D:/siletz/siletz', 'wb') as f: pickle.dump(hspfmodel, f)

print('\nsuccessfully created the HSPF model of the Siletz River."\n')

