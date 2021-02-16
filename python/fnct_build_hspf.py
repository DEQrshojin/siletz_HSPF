# CREATE SILETZ HSPF MODEL
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# These functions create the initial instance of the HSPF model for the
# Siletz River. Once executed, the model does not need to be recreated

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

# FUNCTION TO CREATE THE INITIAL MODEL INSTANCE
def build_hspf_model(hruF, pcpF, petF, modF):

    start = datetime.datetime(2004, 1, 1)

    end = datetime.datetime(2018, 4, 1)

    tstep = 60

    lc_codes = ['FORLO', 'FORHI', 'DEVLO', 'DEVHI', 'GRSLO',
                'GRSHI', 'CULLO', 'CULHI', 'IMPRV']

    wdmFile  = 'c:/siletz_tmdl/03_models/01_hspf/siletz_in.wdm'

    mssgpath = 'c:/siletz_tmdl/03_models/01_hspf/hspfmsg.wdm'

    # READ IN LAND COVER DATA
    with open(hruF) as hruFile:
    
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
    fTables = read_ftables('c:/siletz_tmdl/01_inputs/01_hspf/ftab/ftab.csv')

    # READ IN BASIN, REACH AND OUTLET SHAPEFILES
    basinShp = shapefile.Reader('c:/siletz_tmdl/01_inputs/01_hspf/shp/basins.shp')

    reachShp = shapefile.Reader('c:/siletz_tmdl/01_inputs/01_hspf/shp/reaches.shp')

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
    pcpData = pd.read_csv(pcpF)

    petData = pd.read_csv(petF)
    
    ts_to_wdmFile(wdmFile = wdmFile,
                  pcpData = pcpData,
                  petData = petData)

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

    with open(modF, 'wb') as f: pickle.dump(hspfmodel, f)

    print('\nsuccessfully created the HSPF model of the Siletz River."\n')

# FUNCTION TO CREATE SUBBASINS AND ADD REACH AND BASIN DATA (EXCEPT LANDUSE)
def create_subbasins(basinRecords, reachRecords, year, lc_codes, hru_df, fTables):

    subbasins = {}  # Create the basins for populating with characteristics

    # 1) Compile watershed model from shapefile data
    for basin in range(0, len(basinRecords)):
        
        # Read basin data
        number = str(basinRecords[basin][3]) # The HSPF Basin Number
        planeslope = basinRecords[basin][4] / 100  # length / length
        elev = (reachRecords[basin][7] + reachRecords[basin][6])/2 # (max z + min z) / 2
        centroid = [basinRecords[basin][7], basinRecords[basin][8]] # [lat, lon]
        length = 200 # Overland flow distance in metres (generic value ATM)

        # Reach data
        name = reachRecords[basin][3]
        minelev = reachRecords[basin][6]
        maxelev = reachRecords[basin][7]
        slopelen = reachRecords[basin][5] / 1000
        Qm3s = reachRecords[basin][11]
        Vmps = reachRecords[basin][12]

        # Add the subbasin, reach to the model
        subbasin = Subbasin(number) # Creates the 'basin' instance of subbasin
        subbasin.add_flowplane(length, planeslope, centroid, elev)
        subbasin.add_reach(name, maxelev, minelev, slopelen,
                           ftable = fTables[basin],
                           flow = Qm3s,
                           velocity = Vmps)
        subbasin.add_landuse(year, lc_codes, hru_df[basin])

        # Add subbasin to the subbasins dictionary
        subbasins[number] = subbasin # Add the instance to the subbasin dict

    return (subbasins)

# FUNCTION TO CREATE THE FLOW NETWORK
def create_flownetwork(basinRecords):

    flow_network = {}  # Dictionary of the US/DS network
    
    for basin in range(0, len(basinRecords)):

        # Create basin flow linkage
        flow_network[str(basinRecords[basin][3])] = str(basinRecords[basin][6])

        if str(basinRecords[basin][6]) == '0':
            
            del flow_network[str(basinRecords[basin][3])]

    return (flow_network)

# CREATE FTABLES FROM CSV
def read_ftables(fTabFil):

    with open(fTabFil) as ftableFile:

        readTables = csv.reader(ftableFile, delimiter = ',')

        fTab_df = []

        for fTabRow in readTables:

            fTab_df.append(fTabRow)

    # Convert the fTable lists to a dataframe and rename columns
    fTab_df = pd.DataFrame.from_records(fTab_df)

    tabNames = pd.unique(fTab_df[0])

    # Convert from string to numeric
    for fTabCol in range(1, len(fTab_df.columns)):

        fTab_df.iloc[:, fTabCol] = [float(x) for x in
                                    list(fTab_df.iloc[:, fTabCol])]

    fTables = [] # Subset the ftable dataframe to individual basin lists

    c = 1

    for n in tabNames:

        int_df = fTab_df[fTab_df[0] == n]

        tempList = []

        for i in range(0, len(int_df)):

            row = list(int_df.iloc[i, 1 : 5])

            tempList.append(row)

        fTables.append(tempList)

        c += 1

    return(fTables)

# Write input time series data to wdm file
def ts_to_wdmFile(wdmFile, pcpData, petData):

    mssgpath = 'c:/siletz_tmdl/03_models/01_hspf/hspfmsg.wdm'

    wdm = WDMUtil(verbose = True, messagepath = mssgpath)

    wdm.open(wdmFile, 'w')

    attributes = {'TCODE ': 3, 
                  'TSSTEP': 1, 
                  'TSTYPE': 'WTMP', 
                  'TSFORM': 2,
                  }

    # Start counter for the DSNs
    x = 1 # Precipitation will start at DSN 101, PET at 201

    for basin in range(1, len(petData.columns)):
    
        # Add precipitation data
        attributes['TSTYPE'] = 'precipitation'

        wdm.create_dataset(wdmFile, x + 100, attributes)

        start_date = pcpData['Date'][0]

        date_start = datetime.datetime(int(start_date[6:10]),
                                       int(start_date[0:2]),
                                       int(start_date[3:5]))

        prec_add = [float(x) for x in list(pcpData.iloc[:, basin])]

        wdm.add_data(wdmFile, x + 100, prec_add, date_start)

        # Add PET data
        attributes['TSTYPE'] = 'evaporation'

        wdm.create_dataset(wdmFile, x + 200, attributes)

        start_date = petData['Date'][0]

        date_start = datetime.datetime(int(start_date[6:10]),
                                       int(start_date[0:2]),
                                       int(start_date[3:5]))

        pet_add = [float(x) for x in list(petData.iloc[:, basin])]

        wdm.add_data(wdmFile, x + 200, pet_add, date_start)

        x += 1
    
    # Close the wdm file
    wdm.close(wdmFile)