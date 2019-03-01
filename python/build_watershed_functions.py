# FUNCTION TO BUILD WATERSHED INSTANCE FOR PYHSPF
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# This function is used by the 'build_siletz_model.py' function
# to create the watershed instance of the hspf model using the pyHSPF
# package:
# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

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
