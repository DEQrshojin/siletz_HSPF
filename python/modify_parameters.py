# FUNCTION TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

def modify_parameters(modelInName, modelOutName):

    # LOAD MODEL
    if not os.path.isfile(modelInName):
        print('Missing model file; re-run build_siletz_model.py')
        raise

    with open(modelInName, 'rb') as f: hspfmodel = pickle.load(f)

    targets = ['reach_outvolume'] # outflow volume for each reach

    # READ IN THE ADJUSTED PARAMETERS FROM CSVs
    csvFiles = ['\\calib\\pwat.csv',
                '\\calib\\mint.csv',
                '\\calib\\lzet.csv',
                '\\calib\\rtks.csv']

    modPars = []

    for i in range(0, len(csvFiles)):

        csvFile = os.path.abspath(os.path.curdir) + csvFiles[i]

        temp = read_csv_to_list(csvFile)

        modPars.append(temp)

    # MODIFY THE PARAMETERS IN THE MODEL FROM THE CSV PARAMETERS
    for i in range(0, len(hspfmodel.perlnds)):

        # MODIFY THE VARIABLE LOWER ZONE ET SWITCH IN PWAT-PARM1
        hspfmodel.perlnds[i].set_pwat_parms('Metric', VLE = 1)
    
        # Modify PWAT-PARMS first 
        hspfmodel.perlnds[i].LZSN   = modPars[0][i][0] # A. Lower zone storage
        hspfmodel.perlnds[i].INFILT = modPars[0][i][1] # B. Infiltration rate 
        hspfmodel.perlnds[i].LSUR   = modPars[0][i][2] # C. Overland slope
        hspfmodel.perlnds[i].AGWRC  = modPars[0][i][3] # D. Groundwater recess 
        hspfmodel.perlnds[i].DEEPFR = modPars[0][i][4] # E. Act -> Inact GW fr
        hspfmodel.perlnds[i].BASETP = modPars[0][i][5] # F. Base fract for ET
        hspfmodel.perlnds[i].AGWETP = modPars[0][i][6] # F. Active GW ET parameter
        hspfmodel.perlnds[i].UZSN   = modPars[0][i][7] # H. Upper zone storage
        hspfmodel.perlnds[i].INTFW  = modPars[0][i][8] # J. Interflow parameter
        hspfmodel.perlnds[i].IRC   =  modPars[0][i][9] # K. Interflow recession

        # Modify monthly interception
        monInt = modPars[1][i]
        hspfmodel.perlnds[i].set_monthly(name = 'INTERCEP', values = monInt)

        # Modify monthly lower zone ET
        monET = modPars[2][i]
        hspfmodel.perlnds[i].set_monthly(name = 'LZETPARM', values = monET)

    # MODIFY PARAMETERS FOR RCHRES HYDR-PARM2 ROUTING PARAMETER Ks
    for i in range(0, len(hspfmodel.rchreses)):

        hspfmodel.rchreses[i].KS = modPars[3][i][0]

    # PICKLE THE MODEL WITH A NEW NAME
    with open(modelOutName, 'wb') as f: pickle.dump(hspfmodel, f)

    something = 1 # Just to give an terminus to the 'with' in the previous line

def read_csv_to_list(csvFil):

    with open(csvFil) as file:
    
        readTables = csv.reader(file, delimiter = ',')

        temp = []
    
        for row in readTables:

            temp.append(row)
    
    temp.pop(0) # Deletes the header row

    lists = 0

    for i in temp:

        temp[lists] = [float(row) for row in temp[lists]]

        lists += 1

    return(temp)