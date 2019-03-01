# FUNCTION TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

def proc_hspf_results(n):

    # OPEN THE OUTPUT WDM AND GET THE DSNS
    messagepath = 'hspfmsg.wdm'

    wdm = WDMUtil(verbose = False, messagepath = messagepath)

    wdmFile = 'siletz' + str(n) + '_out.wdm'

    wdm.open(wdmFile, 'r')

    # Get output information from the out.wdm file
    dsns = wdm.get_datasets(wdmFile) # All data sets from the WDM
    # dsns = [3, 11] # Hydrology - Sunshine Creek and Siletz River at USGS
    
    idcons = [wdm.get_attribute(wdmFile, n, 'IDCONS') for n in dsns]

    staids = [wdm.get_attribute(wdmFile, n, 'STAID ') for n in dsns]

    # INITIALIZE THE OUTPUT DATAFRAME INCLUDING THE DATE/TIME
    start = datetime.datetime(2004, 1, 1)

    end = datetime.datetime(2018, 4, 1)

    dttm = [start + t * datetime.timedelta(hours = 1)
            for t in range(int((end - start).total_seconds() / 3600))]
        
    dctOut = {'Date': dttm}

    j = 0

    # EXTRACT MODELED FLOWS FROM WDM OUTPUT FILE AND COMPARE TO 
    for i in range(1, len(dsns) + 1):

        tmpNme = 'BAS_' + staids[j] + '_' + idcons[j] 

        tmpDat = wdm.get_data(wdmFile, i)

        dctOut[tmpNme] = tmpDat

        j += 1

    wdm.close(wdmFile)

    # COERCE TO DATAFRAME AND WRITE TO FILE
    tmp = pd.DataFrame.from_dict(dctOut)

    cutStr = datetime.datetime(2004, 10, 1)

    cutEnd = datetime.datetime(2017, 10, 1)

    dfOut = tmp.loc[(tmp['Date'] >= cutStr) & (tmp['Date'] <= cutEnd), ]

    dfOut.to_csv('calib\\siletz_out_sed.csv', index = False)
