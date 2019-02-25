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

    dsns = [3, 11]

    # INITIALIZE THE OUTPUT DATAFRAME INCLUDING THE DATE/TIME
    start = datetime.datetime(2004, 1, 1)

    end = datetime.datetime(2018, 4, 1)

    dttm = [start + t * datetime.timedelta(hours = 1)
            for t in range(int((end - start).total_seconds() / 3600))]

    datOut = [dttm]

    datNms = ['Date']

    # EXTRACT MODELED FLOWS FROM WDM OUTPUT FILE AND COMPARE TO 
    for i in dsns:

        tmpNme = 'BAS_' + str(i)

        datNms.append(tmpNme)

        tmpDat = wdm.get_data(wdmFile, i)

        tmpDat = [q * 10**6 * 35.314666721 / (60 * 60) for q in tmpDat]

        datOut.append(tmpDat)

    wdm.close(wdmFile)

    # COERCE TO DATAFRAME AND WRITE TO FILE
    data = {'Names': datNms, 'col_2': ['a', 'b', 'c', 'd']}

    tmp = pd.DataFrame.from_items(zip(datNms, datOut))

    # Slice the data to remove dates outside of calibration period
    cutStr = datetime.datetime(2004, 10, 1)

    cutEnd = datetime.datetime(2017, 10, 1)
        
    dfOut = tmp.loc[(tmp['Date'] >= cutStr) & (tmp['Date'] <= cutEnd), ]

    dfOut.to_csv('calib\\qmod.csv', index = False)

