# FUNCTION TO ADD THE TIMESERIES DATA TO THE WDM FILE
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# This function is used by the 'build_siletz_model.py' function
# To create and populate the wdm (watershed data management) file.

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

def ts_to_wdmFile(wdmFile, pcpData, petData):

    mssgpath = 'D:/siletz/hspfmsg.wdm'

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
