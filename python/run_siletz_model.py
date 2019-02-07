# FUNCTION TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

def run_siletz_model(modelName):

    # Model path and file
    with open(modelName, 'rb') as f: hspfmodel = pickle.load(f)

    targets = ['reach_outvolume'] # outflow volume for each reach

    hspfmodel.messagepath = 'C:/siletz/hspfmsg.wdm'

    hspfmodel.build_wdminfile()

    # Set start and end dates
    start = datetime.datetime(2004, 1, 1)
    
    end = datetime.datetime(2018, 4, 1)

    hspfmodel.filename = modelName

    hspfmodel.build_uci(targets, start, end, hydrology = True, verbose = False)

    hspfmodel.run(verbose = False)

