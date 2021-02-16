# FUNCTION TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Lampert, David. (2015). PyHSPF: Data integration software for hydrologic and
#   water quality modeling. 

def run_siletz_model(modelName, outPut):

    # Model path and file
    with open(modelName, 'rb') as f: hspfmodel = pickle.load(f)

    # WDM Output targets
    if outPut == 'reach_flow':
        targets = ['reach_flow', 'reach_volume', ] # Specified in the HSPF control: hspf.ctr
    else:
        targets = [outPut, ]

    hspfmodel.messagepath = 'c:/siletz_tmdl/03_models/01_hspf/hspfmsg.wdm'

    hspfmodel.build_wdminfile()
        
    # Set start and end dates
    start = datetime.datetime(2004, 1, 1)
    
    end = datetime.datetime(2018, 4, 1)

    hspfmodel.filename = modelName

    hspfmodel.build_uci(targets,
                        start, end,
                        hydrology = True,
                        sediment = True,
                        verbose = False)

    hspfmodel.run(verbose = False)

