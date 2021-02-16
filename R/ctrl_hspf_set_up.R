path <- 'C:/siletz_tmdl/04_scripts/01_hspf/'

source(paste0(path, '02_R/fnct_utilities_hspf.R'))

pyF1 <- 'C:\\siletz_tmdl\\04_scripts\\01_hspf\\01_python\\ctrl_build_hspf.py'
pyF2 <- 'C:\\siletz_tmdl\\04_scripts\\01_hspf\\01_python\\ctrl_run_hspf.py'
rF1  <- 'C:\\siletz_tmdl\\04_scripts\\01_hspf\\02_R\\ctrl_proc_hydros.R'

# READ IN THE CONTROL FILE DATA
ctrF <- read_ctrF_H()

tmp1 <- NULL

# HSPF HYDRO BUILD MODEL (.py) _________________________________________________
# If build model is specified then build model & start writing the batch file
if (ctrF[['bldM']] == 'TRUE') {
  
  # Search for the indeces with HSPF input files
  indx <- which(names(ctrF) %in% c('hruF', 'pcpF', 'petF', 'modF'))
  
  # READ IN THE BUILD TEMPLATE
  bldP <- readLines(paste0(path, '01_python/ctrl_build_hspf.tmpl'))
  
  # WRITE TO THE BUILD TEMPLATE DATA
  for (i in indx) {
    cond <- which(bldP == names(ctrF)[i])
    bldP[cond] <- paste0(names(ctrF)[i], ' = \'', ctrF[[i]][1], '\'')
  }
  
  # WRITE TO THE BUILD TEMPLATE SCRIPT
  writeLines(bldP, pyF1)
  
  # WRITE FIRST COMMAND OF THE RUN BATCH FILE (tmp1.bat). This line needs to 
  # be modified based on where the python executable exists in the environment
  # Unfortunately conda is not consistent in where this gets installed 
  tmp1 <- append(tmp1, c('REM Build instance of HSPF model',
                         paste0('C:\\Users\\rshojin\\AppData\\Local\\conda',
                                '\\conda\\envs\\pyHSPF_chk\\python.exe ', pyF1),
                         ''))  

}

# HSPF HYDRO RUN MODEL (.py) ___________________________________________________
if (ctrF[['runH']] == 'TRUE') {

  # READ IN THE RUN TEMPLATE
  runP <- readLines(paste0(path, '01_python/ctrl_run_hspf.tmpl'))
  
  # SWITCH OUT VARIABLES
  # If there are more than one outputs (e.g., perlnd & rchres) involved,
  # run twice but rename the second output first
  if (!is.na(ctrF[['out2']][1])) {
    
    temp <- runP[(length(runP) - 3) : length(runP)]
    
    temp <- gsub('x_out1_x', paste0('\'', ctrF[['out2']][1], '\''), temp)
    
    runP <- append(runP, temp)
    
  }
  
  runP <- gsub('x_Name_x', paste0('\'', ctrF[['name']][1], '\''), runP)
  
  runP <- gsub('x_out1_x', paste0('\'', ctrF[['out1']][1], '\''), runP)
  
  # WRITE TO PYTHON SCRIPT
  writeLines(runP, pyF2)

}

# PROCESS HYDROS FOR WQ HERE ___________________________________________________
# Write the R script with changing name
oPth <- 'C:/siletz_tmdl/02_outputs/01_hspf/'

# Write the script here 
rOut <- c('source(\'C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R\')',
          '',
          paste0('qOut <- proc_flow_4_wq(hydros = \'', oPth, '\','),
          ifelse(ctrF$runH == 'FALSE',
                 paste0('                       ', 'oFil   = \'', ctrF$qOut, '\','),
                 '                       oFil   = NULL,'),
          paste0('                       ', 'nFil   = \'', ctrF$name, '\')'))

writeLines(rOut, rF1)

# HSPF HYDRO BATCH FILE (tmp1.bat) _____________________________________________
# WRITE THE MODEL RUN BATCH FILE
if (ctrF$runH == 'TRUE') {
  
  tmp1 <- append(tmp1,
                 c('REM Modify, run, and process output of HSPF model instance', 
                   paste0('C:\\Users\\rshojin\\AppData\\Local\\conda',
						  '\\conda\\envs\\pyHSPF_chk\\python.exe ', pyF2)))
  
}

# Write the batch file line here
tmp1 <- append(tmp1, c('', 'REM process the HSPF hydro files for water quality',
                       paste0('Rscript C:\\siletz_tmdl\\04_scripts\\01_hspf',
                              '\\02_R\\ctrl_proc_hydros.R')))

writeLines(tmp1, 'C:/siletz_tmdl/03_models/01_hspf/tmp1.bat')

# HSPF WATER QUALITY BATCH FILE (tmp2.bat) _____________________________________
indx <- which(names(ctrF) %in% c('NOx', 'NH3', 'TKN', 'TP', 'PO4', 'OrC'))
  
bch2 <- c('REM Bacth file to run the HSPF water quality', '', '@echo off', '')

mPth <- 'C:\\siletz_tmdl\\03_models\\01_hspf\\'

for (i in 1 : length(indx)) {
  
  if (ctrF[[indx[i]]] == 'TRUE') {
    
    f1   <- paste0(mPth, 'wq_pars\\wq_pars_', names(ctrF)[indx[i]], '.csv')
    
    f2   <- paste0(mPth, 'wq_pars.csv')
    
    bch2 <- append(bch2, paste0('REM Run HSPF routines for ',
                                names(ctrF)[indx[i]]))
    
    bch2 <- append(bch2, paste0('copy ', f1, ' ', f2))
    
    if (i == 1) {
      bch2 <- append(bch2, paste0('Rscript C:\\siletz_tmdl\\04_scripts\\01_hsp',
                                  'f\\02_R\\ctrl_wq_hspf.R'))
    } else {
      bch2 <- append(bch2, paste0('Rscript C:\\siletz_tmdl\\04_scripts\\01_hsp',
                                  'f\\02_R\\ctrl_wq_hspf.R'))
    }
    
    bch2 <- append(bch2, paste0('del ', f2))
    
    bch2 <- append(bch2, "")
    
  }
}

writeLines(bch2, 'C:/siletz_tmdl/03_models/01_hspf/tmp2.bat')

# FILE CLEANUP BATCH  (tmp3.bat) _______________________________________________
# Specify new directory
dir1 <- 'C:\\siletz_tmdl\\03_models\\01_hspf\\'

dir2 <- paste0(dir1, 'archive\\', ctrF[['name']][1], '\\')

dir3 <- paste0(dir1, 'wq_pars\\')

# Create if doesn't exist
if (!file.exists(dir2)) {dir.create(dir2)}

emcdwc <- NULL

for (i in 1 : length(indx)) {
  if (ctrF[[indx[i]]] == 'TRUE') {
    emcdwc <- append(emcdwc, paste0('emcdwc_', names(ctrF)[indx[i]], '.csv'))
  }
}

filD <- c(paste0(ctrF[['name']][1], '.hspf.ech'), 'siletz_network.png',
          'siletz.out', emcdwc)

filM <- c(paste0(ctrF[['name']][1], c('.hspf_out.wdm', '.hspf.uci', '.hspf')),
          'siletz_in.wdm')

filC <- paste0('wq_pars_', c('NOx', 'NH3', 'TKN', 'TP', 'PO4', 'OrC'), '.csv')

tmp3 <- c(
          paste0('DEL ', dir1, filD), 'DEL tmp1.bat', 'DEL tmp2.bat',
          paste0('MOVE ', dir1, filM, ' ', dir2, filM),
          paste0('COPY ', dir3, filC, ' ', dir2, filC),
          paste0('COPY ', c('hspf_ctrl.csv', 'hydro_pars.csv'), ' ', dir2,
                 c('hspf_ctrl.csv', 'hydro_pars.csv'))
         )

writeLines(tmp3, 'C:/siletz_tmdl/03_models/01_hspf/tmp3.bat')



