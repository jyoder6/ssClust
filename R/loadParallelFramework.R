loadParallelFramework <-
function(runParallel, fracOfCores2Use){
  if(runParallel){
    require(foreach) || install.packages('foreach')
    require(doMC) || install.packages('doMC')
    require(parallel) || install.packages('parallel')
    nc = detectCores()
    cat("Using ", floor(fracOfCores2Use*nc), " cores...\n")
    registerDoMC(cores=max(1,floor(fracOfCores2Use*nc))) # use fracOfCores2Use*nc cores!
  } # end if
}
