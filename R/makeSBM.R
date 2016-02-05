 makeSBM = function(n,m,Lam){
#functional port of DEF's codez
   
#    % [ A,observe,truth ] = makeSBM(n,m,Lam)
#    % makes a realization from stochastic block model 
#    % coordinates of (vector) n are number of nonseeds in the blocks
#    % coordinates of (vector) m are number of seeds in the blocks
#    % Lam is the communication matrix
#    % A is adjacency matrix, 
#    % observe and truth are block labels observed and not observed
#    
   
   blockSizes = n + m
   #simulate graph
   require(igraph)
   A.igraph<- sbm.game(sum(blockSizes), pref.matrix = Lam, block.sizes = blockSizes, directed = F, loops = F)
   
   #shovel into obj we need
   A <- get.adjacency(A.igraph)
   #Now that we have obtained the adjacency matrix, let's get the label information
   
   #trueLabels is actual labels (true but unknown)
   trueLabels = NULL
   for( bl in 1:length(blockSizes)) trueLabels <- c(trueLabels, rep(bl, blockSizes[bl]))

   observe = rep(-1, sum(blockSizes)) #-1 means unknown
   # We know the m labels (say at end of each group)
   runningTotal = cumsum(blockSizes)
   for( bl in 1:length(n)) {
     if( m[bl]>0){
       idxObserved = (-(1:m[bl]) + runningTotal[bl])
       observe[idxObserved] <-trueLabels[idxObserved]
     }
   }
   
   mix = sample(1:sum(blockSizes))
   A = A[mix,]
   A = A[,mix]
   observe = observe[mix]
   trueLabels=trueLabels[mix]
   return(list(A=A, observe=observe, truth=trueLabels))
 }
