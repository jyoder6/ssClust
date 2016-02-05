VNviaCANsample <-
  function(n, m, Lam, A, observe, truth, numburn, numsample) {
    #    syntax is list(probs,avgprec,reveal) = VNviaCANsample(n,m,Lam,A,observe,truth,numburn,numsample)
    #     n is vector of nonseed block sizes
    #     m is vector of seed block sizes
    #     Lam is communications matrix
    #     numburn is the number of samples discarded as burn-in
    #     numsample is the number of samples used in MCMC
    #     A is adjacency matrix
    #     probs are aprox conditional probabilties of being class1 given A etc
    #     reveal is indicators of correctness along nomination list
    K = length(n)
    sumn = sum(n)
    summ = sum(m)
    ambiguous = which(observe == -1)
    
    LG = log(Lam)
    mLG = log(matrix(1,K,K) - Lam)
    
    probs = rep(0,sumn+summ)#matrix(0, nrow = 1,ncol = sumn + summ)
    temp = NULL
    for (i in 1:K) {
      temp = c(temp, rep(i,n[i]))
    } #end for
    
    temp = sample(temp) #todo[jdy] : uncomment this!
    state = observe;
    state[(ambiguous)] = temp;
#     oneOverNumSample = (1 / numsample)
    
    for (iter in 1:(numburn + numsample)) {
      st=TRUE
      while(st){
      samp = sample(ambiguous, 2, replace=F)
      aa = samp[1]
      bb = samp[2]
      
      stateAA = state[aa]
      stateBB = state[bb]
      st=FALSE
      if(stateAA==stateBB)st=TRUE
      }
#       st=0
#       while (st==0){ 
#       aa=ceiling((summ+sumn)*runif(1));
#       bb=ceiling((summ+sumn)*runif(1));
#       if ((state[(aa)]!=state[(bb)])&&(observe[(aa)]==-1)&&(observe[(bb)]==-1)){
#       st=1;
#       }
#       }
            stateAA = state[aa]
            stateBB = state[bb]
      
      stateNOTaaORbb = state[-c(aa,bb)]
      Anaaeq1 = A[, aa]
      Anaaeq1 = Anaaeq1[-c(aa,bb)]
      Anbbeq1 = A[, bb]
      Anbbeq1 = Anbbeq1[-c(aa,bb)]


      running = - LG[stateNOTaaORbb, stateAA]*Anaaeq1
      running = running + LG[stateNOTaaORbb, stateBB]*Anaaeq1
      running = running - mLG[stateNOTaaORbb, stateAA]*(1-Anaaeq1)
      running = running + mLG[stateNOTaaORbb, stateBB]*(1-Anaaeq1)
      
      running = running - LG[stateNOTaaORbb, stateBB]*Anbbeq1
      running = running + LG[stateNOTaaORbb, stateAA]*Anbbeq1
      running = running - mLG[stateNOTaaORbb, stateBB]*(1-Anbbeq1)
      running = running + mLG[stateNOTaaORbb, stateAA]*(1-Anbbeq1)
      tot = sum(running)      
      tot = exp(tot);
      if (runif(1) < tot) {
        sw = state[aa];
        state[aa] = state[bb];
        state[bb] = sw;
      } #end if
      
      if (iter > numburn) {
        probs = probs + (state == 1)
      } #end if
    } #end for
    probs=probs/numsample
#     X = cbind(-probs[ambiguous], runif(sumn), 1:sumn);
#     Y = X[order(X[,1]),]
    theOrder = order(probs[ambiguous], decreasing=T)
    probs = probs[ambiguous[theOrder]]
    reveal = truth[ambiguous[theOrder]] == 1;
    
    vecprec = rep(0,n[(1)]);
    for (k in 1:n[(1)]) {
      vecprec[(k)] = sum(reveal[(1:k)]) / k;
    }
    avgprec = mean(vecprec)
    
    return (list(
      probs = probs,avgprec = avgprec,reveal = reveal
    ))
  }