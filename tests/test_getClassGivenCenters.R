library(ssClust)
library(unittest, quietly=T)

x = rbind(c(1,2),
          c(2,2),
          c(3,2),
          c(2,1),
          c(2,2),
          c(2,3))

trueLabels = c(1,1,1,2,2,2)-1
knownLabels = NULL
kk = kppInit(data=x,
             numGroups=2,
             knownLabels=knownLabels,
             trueLabels=trueLabels)
cl = getClassGivenCenters(data = x,
                          centers = kk,
                          knownLabels = knownLabels, 
                          trueLabels=trueLabels,
                          knownCannotLink=NULL,
                          cannotLinkWithIdx=NULL)
ok(length(unique(cl))==2, 'correct number of classes (G=0, k=2)')


knownLabels=c(4,5)
kk = kppInit(data=x,
             numGroups=2,
             knownLabels=knownLabels,
             trueLabels=trueLabels)
cl = getClassGivenCenters(data = x,
                          centers = kk,
                          knownLabels = knownLabels, 
                          trueLabels=trueLabels,
                          knownCannotLink=NULL,
                          cannotLinkWithIdx=NULL)
ok(all(cl[knownLabels]==trueLabels[knownLabels]), 'all known labels are correct (G=1, k=2)')

knownLabels=c(4,5,1,2)
kk = kppInit(data=x,
             numGroups=2,
             knownLabels=knownLabels,
             trueLabels=trueLabels)
cl = getClassGivenCenters(data = x,
                          centers = kk,
                          knownLabels = knownLabels, 
                          trueLabels=trueLabels,
                          knownCannotLink=NULL,
                          cannotLinkWithIdx=NULL)
ok(all(cl[knownLabels]==trueLabels[knownLabels]), 'all known labels are correct (G=k=2)')

knownLabels=c(4,5,1,2)
kk = kppInit(data=x,
             numGroups=3,
             knownLabels=knownLabels,
             trueLabels=trueLabels)
cl = getClassGivenCenters(data = x,
                          centers = kk,
                          knownLabels = knownLabels, 
                          trueLabels=trueLabels,
                          knownCannotLink=NULL,
                          cannotLinkWithIdx=NULL)
ok(all(cl[knownLabels]==trueLabels[knownLabels]), 'all known labels are correct (G=2,k=3)')






