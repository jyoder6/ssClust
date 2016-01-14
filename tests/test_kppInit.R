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
           knownLabels=NULL,
           trueLabels=trueLabels)
ok(length(kk)==2, 'correctly returns right number of groups')

knownLabels=c(4,5)
kk = kppInit(data=x,
             numGroups=2,
             knownLabels=knownLabels,
             trueLabels=trueLabels)
ok(all(kk[[2]]==colMeans(x[knownLabels,])), 'supervised center correct')


knownLabels=c(4,5,1,2)
kk = kppInit(data=x,
             numGroups=2,
             knownLabels=knownLabels,
             trueLabels=trueLabels)
ok(all(kk[[1]]==colMeans(x[c(1,2),])), 'supervised center correct')
ok(all(kk[[2]]==colMeans(x[c(4,5),])), 'supervised center correct')



