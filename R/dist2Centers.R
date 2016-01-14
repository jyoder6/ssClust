dist2Centers <- function(x,centers){
  min(unlist(sapply(seq_len(nrow(centers)), function(rw){sum((x-centers[rw,])^2)})))
} #end dist2Centers