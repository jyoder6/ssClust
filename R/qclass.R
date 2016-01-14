qclass <-
function (x, k) 
{
  x <- as.vector(x)
  q <- NA
  n <- k
  while(length(q) < (k+1))
  { n <- n + 1
    q <- unique(quantile(x, seq(from = 0, to = 1, length = n))) 
  }
  eps <- sqrt(.Machine$double.eps)
  q[1] <- q[1] - eps
  q[length(q)] <- q[length(q)] + eps
  cl <- rep(0, length(x))
  for(i in 1:k) 
  { cl[ x >= q[i] & x < q[i+1] ] <- i }
  return(cl)
}
