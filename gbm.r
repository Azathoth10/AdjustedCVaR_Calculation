GBMf <- function(s0, t, m, sig, I, ch){
  normvars <- rnorm(nrow(ch))
  corrWiener <- ch%*% normvars
  drift <- (m - (sig^2)/2)*t
  rand <- sig*sqrt(t)*corrWiener
  print(drift + rand)
}
