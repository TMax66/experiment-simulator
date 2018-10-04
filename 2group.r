twogroup_fun <- function(nrep, b0, b1, sigma) {

  ngroup = 2
  group = rep( c("group1", "group2"), each = nrep)
  err = rnorm(ngroup*nrep, 0, sigma)
  y = b0 + b1*(group == "group2") + err
  fit<- lm(y ~ group)
  fit
}


