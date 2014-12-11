##############
# convenience function to generate all possible combinations of value
# of d columns with values ranging between a and b in increments of x
# where the sigma of the values in the combination cannot exceed l
# Sample application: to generate lambda 1, lambda 2, lambda 3
# where all lambda are between 0 to 1 (0, 0.5, and 1)
# and constraint sigma lambda is equal to 1
##############

lambdaComb <- function(a=0, b=1, x=.5, d=3, l=1) {
  m.seq <- seq(a, b, x)
  #m <- expand.grid(l1 = m.seq, l2 = m.seq, l3=m.seq)
  m <- do.call(expand.grid, as.data.frame(matrix(rep(m.seq, d), ncol=d)))
  m <- as.matrix(m[rowSums(m)==l, ])
  dimnames(m) <- NULL
  return(m)
}