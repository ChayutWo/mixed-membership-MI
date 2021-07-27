make_MCAR <- function(X, missing_col, missing_prob){
  # Make some prespecified columns MCAR
  # X: fully observed dataset
  # missing_col: column names to be made MCAR
  # missing_prob: the probability of missing 
  
  # return: X_miss
  # X_miss: df with missing values in the missing_col
  
  X_miss = X
  
  # Make data missing at random for each of the selected variables
  for (col in missing_col) {
    missing_ind <- rbernoulli(dim(X)[1], p = missing_prob)
    X_miss[missing_ind, col] <- NA
  }
  return(X_miss)
}