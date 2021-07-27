make_MAR <- function(X, MCAR_col, MAR_col){
  # Make some prespecified columns 30% MAR 
  # X: fully observed dataset
  # MCAR_col: 6 columns to be made 30% MCAR
  # MAR_col: 6 columnes to be made 30% MAR
  
  # return: X_miss
  # X_miss: df with missing values in the missing_col
  
  X_miss = X
  # Make data MCAR 30% on MCAR_col
  missing_prob = 0.3
  # Make data missing at random for each of the selected variables
  for (col in MCAR_col) {
    missing_ind <- rbernoulli(dim(X)[1], p = missing_prob)
    X_miss[missing_ind, col] <- NA
  }
  
  # Make data MAR 30% on MAR_col
  # remove V162290, V162134 from independent variables
  independent_col = c( 'V162145', 'V162176', 'V162180',  'V162193', 'V162208',
                       'V162212',  'V162231', 'V162260', 'V162271')
  independent_df = sapply(X[, independent_col], as.numeric)-2
  # coefficients for logistic function
  beta_1 = c(0,0,0,-2,0,0,-2,-1,1)/2 #4,7,8,9
  beta_2 = c(2,-1.5,-1.25,0,0,0,0,0,1.75)/2 #1,2,3,9
  beta_3 = c(0,-2,0,2,-1.75,-0.8,0,0,0)/2 #2,4,5,6
  beta_4 = c(0,0,0,1.3,-1.75,1.75,-1.75,0,0)/2 #4,5,6,7
  beta_5 = c(0,0,0,0,-1.9,1.75,-0.5,1.5,0)/2 #5,6,7,8
  beta_6 = c(-1.75,1.75,1.75,0,0,0,0,1.3,0)/2 #1,2,3,8
  logistic_coef = rbind(beta_1, beta_2, beta_3, beta_4, beta_5, beta_6)
  constant = c(-1.4,-1.1,-1.2,-1.3,-1.1,-1.1)
  # make each of the six variables missing at random
  for (col in 1:length(MAR_col)) {
    coef = logistic_coef[col,]
    prob = apply(t(t(independent_df)*coef), MARGIN = 1, sum)
    prob = exp(prob+constant[col])/(exp(prob+constant[col])+1)
    #print(sum(prob>0.9))
    indicator = rbernoulli(dim(X)[1], p = prob)
    X_miss[indicator, MAR_col[col]] <- NA
  }
  return(X_miss)
}