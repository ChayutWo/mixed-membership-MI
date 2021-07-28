require(MCMCpack)
library(matrixStats)
MMM_imputation <- function(X_miss, K = 30, Mon = 30000, 
                           burnin = 15000, thin = 5, seed, savename){
  # Perform mixed membership model (MMM) imputation/modelling to the data matrix X_miss
  # The function also saves output using savename as the file directory
  
  # X_miss: Input dataframe of size (N observations x p variables). May include missing values 
  # K: Number of clusters to be used in MMM
  # Mon: Number of MCMC samples including burnin (before thinning)
  # burnin: Number of MCMC samples to be thrown away for burnin
  # thin: Number of thinning factor to be used (total number of samples = (Mon-burnin)/thin)
  # seed: random seed to control the modelling process
  # savename: filepath for saving the output of the modelling
  
  # Output: None
  
  set.seed(seed)
  
  #############################################################################
  # Part I: Define constants and dummy variables to be used for saving output #
  #############################################################################
  level = apply(X_miss, MARGIN=2, max, na.rm = TRUE)
  print(level)
  N = dim(X_miss)[1]
  p = dim(X_miss)[2]
  # hyperprior parameters
  # gamma ~ Gamma(a,b)
  a = 0.25
  b = 0.25
  # alpha0 ~ Gamma(c,d)
  c = 0.25
  d = 0.25
  # storage
  GAMMA = rep(NA, (Mon-burnin)/thin)
  ALPHA0 = rep(NA, (Mon-burnin)/thin)
  BETA = matrix(NA, nrow = (Mon-burnin)/thin, ncol = K)
  PI = array(NA, c(N, K, (Mon-burnin)/thin))
  Z = array(NA, c(N, p, (Mon-burnin)/thin))
  X_SAMPLE = array(NA, c(N, p, (Mon-burnin)/thin))
  PHI = list()
  for (j in 1:p) {
    PHI[[j]] = array(NA, c(K, level[j], (Mon-burnin)/thin))
  }
  
  ###############################
  # Part II: MMM Initialization #
  ###############################
  # initialization for x
  x_temp = X_miss
  R = is.na(X_miss) # missingness indicator 1[x is missing]
  
  # initial imputation for x with observed marginal probability
  for (col in 1:p) {
    missing_ind <- R[,col]
    if(sum(missing_ind>0)){
      x_temp[missing_ind, col] <- sample(1:level[col], size = sum(missing_ind),
                                         table(x_temp[R[,col]!=1,col]), replace = TRUE)
    }
  }
  
  # initialization for z
  z_temp = data.frame(matrix(sample(1:K, size = N*p, replace = TRUE), nrow = N, ncol = p))
  for (col_index in 1:ncol(z_temp)) {
    z_temp[,col_index] = factor(z_temp[,col_index], levels = 1:K)
  }
  
  # nik = # of variables in cluster k for observation i
  nik = matrix(NA, N, K)
  for (k in 1:K) {
    nik[,k] = rowSums(z_temp==k)
  }
  
  # initialization for PHI
  phi_temp = list()
  for (j in 1:p) {
    prob = rdirichlet(n = K, rep(1,level[j])) # Kxlevel matrix
    phi_temp[[j]] = prob
  }
  
  # initialization for gamma and alpha0
  gamma_temp = 1
  alpha0_temp = 1 
  
  # initialization for Vk and Betak
  # Beta ~ GEM(gamma_temp)
  # Sample Vk ~ Beta(1, gamma)
  Vk_temp <- matrix(rbeta(K-1,1,gamma_temp),nrow=K-1)
  Vk_temp <- rbind(Vk_temp,1)
  one_min_Vk_temp <- 1L-Vk_temp
  one_min_Vk_temp <- c(1,cumprod(one_min_Vk_temp[1:(K-1)]))
  # Compute Betak for k = 1,...,K
  beta_temp <- Vk_temp*one_min_Vk_temp
  # Sort Betak to fix label switching
  beta_temp <- sort(beta_temp, decreasing = TRUE)
  
  # initialization for u and Pi
  # Pi_i ~ DP(alpha_temp, Beta_temp) for i = 1,...,N
  one_min_cumsum_beta_temp <-1L-cumsum(beta_temp[1:(K-1)])
  one_min_cumsum_beta_temp[one_min_cumsum_beta_temp<=0] <- 1.0e-20
  u_temp <- matrix(rbeta(N*(K-1),alpha0_temp*rep(beta_temp[1:(K-1)],each=N),
                         alpha0_temp*rep(one_min_cumsum_beta_temp,each=N) ),
                   nrow=N,ncol=(K-1),byrow = F)
  # Prevent instability issue
  u_temp[u_temp==1] = 0.99999; u_temp[u_temp==0] = 1.0e-5
  u_temp <- cbind(u_temp,1)
  one_min_u_temp <- 1L-u_temp
  one_min_u_temp <- cbind(1,t(apply(one_min_u_temp[,-K],1,cumprod)))
  # Compute Pi from uik
  Pi_temp <- u_temp*one_min_u_temp
  
  # initialization for auxiliary variables
  t_temp = array(NA, N)
  sik = matrix(0, N, K)
  
  # matrix used in MCMC
  nik = matrix(NA, N, K)
  
  ##################
  # Part III: MCMC #
  ##################
  id = 0
  for (trial in 1:Mon) {
    # Step 1: Update z_ij
    for (j in 1:p) {
      # get phi_j
      prob = phi_temp[[j]]
      var = x_temp[,j]
      likelihood = t(prob[,var]) # NxK of phi_j_xij
      full_prob_matrix = Pi_temp*likelihood
      full_prob_matrix <- full_prob_matrix/matrix(rowSums(full_prob_matrix),nrow=N,ncol=K) # normalize
      Ran_unif <- runif(N)
      cumul <- full_prob_matrix%*%upper.tri(diag(ncol(full_prob_matrix)),diag=TRUE)
      z_temp[,j] <- factor(rowSums(Ran_unif>cumul) + 1L, levels = 1:K)
    }
    
    # Step 2: Update Phi_k
    for (j in 1:p) {
      contingency_table = table(z_temp[,j], x_temp[,j])
      phi_temp[[j]] = DirichletReg::rdirichlet(K,1+contingency_table)
    }
    
    # Step 3: Update Vk and betak
    # sample auxiliary variable ti
    sik = matrix(0, N, K)
    for (k in 1:K){
      # update count nik of number of variables for each person in different clusters
      nik[,k] = rowSums(z_temp==k) 
      if(sum(nik[,k]>0)>0){
        # sample sik for only k where nk not equal 0
        non_zero_index <- which(nik[,k]>0)
        non_zero_nik <- nik[non_zero_index,k]
        sik[non_zero_index,k] <- 
          unlist(lapply(1:length(non_zero_nik), 
                        function(x) sum(rbernoulli(non_zero_nik[x], 
                                                   ((alpha0_temp*beta_temp[k])/(alpha0_temp*beta_temp[k] + (1:non_zero_nik[x]) -1)) )) ))
      }
    }
    t_temp = rbeta(N,alpha0_temp,rowSums(nik))  
    
    # sample Vk for k = 1,...,K-1
    Vk_temp[1:(K-1)] <- rbeta((K-1),(1L+colSums(sik[,1:(K-1)])),
                              (gamma_temp + (sum(sik) - cumsum(colSums(sik)))[1:K-1]) )
    # Prevent instability issue
    Vk_temp[Vk_temp==1] <- 0.99999; Vk_temp[Vk_temp==0] <- 1.0e-5
    Vk_temp[K] <- 1
    one_min_Vk_temp <- 1L-Vk_temp
    one_min_Vk_temp <- c(1,cumprod(one_min_Vk_temp[1:(K-1)]))
    # compute betak for k = 1,...,K
    beta_temp <- Vk_temp*one_min_Vk_temp
    # Sort Betak to fix label switching
    beta_temp <- sort(beta_temp, decreasing = TRUE)
    
    # Step 4: Update u_ik and pi_ik
    one_min_cumsum_beta_temp <- 1L-cumsum(beta_temp[1:(K-1)])
    # Prevent instability issue
    one_min_cumsum_beta_temp[one_min_cumsum_beta_temp<=0] <- 1.0e-20
    u_temp[,1:(K-1)] <- matrix(rbeta( (N*(K-1)), (alpha0_temp*rep(beta_temp[1:(K-1)],each=N) + c(nik[,1:(K-1)]) ) ,
                                      (alpha0_temp*rep(one_min_cumsum_beta_temp,each=N) +
                                         c(matrix(rowSums(nik),ncol=K-1,nrow=N)-t(apply(nik,1,cumsum))[,1:K-1]) ) ),
                               nrow=N,ncol=(K-1),byrow = F)
    # Prevent instability issue
    u_temp[u_temp==1] = 0.99999; u_temp[u_temp==0] = 1.0e-5
    u_temp[,K] <- 1
    one_min_u_temp <- 1L-u_temp
    one_min_u_temp <- cbind(1,t(apply(one_min_u_temp[,-K],1,cumprod)))
    Pi_temp <- u_temp*one_min_u_temp
    
    # Step 5.1: Update gamma
    gamma_temp = rgamma(1, a + K - 1, b - sum(log(1-Vk_temp[1:(K-1)])))
    # Step 5.2: Update alpha
    alpha0_temp = rgamma(1, c + sum(sik), d - sum(log(t_temp)))
    
    # Step 6: Update x_ij
    for (j in 1:p) {
      # get z_ij for missing entries
      missing_idx = (R[,j] == 1)
      if (sum(missing_idx) > 0) {
        # If that column is missing
        z_cur = z_temp[missing_idx,j]
        # get phi_ij
        prob = phi_temp[[j]][z_cur,] # number of missing entries x dj matrix
        prob_cum = prob%*%upper.tri(diag(ncol(prob)),diag=TRUE)
        Ran_unif <- runif(sum(missing_idx))
        x_temp[missing_idx, j] = rowSums(Ran_unif>prob_cum) + 1L
      }
    }
    
    # Save posterior samples
    if ((trial>burnin) && ((trial-burnin)%%thin==1) ) {
      id = id + 1
      GAMMA[id] = gamma_temp
      ALPHA0[id] = alpha0_temp
      BETA[id,] = beta_temp
      PI[,,id] = Pi_temp
      Z[,,id] = data.matrix(z_temp)
      X_SAMPLE[,,id] = data.matrix(x_temp)
      for (j in 1:p) {
        PHI[[j]][,,id] = phi_temp[[j]]
      }
    }
    print(paste('finish runing MCMC trial:',trial))
  }
  print(paste('saving output to',savename))
  save(BETA, PHI, X_miss, X_SAMPLE, Z, file = savename)
}