source("../../utils/load_data.R")

posterior_diag <- function(datapath, samplespath, max_nway=1){
  # Perform posterior diagnosis on saved MCMC samples from MMM model
  # This includes: plot of Beta, number of clusters used, and coverage
  # datapath: file path of ANES data to be loaded including root
  # samplespath: the name of file that MCMC samples were saved
  # max_nway: maximum level of combination of variables (joint probability) to assess
  
  # return: NA
  
  # load original data
  X = load_data(datapath)
  
  # load posterior samples
  load(samplespath)
  
  # define constants
  N = dim(X)[1]
  p = dim(X)[2]
  K = ncol(BETA)
  level = apply(X, MARGIN=2, max)
  
  # 1. Convergence checking Beta a after accounting for burnin and thining
  beta_df = BETA
  colnames(beta_df) <- 1:K
  matplot(1:dim(beta_df)[1], beta_df, 
          type = 'l', ylab = 'Beta_k', xlab = 'trials', 
          main = 'Checking stability of sampled Beta')
  
  # 2. Check number of active cluster 
  Z_df = Z
  active_cluster = array(0, dim(Z_df)[3])
  for (idx in 1:dim(active_cluster)) {
    for (k in 1:K) {
      if (sum(Z_df[,,idx]==k)!=0) {
        active_cluster[idx] = active_cluster[idx] + 1
      }
    }
  }
  plot(1:dim(active_cluster), active_cluster, type = 'l', ylab = 'Number of active clusters',
       xlab = 'trials', main = 'Checking number of active clusters during MCMC', ylim = c(1,K))
  
  # 3.1 prepare samples from posterior density
  # Compare posterior predictive distribution with true marginal distribution and posterior samples
  X_POST = X_SAMPLE
  
  # sample from posterior predictive distribution
  for (id in 1:dim(X_SAMPLE)[3]) {
    for (j in 1:p) {
      # get z_ij 
      z_cur = Z[,j,id]
      # get phi_ij
      prob = PHI[[j]][z_cur,,id] # number of entries x dj matrix
      prob_cum = prob%*%upper.tri(diag(ncol(prob)),diag=TRUE)
      Ran_unif <- runif(N)
      X_POST[,j,id] = rowSums(Ran_unif>prob_cum) + 1L
    }
  }
  
  # 3.2 X_POST vs X
  X_df = data.frame(X) #dataframe for original data without missing values
  for (n_way in 1:max_nway) {
    combinations = combn(1:p, n_way)
    
    # calculate true pmf without missingness
    true_pmf = c()
    for (i in 1:(dim(combinations)[2])) {
      variables = combinations[, i]
      true_pmf_temp = table(X_df[,variables])
      true_pmf_temp = c(true_pmf_temp/sum(true_pmf_temp))
      true_pmf = c(true_pmf, true_pmf_temp)
    }
    
    # calculate imputed pmf
    imputed_pmf = c()
    for (t in 1:(dim(X_POST)[3])) {
      # for each imputed dataset, calculate joint pmf
      X_POST_df = data.frame(X_POST[,,t])
      imputed_pmf_temp = c()
      for (i in 1:(dim(combinations)[2])) {
        variables = combinations[, i]
        imputed_pmf_temp = c(imputed_pmf_temp, c(table(X_POST_df[,variables])/sum(table(X_POST_df[,variables]))))
      }
      # imputed_pmf has dimension #samples x #pmfs
      imputed_pmf = rbind(imputed_pmf, imputed_pmf_temp)
    }
    
    # calculate mean and 95% credible interval
    imputed_pmf_mean = apply(imputed_pmf, MARGIN = 2, FUN = mean)
    imputed_pmf_ci = apply(imputed_pmf, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975))
    pmf_df <- data.frame(true_pmf,imputed_pmf_mean, imputed_pmf_ci[1,], imputed_pmf_ci[2,])
    names(pmf_df) <- c('true_pmf', "expectation","Q2.5", "Q97.5")
    
    # Calculate coverage
    lower_bound = imputed_pmf_ci[1,]
    upper_bound = imputed_pmf_ci[2,]
    coverage = (lower_bound<=true_pmf) & (true_pmf<=upper_bound)
    
    
    # Make a plot of true pmf vs posterior predictive pmf
    title = paste('True pmf vs Imputed pmf: n_way', n_way, 
                  ', coverage', round(mean(coverage),2))
    plt <- ggplot(data = pmf_df, aes(x = true_pmf, y = expectation))+
      geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), size = 0.4, alpha = 0.6, colour = 'blue')+
      geom_abline(intercept = 0, slope = 1, colour = 'red', alpha = 0.2)+
      scale_x_continuous("true pmf", breaks = seq(0, 1, 0.05)) +
      scale_y_continuous(expression('imputed pmf')) +
      ggtitle(title)
    show(plt)
    
    plt <- ggplot(data = pmf_df, aes(x = true_pmf, y = expectation))+
      geom_point(size = 1, alpha = 0.7, colour = 'blue')+
      geom_abline(intercept = 0, slope = 1, colour = 'red', alpha = 0.5)+
      scale_x_continuous("true pmf", breaks = seq(0, 1, 0.05)) +
      scale_y_continuous(expression('imputed pmf')) +
      ggtitle(title)
    show(plt)
    print(dim(pmf_df))
    
    # make coverage plot
    title = paste('95% CI: n_way', n_way, 
                  ', coverage', round(mean(coverage),2))
    PredCI_col <- ifelse(coverage==1,"lightblue3","red4")
    PredCI_lwd <- ifelse(coverage==1,1,2)
    plot(pmf_df[,1], pch=4, ylim=range(pretty(c(pmf_df[,3], pmf_df[,4]))),
         xlab="Index", ylab="Joint probabilities", las=1,col="orange4",
         main=title)
    segments(seq_len(length(pmf_df[,1])), pmf_df[,3], y1=pmf_df[,4], lend=1,
             lwd=PredCI_lwd,col=PredCI_col)
    points(pmf_df[,3], pch="-", bg=PredCI_col); points(pmf_df[,4], pch="-", bg=PredCI_col)
    legend("topright", inset=.05, bty="n",
           legend=c("Intervals containing true pmf",
                    "Intervals NOT containing true pmf","true joint probabilities"),
           lwd=c(1,2,1), lty=c(1,1,NA),pch=c(NA,NA,4), col=c("lightblue3","red4","orange4"))
  }
  
  # 3.3 X_SAMPLE vs X
  if (sum(is.na(X_miss))>0) {
    R = is.na(X_miss)
    missing_col = which(apply(R, MARGIN = 2, sum)>0)
    # plot this case only when we perform imputation
    X_df = data.frame(X) #dataframe for original data without missing values
    for (n_way in 1:max_nway) {
      combinations = combn(1:p, n_way)
      
      # calculate true pmf without missingness
      true_pmf = c()
      for (i in 1:(dim(combinations)[2])) {
        variables = combinations[, i]
        if (any(missing_col %in% variables)) {
          true_pmf_temp = table(X_df[,variables])
          true_pmf_temp = c(true_pmf_temp/sum(true_pmf_temp))
          true_pmf = c(true_pmf, true_pmf_temp)
        }
      }
      
      # calculate imputed pmf
      imputed_pmf = c()
      for (t in 1:(dim(X_SAMPLE)[3])) {
        # for each imputed dataset, calculate joint pmf
        X_SAMPLE_df = data.frame(X_SAMPLE[,,t])
        imputed_pmf_temp = c()
        for (i in 1:(dim(combinations)[2])) {
          variables = combinations[, i]
          if (any(missing_col %in% variables)) {
            imputed_pmf_temp = c(imputed_pmf_temp,
                                 c(table(X_SAMPLE_df[,variables])/sum(table(X_SAMPLE_df[,variables]))))
          }
        }
        # imputed_pmf has dimension #samples x #pmfs
        imputed_pmf = rbind(imputed_pmf, imputed_pmf_temp)
      }
      
      # calculate mean and 95% credible interval
      imputed_pmf_mean = apply(imputed_pmf, MARGIN = 2, FUN = mean)
      imputed_pmf_ci = apply(imputed_pmf, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975))
      pmf_df <- data.frame(true_pmf,imputed_pmf_mean, imputed_pmf_ci[1,], imputed_pmf_ci[2,])
      names(pmf_df) <- c('true_pmf', "expectation","Q2.5", "Q97.5")
      
      # Calculate coverage
      lower_bound = imputed_pmf_ci[1,]
      upper_bound = imputed_pmf_ci[2,]
      coverage = (lower_bound<=true_pmf) & (true_pmf<=upper_bound)
      
      
      # Make a plot of true pmf vs posterior predictive pmf
      title = paste('True pmf vs Imputed pmf: n_way', n_way, 
                    ', coverage', round(mean(coverage),2))
      plt <- ggplot(data = pmf_df, aes(x = true_pmf, y = expectation))+
        geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), size = 0.4, alpha = 0.6, colour = 'blue')+
        geom_abline(intercept = 0, slope = 1, colour = 'red', alpha = 0.2)+
        scale_x_continuous("true pmf", breaks = seq(0, 1, 0.05)) +
        scale_y_continuous(expression('imputed pmf')) +
        ggtitle(title)
      show(plt)
      
      plt <- ggplot(data = pmf_df, aes(x = true_pmf, y = expectation))+
        geom_point(size = 1, alpha = 0.7, colour = 'blue')+
        geom_abline(intercept = 0, slope = 1, colour = 'red', alpha = 0.5)+
        scale_x_continuous("true pmf", breaks = seq(0, 1, 0.05)) +
        scale_y_continuous(expression('imputed pmf')) +
        ggtitle(title)
      show(plt)
      print(dim(pmf_df))
      
      
      # make coverage plot
      title = paste('95% CI: n_way', n_way, 
                    ', coverage', round(mean(coverage),2))
      PredCI_col <- ifelse(coverage==1,"lightblue3","red4")
      PredCI_lwd <- ifelse(coverage==1,1,2)
      plot(pmf_df[,1], pch=4, ylim=range(pretty(c(pmf_df[,3], pmf_df[,4]))),
           xlab="Index", ylab="Joint probabilities", las=1,col="orange4",
           main=title)
      segments(seq_len(length(pmf_df[,1])), pmf_df[,3], y1=pmf_df[,4], lend=1,
               lwd=PredCI_lwd,col=PredCI_col)
      points(pmf_df[,3], pch="-", bg=PredCI_col); points(pmf_df[,4], pch="-", bg=PredCI_col)
      legend("topright", inset=.05, bty="n",
             legend=c("Intervals containing true pmf",
                      "Intervals NOT containing true pmf","true joint probabilities"),
             lwd=c(1,2,1), lty=c(1,1,NA),pch=c(NA,NA,4), col=c("lightblue3","red4","orange4"))
      
    }
  }
  
}