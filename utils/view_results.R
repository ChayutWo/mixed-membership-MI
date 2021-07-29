source("../../utils/load_data.R")

view_PHI_by_var <- function(samplespath){
  # Plot posterior samples of PHI for different variabls for the top 7 clusters
  # samplespath: the name of file that MCMC samples were saved
  
  # return: NA
  
  # load posterior samples
  load(samplespath)
  
  for (var in var_name) {
    # plotting section
    par(mfrow=c(4,2), oma = c(5.5,4.5,5,0.2) + 0.1, mar = c(0.75,0.25,1.25,1.25) + 0.5)
    idx = meta[[var]][4] # index for that variable
    for (k in 1:7) {
      # plot PHI for variable named var and for group k
      phi = t(PHI[[strtoi(idx)]][k,,]) # matrix of size samples x # levels
      num_level= strtoi(meta[[var]][2])
      matplot(1:dim(phi)[1], phi, 
              type = 'l', ylab = 'phi', xlab = '',
              main = paste('group: ', k), ylim = c(0,1),
              col=topo.colors(num_level), lwd = 1.5, cex.main = 1)
      legend('right', legend = 1:num_level, col=topo.colors(num_level), lwd = 1.5)
    }
    # create empty plot for level description
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, meta[[var]][3], cex = 1.2, col = "black")
    title_text = paste(var,': ', meta[[var]][1])
    title(title_text, line = 2, outer = TRUE)
  }
}

view_PHI_by_group <- function(samplespath, num_big_cluster=7){
  # Plot posterior samples of PHI for different groups for 8 fixed variables of interest
  # samplespath: the name of file that MCMC samples were saved
  # num_big_cluster: maximum number of biggest clusters to be included in the plots
  
  # return: NA
  
  # load posterior samples
  load(samplespath)
  
  #list of variables to be plotted
  var_list = c('V162140', 'V162148', 'V162158', 'V162179', 'V162192', 'V162209', 'V162214', 'V162269')
  
  for (group in 1:num_big_cluster) {
    # Plot pmf of response for different groups of interest
    par(mfrow=c(4,2), oma = c(5.5,4.5,5,0.2) + 0.1, mar = c(0.75,0.25,1.25,1.25) + 0.5)
    
    for (var in var_list) {
      idx = meta[[var]][4] # index for that variable
      # plot PHI for variable named var and for the selected group
      phi = t(PHI[[strtoi(idx)]][group,,]) # matrix of size samples x # levels
      num_level= strtoi(meta[[var]][2])
      matplot(1:dim(phi)[1], phi, 
              type = 'l', ylab = 'phi', xlab = '',
              main = paste(var,': ', meta[[var]][1]), ylim = c(0,1),
              col=topo.colors(num_level), lwd = 1.5, cex.main = 0.7)
      legend('right', legend = 1:num_level, col=topo.colors(num_level), lwd = 1.5)
    }
    title_text = paste('group: ', group)
    title(title_text, line = 2, outer = TRUE)
  }
}


compare_imputed_pmf <- function(datapath, samplespath){
  # Plot histogram of original pmf vs observed pmf vs imputed pmf
  # datapath: file path of ANES data to be loaded including root
  # samplespath: the name of file that MCMC samples were saved

  # return: NA
  
  # load original data
  X = load_data(datapath)
  # load posterior samples: we need only X_miss and X_SAMPLE from this
  load(samplespath)
  
  # get missing column index
  missing_col = c(MCAR_col, MAR_col)
  missing_col = which(var_name %in% missing_col)
  
  # Check distribution
  for (var_index in missing_col) {
    y_original = X[,var_index]
    original_pmf = table(y_original)/length(y_original)
    
    # Observed distribution
    missing_indicator = is.na(X_miss)[,var_index]
    y_observed = y_original[!missing_indicator]
    observed_pmf = table(y_observed)/length(y_observed)
    
    # calculate imputed pmf
    imputed_pmf = c()
    for (t in 1:(dim(X_SAMPLE)[3])) {
      # for each imputed dataset, calculate joint pmf
      X_SAMPLE_df = data.frame(X_SAMPLE[,,t])
      y_imputed = X_SAMPLE_df[, var_index]
      imputed_pmf_temp = table(y_imputed)/length(y_imputed)
      # imputed_pmf has dimension #samples x #levels
      imputed_pmf = rbind(imputed_pmf, imputed_pmf_temp)
    }
    # calculate posterior mean of imputed pmf
    imputed_pmf_mean = apply(imputed_pmf, MARGIN = 2, FUN = mean)
    
    results = rbind(original_pmf, observed_pmf, imputed_pmf_mean)
    colnames(results)<- 1:length(observed_pmf)
    barplot(results, xlab = 'Category', beside = TRUE, 
            legend = TRUE, args.legend = list(x = "bottomleft",bty = "n"),
            main = paste('Histogram:', var_name[var_index]))
    
  }
}