compare_two_groups <- function(filepath_nonmissing, group_nonmissing, filepath_missing, group_missing, group_desc){
  # Compare imputed/posterior distributions of response to different questions of interest between 2 groups
  # One group will be from nonmissing data and the other will be from data with missing values
  # It will produce figures + table summary of pmf across different levels of each of variables of interest
  
  # filepath_nonmissing: file path for the result of nonmissing data
  # filepath_missing: file path for the result of data with missing values
  # groups to be plotted: 
  # one group from nonmissing data (group_nonmissing), the other from missing data (group_missing)
  # group_desc: short description of groups to help remember like 'dem' or 'rep'
  
  # return: NA
  
  # Part I: Load data
  # Load results with nonmissing data 
  load(filepath_nonmissing)
  PHI_nonmissing = PHI
  cluster_size_nonmissing = mean(BETA[, group_nonmissing])
  # Load results with missing data
  load(filepath_missing)
  PHI_missing = PHI
  cluster_size_missing = mean(BETA[, group_missing])
  # selected variables to be plotted
  var_list = c(MAR_col, MCAR_col)
  #var_list = var_name
  # Part II: Comparison of plots of pmf
  par(mfrow=c(length(var_list),2), oma = c(1.5,4.5,1,0.2) + 0.1, mar = c(0.75,0.25,1.25,0.25) + 0.5)
  for (var in var_list) {
    idx = meta[[var]][4] # index for that variable
    
    # plot PHI for variable named var and for the selected group: nonmissing data
    phi = t(PHI_nonmissing[[strtoi(idx)]][group_nonmissing,,]) # matrix of size samples x # levels
    num_level= strtoi(meta[[var]][2])
    matplot(1:dim(phi)[1], phi, 
            type = 'l', ylab = 'phi', xlab = '',
            main = paste(var,': ', meta[[var]][1]), ylim = c(0,1),
            col=topo.colors(num_level), lwd = 1.5, cex.main = 0.7, cex.axis = 0.7)
    legend('right', legend = 1:num_level, col=topo.colors(num_level), lwd = 1.5)
    
    # Add text for variable name
    mtext(text = var, side = 2, line = 1, cex = 0.5, las = 2, adj = 1.5, font = 2)
    
    # plot PHI for variable named var and for the selected group: missing data
    phi = t(PHI_missing[[strtoi(idx)]][group_missing,,]) # matrix of size samples x # levels
    num_level= strtoi(meta[[var]][2])
    matplot(1:dim(phi)[1], phi, 
            type = 'l', ylab = 'phi', xlab = '',
            main = paste(var,': ', meta[[var]][1]), ylim = c(0,1),
            col=topo.colors(num_level), lwd = 1.5, cex.main = 0.7, cex.axis = 0.7,
            yaxt = 'n')
    axis(side = 2, tick = TRUE, labels = FALSE)
    legend('right', legend = 1:num_level, col=topo.colors(num_level), lwd = 1.5)
  }
  
  # Part III: Summary table of pmfs
  summary = c()
  for (var in var_list) {
    idx = meta[[var]][4] # index for that variable
    
    # Get posterior mean of the pmf of that variable: without missing data
    phi = t(PHI_nonmissing[[strtoi(idx)]][group_nonmissing,,]) # matrix of size samples x # levels
    pmf_nonmissing =apply(phi, MARGIN=2, mean)
    
    
    # Get posterior mean of the pmf of that variable: with missing data
    phi = t(PHI_missing[[strtoi(idx)]][group_missing,,]) # matrix of size samples x # levels
    pmf_missing =apply(phi, MARGIN=2, mean)
    summary = rbind(summary, c(pmf_nonmissing, pmf_missing))
  }
  colnames(summary) <- c('level = 1', 'level = 2', 'level = 3', 
                         'level = 1', 'level = 2', 'level = 3')
  rownames(summary) <- var_list
  caption = paste(group_desc, " nonmissing: ", group_nonmissing, " size = ", cluster_size_nonmissing, 
                  ", missing: ", group_missing, " size = ", cluster_size_missing, sep = '')
  xtable(summary, digits = 2, caption = caption)
}