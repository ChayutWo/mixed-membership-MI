
calculate_disagreement <- function(samplespath, num_big_cluster=7, cutoff = 0.75){
  # Calculate and plot disagreement ratio for MCMC samples in samplespath
  # disagreement is posterior mean of argmax{Phi_k^(j)} != argmax{Phi_k'^(j)} for k and k' clusters
  # samplespath: the name of file that MCMC samples were saved
  # num_big_cluster: maximum number of biggest clusters to be included in the calculation
  # cutoff: cutoff for disagreement above which two groups will be deemed disagreeing with each other
  
  # return: disagreement is a matrix of size #var x #combination of groups (#groups choose 2)
  
  # load posterior samples
  load(samplespath)
  
  group_list = 1:num_big_cluster # clusters to be compared pairwise
  combinations = combn(group_list, 2)
  disagreement = matrix(0, length(var_name), dim(combinations)[2])
  for (i in 1:(dim(combinations)[2])) {
    #for each combination of groups, calculate the disagreement probability
    cluster = combinations[, i]
    for (j in 1:length(var_name)) {
      # For each variable of interest, get its index and extract posterior samples of Phi^j: Phi_varj
      var = var_name[j]
      idx = meta[[var]][4]
      Phi_varj = PHI[[strtoi(idx)]]
      # Get their respective Phi_k^(j)
      Phi_varj_1 = Phi_varj[cluster[1],,]
      Phi_varj_2 = Phi_varj[cluster[2],,]
      # Find max position of Phi_k^(j)
      argmax_1 = max.col(t(Phi_varj_1))
      argmax_2 = max.col(t(Phi_varj_2))
      disagreement[j, i] = mean(argmax_1 != argmax_2)
    }
  }
  rownames(disagreement) <- var_name
  colnames(disagreement) <- paste('group ',combinations[1,], '&', combinations[2,], sep = '')
  
  # apply cutoff and sum across all variables
  disagreement_indicator = (disagreement >= cutoff)
  disagreement_ratio = apply(disagreement_indicator, MARGIN = 2, mean)
  print(disagreement)
  heatmap.2(disagreement, dendrogram="col", col=bluered,
            main = 'disagreement (original)', cexCol = 0.9)
  # after applying the cutoff of 0.75 to the disagreement
  heatmap.2(disagreement_indicator*1, dendrogram="col", 
            col=bluered, main = 'disagreement_indicator', cexCol = 0.9)
  # the result from new metrics: the disagreement with cutoffs -> may need to be interpreted by taking the topics of questions into account.
  barplot(sort(disagreement_ratio), las = 2, main = 'disagreement ratio', ylim = c(0,1))
  return(disagreement)
}

calculate_cohesion_ratio <- function(samplespath, num_big_cluster=7){
  # Calculate and plot cohesion ratio to response for different groups for MCMC samples in samplespath
  # cohesion_ratio_jk represents posterior mean of (max{Phi_k^(j)} - min{Phi_k^(j)})/max{Phi_k^(j)}
  # samplespath: the name of file that MCMC samples were saved
  # num_big_cluster: maximum number of biggest clusters to be included in the calculation
  
  # return: cohesion_ratio is a matrix of size #var x #cluster
  
  # load posterior samples
  load(samplespath)
  
  cohesion_ratio = matrix(0, length(var_name), num_big_cluster)
  max_response = matrix(0, length(var_name), num_big_cluster)
  for (j in 1:length(var_name)) {
    # For each variable of interest, get its index and extract posterior samples of Phi^j: Phi_varj
    var = var_name[j]
    idx = meta[[var]][4]
    Phi_varj = PHI[[strtoi(idx)]]
    for (k in 1:num_big_cluster){
      # For each cluster of interest, extract Phi_k^(j) and calculate cohesion_ratio metric
      Phi_k_varj = Phi_varj[k,,]
      # get max{Phi_k^(j)} and min{Phi_k^(j)} for each posterior samples: max_phi, min_phi
      max_phi = apply(Phi_k_varj, MARGIN = 2, max)
      min_phi = apply(Phi_k_varj, MARGIN = 2, min)
      # Calculate cohesion_ratio for cluster k and variable j
      cohesion_ratio[j,k] = mean((max_phi-min_phi)/max_phi)
      max_response[j,k] = mean(max_phi)
    }
  }
  rownames(cohesion_ratio) <- var_name
  colnames(cohesion_ratio) <- paste('group', 1:num_big_cluster)
  print(cohesion_ratio)
  plot(x = 1:num_big_cluster, y = apply(cohesion_ratio, MARGIN = 2, mean), 
       ylab = 'mean of cohesion_ratio score', xlab = 'cluster index')
  # cohesion ratio result in a lot of values that are close to 1.00 as pmf min gets smaller
  heatmap.2(cohesion_ratio, dendrogram="row", col=bluered, main = 'cohesion_ratio', cexCol = 0.9)
  return(cohesion_ratio)
}

calculate_cohesion <- function(samplespath, num_big_cluster=7){
  # Calculate and plot cohesion to response for different groups for MCMC samples in samplespath
  # cohesion_jk represents posterior mean of max{Phi_k^(j)}/min{Phi_k^(j)}
  # samplespath: the name of file that MCMC samples were saved
  # num_big_cluster: maximum number of biggest clusters to be included in the calculation
  
  # return: cohesion is a matrix of size #var x #cluster
  
  # load posterior samples
  load(samplespath)
  cohesion = matrix(0, length(var_name), num_big_cluster) # placeholder
  
  for (j in 1:length(var_name)) {
    # For each variable of interest, get its index and extract posterior samples of Phi^j: Phi_varj
    var = var_name[j]
    idx = meta[[var]][4]
    Phi_varj = PHI[[strtoi(idx)]]
    for (k in 1:num_big_cluster){
      # For each cluster of interest, extract Phi_k^(j) and calculate cohesion metric
      Phi_k_varj = Phi_varj[k,,]
      # get max{Phi_k^(j)} and min{Phi_k^(j)} for each posterior samples: max_phi, min_phi
      max_phi = apply(Phi_k_varj, MARGIN = 2, max)
      min_phi = apply(Phi_k_varj, MARGIN = 2, min)
      # Calculate cohesion for cluster k and variable j
      cohesion[j,k] = mean(max_phi/min_phi)
    }
  }
  rownames(cohesion) <- var_name
  colnames(cohesion) <- paste('group', 1:num_big_cluster)
  print(cohesion)
  plot(x = 1:num_big_cluster, y = apply(cohesion, MARGIN = 2, mean), 
       ylab = 'mean of cohesion score', xlab = 'cluster index')
  # ln cohesion make the dist more uniform across different values of original cohesion
  heatmap.2(log(cohesion), dendrogram="row", col=bluered, main = 'ln(cohesion)', cexCol = 0.9)
  return(cohesion)
}