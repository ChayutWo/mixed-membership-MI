load_data <- function(filepath){
  # load data from the provided filepath and format it according to ANES format
  # filepath: file path to be loaded including root

  # return: X
  # X: dataframe of 23 variables that we selected for this study with each variable has 3 levels

  # read input data
  X = read.csv(filepath, sep= '|')
  # select variables: 23 variables
  var_name = c('V162123', 'V162134', 'V162140', 'V162145',
               'V162148', 'V162158', 'V162170', 'V162176', 'V162179',
               'V162180', 'V162192', 'V162193', 'V162207', 'V162208',
               'V162209', 'V162212', 'V162214', 'V162231', 
               'V162246', 'V162260', 'V162269', 'V162271', 
               'V162290')
  X = X[, var_name]
  # extract complete cases
  complete_ind = (apply(X <= 0,MARGIN = 1, sum) == 0)
  X = X[complete_ind,]
  row.names(X) <- NULL # reset row index
  # fix problem with the label of V162290: levels 1,2,4,5 to 1,2,3,4
  ind_4 = X[,'V162290']==4
  X[ind_4,'V162290'] = 3
  ind_5 = X[,'V162290']==5
  X[ind_5,'V162290'] = 4
  # Merge level 3 and 4 from V162192
  ind_4 = X[,'V162192']==4
  X[ind_4,'V162192'] = 3
  
  ## Merge levels of response to simplify the model: old levels -> new level assignment
  # V162123: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162123']==1) | (X[,'V162123']==2)
  ind_2 = (X[,'V162123']==3)
  ind_3 = (X[,'V162123']==4) | (X[,'V162123']==5)
  X[ind_1,'V162123'] = 1
  X[ind_2,'V162123'] = 2
  X[ind_3,'V162123'] = 3
  # V162134: 1+2->1, 3+4->2, 5->3
  ind_1 = (X[,'V162134']==1) | (X[,'V162134']==2)
  ind_2 = (X[,'V162134']==3) | (X[,'V162134']==4)
  ind_3 = (X[,'V162134']==5)
  X[ind_1,'V162134'] = 1
  X[ind_2,'V162134'] = 2
  X[ind_3,'V162134'] = 3
  # V162140: no change
  # V162145: no change
  # V162148: no change
  # V162158: 1->1, 2+3->2, 4->3
  ind_1 = (X[,'V162158']==1)
  ind_2 = (X[,'V162158']==2) | (X[,'V162158']==3)
  ind_3 = (X[,'V162158']==4)
  X[ind_1,'V162158'] = 1
  X[ind_2,'V162158'] = 2
  X[ind_3,'V162158'] = 3
  # V162170: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162170']==1) | (X[,'V162170']==2)
  ind_2 = (X[,'V162170']==3)
  ind_3 = (X[,'V162170']==4) | (X[,'V162170']==5)
  X[ind_1,'V162170'] = 1
  X[ind_2,'V162170'] = 2
  X[ind_3,'V162170'] = 3
  # V162176: no change
  # V162179: no change
  # V162180: no change
  # V162192: no change
  # V162193: no change
  # V162207: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162207']==1) | (X[,'V162207']==2)
  ind_2 = (X[,'V162207']==3)
  ind_3 = (X[,'V162207']==4) | (X[,'V162207']==5)
  X[ind_1,'V162207'] = 1
  X[ind_2,'V162207'] = 2
  X[ind_3,'V162207'] = 3
  # V162208: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162208']==1) | (X[,'V162208']==2)
  ind_2 = (X[,'V162208']==3)
  ind_3 = (X[,'V162208']==4) | (X[,'V162208']==5)
  X[ind_1,'V162208'] = 1
  X[ind_2,'V162208'] = 2
  X[ind_3,'V162208'] = 3
  # V162209: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162209']==1) | (X[,'V162209']==2)
  ind_2 = (X[,'V162209']==3)
  ind_3 = (X[,'V162209']==4) | (X[,'V162209']==5)
  X[ind_1,'V162209'] = 1
  X[ind_2,'V162209'] = 2
  X[ind_3,'V162209'] = 3
  # V162212: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162212']==1) | (X[,'V162212']==2)
  ind_2 = (X[,'V162212']==3)
  ind_3 = (X[,'V162212']==4) | (X[,'V162212']==5)
  X[ind_1,'V162212'] = 1
  X[ind_2,'V162212'] = 2
  X[ind_3,'V162212'] = 3
  # V162214: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162214']==1) | (X[,'V162214']==2)
  ind_2 = (X[,'V162214']==3)
  ind_3 = (X[,'V162214']==4) | (X[,'V162214']==5)
  X[ind_1,'V162214'] = 1
  X[ind_2,'V162214'] = 2
  X[ind_3,'V162214'] = 3
  # V162231: no change
  # V162246: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162246']==1) | (X[,'V162246']==2)
  ind_2 = (X[,'V162246']==3)
  ind_3 = (X[,'V162246']==4) | (X[,'V162246']==5)
  X[ind_1,'V162246'] = 1
  X[ind_2,'V162246'] = 2
  X[ind_3,'V162246'] = 3
  # V162260: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162260']==1) | (X[,'V162260']==2)
  ind_2 = (X[,'V162260']==3)
  ind_3 = (X[,'V162260']==4) | (X[,'V162260']==5)
  X[ind_1,'V162260'] = 1
  X[ind_2,'V162260'] = 2
  X[ind_3,'V162260'] = 3
  # V162269: 1+2->1, 3->2, 4+5->3
  ind_1 = (X[,'V162269']==1) | (X[,'V162269']==2)
  ind_2 = (X[,'V162269']==3)
  ind_3 = (X[,'V162269']==4) | (X[,'V162269']==5)
  X[ind_1,'V162269'] = 1
  X[ind_2,'V162269'] = 2
  X[ind_3,'V162269'] = 3
  # V162271: 1->1, 2+3->2, 4->3
  ind_1 = (X[,'V162271']==1)
  ind_2 = (X[,'V162271']==2) | (X[,'V162271']==3)
  ind_3 = (X[,'V162271']==4)
  X[ind_1,'V162271'] = 1
  X[ind_2,'V162271'] = 2
  X[ind_3,'V162271'] = 3
  # V162290: 1->1, 2+3->2, 4->3
  ind_1 = (X[,'V162290']==1)
  ind_2 = (X[,'V162290']==2) | (X[,'V162290']==3)
  ind_3 = (X[,'V162290']==4)
  X[ind_1,'V162290'] = 1
  X[ind_2,'V162290'] = 2
  X[ind_3,'V162290'] = 3
  
  # format data into categorical variables
  level = apply(X, MARGIN=2, max)
  X = data.frame(X)
  for (col_index in 1:ncol(X)) {
    X[,col_index] = factor(X[,col_index], levels = 1:level[col_index])
  }
  return(X)
}