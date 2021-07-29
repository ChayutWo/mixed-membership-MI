# This file contains all metadata needed to work with ANES dataset
# Mostlikely, this file should be sourced when perform post-processing or analyzing MCMC samples
source("../../utils/load_data.R")
filepath = '../../ANES_2016/anes_timeseries_2016/anes_timeseries_2016_rawdata.txt'
rawData = load_data(filepath)

# Number of levels for each variable
level = apply(rawData, MARGIN=2, max)

# various constants
N = dim(rawData)[1]
p = dim(rawData)[2]
K = 30
rm(rawData)
# variables subjected to MCAR
MCAR_col = c('V162123', 'V162148', 'V162170', 'V162192','V162209', 'V162246')
# variables subjected to MAR
MAR_col = c('V162140', 'V162158', 'V162179','V162207','V162214', 'V162269')

# variable names
var_name = c('V162123', 'V162134', 'V162140', 'V162145', 'V162148', 'V162158', 
             'V162170', 'V162176', 'V162179', 'V162180', 'V162192', 'V162193', 
             'V162207', 'V162208', 'V162209', 'V162212', 'V162214', 'V162231', 
             'V162246', 'V162260', 'V162269', 'V162271', 'V162290')

# variable description
description = c('Agree/disagree: Better if rest of world more like America', 
                'How much opportunity is there in America today to get ahead?',
                'Favor or oppose tax on millionaires',
                'Health Care Law effect on cost of health care',
                'Favor or oppose government reducing income inequality',
                'How likely immigration will take away jobs',
                'Agree/disagree: Country needs strong leader to take us back to true path',
                'Favor or oppose free trade agreements with other countries',
                'Should marijuana be legal',
                'Should the government do more or less to regulate banks',
                'Should the minimum wage be raised',
                'Increase or decrease government spending to help people pay for health care',
                'Agree/disagree: world is changing and we should adjust',
                'Agree/disagree: newer lifestyles breaking down society',
                'Agree/disagree: We should be more tolerant of other moral standards',
                'Agree/disagree: past slavery and discrimination made it difficult for blacks to work their way out of the lower class',
                'Agree/disagree: if blacks would only try harder they could be just as well off as whites',
                'Should the news media pay more attention to discrimination against women?',
                'Agree/disagree: If people were treated more equally in this country we would have many fewer problems',
                'Agree/disagree: Most politicians do not care about the people',
                'Agree/disagree: America’s culture is generally harmed by immigrants',
                'Agree/disagree: To be truly American important to have been born in U.S.',
                'Satisfied with way democracy works in the U.S.'
)

# full level description for each variable
level_desc = c('1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=A great deal/A lot,
2= A moderate amount/A little,
3=None',
               '1=Favor,
2= Oppose,
3= Neither favor nor oppose',
               '1=Increased,
2= Decreased,
3= Had no effect',
               '1=Favor,
2= Oppose,
3= Neither favor nor oppose',
               '1=Extremely likely,
2= Very likely/Somewhat likely,
3=Not at all likely',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Favor,
2= Oppose,
3= Neither favor nor oppose',
               '1=Favor,
2= Oppose,
3= Neither favor nor oppose',
               '1=More,
2= Less,
3= The same',
               '1=Raised,
2= Kept the same,
3=Lowered or Eliminated',
               '1=Increase,
2= Decrease,
3= No change',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=More attention,
2= Less attention,
3=Same amount of attention',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Agree,
2=Neither agree nor disagree,
3=Disagree',
               '1=Very important,
2=Fairly important/Not very important,
3=Not important at all', 
               '1= Very satisfied,
2=Fairly satisfied/Not very satisfied,
3=Not at all satisfied')

# Combine everything into a metadata list for lookup
meta = list()
for (i in 1:p) {
  # description, number of levels, index
  meta[[var_name[i]]] = c(description[i], level[i], level_desc[i], i)
}