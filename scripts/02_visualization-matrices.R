#### Create the migration flow matrix ####

#the visualization requires a matrix showing the number 
#of immigrants and emigrants for all combinations of subregions

#install packages
install.packages("reshape2")

#load libraries
library(readr)
library(reshape2)

#load data
data = read_csv(here("data-processed/data.csv"))

#find all subregions in the dataset
factors = unique(c(unique(data$subregion_to), 
                   unique(data$subregion_from)
                   )
)

#initialise flow matrix as a quadratic matrix with all values of 0 x = rep(0, 16*16)
x = rep(0, length(factors)^2)
row_n = factors #row names for matrix
col_n = factors #column names for matrix

flow_matrix = matrix(x, 
                     nrow = length(factors), 
                     byrow = TRUE,
                     dimnames = list(row_n, col_n)
) #create matrix

#get number of immigrants and emigrants per subregion 
subregions = data[ ,3:5] #retrieve subregions and number of migrants from dataset

subregions = subregions %>%
  group_by(subregion_from, subregion_to) %>%
  summarize(no = sum(number)) #number of total migrants per subregion

#convert subregions dataframe into wide format

subregions = dcast(subregions,
                   subregion_from ~ subregion_to, #origin subregions as rows
                   value.var = "no"
) #convert into wide format

#set rownames to subregion_from to facilitate indexing
rownames(subregions) = subregions$subregion_from 

#update flow_matrix with values from subregions
for(i in factors) { #take each unique subregion
  for(j in factors) { #combine it with all subregions
    flow_matrix[i, j] = ifelse( #for each combination
      (flow_matrix[i, j] != subregions[i, j] && #if subregions value is different from flow_matrix value
         !(is.na(subregions[i, j]))),  #providing subregions value is not missing
      subregions[i, j], #replace value in flow_matrix with subregions value 
      0 #otherwise keep 0 in flow_matrix
    )
  }
}

#replace all NAs in flow_matrix with 0 to mark the absence of migrants
flow_matrix[is.na(flow_matrix)] = 0

#### Create the plotting details data frame #### 




