#### Packages used ####
#install packages
install.packages("reshape2")

#load libraries
library(readr)
library(stringr)
library(reshape2)

#### Create the migration flow matrix ####

#the planned visualization requires a matrix showing the number 
#of immigrants and emigrants for all combinations of subregions
#this code is an adaptation of the instructions of Sander et al. (2014)

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

#the planned visualization requires a data frame assigning colours 
#and other plotting parameters to all subregions
#this code is an adaptation of the instructions of Sander et al. (2014)

#Compute number of emigrants per region
df_from = data %>%
  group_by(subregion_from) %>%
  summarize(sum(number))

#Compute number of immigrants per region
df_to = data %>%
  group_by(subregion_to) %>%
  summarize(sum(number))

#Create subregion_details data frame with info about migrants and emigrants
subregion_details = left_join(df_from, 
                              df_to, 
                              by = c("subregion_from" = "subregion_to")
) #join dataframes

colnames(subregion_details) = c("subregion", "outer", "inner") #rename columns

subregion_details$total = rowSums(subregion_details[ ,c("outer", "inner")], 
                                 na.rm = TRUE) #add total migrants to datframe

#Add rgb codes to each subregion for sectors and links

subregion_details$rgb = c("255,0,0", "0,255,0", "0,0,255", 
                          "255,255,0", "0,255,255", "255,0,255",
                          "128,128,128", "128,0,0", "128,128,0", 
                          "0,128,0", "128,0,128", "0,128,128",
                          "0,0,128", "152,251,152", "30,144,255", 
                          "138,43,226"
) #thank you Google

#Split rgb codes into 3 variables - adapted from Sander et al. (2014)
n = nrow(subregion_details)
subregion_details = cbind(subregion_details, #split codes and treat them as numbers
                          matrix(as.numeric(unlist(strsplit(subregion_details$rgb, split = ","))), 
                                 nrow = n, byrow = TRUE 
                                 ) #arrange them in n columns in a matrix
)

subregion_details = subregion_details %>%
  rename( #rename columns according to the colour index
    r = '1',
    g = '2',
    b = '3',
  )

#add two similar colours varying in transparency per subregion - adapted from Sander et al. (2014)
subregion_details$rcol = rgb(subregion_details$r, 
                             subregion_details$g, 
                             subregion_details$b, 
                             max = 255
)

subregion_details$lcol = rgb(subregion_details$r, 
                             subregion_details$g, 
                             subregion_details$b, 
                             alpha=200, 
                             max = 255
)

#add minimum number of migrants per subregion as 0
subregion_details$xmin = rep(0, nrow(subregion_details))

#replace NAs with 0 to reflect no migrants (and because the plotting function cannot handle NAs)
subregion_details[is.na(subregion_details)] = 0

#### Order data ####

#the planned visualization will plot graphical elements in ascending order
#this code is an adaptation of the instructions of Sander et al. (2014)

#order subregion_details based on total migration flow
subregion_details = subregion_details %>%
  arrange(total) %>% #order ascendently based on total
  mutate(order = c(1:nrow(subregion_details))) #add order variable to index position

subregion_details$subregion = factor(subregion_details$subregion, #treat subregion as factor
                                     levels = subregion_details$subregion) 

flow_matrix = flow_matrix[levels(subregion_details$subregion), #order rows by total flow
                          levels(subregion_details$subregion)] #order columns by total flow
