#### Packages used ####

#install.packages("stringr")
#install.packages("reshape2")

#load libraries
library(readr)
library(here)
library(stringr)
library(reshape2)

#renv::snapshot()

#### Create the migration flow matrix ####

#a matrix showing the number of migrants from all subregions to all subregions
#this code is an adaptation of the instructions of Sander et al. (2014)

#load data
data = read_csv(here("data-processed/data.csv"))

unique(data$subregion_to) == unique(data$subregion_from) #origin and destination subregions are different

#find all subregions in the dataset
unique_subreg = unique(c(unique(data$subregion_to), unique(data$subregion_from)))

#initialise flow matrix as a square matrix with all values of 0 
x = rep(0, length(unique_subreg)^2)
row_n = unique_subreg #row names for matrix - treat as origin subregions
col_n = unique_subreg #column names for matrix - treat as destination subregions

flow_matrix = matrix(x, 
                     nrow = length(unique_subreg), 
                     byrow = TRUE,
                     dimnames = list(row_n, col_n)
) #create matrix

#get number of immigrants/emigrants at subregion level
subregions = data %>%
  group_by(subregion_from, subregion_to) %>%
  summarize(subregion_number = sum(number))

#convert subregions dataframe into wide format
subregions = dcast(subregions,
                   subregion_from ~ subregion_to, #origin subregions as rows
                   value.var = "subregion_number"
) #convert into wide format

#set rownames to subregion_from to facilitate indexing
rownames(subregions) = subregions$subregion_from 

#update flow_matrix with values from subregions
for(i in unique_subreg) { #take each unique subregion
  for(j in unique_subreg) { #combine it with all subregions
    flow_matrix[i, j] = ifelse( #for each combination
      (flow_matrix[i, j] != subregions[i, j] && #if subregions value is different from flow_matrix value
         !(is.na(subregions[i, j]))),  #providing subregions value is not missing
      subregions[i, j], #replace value in flow_matrix with subregions value 
      flow_matrix[i, j] #otherwise keep 0 in flow_matrix
    )
  }
} 

#replace all NAs caused by missing subregions in subregions data frame in flow_matrix with 0 
flow_matrix[is.na(flow_matrix)] = 0

#### Create the plotting details data frame #### 

#a data frame assiging plotting parameters to all subregions
#this code is an adaptation of the instructions of Sander et al. (2014)

#Compute number of emigrants per region 
df_from = data %>%
  group_by(subregion_from) %>%
  summarize(emig = sum(number))

#Compute number of immigrants per region
df_to = data %>%
  group_by(subregion_to) %>%
  summarize(immig = sum(number))

#Create subregion_details data frame with info about total migration flow (immigrants + emigrants)
subregion_details = left_join(df_from, 
                              df_to, 
                              by = c("subregion_from" = "subregion_to") 
) #join dataframes

subregion_details = subregion_details %>%
  rename(subregion = "subregion_from") #subregion_from was here because I left-joined df_to to df_from
#however, subregion_details now includes details about individual subregions, rather than migration flow
#hence the name change
 
remove(df_from, df_to) #remove intermediary objects from the environment

#calculate total migrants per subregion
subregion_details$total = rowSums(subregion_details[ ,c("emig", "immig")], 
                                  na.rm = TRUE)

#Add rgb codes to each subregion

rgb_pool =  c("30,144,255", "128,0,128","255,0,0", 
              "0,255,0", "0,0,255", "218,165,32", 
              "0,255,255", "188,143,143", "255,0,255", 
              "128,128,128", "127,255,212", "128,0,0", 
              "128,128,0", "0,128,0", "0,128,128", 
              "0,0,128", "152,251,152" 
) #Googled 17 rgb codes that enhance contrast; 17 = length(unique_subreg), hence the user can include all subregions in the plot
 
#eliminate subregions with very tiny numbers of migrants as they will muddle the plot
(tiny_subreg = subset(subregion_details, 
                      total < quantile(total, 0.2)
)) #Micronesia and Polynesia had 0 and 3 migrants, respectively, with the next lowest value being 77
#in included in the plot, they will reduce readability, hence I will remove them

subregion_details = subregion_details[!(subregion_details$subregion %in% tiny_subreg$subregion), ]

#select as many colours as needed
subregion_details$rgb = rgb_pool[1:nrow(subregion_details)]

#Split rgb codes into 3 variables - adapted from Sander et al. (2014)
n = nrow(subregion_details)
subregion_details = cbind(subregion_details, #split codes and treat them as numbers
                          matrix(as.numeric(unlist(strsplit(subregion_details$rgb, split = ","))), 
                                 nrow = n, 
                                 byrow = TRUE 
                          ) #arrange them in n rows in a matrix
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
) #converted into HEX

subregion_details$lcol = rgb(subregion_details$r, 
                             subregion_details$g, 
                             subregion_details$b, 
                             alpha = 150, #transparency index
                             max = 255
) #converted into HEX

#add plotting variables to dynamically adapt plot
subregion_details$xmin = rep(0, nrow(subregion_details))
subregion_details$xmax = subregion_details$total

#replace NAs with 0 to reflect no migrants (and because the plotting function cannot handle NAs)
subregion_details[is.na(subregion_details)] = 0

#### Order data ####

#the planned visualization will plot graphical elements in ascending order
#this code is an adaptation of the instructions of Sander et al. (2014)

#order subregion_details ascendently based on total migration flow
subregion_details = subregion_details %>%
  arrange(total) %>% #order ascendently based on total
  mutate(order = c(1:nrow(subregion_details))) #add order variable to index position

subregion_details$subregion = factor(subregion_details$subregion, #treat subregion as factor
                                     levels = subregion_details$subregion) 

flow_matrix = flow_matrix[levels(subregion_details$subregion), #order rows by total flow
                          levels(subregion_details$subregion)] #order columns by total flow