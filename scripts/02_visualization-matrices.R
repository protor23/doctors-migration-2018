#### Packages used ####

#install.packages("stringr")
#install.packages("reshape2")

#load libraries
library(readr)
library(here)
library(stringr)
library(reshape2)
library(dplyr)

#renv::snapshot()

#### The migration flow matrix #### 
#matrix containing number of migrants moving between all combination of subregions

#the creation of this object is guided by Sander et al. (2014), but the data processing involved is mine

#load data
data = read_csv(here("data-processed/data.csv"))

#find all subregions in the dataset
unique_subreg = unique(c(unique(data$subregion_to), unique(data$subregion_from)))

#initialise flow matrix as a square matrix with all values of 0 
x = rep(0, length(unique_subreg)^2)
row_n = unique_subreg #treat as origin subregions
col_n = unique_subreg #treat as destination subregions

flow_matrix = matrix(x, 
                     nrow = length(unique_subreg), 
                     byrow = TRUE,
                     dimnames = list(row_n, col_n)
) #create matrix

#get number of migrants at subregion level
subregions = data %>%
  group_by(subregion_from, subregion_to) %>%
  summarize(subregion_number = sum(number))

#convert subregions dataframe into wide format
subregions = dcast(subregions,
                   subregion_from ~ subregion_to, #origin subregions as rows
                   value.var = "subregion_number"
)

#set rownames as subregion names to facilitate indexing
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

#replace all missing values in flow_matrix
#NAs caused by missing subregion combinations in the subregions data frame 
flow_matrix[is.na(flow_matrix)] = 0

#### The subregion plotting details data frame #### 
#a data frame assiging plotting parameters to all subregions
##the creation of this object and the data processing involved is guided by Sander et al. (2014)

#get number of emigrants per region 
df_from = data %>%
  group_by(subregion_from) %>%
  summarize(emig = sum(number))

#get number of immigrants per region
df_to = data %>%
  group_by(subregion_to) %>%
  summarize(immig = sum(number))

#create subregion_details data frame with immigrants and emigrants per subregion
subregion_details = left_join(df_from, 
                              df_to, 
                              by = c("subregion_from" = "subregion_to") 
)

subregion_details = subregion_details %>%
  rename(subregion = "subregion_from") #subregion_from was here because I left-joined df_from to df_to
#however, this column now represents only the name of subregions, not an origin, hence the name change
 
remove(df_from, df_to) #remove intermediary objects from the environment

#calculate total migrants per subregion
subregion_details$total = rowSums(subregion_details[ ,c("emig", "immig")], 
                                  na.rm = TRUE)

#order subregion_details ascendently based on total migration flow
#the planned visualization will plot graphical elements in ascending order
subregion_details = subregion_details %>%
  arrange(total) %>% #order ascendently based on total
  mutate(order = c(1:nrow(subregion_details))) #add order variable to index position

#add rgb codes to each subregion
rgb_pool =  c("255,0,0", #red
              "0,255,0", #lime
              "0,0,255", #blue
              "148,0,211", #dark violet
              "0,206,209", #dark turquoise
              "255,0,255", #magenta
              "128,0,0", #maroon
              "255,99,71", #tomato
              "128,128,0", #olive
              "0,128,0", #green
              "128,0,128", #purple
              "0,128,128", #teal
              "0,0,128", #navy
              "250,128,144", #salmon
              "100,149,237", #corn flower blue
              "153,50,204", #dark orchid
              "60,179,113" #medium sea green
) #googled 17 rgb codes that enhance contrast; 17 = length(unique_subreg)
 
#eliminate subregions with tiny numbers of migrants as they will muddle the plot
(tiny_subreg = subset(subregion_details, 
                      total < quantile(total, 0.2)
)) #select subregions whose total migrants number is in the bottom 20%

subregion_details = subregion_details[!(subregion_details$subregion %in% tiny_subreg$subregion), ] #remove low-migrant subregions

#select as many colours as needed - they will be allocated in the order specified earlier
subregion_details$rgb = rgb_pool[1:nrow(subregion_details)]

#split rgb codes into 3 variables - adapted from Sander et al. (2014)
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
                             alpha = 200, #transparency index
                             max = 255
) #converted into HEX

#add plotting variables - thee will be axis boundaries for migrant numbers per subregion
subregion_details$xmin = rep(0, nrow(subregion_details))
subregion_details$xmax = subregion_details$total

#replace NAs with 0 to reflect no migrants (and because the plotting function cannot handle NAs)
subregion_details[is.na(subregion_details)] = 0

#### Order flow_matrix ####
subregion_details$subregion = factor(subregion_details$subregion, #treat subregion as factor
                                     levels = subregion_details$subregion) 

flow_matrix = flow_matrix[levels(subregion_details$subregion), #order rows by total flow
                          levels(subregion_details$subregion)] #order columns by total flow