#### Packages used ####

#renv::init()

#install.packages("here")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("stringr")

library(here)
library(readr)
library(dplyr)
library(stringr)

#renv::snapshot()

#### Load data ####

wf_mig = read_csv(here("data-raw/workforce-migration.csv"))
sub_reg = read_csv(here("data-raw/subregions.csv"))

#### Data cleaning ####

#wf_mig - keep relevant columns and rename them
wf_mig = wf_mig[, c("COU", "Country", "CO2", "Country of origin", "Value")]

wf_mig = wf_mig %>%
  rename(code_to = "COU",
         country_to = "Country",
         code_from = "CO2",
         country_from = "Country of origin",
         number = "Value"
)

#wf_mig - remove domestic migration
wf_mig = wf_mig %>%
  filter(country_to != country_from)

#sub_reg - keep relevant columns and rename them
sub_reg = sub_reg[, c("name", "alpha-3", "sub-region")]
sub_reg = sub_reg %>%
  rename(country = name,
         code = "alpha-3",
         subregion = "sub-region")

#capitalise E in South-eastern Asia
sub_reg$subregion = ifelse(sub_reg$subregion == "South-eastern Asia", 
                           "South-Eastern Asia", 
                           sub_reg$subregion
)

#### Data processing ####

#join wf_mig and sub_reg to assign subregions to botg origin and destination countries

#find countries in wf_mig not included in sub_reg
data.frame(unique(c(setdiff(wf_mig$code_to, sub_reg$code), setdiff(wf_mig$code_from, sub_reg$code)))) #0, the join can be done

#join datasets based on country code
data = left_join(wf_mig, sub_reg, 
                 by = c("code_to" = "code")
) #add subregions for destination countries

data = rename(data, subregion_to =  subregion) #indicate they are destination subregions

data = left_join(data, sub_reg, 
                 by = c("code_from" = "code")
) #add subregions for origin countries

data = rename(data, subregion_from = subregion) #indicate they are origin subregions

#check for missing data after the join
sapply(data, 
       function(x) sum(is.na(x))
) #none

#keep only relevant columns
data = subset(data, select = -c(country.x, country.y)) 
#these were duplicates of origin and destination countries resulting from the join

#save final datafile
write_csv(data,
          here("data-processed/data.csv")
)