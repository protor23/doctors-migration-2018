#### Packages used ####

packrat::init()

install.packages("here")
install.packages("readr")
install.packages("dplyr")
install.packages("data.table")
install.packages("stringr")
install.packages("circlize")
install.packages("tibble")

packrat::snapshot()

library(here)
library(readr)
library(dplyr)
library(stringr)

#### Load data ####

wf_mig = read_csv(here("data-raw/workforce-migration.csv")) #migration data
sub_reg = read_csv(here("data-raw/subregions.csv")) #subregions data

#### Basic data cleaning ####

wf_mig = wf_mig[, -3] #eliminate year column
colnames(wf_mig) = c("to", "from", "number") #rename columns

#remove domestic migration and cases with 0 migrants
wf_mig = wf_mig %>%
  filter(to != from) %>%
  filter(number != 0)

#remove Others and Total categories; only keep country-specific data
wf_mig = wf_mig %>%
  filter(!(from %in% c("Total", "Others (not elsewhere classified)")))

sub_reg = sub_reg[ ,c("name", "sub-region")] #select relevant columns
colnames(sub_reg) = c('country', "subregion") #rename columns

#### Data cleaning - joining datasets ####

#prepare to join wf_mig and sub_reg to get subregions for origin and destination countries
#check whether all countries in wf_mig are also present in sub_reg to allow the join

#find countries in wf_mig not included in sub_reg
diff = data.frame(country = unique(c(setdiff(wf_mig$to, sub_reg$country), 
                                     setdiff(wf_mig$from, sub_reg$country)
                                  )
                  )
)

length(diff$country) #only 17 countries

#look for countries in sub_reg like the ones in diff - check why misidentification occurs
missing_countries = c("Bolivia", "Macedonia", "Laos", "Moldova", 
                      "Swaziland", "United Kingdom", "Chinese Taipei", 
                      "Czech", "Iran", "Slovak", "Syria",
                      "Tanzania", "Venezuela", "Korea", "Congo", 
                      "United States"
) #hardcoded based on diff

for(i in 1:length(missing_countries)) { #for every missing country
  for (j in 1:length(unique(sub_reg)$country)) #check all unique countries in sub_reg
    if(str_detect(unique(sub_reg)$country[j], missing_countries[i])) { #find similarities
      print(c(unique(sub_reg)$country[j], missing_countries[i])) #print similarities
    }
} #check if similarities make sense - they do, except for Macedonia 
#North Macedonia in sub_reg is different from former Yugoslav Republic of Macedonia in diff

#add country names in sub_reg following format in wf_mig to create a join key
sub_reg$country_join = case_when(
  sub_reg$country == "Bolivia (Plurinational State of)" ~ "Bolivia", 
  sub_reg$country == "Moldova, Republic of" ~ "Republic of Moldova",
  sub_reg$country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
  sub_reg$country == "Czechia" ~ "Czech Republic",
  sub_reg$country == "Iran (Islamic Republic of)" ~ "Iran", 
  sub_reg$country == "Slovakia" ~ "Slovak Republic", #replace Slovakia with Slovak Republic
  sub_reg$country == "Syrian Arab Republic" ~ "Syria",
  sub_reg$country == "Tanzania, United Republic of" ~ "United Republic of Tanzania",
  sub_reg$country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
  sub_reg$country == "Korea, Republic of" ~ "Republic of Korea",
  sub_reg$country == "Korea (Democratic People's Republic of)" ~ "Democratic People's Republic of Korea",
  sub_reg$country == "Congo" ~ "Congo",
  sub_reg$country == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
  sub_reg$country == "United States of America" ~ "United States",
  sub_reg$country == "United States Mino Outlying Islands" ~ "United States",
  TRUE ~ sub_reg$country
) #different separators - comas and brackets
#multiple dependencies (e.g., Korea - Republic, Democratic Republic)
#therefore, cannot avoid hardcoding

#join data sets based on country
data = left_join(wf_mig, sub_reg, 
                 by = c("to" = "country_join")
)                  #add subregions to destination countries

data = rename(data, subregion_to =  subregion) #rename columns to maintain consistency

data = left_join(data, sub_reg, 
                 by = c("from" = "country_join")
) #add subregions to origin countries

data = rename(data, subregion_from = subregion) #rename columns to maintain consistency

#check for NAs in data 
sapply(data, 
       function(x) sum(is.na(x))
) #18 origin countries have missing subregions and country names

identical(data$from[is.na(data$country.y)], 
          data$from[is.na(data$subregion_from)]
          ) #identical, so different country names in wf_mig and sub_reg prevent the join

unique(data$from[is.na(data$country.y)]) #find which countries prevent the join
#Laos, Swaziland, Chinese Taipei, The former Yugoslav Republic of Macedonia

#google subregions for these and include them
data = mutate(data, 
              subregion_to = case_when(
                to %in% c("Swaziland") ~ "Sub-Saharan Africa",
                to %in% c("Laos") ~ "South-eastern Asia",
                to %in% c("Chinese Taipei") ~ "Eastern Asia",
                to %in% c("The former Yugoslav Republic of Macedonia") ~ "Southern Europe",
                TRUE ~ subregion_to
              )
)

data = mutate(data, 
              subregion_from = case_when(
                from %in% c("Swaziland") ~ "Sub-Saharan Africa",
                from %in% c("Laos") ~ "South-eastern Asia",
                from %in% c("Chinese Taipei") ~ "Eastern Asia",
                from %in% c("The former Yugoslav Republic of Macedonia") ~ "Southern Europe",
                TRUE ~ subregion_from
              )
)

#recheck NAs
sapply(data, 
       function(x) sum(is.na(x))
) #no more missing subregions

#keep only relevant columns
data = data[c("to", "from", "number", "subregion_to", "subregion_from")]