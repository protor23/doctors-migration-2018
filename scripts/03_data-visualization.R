#please check the figs folder to retrieve the plot after running this script - it looks dreadful in R

#### Packages used ####

#install.packages("circlize")
#install.packages("scales")

#renv::snapshot()

#load libraries
library(dplyr)
library(here)
library(circlize)
library(reshape2)
library(stringr)

#run previous script to bring objects in the environment
source(here("scripts/02_visualization-matrices.R"))

#### Initialize circular diagram ####

#this code is an adaptation of the instructions of Sander et al. (2014)

png(here("figs/01_migration-flow.png"),
    width = 15,
    height = 15,
    units = "cm",
    pointsize = 3,
    res = 2048) #open graphics device to save the plot later

circos.clear() #reset circular layout parameters - if not done, alterations to the code will build on the existing plot

#set plotting parameters 
par(mar = c(0, 0, 0, 0)) #margin around chart
circos.par(cell.padding = c(0, 0, 0, 0), 
           track.margin = c(0, 0.1), 
           start.degree = 45, #start plotting at 2 o'clock
           gap.degree = 2, #gap between circle sectors
           points.overflow.warning = FALSE, 
           canvas.xlim = c(-1.3, 1.3), #size of circle
           canvas.ylim = c(-1.3, 1.3)  #size of circle
)

circos.initialize(factors = subregion_details$subregion, #allocate sectors on circle to subregions
                  xlim = cbind(subregion_details$xmin, 
                               subregion_details$xmax) #set limits of the x axis for each sector (between xmin = 0 and xmax = total migrant flow)
)

options(scipen = 10) #prevent scientific notation on plot

#### Plot sectors ####

circos.trackPlotRegion(ylim = c(0, 1), #y-axis limits for each sector
                       factors = subregion_details$subregion, 
                       track.height = 0.1, 
                       panel.fun = function(x, y) { #for each new cell (i.e., intersection between sector and track)
                         name = get.cell.meta.data("sector.index") #retrieve cell meta data
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #plot subregion names
                         circos.text(x = mean(xlim), #position text at middle of sector
                                     y = ifelse(str_length(name) > 25, 4.5, 
                                                ifelse(str_length(name) > 20, 4, 
                                                       ifelse(str_length(name) >= 14, 3.3, 3))
                                                ), #distance of text from plot based on length of subregion name
                                     labels = name, #name of subregion
                                     facing = "clockwise", 
                                     niceFacing = TRUE, #adjust text orientation to make it human readable
                                     cex = 1.7, #scale text
                                     col = subregion_details$rcol[i] #colour matching region label - less transparent colour
                         )
                         
                         #plot the sector cells for each subregion with its respective colour
                         circos.rect(xleft = xlim[1], #coordinates vary by sector
                                     ybottom = ylim[1], 
                                     xright = xlim[2], 
                                     ytop = ylim[2], 
                                     col = subregion_details$rcol[i], #use less transparent colours
                                     border = subregion_details$rcol[i]
                         )
                         
                         #distinguish between immigrants and emigrants in each subregion cell
                         circos.rect(xleft = xlim[1], 
                                     ybottom = ylim[1], 
                                     xright = xlim[2] - rowSums(flow_matrix)[i], #total - emigrants
                                     ytop = ylim[1] + 0.3,
                                     col = "white", 
                                     border = "white"
                         ) 
                         
                         #add a white contour to separate the previous two sector cells
                         circos.rect(xleft = xlim[1], 
                                     ybottom = 0.3, 
                                     xright = xlim[2], 
                                     ytop = 0.32, 
                                     col = "white", 
                                     border = "white"
                         )
                         
                         #add axis to indicate migrant numbers
                         circos.axis(labels.cex = 1.3, #size of label text
                                     lwd = 0.5, #size of ticks
                                     labels.niceFacing = TRUE, #human-readable
                                     major.tick.length = 0.3,
                                     minor.ticks = 3,
                                     major.at = seq(0, xlim[2] + 20000, by = 20000), #major ticks every 20000 units
                                     labels = scales::comma(seq(0, xlim[2] + 20000, by = 20000)) #add commas to plotted numbers
                          )
        
                       }
)

#### Plot links ####

#transform flow_matrix into its long form
flow_matrix_long = melt(flow_matrix,
                        value.name = "number" #number of migrants
)

colnames(flow_matrix_long) = c("subregion_from",
                               "subregion_to",
                               "number") #rename columns for consistency

#sort descendently according to the number of migrants
flow_matrix_long = flow_matrix_long %>%
  arrange(desc(number)) 

#keep only the largest flows to increase readability
flow_matrix_long = subset(flow_matrix_long, 
                          number > quantile(number, 0.70)
)

#add plotting parameters that dynamically update the coordinates of circle links - see Sander et al. (2014)
subregion_details$sum1 <- colSums(flow_matrix)
subregion_details$sum2 <- numeric(nrow(subregion_details)) 

#not yet clear why this is needed, but it prevents links from being plotted outside of range
circos.par(track.margin = c(0, 0)) 

#plot links for each combination of regions
for(k in 1:nrow(flow_matrix_long)){ #for each combination of subregions
  i = match(flow_matrix_long$subregion_from[k],
            subregion_details$subregion) #get plotting details for subregion of origin
  j = match(flow_matrix_long$subregion_to[k],
            subregion_details$subregion) #get plotting details for destination subregion
  
  circos.link(sector.index1 = subregion_details$subregion[i], #plot a link from subregion of origin 
              point1 = c(subregion_details$sum1[i], 
                         subregion_details$sum1[i] + abs(flow_matrix[i, j])), #starting point of link
              
              sector.index2 = subregion_details$subregion[j], ##end the link in the destination subregion
              point2=c(subregion_details$sum2[j], 
                       subregion_details$sum2[j] + abs(flow_matrix[i, j])), #endpoint of link
              
              border = subregion_details$lcol[i],
              col = subregion_details$lcol[i], #use the more transparent colour to see overlaps
  )
  
  #update sum1 and sum2 - move on the circle to the next subregion sector
  subregion_details$sum1[i] = subregion_details$sum1[i] + abs(flow_matrix[i, j]) 
  subregion_details$sum2[j] = subregion_details$sum2[j] + abs(flow_matrix[i, j])
}

#save plot
dev.off() #close graphing device