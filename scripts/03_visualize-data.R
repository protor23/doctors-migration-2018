#### Packages used ####

#install.packages("circlize")

#renv::snapshot()

#load libraries
library(dplyr)
library(here)
library(circlize)
library(reshape2)
library(stringr)

#run previous script to bring objects in the environment
source(here("scripts/02_visualization-matrices.R"))

#### Initialize circular migration flow diagram ####

#this code is an adaptation of the instructions of Sander et al. (2014)

png(here("figs/01_migration-flow.png"),
    width = 10,
    height = 10,
    units = "cm",
    pointsize = 3,
    res = 2048) #open graphics device to save the plot later

circos.clear() #reset circular layout parameters

par(mar = c(0, 0, 0, 0)) #set margin values 
circos.par(cell.padding = c(0, 0, 0, 0), 
           track.margin = c(0, 0.1), 
           start.degree = 90, #start plotting at 12 o'clock
           gap.degree = 2, #gap between circle sectors
           points.overflow.warning = FALSE, 
           canvas.xlim = c(-1.7, 1.7), 
           canvas.ylim = c(-1.7, 1.7) 
)

circos.initialize(factors = subregion_details$subregion, #allocate sectors on circle to subregions
                  xlim = cbind(subregion_details$xmin, 
                               subregion_details$total) #set limits of the x axis for each sector between 0 and total flow
)

#### Plot sectors ####

circos.trackPlotRegion(ylim = c(0, 1), #y-axis limits for each sector
                       factors = subregion_details$subregion, 
                       track.height = 0.2, 
                       panel.fun = function(x, y) { #for each new cell (i.e., intersection between sector and track)
                         name = get.cell.meta.data("sector.index") #retrieve cell meta data
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #plot subregion names
                         circos.text(x = mean(xlim), #position text at middle of sector
                                     y = ifelse(str_length(name) > 20, 3.5, 3),
                                     labels = name, #name of subregion
                                     facing = "clockwise", 
                                     niceFacing = TRUE,
                                     cex = 1.3 #scale text
                         )
                         
                         #plot a sector for each subregion
                         circos.rect(xleft = xlim[1], 
                                     ybottom = ylim[1], 
                                     xright = xlim[2], 
                                     ytop = ylim[2], 
                                     col = subregion_details$rcol[i], #use less transparent colours
                                     border = subregion_details$rcol[i]
                         )
                         
                         #distinguish between immigrants and emigrants in each subregion
                         circos.rect(xleft = xlim[1], 
                                     ybottom = ylim[1], 
                                     xright = xlim[2] - rowSums(flow_matrix)[i], #i.e., total - emigrants
                                     ytop = ylim[1] + 0.3,
                                     col = "white", 
                                     border = "white"
                         ) 
                         
                         #add a white contour to separate the previous two rectangles
                         circos.rect(xleft = xlim[1], 
                                     ybottom = 0.3, 
                                     xright = xlim[2], 
                                     ytop = 0.32, 
                                     col = "white", 
                                     border = "white"
                         )
                         
                         #add axis to indicate migrant numbers
                         circos.axis(labels.cex = 1,
                                     lwd = 0.4,
                                     labels.niceFacing = TRUE,
                                     major.tick.length = 0.1,
                                     minor.ticks = 3
                        )
                         
                       }
)


#### Plot links ####

#transform flow_matrix into its long form and sort it 
flow_matrix_long = melt(flow_matrix,
                        value.name = "number"
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


subregion_details$sum1 <- colSums(flow_matrix) #number of immigrants
subregion_details$sum2 <- numeric(nrow(subregion_details)) #number of subregions

#not yet clear why this is needed, but it prevents links from being plotted outside of range
circos.par(track.margin = c(0, 0)) 

#plot links for each combination of regions
for(k in 1:nrow(flow_matrix_long)){ #for each row in the flow matrix
  i = match(flow_matrix_long$subregion_from[k],
            subregion_details$subregion) #get plotting details for subregion of origin
  j = match(flow_matrix_long$subregion_to[k],
            subregion_details$subregion) #get plotting details for destination subregion
  
  circos.link(sector.index1 = subregion_details$subregion[i], #need to identify indices to identify 
              point1 = c(subregion_details$sum1[i], 
                         subregion_details$sum1[i] + abs(flow_matrix[i, j])), #starting point of link
              
              sector.index2 = subregion_details$subregion[j], 
              point2=c(subregion_details$sum2[j], 
                       subregion_details$sum2[j] + abs(flow_matrix[i, j])), #endpoint of link
              
              border = subregion_details$lcol[i],
              col = subregion_details$lcol[i], #use the more transparent colour to increase visibility
  )
  
  #update sum1 and sum2 for use when plotting the next link
  subregion_details$sum1[i] = subregion_details$sum1[i] + abs(flow_matrix[i, j]) 
  subregion_details$sum2[j] = subregion_details$sum2[j] + abs(flow_matrix[i, j])
}

#save plot
dev.off() #close graphing device
