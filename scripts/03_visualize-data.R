#### Packages used ####
#install packages
install.packages("circlize")

#load libraries
library(circlize)

#### Initialize circular migration flow diagram ####

#this code is an adaptation of the instructions of Sander et al. (2014)

circos.clear() #reset circular layout parameters

par(mar = rep(0 , 4)) #set margins to 0 
circos.par(cell.padding = c(0, 0, 0, 0), 
           track.margin = c(0, 0.1), 
           start.degree = 90, #start plotting at 12 o'clock
           gap.degree = 2, #gap between circle sectors
           points.overflow.warning = FALSE, 
           canvas.xlim = c(-1.5, 1.5), 
           canvas.ylim = c(-1.5, 1.5) 
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
                                     y = 2, #distance from circle
                                     labels = name, #name of subregion
                                     facing = "clockwise", 
                                     niceFacing = TRUE,
                                     cex = 0.5 #scale text
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
                                     xright = xlim[2] - rowSums(flow_matrix)[i], #i.e., total - immigrants
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
                      
                       }
)


