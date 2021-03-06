# Doctors' Migration in 2018

This project is my assignment for the PSY6422 Data Management and Visualizaiton module. 

It presents a circular flow diagram illustrating the migration flow of foreingly trained doctors in 2018 in OECD countries, implemented using the circlize package in R. The files above include:

* data-processed => data following my cleaning steps along with a codebook
* data-raw => data before any cleaning along with a codebook detailing its origins and meaning
* figs => the main data visualization saved as .png (please note the visualization is not presented in RStudio, but only saved automatically in this folder)
* notes => includes my reflection on my time on this project and the PSY6422 module more broadly
* renv => this is a local library storing all the packages that I used in the project along with their dependencies
* scripts => three separate scripts for data processing, intermediary building of visualization matrices needed for the final plot, and data visualization; if impatient, run the data-visualization script without paying attention to the first two - but open them in the RProject associated with the file, not independently, otherwise my code won't be able to find paths to relevant files
* report.Rmd => a presentation of the process behind this data visualization, which can also be viewed using GitHub pages here: https://protor23.github.io/doctors-migration-2018/

#NB
When you download/clone my repository, make sure to open scripts after opening the R Project - you will be prompted to load the packages in the renv folder, so please follow the instructions presented to you in the console. 
