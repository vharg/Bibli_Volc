# Bibli_Volc
This repository contains R scripts and raw data for bibliometric analysis of volcanology literature sourced from the Web of Science and Scopus. The scripts can be used to extract the names of volcanoes from article metadata and the countries these volcanoes are located in can then be compared with the affiliation addresses of the authors. This code has been used to conduct a study which is in the process of being submitted to Volcanica for publication.

Unfortunately we were only given permission to share article metadata from Scopus (and not from the Web of Science which was our main source of data) meaning <1% of the data we downloaded can be uploaded here for the purpose of sharing the code, workflow and results of this study. 

For anyone who has access to to the Web of Science and would like to perform a similar analysis to that conducted in this study. We have provided a [word document](https://github.com/vharg/Bibli_Volc/blob/main/S8_WoSDataDownloadInstructions.docx) outlining a step by step process for how to integrate new WoS data into the existing set of scripts here.


### If you wish to test out the scripts shared here (even with the small amount of Scopus data available) you should:

1. Install R Studio from [here](https://www.rstudio.com/products/rstudio/download/)
2. Download this repository to your computer
3. Click on the "Bibli_volc.Rproj" Rproject file to open the project in R Studio (not base R)
4. Click on the "install_required_packages.R" R script and run that to install all packages required for the remainder of the analysis
5. Navigate to the "processing_scripts" folder and open the first script "Step0.1-binding_raw_data.R"
6. Run this script then proceed to run the subsequent scripts in this folder in the order they are numbered
7. This will produce summary results and figures that can be supplemented with additional article metadata to reproduce or update our original study


### Then you can make figures like this one

![alt text](https://github.com/vharg/Bibli_Volc/blob/main/figs/inclusion_leadership_bars.png)

### Or map the data out like this

![alt text](https://github.com/vharg/Bibli_Volc/blob/main/pie_map_plot.png?raw=true)

Boom 👍 🌋
