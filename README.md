# testis_GCcysts

This repository contains data files and R scripts to the manuscript titled 'Intercellular bridges connect synchronous and asynchronous germ cells in human second trimester, prepubertal and adult testes.'

The purpose of the repository is to share raw data on germ cell (GC) cysts characterized in the human testis by manual 3D image analysis in Imaris software. 

Each subfolder indicates the Figure in the manuscript for which the script was used. It contains Excel files with data collected through Imaris and R scripts to generate plots as in the paper. Subfolder Plots contains the resulting plots in .png and .pdf used for the manuscript. 

* Fig1G: From TEX14/POU5F1/DDX4 stainings, R file plots pie charts on GC types and Bridge categories for donor of 17WPF and 18WPF (week of gestation (WG)19 and WG20). The pie charts in Fig. 2C and 3D are generated with a very similar script (not included). 
* Fig1H: From TEX14/POU5F1/DDX4 stainings, R file plots scatter plot of cyst size and longest length for donor of 17WPF and 18WPF, with a red bar indicating the mean. 
* Fig4A-D: From TEX14(and KIF23)/MAGEA3/DDX4 stainings, R file plots pie charts with synchronous/asynchronous bridges for each donor as in Fig. 4A. In additiona, cyst size, longest length and branch index are calculated and plotted in scatter plots (absolute values) and stacked bar charts (binned data) per donor, as in Fig. 4B-D. 
* Fig4E-G: From TEX14(and KIF23)/MAGEA3/DDX4 stainings, 2 R files plot pie charts in Fig. 4E using 2 Excel files on all cysts (20250318_7B.1_TEX_quant_tidy_MAGE.xlsx) and branching cysts (20250321_7B.1_TEX_quant_tidy_MAGE_branches.xlsx). Scatter plots and stacked bar charts present longest lengths of cysts with branches and relative branch position. 