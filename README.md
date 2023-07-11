# Analysis of eDNA concentrations of salmonids in Grense Jakobselv
This repository contains data and R-scripts for analysing the eDNA concentrations of the four salmonid species pink salmon, Atlantic salmon, trout and Arctic char.
The eDNA concentrations were quantified by qPCR with species-specific assays, and the data were exported as .txt-files from Bio-Rad CFX Maestro 2.3 (5.3.022.1030).

The exported qPCR-data can be found in "pcrd_files".
The meta-data for the field stations, and processed files with eDNA-concentrations can be found in "data".
The results from the hydrodynamic modeling with an eDNA reaction-transport module can be found in "Data_for_figures".

anadromous_grense_jakobs_river.Rmd contains the analysis and plotting of the qPCR data.
hydrodyn_modeling_plotting.R contains code for plotting the data obtained by modeling.
salmonid_parameters.R contains code for plotting the parameter values included in the modeling.
