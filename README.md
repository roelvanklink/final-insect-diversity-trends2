# final-insect-diversity-trends2
reproducable code for of 'Disproportionate declines of formerly abundant species underlie insect loss'

files and their functions:

In order of use:

    'load and check data 20221208.R' This will load all the original datafiles, clean them and arrange them so, as to produce the datafram on which all main analyses are done. The raw data files are only partially provided on KNB, but the end result of this code is available in the file Supplementary Data 1 and Supplementary Data 2.
     
Data:

    Supplementary Data 1 # this file contains all biodiversity metrics 
    Supplementary Data 2 # this file contains all population counts 
    Supplementary Data 3 # this file contains the population level random effect estimates from the population analysis. 
    Note: some datasets in all these files have been removed because derived products cannot be shared, and some species names have been obfuscated. 
     

Functions:

    FUNCTION Calculate metrics.R # this script calculates all biodiversity metrics, including the density kernels, used in the manuscript and more.
    FUNCTION effort rarefaction.R # this script is used to rarefy communities based on a species X time matrix. Originally written by Thore Engel, adapted by Roel van Klink
    FUNCTION 'function_cleaning_taxon_names.R' # this function probabilistically allocates unidentified individuals to the species of its genus found in the time series Written by Alban Sagouis
    FUNCTION 'FUNCTION add zeroes.R' # this script adds 0 counts for all taxa that were not observed in a given year, but were present in other years

Models: 
The models were run on a High Performance Cluster, using an array job. The files of the models run are supplied as 'metrics.csv' (biodiversity models)  'popModels.csv' (population models), 'sensitivityanalyses.csv' (for the majority of sensitivity analyses), and 'sensitivityContinent.csv'   

    'MODEL all biodiversity metrics.R' this is an array script for use on a high performance cluster. it runs all INLA models in the main text (and a few more). Approximate memmory use: 200gb
    'MODEL Continental breakdown' runs the models with continent as additional fixed effect
    'MODEL all Sensitivity Inla Models' This script runs the models needed to produce the supplementary / extended data graphs. It requires a number of additional input dataframes that are created in the script 'GRAPHS Sensitivity analyses.R'
    'MODEL inla Population Models' rund all population models, including those to estimate the expected RtM effect

Making the graphs: 

    'GRAPHS Fig 1 conceptual figure.R' This makes the graphs in the conceptual figure of Box 1
    'GRAPHS models main text and part of Extended Data.R' This script takes the output files of the models and plots them to produce the graphs in the main text and
    a few Extended Data Figures
    'GRAPHS Sensitivity analyses' Makes the files and plots the graphs of the sensitivity analyses, as available in the Extended Data 
    

Dependencies: map_preparation.R # a script for preparing the map properties for plotting. Not supplied. This code will be defunct with the retirement of several dependent packages

other files: 
Greenland data processing rarefaction 2021.R This file contains the processing code for the Greenland data, which may not be shared in a derived form.
