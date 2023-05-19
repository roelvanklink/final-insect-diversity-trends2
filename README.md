# final-insect-diversity-trends2
reproducable code for review for the manuscript called 'Widespread declines of dominant insect species are changing the structure of insect assemblages '

files and their functions:

In order of use:

    'load and check data 20221208.R' This will load all the original datafiles, clean them and arrange them so, as to produce the datafram on which all main analyses are done. The raw data files are only partially provided on KNB, but all results are available in the file "completedata202021pure.rds".
    Better is to use the files 'Supplementary Data 1.rds' for the biodiversity metrics and 'Supplementary Data 2.rds' for the population analyses 
    as supplied as suppementary data files. These are cleaned and ready for use in models. 

Functions:

    FUNCTION Calculate metrics.R # this script calculates all biodiversity metrics, including the density kernels, used in the manuscript and more.
    FUNCTION effort rarefaction.R # this script is used to rarefy communities based on a species X time matrix. Originally written by Thore Engel, adapted by Roel van Klink
    FUNCTION 'function_cleaning_taxon_names.R' # this function probabilistically allocates unidentified individuals to the species of its genus found in the time series Written by Alban Sagouis
    FUNCTION 'FUNCTION add zeroes.R' # this script adds 0 counts for all taxa that were not observed in a given year, but were present in other years

Models: 

    'MODEL all biodiversity metrics.R' this is an array script for use on a high performance cluster. it runs all INLA models in the main text (and a few more). Approximate memmory use: 200gb
    'MODEL Continental breakdown' runs the models with continent as additional fixed effect
    'MODEL all Sensitivity Inla Models' This script runs the models needed to produce the supplementary / extended data graphs. It requires a number of additional input dataframes that are created in the script XXXXXXXX
    'MODEL inla Population Models' rund all population models, including those to estimate the expected RtM effect

Making the graphs: 

    'GRAPHS Fig 1 conceptual figure.R' This makes teh graphs in the conceptual figure of Box 1
    'graphs models main text and part of Extended Data.R' This script takes the output files of the models and plots them to produce the graphs in the main text and
    a few Extended Data Figures
    'GRAPHS Sensitivity analyses' Makes the files and plots the graphs of the sensitivity analyses, as available in the Extended Data 
    

Dependencies: map_preparation.R # a script for preparing the map properties for plotting. Not supplied. 

other files: 
Greenland data processing rarefaction 2021.R This file contains the processing code for the Greenland data, which may not be shared in a derived form.
