################################################################################
# Original R code for the analysis in:
#
#  O'Brien E, et al. Short-Term association between sulfur dioxide and 
#    mortality: a multicountry analysis in 399 cities. Environmental Health
#    Perspectives. 2023;131(3):37002.
#  https://doi.org/10.1289/ehp11112
#
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/MCC-SO2
################################################################################

################################################################################
# LOAD THE PACKAGES
################################################################################

library(dlnm) ; library(mixmeta) ; library(splines)
library(tsModel) ; library(mgcv)
library(foreach) ; library(doParallel)
library(MASS) ; library(abind)
library(dplyr) ; library(data.table)
library(magrittr) ; library(scales)
library(Epi)
library(sf) ; library(ggplot2)
library(rnaturalearth) ; library(rnaturalearthdata)
