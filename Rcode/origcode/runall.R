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

# SOURCE ALL THE SCRIPTS
source('00.pkg.R')
source('01.prepdata.R')
source('02.param.R')
source('03.firststage.R') 
source('04.secondstage.R')
source('05.attr.R')
source('06.nonlin.R')
source('07.complexlag.R') 
source('08.byperiod.R')
source('09.bipollutant.R')
source('10.tables.R')
source('11.graphs.R')
source('12.addres.R')