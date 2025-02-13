# A multi-country analysis of the short-term mortality risks of sulphur dioxide

An example of applications of the two-stage time series design for the analysis of short-term effects of air pollution using a multi-country multi-city dataset

------------------------------------------------------------------------

This repository stores the updated R code and data to reproduce the analyis presented in the article:

O'Brien E, et al. Short-Term association between sulfur dioxide and mortality: a multicountry analysis in 399 cities. *Environmental Health Perspectives*. 2023;131(3):37002. DOI: doi.org/10.1289/ehp11112. [[freely available here](http://www.ag-myresearch.com/2023_obrien_ehp.html)]

### Data

The original analysis is based on datasets separately collected from 399 cities within 23 countries between 1980 and 2018. Each dataset includes daily time series observations of mortality counts, air pollutants, and weather variables, in addition to city-specific metadata. The data was collected within the Multi-Country Multi-City (MCC) Collaborative Research Network ([mccstudy.lshtm.ac.uk/](https://mccstudy.lshtm.ac.uk/)) and cannot be made publicly available. Therefore, a simplified simulated version is provided here, together with a revised code that illustrates the various steps of the analysis.

The simulated data are included in a single file simdata.csv stored in the folder *data*. This includes the time series datasets for 40 cities in 8 countries stacked together. The labels of the variables are easily interpretable.

**Note**: while the data are simulated in a way to represent real observations and the results reproduced by the code are mostly similar to the original analysis, the estimates obtained must not be interpreted as actual evidence on the association of interest. The code and simulated data are provided only for illustrative purposes.

### R code

The R code for replicating the analysis with the simulated data is provided in the folder *Rcode*. The code is separated in a series of scripts to be run sequentially. The code includes only minor revisions compared with the original version, mostly due to adapting it to the different data structure. The original code, which cannot be run without the original data, is also provided for completeness in the sub-folder *origcode*.
