# A fast approach for analyzing spatio-temporal patterns in ischaemic heart disease mortality across US counties (1999-2021)
This repository contains the R code to reproduce the analysis presented in the paper entitled "A fast approach for analyzing spatio-temporal patterns in ischaemic heart disease mortality across US counties (1999-2021)" (Urdangarin et al., 2024).

## Table of contents

- [Data](#Data)
- [Imputed data](#SimulatedData)
- [R code](#R-code)
- [References](#References)


# Data
### Ischaemic heart disease (IHD) mortality data in US counties during 1999-2021
The IHD annual mortality totals for each county in the US between 1999 and 2021, along with the corresponding county populations, has been obtained from the Centers for Disease Control (CDC) and Prevention’s Wide-ranging Online Data for Epidemiologic Research (WONDER) website [**https://wonder.cdc.gov/**](https://wonder.cdc.gov/). Here, we use the publicly available data provided by NCHS to CDC-WONDER with counts less than 10 suppressed by confidentiality reasons.

The [**IHD_data.Rdata**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Data/IHD_data.Rdata) file contains the following objects:
  - **_counts_**: contains the IHD mortality counts for each year. Supressed counts are denoted as NAs. It is a dataframe where the first column labeled **_GEOID_**, serves as the identifier for counties, and the subsequent columns correspond to the counties for each year during 1999-2021.
  - **_pop_**: contains the population for each county in each year. It is a dataframe where the first column labeled **_GEOID_**, serves as the identifier for counties, and the subsequent columns correspond to the population for each year during 1999-2021.
  - **_carto_**: cartography of the 3105 counties of US.
    - **_STATEFP_**: state level FIPS codes
    - **_GEOID_**: identifiers of the counties
    - **_NAME_**: names of the counties
    - **_STATEFP2_**: state level FIPS codes of the partition chosen for "divide and conquer" approach
    - **_State.Name_**: names of the states
    - **_Region_**: 1=West, 2=Midwest, 3=South, 4=Northeast

The data set includes 3105 counties from all the US states except Alaska and Hawaii. Additionally, Puerto Rico, Samoa, the Dukes and Nantucket islands in Massachusetts, as well as San Juan Island in Washington, are excluded. Details regarding the data preprocessing procedures are provided
in Supplementary Material A. The US is administratively divided into four geographic regions,
namely, West, Midwest, South, and Northeast, encompassing the selected 48 states. Additionally,
following Kulshreshtha et al. (2014), we will classify counties into three population levels: large
metropolitan areas or metros, medium metros, and non metros or rural areas.

# Imputed data

# R code

The folder labeled [**R**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/tree/main/R) contains the code to imputed the missing counts and reproduce all the tables and figures of the paper. 


  



Computations were run using R-4.2.1, INLA version 22.12.16 (dated 2022-12-23).

# Acknowledgements
This work has been supported by Project PID2020-113125RB-I00/ MCIN/ AEI/ 10.13039/501100011033.

![image](https://github.com/spatialstatisticsupna/Comparing-R-INLA-and-NIMBLE/blob/main/micin-aei.jpg)
