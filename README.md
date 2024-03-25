# A fast approach for analyzing spatio-temporal patterns in ischaemic heart disease mortality across US counties (1999-2021)
This repository contains the R code to reproduce the analysis presented in the paper entitled "A fast approach for analyzing spatio-temporal patterns in ischaemic heart disease mortality across US counties (1999-2021)" (Urdangarin et al., 2024).

## Table of contents

- [Data](#Data)
- [Imputed data](#Imputed-Data)
- [R code](#R-code)
- [References](#References)


# Data
### Ischaemic heart disease (IHD) mortality data in US counties during 1999-2021
The IHD annual mortality totals for each county in the US between 1999 and 2021, along with the corresponding county populations, has been obtained from the Centers for Disease Control (CDC) and Prevention’s Wide-ranging Online Data for Epidemiologic Research (WONDER) website [**https://wonder.cdc.gov/**](https://wonder.cdc.gov/). Here, we use the publicly available data provided by NCHS to CDC-WONDER with counts less than 10 suppressed for confidentiality reasons.

The datasets in [**IHD_data.Rdata**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Data/IHD_data.Rdata) include 3105 counties from all the US states except Alaska and Hawaii. Additionally, Puerto Rico, Samoa, the Dukes and Nantucket islands in Massachusetts, as well as San Juan Island in Washington, are excluded. Details regarding the data preprocessing procedures are provided in Supplementary Material A. We have considered the US administrative division into four geographic regions, namely, West, Midwest, South, and Northeast, encompassing the selected 48 states.

The [**IHD_data.Rdata**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Data/IHD_data.Rdata) file contains the following objects:
  - **_counts_**: contains the IHD mortality counts for each year. Suppressed counts are denoted as NAs. It is a dataframe where the first column labeled **_GEOID_**, serves as the identifier for counties, and the subsequent columns correspond to the counties for each year during 1999-2021.
  - **_pop_**: contains the population for each county in each year. It is a dataframe where the first column labeled **_GEOID_**, serves as the identifier for counties, and the subsequent columns correspond to the population for each year during 1999-2021.
  - **_carto_**: cartography of the 3105 counties of US.
    - **_STATEFP_**: state level FIPS codes
    - **_GEOID_**: identifiers of the counties
    - **_NAME_**: names of the counties
    - **_STATEFP2_**: state level FIPS codes of the partition chosen for "divide and conquer" approach
    - **_State.Name_**: names of the states
    - **_Region_**: 1=West, 2=Midwest, 3=South, 4=Northeast
   
 The folder [**Data**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/tree/main/Data) includes three .txt files with some additional information. 
 - [**Capitals.txt**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Data/Capitals.txt): includes information on the capitals of the 48 states in the US. For each state, the capital city is listed under the column **_Capital_** and the column **_County_** contains the county where the capital is situated. The last two columns refer to the FIPS (Federal Information Process System) code of the state and the corresponding GEOID (Geographic Identifiers) of the county.
 - [**FP_StateNames.txt**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Data/FP_StateNames.txt): comprises the FIPS code that corresponds to each state and the region where it is located each state.
 - [**GEOID_vs_CountyNames.txt**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Data/GEOID_vs_CountyNames.txt): comprises the GEOID of each county in the US.

 
# Imputed data
The [**IHD_imputed_data.Rdata**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/Imputed_data/IHD_imputed_data.Rdata) encompasses the counts dataset after imputing the missing values. Specifically, it contains the following objects:
  - **_counts_**: contains the IHD mortality counts for each year after imputing the missing data.
  - **_pop_**: contains the population for each county in each year.
  - **_exp_**: contains the expected cases for each county in each year computed using the overall rate in the US in the whole period of study (for details see page 6 of the paper)
  - **_carto_**: cartography of the 3105 counties of US.

# R code

The folder labeled [**R**](https://github.com/spatialstatisticsupna/IHD_ST_patterns/tree/main/R) contains the code to impute the missing counts and reproduce all the tables and figures of the paper.
- [Section2A_Figure1.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section2A_Figure1.R): R script to reproduce Figure 1 of the paper.
- [Section2B_Imputation_NA_counts.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section2B_Imputation_NA_counts.R): contains the R code to impute the missing counts.
- [Section2C_Descriptive_summary.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section2C_Descriptive_summary.R): R code to reproduce the descriptive analysis after imputing the missing counts. The code to reproduce Figure 2, Figure 3 and Table 1 is shown.
- [Section2D_ST_models_partitioned.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section2D_ST_models_partitioned.R): R script to fit the partitioned spatio-temporal models with ICAR/BYM2 spatial prior, RW1 temporal prior and Type I, II, III and IV space-time interactions. It includes the code to reproduce Table 2 of the paper as well.
- [Section2D_ST_models_global.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section2D_ST_models_global.R): R script to fit global spatio-temporal models with ICAR spatial prior, RW1 temporal prior and Type I, II, III and IV space-time interactions.
- [Section3_Figure4.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section3_Figure4.R): R script to reproduce Figure 4 of the paper.
- [Section3_Figure5.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section3_Figure5.R): R script to reproduce Figure 5 of the paper.
- [Section3_Figures6-7.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/Section3_Figures6-7.R): R script to reproduce Figure 6 and Figure 7 of the paper.
- [SectionB_Supplementary.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/SectionB_Supplementary.R):  R code to reproduce Figure B.1 and Table B.1 in the Supplementary material.
- [SectionC_Supplementary.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/SectionC_Supplementary.R):  R code to reproduce Table C.1 in the Supplementary material.
- [SectionD_Supplementary.R](https://github.com/spatialstatisticsupna/IHD_ST_patterns/blob/main/R/SectionD_Supplementary.R):  R code to reproduce Table D.1, D.2 and D.3 in the Supplementary material.
  
Computations were run using R-4.2.1, R-INLA version 22.12.16 (dated 2022-12-23) and bigDM version 0.5.3.


# Acknowledgements
This work has been supported by Project PID2020-113125RB-I00/ MCIN/ AEI/ 10.13039/501100011033 and by Project UNEDPAM/PI/PR24/05A.
![image](https://github.com/spatialstatisticsupna/Comparing-R-INLA-and-NIMBLE/blob/main/micin-aei.jpg)

# References
Urdangarin, A., Goicoa, T. , Congdon, P. and Ugarte, M.D. (2024). A fast approach for analyzing spatio-temporal patterns in ischaemic heart disease mortality across US counties (1999-2021).
