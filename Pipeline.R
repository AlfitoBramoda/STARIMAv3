setwd("C:\\Users\\hp\\Documents\\Baby\\STARIMAv3")

source("Reset.R")

source("program/00_Setup.R")

source("program/01_Load_Data.R")

source("program/02_Stationarity_Test.R")

source("program/03_Differencing.R")

source("program/04_Data_Centering.R")

source("program/05_Spatial_Weights.R")

source("program/06_Data_Split.R")

source("program/07_STACF_Analysis.R")

source("program/08_STPACF_Analysis.R")

source("program/09_Model_Structure.R")

source("program/10a_STARIMA_Estimation_Uniform.R")
source("program/10b_STARIMA_Estimation_Distance.R")
source("program/10c_STARIMA_Estimation_Correlation.R")

source("program/11a_Residual_Diagnostic_Uniform.R")
source("program/11b_Residual_Diagnostic_Distance.R")
source("program/11c_Residual_Diagnostic_Correlation.R")

source("program/12_Model_Selection.R")
source("program/13_Residual Analysis.R")
source("program/14_STARIMA_Forecasting_Per_Region.R")
