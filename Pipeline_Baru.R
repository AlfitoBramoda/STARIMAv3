setwd("D:\\TUGAS AKHIR\\STARIMAv3")

source("Reset.R")

source("Baru/00_Setup.R")

source("Baru/01_Load Data.R")

source("Baru/02_Data Split.R")

source("Baru/03_Stationary_Test.R")

source("Baru/04_Boxcox Transformasi.R")

source("Baru/05_Differencing.R")

source("Baru/06_Data Centering.R")

source("Baru/07_Spatial Weight.R")

source("Baru/08_STACF Analysis.R")

source("Baru/09_STPACF Analysis.R")

source("Baru/10_Model Structure.R")

source("Baru/11a_STARIMA_Estimation_Uniform.R")
source("Baru/11b_STARIMA_Estimation_Distance.R")
source("Baru/11c_STARIMA_Estimation_Correlation.R")

source("Baru/12_STARIMA_Evaluation_Per_Region.R")

source("Baru/13a_Residual_Diagnostic_Uniform.R")
source("Baru/13b_Residual_Diagnostic_Distance.R")
source("Baru/13c_Residual_Diagnostic_Correlation.R")

source("Baru/14_Model_Selection.R")
source("Baru/15_Residual Analysis.R")
source("Baru/16_STARIMA_Forecasting_Per_Region.R")