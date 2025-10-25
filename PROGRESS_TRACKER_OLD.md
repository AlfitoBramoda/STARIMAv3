# STARMA Forecasting Progress Tracker

## 📊 Dataset Overview
- **Wilayah**: 5 (Barat, Selatan, Tengah, Timur, Utara)
- **Periode**: 2015-2024 (120 bulan)
- **Variabel**: Curah hujan (mm/day)
- **Data Split**: Train (2015-2022) vs Test (2023-2024)
- **Pembobotan**: 3 metode (Uniform, Inverse Distance, Cross-Correlation)

---

## Phase 1: Data Preparation

### ✅ 00_Setup.R
**Status**: COMPLETED  
**Expected Output**: Library loading messages, "Setup selesai" message
**What it does**: Menginstall dan memuat semua library yang diperlukan untuk STARMA forecasting (readxl, forecast, tseries, dplyr, ggplot2, Matrix, corrplot, gridExtra, lubridate, tidyr). File ini memastikan environment R siap untuk analisis dan membuat folder output (results/, plots/, models/).

### ⏳ 01_Load_Data.R  
**Status**: PENDING  
**Expected Output**: Dataset structure, dimensions, column names, head/tail data
**What it does**: Membaca dataset curah hujan dari file Excel, menampilkan struktur data, dimensi (600 baris x 5 kolom), dan melakukan pengecekan dasar seperti missing values. File ini memuat data mentah ke dalam R environment.

### ⏳ 02_Data_Exploration.R
**Status**: PENDING  
**Expected Output**: 
- Statistik per wilayah (mean, min, max, zero values, koordinat)
- 5 plot time series
- Correlation matrix 5x5
- Pola musiman (rata-rata per bulan)
**What it does**: Melakukan Exploratory Data Analysis (EDA) pada data curah hujan 5 wilayah. Menganalisis karakteristik statistik setiap wilayah, membuat visualisasi time series, menghitung korelasi spasial antar wilayah, dan mengidentifikasi pola musiman curah hujan.

### ⏳ 03_Data_Transform.R
**Status**: PENDING  
**Expected Output**:
- Spatio-temporal array/matrix format
- Zero handling strategy applied
- Transformed data summary
**What it does**: Mengkonversi data dari format long ke spatio-temporal matrix (120 waktu x 5 wilayah). Menangani zero values dengan menambahkan konstanta kecil (epsilon=0.001). Membuat time series objects untuk setiap wilayah dan menyimpan hasil transformasi dalam file RData untuk digunakan tahap selanjutnya.

### ⏳ 04a_Spatial_Matrix_Uniform.R
**Status**: PENDING  
**Expected Output**:
- 5x5 uniform weight matrix (1/n)
- Equal weights for all neighbors
**What it does**: Membuat uniform spatial weight matrix berukuran 5x5 dimana setiap wilayah memiliki bobot yang sama (0.25) terhadap semua wilayah tetangga. Matrix ini menggunakan konsep equal influence dimana tidak ada wilayah yang lebih berpengaruh dari yang lain. Diagonal matrix = 0 (no self-influence) dan setiap baris sum = 1.0 (row-normalized). Hasil disimpan dalam uniform_weight_matrix.RData untuk digunakan dalam model STARMA.

### ⏳ 04b_Spatial_Matrix_Distance.R
**Status**: PENDING  
**Expected Output**:
- Distance matrix from coordinates
- 5x5 inverse distance weight matrix
- Row-normalized weights

### ⏳ 04c_Spatial_Matrix_Correlation.R
**Status**: PENDING  
**Expected Output**:
- Cross-correlation matrix
- 5x5 correlation-based weight matrix
- Normalized correlation weights

---

## Phase 2: Data Analysis

### ⏳ 05_Stationarity_Test.R
**Status**: PENDING  
**Expected Output**:
- ADF test results per wilayah
- KPSS test results
- Differencing recommendations
**What it does**: Melakukan uji stasioneritas menggunakan Augmented Dickey-Fuller (ADF) dan Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test untuk setiap wilayah. ADF test menguji H0: non-stationary, sedangkan KPSS test menguji H0: stationary. Jika series non-stationary, otomatis melakukan first differencing. Hasil berupa summary table dan time series yang sudah di-adjust disimpan dalam stationarity_results.RData.

### ⏳ 06_Seasonal_Analysis.R
**Status**: PENDING  
**Expected Output**:
- Seasonal decomposition plots
- Seasonality strength metrics
- Trend analysis

### ⏳ 07_Data_Split.R
**Status**: PENDING  
**Expected Output**:
- Train data: 2015-2022 (96 obs per wilayah)
- Test data: 2023-2024 (24 obs per wilayah)
- Split confirmation

---

## Phase 3: Model Development

### 🔄 UNIFORM WEIGHTING
### ⏳ 08a_Model_Selection_Uniform.R
**Status**: PENDING  
**Expected Output**: Grid search results, optimal order for uniform weights

### ⏳ 09a_Model_Fitting_Uniform.R
**Status**: PENDING  
**Expected Output**: Fitted STARMA model with uniform weights

### ⏳ 10a_Model_Diagnostic_Uniform.R
**Status**: PENDING  
**Expected Output**: Residual analysis for uniform model

### 🔄 INVERSE DISTANCE WEIGHTING
### ⏳ 08b_Model_Selection_Distance.R
**Status**: PENDING  
**Expected Output**: Grid search results, optimal order for distance weights

### ⏳ 09b_Model_Fitting_Distance.R
**Status**: PENDING  
**Expected Output**: Fitted STARMA model with distance weights

### ⏳ 10b_Model_Diagnostic_Distance.R
**Status**: PENDING  
**Expected Output**: Residual analysis for distance model

### 🔄 CROSS-CORRELATION WEIGHTING
### ⏳ 08c_Model_Selection_Correlation.R
**Status**: PENDING  
**Expected Output**: Grid search results, optimal order for correlation weights

### ⏳ 09c_Model_Fitting_Correlation.R
**Status**: PENDING  
**Expected Output**: Fitted STARMA model with correlation weights

### ⏳ 10c_Model_Diagnostic_Correlation.R
**Status**: PENDING  
**Expected Output**: Residual analysis for correlation model

---

## Phase 4: Forecasting & Evaluation

### 🔄 UNIFORM WEIGHTING
### ⏳ 11a_Forecasting_Uniform.R
**Status**: PENDING  
**Expected Output**: 24-step forecasts with uniform weights

### ⏳ 12a_Model_Evaluation_Uniform.R
**Status**: PENDING  
**Expected Output**: Accuracy metrics for uniform model

### ⏳ 13a_Visualization_Uniform.R
**Status**: PENDING  
**Expected Output**: Forecast plots for uniform model

### ⏳ 14a_Export_Results_Uniform.R
**Status**: PENDING  
**Expected Output**: Saved uniform model and results

### 🔄 INVERSE DISTANCE WEIGHTING
### ⏳ 11b_Forecasting_Distance.R
**Status**: PENDING  
**Expected Output**: 24-step forecasts with distance weights

### ⏳ 12b_Model_Evaluation_Distance.R
**Status**: PENDING  
**Expected Output**: Accuracy metrics for distance model

### ⏳ 13b_Visualization_Distance.R
**Status**: PENDING  
**Expected Output**: Forecast plots for distance model

### ⏳ 14b_Export_Results_Distance.R
**Status**: PENDING  
**Expected Output**: Saved distance model and results

### 🔄 CROSS-CORRELATION WEIGHTING
### ⏳ 11c_Forecasting_Correlation.R
**Status**: PENDING  
**Expected Output**: 24-step forecasts with correlation weights

### ⏳ 12c_Model_Evaluation_Correlation.R
**Status**: PENDING  
**Expected Output**: Accuracy metrics for correlation model

### ⏳ 13c_Visualization_Correlation.R
**Status**: PENDING  
**Expected Output**: Forecast plots for correlation model

### ⏳ 14c_Export_Results_Correlation.R
**Status**: PENDING  
**Expected Output**: Saved correlation model and results

---

## Phase 5: Comparison & Final Results

### ⏳ 15_Comparison_Analysis.R
**Status**: PENDING  
**Expected Output**:
- Performance comparison table (3 methods)
- Best method identification
- Statistical significance tests

### ⏳ 16_Final_Visualization.R
**Status**: PENDING  
**Expected Output**:
- Side-by-side forecast comparison
- Performance metrics visualization
- Final summary plots

---

## 🎯 Current Status
**Completed**: 1/29 files (3%)  
**Next Step**: 01_Load_Data.R

## 📊 Progress by Phase
- **Phase 1**: 1/6 files (17%) ← **Current Phase**
- **Phase 2**: 0/3 files (0%)
- **Phase 3**: 0/9 files (0%)
- **Phase 4**: 0/12 files (0%)
- **Phase 5**: 0/2 files (0%)

## 🎉 Recent Achievements
- ✅ **Setup Completed**: Environment siap dengan semua library dan folder output
- 📁 **Project Structure**: Folder program/, dataset/, results/, plots/, models/ tersedia
- 🔧 **Libraries Ready**: readxl, forecast, tseries, dplyr, ggplot2, Matrix, corrplot, gridExtra, lubridate, tidyr

## 📝 Notes
- Gunakan format: `Rscript filename.R` untuk menjalankan
- Cek expected output untuk debugging
- Update status setelah selesai setiap file