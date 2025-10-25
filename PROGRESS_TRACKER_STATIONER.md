# STARMA Forecasting Progress Tracker - STATIONER VERSION
## 📘 Mengikuti Metodologi Resmi Paket `starma` (Felix Cheysson)
## 🎯 Menggunakan Dataset Stasioner (Box-Cox + Differencing)

## 📊 Dataset Overview
- **Wilayah**: 5 (Barat, Selatan, Tengah, Timur, Utara)
- **Periode**: 2016-2024 (108 bulan) - **Data Bulanan**
- **Variabel**: Curah hujan stasioner (diff1 column)
- **Data Split**: Train (2016-2023) vs Test (2024 only)
- **Pembobotan**: 3 metode (Uniform, Distance-based, Correlation-based)
- **Metodologi**: Box-Jenkins diperluas Pfeifer & Deutsch (1980)
- **✅ SUDAH STASIONER**: Box-Cox transformation + First differencing

---

## Phase 1: Data Preparation

### ✅ 00_Setup.R
**Status**: COMPLETED  
**Expected Output**: Library loading messages, "Setup selesai" message
**What it does**: Menginstall dan memuat semua library yang diperlukan untuk STARMA forecasting termasuk paket `starma`, `spdep` untuk spatial weights, dan library pendukung lainnya.

### ✅ 01_Load_Stationer_Data.R  
**Status**: COMPLETED  
**Expected Output**: Stationer dataset structure, 108×5 matrix format
**What it does**: Membaca dataset stasioner dari folder `dataset/Stationer/` dan mengkonversi kolom `diff1` ke format matriks spatio-temporal (108 baris=bulan, 5 kolom=wilayah). Data sudah stasioner sehingga siap untuk tahap centering.
**✅ Results**: Matrix 108×5 created, no missing values, numeric format ready for STARMA

### ✅ 02_Data_Centering.R
**Status**: COMPLETED  
**Expected Output**: 
- Centered stationer data (mean=0, sd=1)
- Before/after statistics comparison
- Centered data summary
**What it does**: Menggunakan fungsi `stcenter()` dari paket starma untuk melakukan centering dan scaling data stasioner. Tahap ini WAJIB dilakukan sebelum identifikasi agar model tidak bias.
**✅ Results**: Global mean=0, Global SD=1, stcenter() applied correctly with global centering

### ✅ 03_Spatial_Weights.R
**Status**: COMPLETED  
**Expected Output**:
- 3 tipe spatial weight matrices (wlist format)
- Uniform, distance-based, correlation-based weights
- Matrix validation dan properties
**What it does**: Membuat spatial weight matrices menggunakan koordinat dan data centered. Menghasilkan 3 tipe bobot: uniform (equal weights), distance-based (inverse distance), dan correlation-based (cross-correlation). Semua matrix disimpan dalam format list sesuai requirement fungsi `starma()`.
**✅ Results**: 3 weight matrices created, very high correlations (0.96-0.99), all matrices validated

### ✅ 04_Data_Split.R
**Status**: COMPLETED  
**Expected Output**:
- Train data: 2016-2023 (96 obs per wilayah)
- Test data: 2024 only (12 obs per wilayah) 
- Split confirmation dan summary
**What it does**: Membagi data stasioner menjadi training (96 bulan) dan testing set (12 bulan). Training data digunakan untuk tahap identifikasi, estimasi, dan diagnostik model STARMA. Split 89-11 memberikan maximum training data dengan 1 tahun test yang representatif.
**✅ Results**: Perfect 89-11 split achieved, clean temporal separation, 5 files exported

---

## Phase 2: STARMA Identification (Box-Jenkins Step 1)

### ✅ 05_STACF_Analysis.R
**Status**: COMPLETED  
**Expected Output**:
- Space-Time ACF computation dengan 3 weight types
- 60 significant lags identified (|STACF| > 0.1)
- MA order recommendation: (12,1)
- 5 output files generated
**What it does**: Menggunakan fungsi `stacf()` pada training data untuk menghitung space-time autocorrelation function dengan 3 jenis spatial weights. Mengidentifikasi lag signifikan untuk menentukan orde Moving Average (MA) dalam model STARMA.
**✅ Results**: MA(12,1) identified - seasonal pattern with spatial lag 1, consistent across all weight types

### ✅ 06_STPACF_Analysis.R
**Status**: COMPLETED  
**Expected Output**:
- Space-Time PACF computation dengan 3 weight types
- 19 significant lags identified (|STPACF| > 0.1)
- AR order recommendation: (12,0)
- Complete STARMA order: STARMA(12,0; 12,1)
- 6 output files generated
**What it does**: Menggunakan fungsi `stpacf()` pada training data untuk menghitung space-time partial autocorrelation function dengan 3 jenis spatial weights. Mengidentifikasi lag signifikan untuk menentukan orde AutoRegressive (AR) dalam model STARMA.
**✅ Results**: AR(12,0) identified - temporal AR only, no spatial AR effects, consistent across all weight types

---

## Phase 3: STARMA Estimation (Box-Jenkins Step 2)

### ⏳ 07_Model_Structure.R
**Status**: PENDING  
**Expected Output**:
- AR dan MA mask matrices (0/1 structure)
- Parameter structure definition
- Model specification summary
**What it does**: Berdasarkan hasil STACF/STPACF, membuat matriks mask 0/1 untuk menentukan parameter AR dan MA mana yang akan diestimasi. Struktur ini menentukan kompleksitas model dan parameter yang signifikan.

### 🔄 UNIFORM WEIGHTING
### ⏳ 08a_STARMA_Estimation_Uniform.R
**Status**: PENDING  
**Expected Output**: Fitted STARMA model menggunakan `starma()` dengan uniform weights
**What it does**: Menggunakan fungsi `starma(data, wlist, ar, ma)` dengan uniform spatial weights pada data stasioner. Estimasi parameter menggunakan Kalman Filter untuk efisiensi komputasi.

### 🔄 DISTANCE WEIGHTING  
### ⏳ 08b_STARMA_Estimation_Distance.R
**Status**: PENDING  
**Expected Output**: Fitted STARMA model menggunakan `starma()` dengan distance weights

### 🔄 NEIGHBOR WEIGHTING
### ⏳ 08c_STARMA_Estimation_Neighbor.R
**Status**: PENDING  
**Expected Output**: Fitted STARMA model menggunakan `starma()` dengan neighbor weights

---

## Phase 4: STARMA Diagnostic (Box-Jenkins Step 3)

### ⏳ 09a_Residual_Diagnostic_Uniform.R
**Status**: PENDING  
**Expected Output**: 
- `stcor.test()` results untuk white noise test
- Residual ACF/PACF plots
- Model adequacy assessment
**What it does**: Menggunakan `stcor.test(residuals, wlist, fitdf)` untuk menguji apakah residual model bersifat white noise. Jika masih ada pola autokorelasi, model perlu diperbaiki dan proses diulang dari identifikasi.

### ⏳ 09b_Residual_Diagnostic_Distance.R
**Status**: PENDING  
**Expected Output**: Diagnostic tests untuk distance-based model

### ⏳ 09c_Residual_Diagnostic_Neighbor.R
**Status**: PENDING  
**Expected Output**: Diagnostic tests untuk neighbor-based model

### ⏳ 10_Model_Selection.R
**Status**: PENDING  
**Expected Output**:
- Model comparison (BIC, AIC, loglikelihood)
- Best model identification
- Parameter significance tests
**What it does**: Membandingkan ketiga model berdasarkan kriteria informasi dan hasil diagnostic. Memilih model terbaik untuk tahap forecasting.

---

## Phase 5: Forecasting & Evaluation

### ⏳ 11_STARMA_Forecasting.R
**Status**: PENDING  
**Expected Output**:
- Multi-step ahead forecasts (12 periods)
- Forecast confidence intervals
- Prediction summary
**What it does**: Menggunakan model STARMA terbaik untuk melakukan forecasting pada test data. Menghasilkan prediksi 12 bulan (tahun 2024) dengan confidence intervals.

### ⏳ 12_Forecast_Evaluation.R
**Status**: PENDING  
**Expected Output**:
- Accuracy metrics (RMSE, MAE, MAPE)
- Forecast vs actual comparison
- Performance assessment
**What it does**: Mengevaluasi akurasi forecasting dengan membandingkan prediksi vs data aktual pada test set. Menghitung berbagai metrik evaluasi.

### ⏳ 13_Final_Visualization.R
**Status**: PENDING  
**Expected Output**:
- Forecast plots dengan confidence bands
- Model summary visualization
- Final results presentation
**What it does**: Membuat visualisasi komprehensif hasil forecasting dan performa model STARMA.

---

## 🎯 Current Status
**Completed**: 7/16 files (44%)  
**Next Step**: 07_Model_Structure.R

## 📊 Progress by Phase
- **Phase 1 (Data Prep)**: 5/5 files (100%) ← **COMPLETED!**
- **Phase 2 (Identification)**: 2/2 files (100%) ← **COMPLETED!**
- **Phase 2 (Identification)**: 0/2 files (0%)
- **Phase 3 (Estimation)**: 0/4 files (0%)
- **Phase 4 (Diagnostic)**: 0/4 files (0%)
- **Phase 5 (Forecasting)**: 0/3 files (0%)

## 🎉 Recent Achievements
- ✅ **Setup Completed**: Environment siap dengan paket `starma` dan `spdep`
- ✅ **Data Loading Completed**: Matrix 108×5 berhasil dibuat dari dataset stasioner
- ✅ **Data Centering Completed**: Global centering dengan `stcenter()` berhasil (mean=0, SD=1)
- ✅ **Spatial Weights Completed**: 3 weight matrices (Uniform, Distance, Correlation)
- ✅ **High Correlations Detected**: Korelasi antar wilayah sangat tinggi (0.96-0.99)
- ✅ **Phase 1 COMPLETED**: Data preparation phase selesai 100%
- ✅ **Data Split Completed**: 96-12 split (89-11%) berhasil diimplementasikan
- ✅ **Clean Temporal Separation**: 2016-2023 training vs 2024 testing
- ✅ **All Phase 1 COMPLETED**: Ready untuk STARMA Identification phase
- ✅ **STACF Analysis COMPLETED**: MA(12,1) identified from seasonal patterns
- ✅ **Seasonal Pattern Detected**: Strong negative correlation at lag 12 (-0.36)
- ✅ **Spatial Consistency**: All 3 weight types show identical patterns
- ✅ **60 Significant Lags**: Comprehensive lag structure identified
- ✅ **STPACF Analysis COMPLETED**: AR(12,0) identified from direct temporal effects
- ✅ **Complete STARMA Order**: STARMA(12,0; 12,1) - seasonal temporal AR + spatial MA
- ✅ **Phase 2 COMPLETED**: Identification phase selesai 100%
- ✅ **Model Structure Identified**: Ready untuk parameter estimation
- 📁 **Project Structure**: Mengikuti metodologi Box-Jenkins yang diperluas
- 🔧 **Libraries Ready**: starma, spdep, dan library pendukung lainnya
- 📘 **Methodology**: Mengikuti prosedur resmi dari tutorial starma.pdf

## 📝 Notes - STATIONER VERSION
- **SKIP Stationarity Test**: Data sudah stasioner (diff1 column)
- **Data Resolution**: Bulanan (108 obs) lebih stabil untuk STARMA
- **Transformations Applied**: Box-Cox + First differencing
- **Ready for**: `stcenter()` → `stacf()` → `stpacf()` → `starma()`
- **Forecasting Horizon**: 12 bulan (2024 only)
- **Training Period**: 96 bulan (2016-2023)

## 🚀 Advantages of Stationer Version
- **Faster Processing**: Skip preprocessing steps
- **Better Stability**: Monthly data reduces noise
- **Professional Preprocessing**: Box-Cox + differencing applied
- **Ready for STARMA**: Direct implementation possible
- **Optimal Resolution**: 108 observations ideal for spatio-temporal modeling