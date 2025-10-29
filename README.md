# ALUR PIPELINE STARIMA FORECASTING
## Rangkuman Lengkap untuk Presentasi Dosen

---

## 🎯 **TUJUAN PENELITIAN**
Membandingkan efektivitas **3 jenis pembobotan spasial** (Uniform, Distance, Correlation) dalam model **STARIMA(3,0,1,3)** untuk prediksi curah hujan di 5 wilayah Indonesia.

---

## 📊 **ALUR PIPELINE LENGKAP**

### **FASE 1: PERSIAPAN DATA** 
#### File 01-05: Data Preprocessing
```
01_Load_Data.R → 02_Stationarity_Test.R → 03_Differencing.R → 04_Data_Centering.R → 05_Spatial_Weights.R
```

**Output**: 
- Data curah hujan bersih (2015-2024)
- 3 jenis spatial weights matrix
- Training data (2015-2023) & Test data (2024)

---

### **FASE 2: MODEL IDENTIFICATION**
#### File 07-09: Identifikasi Model
```
07_STACF_Analysis.R → 08_STPACF_Analysis.R → 09_Model_Structure.R
```

**Output**: 
- Model STARIMA(3,0,1,3) terpilih
- AR mask dan MA mask untuk estimation

---

## 📋 **PENJELASAN TERPERINCI FILE 09-15**

### **FILE 09: Model Structure (Persiapan Arsitektur Model)** ⭐

#### **Tujuan:**
Membuat struktur model STARIMA dengan menentukan AR mask dan MA mask untuk ketiga jenis spatial weights.

#### **Yang Dilakukan:**
1. **Konfigurasi Parameter:**
   - **p_order = 3** (AR order)
   - **q_order = 3** (MA order) 
   - **d_order = 0, D_order = 1** (differencing)
   - **max_spatial_lag = 2**

2. **Pembuatan Mask Matrices:**
   - **AR Mask:** Matrix 3×3 untuk menentukan parameter AR mana yang diestimasi
   - **MA Mask:** Matrix 3×3 untuk menentukan parameter MA mana yang diestimasi
   - Setiap spatial weight (uniform, distance, correlation) mendapat mask yang sama

3. **Output:**
   - **Total parameter per model:** 18 parameter (9 AR + 9 MA)
   - **Degrees of freedom:** 96 - 18 = 78
   - **Complexity level assessment**

---

### **FILE 10a-10c: Parameter Estimation (Estimasi Koefisien Model)** ⭐⭐

#### **Tujuan:**
Mengestimasi parameter STARIMA(3,0,1,3) menggunakan Maximum Likelihood Estimation via Kalman Filter.

#### **Yang Dilakukan:**

##### **10a - Uniform Weights:**
1. **Persiapan Spatial Weights:**
   - **Spatial lag 0:** Identity matrix (efek dalam wilayah)
   - **Spatial lag 1:** Uniform matrix (tetangga langsung)
   - **Spatial lag 2:** Uniform² matrix (tetangga tingkat 2)

2. **Estimasi Model:**
   - Menggunakan fungsi `starma()` dari package starma
   - **Input:** training data (96 obs), wlist, AR mask, MA mask
   - **Output:** 18 koefisien terestimasi

3. **Hasil:**
   - Log-likelihood, AIC, BIC
   - Coefficient table dengan p-values
   - Residual analysis

##### **10b - Distance Weights & 10c - Correlation Weights:**
Proses identik dengan 10a, hanya berbeda pada spatial weights matrix yang digunakan.

#### **Metodologi Estimasi:**
- **Kalman Filter:** Untuk menangani missing values dan temporal dependencies
- **Maximum Likelihood:** Optimasi parameter untuk memaksimalkan likelihood
- **Spatial Lag Structure:** Mengakomodasi dependensi spasial hingga lag 2

---

### **FILE 11a-11c: Residual Diagnostic (Validasi Model)** ⭐

#### **Tujuan:**
Memvalidasi kecukupan model melalui analisis residual dan uji diagnostik.

#### **Yang Dilakukan:**

1. **Residual Analysis:**
   - Distribusi residual (normalitas)
   - Time series plot residual
   - Statistik deskriptif (mean, std, skewness, kurtosis)

2. **White Noise Tests:**
   - **Ljung-Box Test:** Uji autokorelasi temporal per region
   - **Moran's I Test:** Uji autokorelasi spasial pada residual

3. **ACF/PACF Residual:**
   - Cek apakah residual masih mengandung pola temporal
   - Validasi kecukupan order AR dan MA

#### **Interpretasi Hasil:**
- **White Noise:** Residual harus random (tidak ada pola)
- **Spatial Independence:** Tidak ada korelasi spasial tersisa
- **Normality:** Residual mendekati distribusi normal

---

### **FILE 12: Model Selection (Pemilihan Model Terbaik)** ⭐⭐

#### **Tujuan:**
Membandingkan ketiga model STARIMA dan memilih yang terbaik berdasarkan kriteria statistik.

#### **Yang Dilakukan:**

1. **Information Criteria Comparison:**
   - **AIC (Akaike Information Criterion):** Mengukur trade-off antara goodness of fit dan kompleksitas
   - **BIC (Bayesian Information Criterion):** Lebih ketat dalam penalti kompleksitas
   - **Log-likelihood:** Mengukur seberapa baik model menjelaskan data

2. **Parameter Consistency Analysis:**
   - **Coefficient of Variation (CV):** Mengukur variabilitas parameter antar spatial weights
   - **Significance Agreement:** Konsistensi signifikansi parameter
   - **Stability Assessment:** Robustness model terhadap perubahan spatial weights

3. **Ranking System:**
   - Model diranking berdasarkan AIC, BIC, dan log-likelihood
   - Model terbaik: yang memiliki AIC/BIC terendah dan log-likelihood tertinggi

#### **Kriteria Pemilihan:**
- **Statistical Fit:** AIC, BIC, log-likelihood
- **Parameter Stability:** Konsistensi koefisien
- **Diagnostic Quality:** Hasil uji residual
- **Practical Considerations:** Kemudahan implementasi

---

### **FILE 14a-14c: STARIMA Forecasting (Prediksi)** ⭐⭐⭐

#### **Tujuan:**
Melakukan forecasting 12 bulan ke depan (2024) menggunakan model STARIMA terpilih.

#### **Yang Dilakukan:**

1. **Manual STARIMA Implementation:**
   ```r
   Y(t) = AR_component + MA_component + Spatial_component + Seasonal_component
   ```

2. **Komponen Forecasting:**
   - **AR Component:** φ₁×Y(t-1) + φ₂×Y(t-2) + φ₃×Y(t-3)
   - **MA Component:** θ₁×ε(t-1) + θ₂×ε(t-2) + θ₃×ε(t-3)
   - **Spatial Component:** W×neighbor_values (berbeda per spatial weights)
   - **Seasonal Component:** Monthly patterns dari training data

3. **Standardization Process:**
   - **Coefficient Scaling:** Untuk correlation weights yang ekstrem
   - **Safety Bounds:** Mencegah forecast yang tidak realistis
   - **Error Handling:** Robust terhadap numerical instability

#### **Output:**
- Forecast 12 bulan untuk 5 wilayah
- Confidence intervals
- Visualization plots

---

### **FILE 15: Compare All Weights (Evaluasi Final)** ⭐⭐⭐

#### **Tujuan:**
Evaluasi komprehensif performa forecasting dari ketiga spatial weights.

#### **Yang Dilakukan:**

1. **Performance Metrics:**
   - **MAE (Mean Absolute Error):** Rata-rata kesalahan absolut
   - **MSE (Mean Squared Error):** Rata-rata kuadrat kesalahan
   - **RMSE (Root Mean Squared Error):** Akar rata-rata kuadrat kesalahan

2. **Regional Analysis:**
   - Performa per wilayah (Utara, Selatan, Timur, Barat, Tengah)
   - Identifikasi wilayah dengan prediksi terbaik/terburuk

3. **Spatial Weight Comparison:**
   - **Uniform:** Baseline comparison
   - **Distance:** Terbaik untuk model fitting
   - **Correlation:** Berpotensi terbaik untuk forecasting

#### **Final Ranking:**
Model diranking berdasarkan kombinasi:
- Model selection criteria (AIC/BIC)
- Forecasting accuracy (MAE/RMSE)
- Stability dan robustness

---

## 🎯 **KONTRIBUSI METODOLOGI UNTUK DOSEN**

### **1. Inovasi Teknis:**
- ✅ **Fair Comparison Framework:** Standardisasi untuk membandingkan 3 spatial weights secara adil
- ✅ **Manual STARIMA Implementation:** Implementasi forecasting yang robust dan dapat dikontrol
- ✅ **Comprehensive Diagnostic:** Validasi model yang menyeluruh

### **2. Temuan Ilmiah:**
- 🔍 **Spatial Weight Impact:** Signifikan mempengaruhi model fitting (Δ AIC > 300)
- 📈 **Model Selection Paradox:** AIC/BIC favors distance, forecasting may favor correlation
- 🎯 **Parameter Consistency:** Mengukur robustness model terhadap spatial weights

### **3. Aplikasi Praktis:**
- 🌧️ **Multi-Regional Rainfall Forecasting:** 5 wilayah Indonesia dengan karakteristik berbeda
- 📍 **12-Month Prediction:** Forecast lengkap untuk perencanaan
- 🔮 **Robust Error Handling:** Siap untuk implementasi operasional

---

## 📁 **FILE STRUKTUR**
```
STARIMAv3/
├── program/
│   ├── 01-05: Data Preparation
│   ├── 07-09: Model Identification  
│   ├── 10a-10c: STARIMA_Estimation ⭐⭐
│   ├── 11a-11c: Residual_Diagnostic ⭐
│   ├── 12: Model_Selection ⭐⭐
│   ├── 14a-14c: STARIMA_Forecasting ⭐⭐⭐
│   └── 15: Compare_All_Weights ⭐⭐⭐
├── output/: RData files
├── plots/: Visualizations
└── ALUR_PIPELINE_STARIMA.md: This summary
```

**Total**: File utama dengan alur sistematis untuk penelitian STARIMA(3,0,1,3).