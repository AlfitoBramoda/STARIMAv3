# ALUR PIPELINE STARIMA FORECASTING (UPDATED)
## Rangkuman Lengkap untuk Presentasi Dosen

---

## 🎯 **TUJUAN PENELITIAN**
Membandingkan efektivitas **3 jenis pembobotan spasial** (Uniform, Distance, Correlation) dalam model **STARIMA(3,0,1,3)** untuk prediksi curah hujan di 5 wilayah Indonesia dan memilih model terbaik berdasarkan **forecasting performance**.

---

## 📊 **ALUR PIPELINE LENGKAP (UPDATED)**

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

### **FASE 3: ESTIMASI MODEL** ⭐⭐
#### File 10a-10c: STARIMA Estimation
```
10a_STARIMA_Estimation_Uniform.R → 10b_STARIMA_Estimation_Distance.R → 10c_STARIMA_Estimation_Correlation.R
```

**Proses**:
1. **Input**: Training data + spatial weights
2. **Kalman Filter**: `starma()` function mengestimasi parameter
3. **Output**: Koefisien AR (φ), MA (θ), spatial parameters

**Hasil**:
- **3 model terpisah** dengan spatial weights berbeda
- **18 parameter per model** (9 AR + 9 MA)
- **AIC, BIC, Log-likelihood** untuk setiap model

---

### **FASE 4: DIAGNOSTIK MODEL**
#### File 11a-11c: Residual Diagnostic
```
11a_Residual_Diagnostic_Uniform.R → 11b_Residual_Diagnostic_Distance.R → 11c_Residual_Diagnostic_Correlation.R
```

**Output**: 
- Residual analysis untuk setiap model
- White noise tests (Ljung-Box, Moran's I)
- Model adequacy assessment

---

### **FASE 5: FORECASTING** ⭐⭐⭐
#### File 14a-14c: STARIMA Forecasting
```
14a_STARIMA_Forecasting_Uniform.R → 14b_STARIMA_Forecasting_Distance.R → 14c_STARIMA_Forecasting_Correlation.R
```

**Formula STARIMA Manual**:
```r
Y(t) = base_forecast + trend + AR_component + MA_component + spatial_effect + seasonal_variation
```

**Komponen**:
- **AR**: φ₁×Y(t-1) + φ₂×Y(t-2) + φ₃×Y(t-3)
- **MA**: θ₁×ε(t-1) + θ₂×ε(t-2) + θ₃×ε(t-3)
- **Spatial**: W×neighbor_values (berbeda per pembobotan)
- **Seasonal**: Monthly patterns dari training data

**Output**: 
- **12-month forecast** untuk setiap spatial weight
- **MAE, MSE, RMSE** per region per model

---

### **FASE 6: PERBANDINGAN PERFORMA** ⭐⭐
#### File 15: Compare All Weights
```
15_Compare_All_Weights.R
```

**Proses**:
1. **Load** semua hasil forecasting (14a, 14b, 14c)
2. **Hitung** metrics: MAE, MSE, RMSE per region
3. **Bandingkan** performa antar pembobotan
4. **Visualisasi** hasil perbandingan

**Output**:
- Comparison table semua metrics
- Regional performance analysis
- Visualization plots

---

### **FASE 7: PEMILIHAN MODEL TERBAIK** ⭐⭐⭐
#### File 16: Best Model Selection (NEW!)
```
16_Best_Model_Selection.R
```

**Proses**:
1. **Extract** performance metrics dari file 15
2. **Ranking** berdasarkan Average RMSE terkecil
3. **Statistical tests** untuk signifikansi perbedaan
4. **Regional analysis** - model mana yang menang per wilayah
5. **Final selection** berdasarkan overall performance

**Kriteria Pemilihan**:
- **Primary**: Average RMSE terkecil
- **Secondary**: MAE dan MSE
- **Regional wins**: Berapa wilayah yang diprediksi terbaik
- **Statistical significance**: Uji t-test antar model

**Output**:
- **Best spatial weighting scheme**
- **Performance improvement percentage**
- **Regional performance breakdown**
- **Statistical significance results**

---

## 🎯 **KEUNGGULAN ALUR BARU**

### **1. Metodologi yang Lebih Sound:**
- ✅ **Performance-based selection**: Pilih model berdasarkan kemampuan prediksi, bukan fitting
- ✅ **Real-world validation**: Menggunakan data test yang tidak pernah dilihat model
- ✅ **Comprehensive comparison**: Semua 3 pembobotan dibandingkan secara adil

### **2. Hasil yang Lebih Bermakna:**
- 🎯 **Practical relevance**: Model terbaik untuk forecasting, bukan hanya fitting
- 📊 **Statistical rigor**: Uji signifikansi untuk memastikan perbedaan nyata
- 🌍 **Regional insights**: Pemahaman mana spatial weight terbaik per wilayah

### **3. Kontribusi Penelitian:**
- 🔬 **Novel approach**: Model selection berdasarkan forecasting performance
- 📈 **Quantified improvement**: Berapa persen peningkatan performa
- 🎯 **Actionable results**: Rekomendasi spatial weight untuk implementasi

---

## 📋 **PENJELASAN UNTUK DOSEN**

### **Mengapa Model Selection di Akhir?**
1. **AIC/BIC mengukur fitting**, bukan forecasting ability
2. **Forecasting performance** lebih relevan untuk aplikasi praktis
3. **Out-of-sample validation** memberikan hasil yang lebih robust

### **Mengapa 3 Model Terpisah?**
1. **Fair comparison**: Setiap spatial weight mendapat kesempatan yang sama
2. **Independent estimation**: Tidak ada bias dari satu model ke model lain
3. **Comprehensive analysis**: Bisa analisis kelebihan/kekurangan masing-masing

### **Kontribusi Metodologi:**
1. **Framework perbandingan** yang standardized dan fair
2. **Performance-based model selection** untuk STARIMA
3. **Regional analysis** untuk memahami spatial heterogeneity

---

## 📁 **FILE STRUKTUR (UPDATED)**
```
STARIMAv3/
├── program/
│   ├── 01-05: Data Preparation
│   ├── 07-09: Model Identification  
│   ├── 10a-10c: STARIMA_Estimation ⭐⭐
│   ├── 11a-11c: Residual_Diagnostic ⭐
│   ├── 14a-14c: STARIMA_Forecasting ⭐⭐⭐
│   ├── 15: Compare_All_Weights ⭐⭐
│   └── 16: Best_Model_Selection ⭐⭐⭐ (NEW!)
├── output/: RData files
├── plots/: Visualizations
└── ALUR_PIPELINE_STARIMA_UPDATED.md: This summary
```

**Total**: Pipeline sistematis dengan model selection berdasarkan forecasting performance untuk penelitian STARIMA(3,0,1,3).