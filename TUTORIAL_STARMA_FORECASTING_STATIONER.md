# 📚 Tutorial STARMA Forecasting: Panduan Lengkap untuk Pemula
## 🎯 Prediksi Curah Hujan Menggunakan Space-Time AutoRegressive Moving Average

---

## 📖 **Apa itu STARMA?**

**STARMA (Space-Time AutoRegressive Moving Average)** adalah metode forecasting yang menggabungkan:
- **Time Series Analysis**: Memprediksi berdasarkan pola waktu
- **Spatial Analysis**: Mempertimbangkan pengaruh antar lokasi geografis

### 🌍 **Mengapa STARMA Penting?**
- **Cuaca tidak terisolasi**: Hujan di satu wilayah mempengaruhi wilayah lain
- **Akurasi lebih tinggi**: Menggunakan informasi spasial dan temporal
- **Aplikasi luas**: Cuaca, ekonomi, epidemiologi, lingkungan

---

## 🎯 **Studi Kasus: Curah Hujan Surabaya**

### 📊 **Dataset yang Digunakan:**
- **Lokasi**: 5 wilayah Surabaya (Barat, Selatan, Tengah, Timur, Utara)
- **Periode**: 2016-2024 (108 bulan data bulanan)
- **Variabel**: Curah hujan yang sudah stasioner
- **Tujuan**: Prediksi curah hujan 24 bulan ke depan

### 🗺️ **Koordinat Wilayah:**
```
Barat:   112.61°E, -7.24°S
Selatan: 112.73°E, -7.35°S  
Tengah:  112.74°E, -7.26°S
Timur:   112.84°E, -7.31°S
Utara:   112.73°E, -7.20°S
```

---

## 🔄 **Metodologi Box-Jenkins untuk STARMA**

STARMA mengikuti metodologi **Box-Jenkins yang diperluas** dengan 3 tahap utama:

### 1️⃣ **Identification (Identifikasi)**
- Menentukan orde model AR dan MA
- Menggunakan STACF dan STPACF plots

### 2️⃣ **Estimation (Estimasi)**
- Mengestimasi parameter model
- Menggunakan Maximum Likelihood

### 3️⃣ **Diagnostic (Diagnostik)**
- Menguji kelayakan model
- Memastikan residual white noise

---

## 📋 **Langkah-Langkah yang Sudah Dilakukan**

### **PHASE 1: DATA PREPARATION** ✅ **COMPLETED**

#### **Step 1: Setup Environment (00_Setup.R)**
**Apa yang dilakukan:**
- Install dan load library yang diperlukan
- Setup working directory dan folder struktur

**Kode Utama yang Digunakan:**
```r
# Daftar library yang diperlukan
required_packages <- c(
  "starma",      # Paket utama untuk STARMA modeling
  "spdep",       # Spatial dependence analysis
  "dplyr",       # Data manipulation
  "ggplot2",     # Visualisasi
  "forecast",    # Time series utilities
  "Matrix"       # Matrix operations
)

# Function untuk install dan load packages
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Install dan load semua packages
for (pkg in required_packages) {
  install_and_load(pkg)
}

# Set working directory
setwd("c:/Users/hp/Documents/Baby/STARMA")

# Create output directories
output_dirs <- c("results/stationer", "plots/stationer")
for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}
```

**Penjelasan Kode:**
- **`required_packages`**: Vector berisi nama semua library yang dibutuhkan
- **`install_and_load()`**: Function custom untuk install library jika belum ada, lalu load
- **`for` loop**: Iterasi untuk install/load semua packages secara otomatis
- **`setwd()`**: Set working directory ke folder project
- **`dir.create()`**: Buat folder output jika belum ada

**Mengapa penting:**
- Memastikan semua tools tersedia sebelum analisis
- Struktur project yang rapi dan terorganisir
- Reproducible environment setup

---

#### **Step 2: Load Data Stasioner (01_Load_Stationer_Data.R)**
**Apa yang dilakukan:**
- Membaca 5 file CSV data stasioner
- Mengkonversi ke format matrix 108×5
- Ekstrak koordinat geografis

**Kode Utama yang Digunakan:**
```r
# Load required libraries
library(readr)
library(dplyr)

# List stationer files
stationer_files <- list.files("dataset/Stationer", 
                             pattern = "*.csv", 
                             full.names = TRUE)

# Load all stationer datasets
stationer_data <- list()
regions <- c("Barat", "Selatan", "Tengah", "Timur", "Utara")

for (i in 1:length(regions)) {
  region <- regions[i]
  file_path <- paste0("dataset/Stationer/", region, "_Stasioner.csv")
  
  if (file.exists(file_path)) {
    data <- read_delim(file_path, delim = ",", 
                      locale = locale(decimal_mark = "."))
    stationer_data[[region]] <- data
  }
}

# Create spatio-temporal matrix
n_months <- nrow(stationer_data[[1]])
n_regions <- length(regions)
starma_matrix <- matrix(NA, nrow = n_months, ncol = n_regions)
colnames(starma_matrix) <- regions

# Fill matrix with diff1 values (stationer data)
for (i in 1:length(regions)) {
  region <- regions[i]
  diff1_values <- as.numeric(stationer_data[[region]]$diff1)
  starma_matrix[, i] <- diff1_values
}

# Ensure matrix is numeric
starma_matrix <- apply(starma_matrix, 2, as.numeric)

# Extract coordinates
coordinates <- data.frame(
  Region = regions,
  Longitude = sapply(stationer_data, function(x) x$Longitude[1]),
  Latitude = sapply(stationer_data, function(x) x$Latitude[1])
)
```

**Penjelasan Kode:**
- **`list.files()`**: Mencari semua file CSV di folder dataset
- **`for` loop pertama**: Membaca setiap file CSV dan simpan dalam list
- **`read_delim()`**: Membaca CSV dengan delimiter koma dan decimal point
- **`matrix()`**: Membuat matrix kosong dengan dimensi yang tepat
- **`for` loop kedua**: Mengisi matrix dengan kolom diff1 dari setiap region
- **`as.numeric()`**: Memastikan semua data dalam format numeric
- **`sapply()`**: Ekstrak koordinat dari data pertama setiap region

**Input:** 5 file CSV (Barat_Stasioner.csv, dll)
**Output:** Matrix 108 bulan × 5 wilayah

**Hasil penting:**
```
Matrix dimensions: 108 × 5
No missing values: ✅
Data sudah stasioner: ✅
```

**Mengapa penting:**
- Data stasioner adalah syarat wajib STARMA
- Format matrix diperlukan untuk fungsi starma()
- Koordinat diperlukan untuk spatial weights

---

#### **Step 3: Data Centering (02_Data_Centering.R)**
**Apa yang dilakukan:**
- Menggunakan fungsi `stcenter()` dari paket starma
- Melakukan **global centering** (bukan per kolom)
- Memastikan mean=0 dan standard deviation=1

**Kode Utama yang Digunakan:**
```r
# Load required libraries
library(starma)

# Load processed stationer data
load("results/stationer/stationer_data_loaded.RData")

# Calculate original statistics
original_stats <- data.frame(
  Region = colnames(starma_matrix),
  Mean = round(colMeans(starma_matrix), 4),
  SD = round(apply(starma_matrix, 2, sd), 4),
  Min = round(apply(starma_matrix, 2, min), 4),
  Max = round(apply(starma_matrix, 2, max), 4)
)

# Apply stcenter() function - KUNCI UTAMA
centered_data <- stcenter(starma_matrix)

# Verify global centering
global_mean <- sum(centered_data) / (nrow(centered_data) * ncol(centered_data))
global_sd <- sqrt(sum(centered_data^2) / (nrow(centered_data) * ncol(centered_data) - 1))

# Calculate centered statistics
centered_stats <- data.frame(
  Region = colnames(centered_data),
  Mean = round(colMeans(centered_data), 6),
  SD = round(apply(centered_data, 2, sd), 4),
  Min = round(apply(centered_data, 2, min), 4),
  Max = round(apply(centered_data, 2, max), 4)
)

# Verification checks
mean_check <- abs(global_mean) < 1e-10
sd_check <- abs(global_sd - 1) < 1e-10

# Save centered data
save(centered_data, original_stats, centered_stats,
     file = "results/stationer/centered_data.RData")
```

**Penjelasan Kode Detail:**
- **`load()`**: Memuat data matrix yang sudah dibuat sebelumnya
- **`colMeans()`, `apply()`**: Menghitung statistik deskriptif original data
- **`stcenter()`**: **FUNGSI KUNCI** - melakukan global centering dan scaling
- **Global mean calculation**: `sum(data) / total_elements` - menghitung mean global
- **Global SD calculation**: Formula standard deviation untuk seluruh matrix
- **Verification**: Memastikan hasil centering benar (mean≈0, SD≈1)
- **`save()`**: Menyimpan hasil untuk tahap selanjutnya

**Konsep Global Centering:**
```r
Global Mean = sum(seluruh_data) / (108 × 5) = 0
Global SD = sqrt(sum(data²) / (108×5 - 1)) = 1
```

**Perbedaan dengan Column-wise Centering:**
- **Column-wise**: `scale(data)` - setiap kolom di-center terpisah
- **Global**: `stcenter(data)` - seluruh matrix sebagai satu kesatuan
- **STARMA butuh global** karena spatio-temporal = satu proses

**Hasil:**
```
Global mean: 0.000000 ✅
Global SD: 1.000000 ✅
```

**Mengapa penting:**
- STARMA tidak mengestimasi intercept
- Centering mencegah bias dalam estimasi parameter
- Global centering sesuai konsep spatio-temporal sebagai satu proses

**File Output:**
- `centered_data.RData`: Data yang sudah di-center untuk tahap selanjutnya

---

#### **Step 4: Spatial Weights Creation (03_Spatial_Weights.R)**
**Apa yang dilakukan:**
- Membuat 3 jenis spatial weight matrices
- Validasi dan normalisasi semua matrices
- Konversi ke format wlist untuk starma()

**Kode Utama yang Digunakan:**
```r
# Load required libraries
library(spdep)
library(starma)

# Load centered data and coordinates
load("results/stationer/centered_data.RData")
load("results/stationer/stationer_data_loaded.RData")

# Extract coordinates for spatial weights
coords <- as.matrix(coordinates[, c("Longitude", "Latitude")])
rownames(coords) <- coordinates$Region
n_regions <- nrow(coords)

# Calculate distance matrix
dist_matrix <- as.matrix(dist(coords))
dist_matrix_km <- dist_matrix * 111  # Convert to km (1 degree ≈ 111 km)

# ============================================================================
# 1. UNIFORM WEIGHTS
# ============================================================================
uniform_weights <- matrix(1, nrow = n_regions, ncol = n_regions)
diag(uniform_weights) <- 0  # No self-influence
uniform_weights <- uniform_weights / (n_regions - 1)  # Normalize

# ============================================================================
# 2. DISTANCE-BASED WEIGHTS
# ============================================================================
distance_weights <- 1 / (dist_matrix_km + 0.01)  # Inverse distance
diag(distance_weights) <- 0  # No self-influence
# Row-normalize
row_sums <- rowSums(distance_weights)
distance_weights <- distance_weights / row_sums

# ============================================================================
# 3. CORRELATION-BASED WEIGHTS
# ============================================================================
correlation_matrix <- cor(centered_data)
correlation_weights <- abs(correlation_matrix)
diag(correlation_weights) <- 0  # No self-influence
# Row-normalize
row_sums <- rowSums(correlation_weights)
correlation_weights <- correlation_weights / row_sums

# ============================================================================
# CREATE WLIST FORMAT FOR STARMA
# ============================================================================
wlist_uniform <- list(uniform_weights)
wlist_distance <- list(distance_weights)
wlist_correlation <- list(correlation_weights)

# Validation function
validate_weights <- function(weights, name) {
  cat("\n", name, "validation:\n")
  cat("  - Dimensions:", dim(weights)[1], "x", dim(weights)[2], "\n")
  cat("  - Row sums range:", round(min(rowSums(weights)), 4), 
      "to", round(max(rowSums(weights)), 4), "\n")
  cat("  - Diagonal elements:", all(diag(weights) == 0), "\n")
  cat("  - Non-negative elements:", all(weights >= 0), "\n")
}

# Validate all matrices
validate_weights(uniform_weights, "UNIFORM WEIGHTS")
validate_weights(distance_weights, "DISTANCE WEIGHTS")
validate_weights(correlation_weights, "CORRELATION WEIGHTS")
```

**Penjelasan Kode Detail:**

**A. Setup dan Persiapan:**
- **`as.matrix(coords)`**: Konversi koordinat ke format matrix untuk perhitungan jarak
- **`dist()`**: Menghitung euclidean distance antar koordinat
- **`* 111`**: Konversi degree ke kilometer (1 degree ≈ 111 km)

**B. Uniform Weights:**
- **`matrix(1, ...)`**: Buat matrix berisi angka 1
- **`diag() <- 0`**: Set diagonal = 0 (tidak ada self-influence)
- **`/ (n_regions - 1)`**: Normalisasi agar setiap baris sum = 1

**C. Distance Weights:**
- **`1 / (dist + 0.01)`**: Inverse distance (+ 0.01 hindari division by zero)
- **`rowSums()`**: Hitung total setiap baris untuk normalisasi
- **Row normalization**: Setiap baris dijumlah = 1

**D. Correlation Weights:**
- **`cor(centered_data)`**: Hitung correlation matrix antar wilayah
- **`abs()`**: Gunakan absolute value correlation
- **Same normalization**: Row-wise normalization

**E. wlist Format:**
- **`list(weights)`**: Konversi ke format list yang dibutuhkan starma()
- **Spatial lag 1**: Hanya menggunakan immediate neighbors

**F. Validation:**
- **Row sums = 1**: Memastikan normalisasi benar
- **Diagonal = 0**: Tidak ada self-influence
- **Non-negative**: Semua bobot positif

**3 Jenis Pembobotan:**

##### **1. Uniform Weights (Pembobotan Seragam)**
```
Konsep: Semua wilayah berpengaruh sama
Bobot: 0.25 untuk setiap wilayah lain
Filosofi: "Semua wilayah saling mempengaruhi dengan kekuatan sama"
```

##### **2. Distance Weights (Pembobotan Jarak)**
```
Konsep: Semakin dekat = semakin berpengaruh
Formula: 1 / jarak_geografis
Contoh: Tengah-Utara (6.75 km) → bobot tinggi (0.44)
        Barat-Timur (26.7 km) → bobot rendah (0.16)
```

##### **3. Correlation Weights (Pembobotan Korelasi)**
```
Konsep: Berdasarkan kekuatan hubungan statistik
Formula: |correlation| / sum(|correlations|)
Hasil menarik: Korelasi sangat tinggi (0.96-0.99)
Interpretasi: Curah hujan Surabaya sangat homogen
```

**Insight Penting:**
- Korelasi antar wilayah 96-99% → cuaca sangat mirip
- Correlation weights hampir uniform → efek spasial minimal
- Distance weights paling bervariasi → kemungkinan performa terbaik

**File Output yang Dihasilkan:**
```
results/stationer/
├── spatial_weights_report.txt     # Laporan lengkap semua hasil
├── uniform_weights.csv            # Matrix uniform weights
├── distance_weights.csv           # Matrix distance weights  
├── correlation_weights.csv        # Matrix correlation weights
├── correlation_matrix.csv         # Matrix korelasi antar wilayah
├── weights_summary.csv            # Ringkasan statistik
└── spatial_weights.RData          # Data R untuk analisis lanjutan
```

**Kegunaan File Output:**
- **Report.txt**: Dokumentasi lengkap untuk laporan
- **CSV files**: Import ke Excel/software lain untuk analisis
- **RData**: Untuk melanjutkan analisis di R

---

#### **Step 5: Data Splitting (04_Data_Split.R)**
**Apa yang dilakukan:**
- Membagi data centered menjadi training dan testing sets
- Implementasi split 89-11% (96 training, 12 testing)
- Validasi kualitas data dan kontinuitas temporal

**Kode Utama yang Digunakan:**
```r
# Load required libraries
library(dplyr)
library(lubridate)

# Load centered data
load("results/stationer/centered_data.RData")
load("results/stationer/stationer_data_loaded.RData")

# Check and create dates if needed
if (!exists("dates") || is.null(dates) || any(is.na(dates))) {
  start_date <- as.Date("2016-01-01")
  dates <- seq(start_date, by = "month", length.out = nrow(centered_data))
}

# Define split parameters
n_total <- nrow(centered_data)
n_test <- 12  # 1 year test (2024)
n_train <- n_total - n_test  # 96 months training

# Create training and testing datasets
train_data <- centered_data[1:n_train, ]
test_data <- centered_data[(n_train+1):n_total, ]

# Create corresponding date vectors
train_dates <- dates[1:n_train]
test_dates <- dates[(n_train+1):n_total]

# Calculate statistics for both datasets
train_stats <- data.frame(
  Region = colnames(train_data),
  Mean = round(colMeans(train_data), 4),
  SD = round(apply(train_data, 2, sd), 4),
  Min = round(apply(train_data, 2, min), 4),
  Max = round(apply(train_data, 2, max), 4)
)

test_stats <- data.frame(
  Region = colnames(test_data),
  Mean = round(colMeans(test_data), 4),
  SD = round(apply(test_data, 2, sd), 4),
  Min = round(apply(test_data, 2, min), 4),
  Max = round(apply(test_data, 2, max), 4)
)

# Data quality validation
train_na <- sum(is.na(train_data))
test_na <- sum(is.na(test_data))
expected_total <- n_train + n_test
actual_total <- nrow(train_data) + nrow(test_data)

# Temporal continuity check
last_train_date <- max(train_dates)
first_test_date <- min(test_dates)
date_gap <- as.numeric(first_test_date - last_train_date)

# Column consistency check
col_consistency <- all(colnames(train_data) == colnames(test_data))

# Export results to files
split_report <- c(
  "=== STARMA DATA SPLIT REPORT ===",
  paste("Generated on:", Sys.time()),
  paste("Total observations:", n_total, "months"),
  paste("Training size:", n_train, "months"),
  paste("Testing size:", n_test, "months")
  # ... more report content
)

writeLines(split_report, "results/stationer/data_split_report.txt")
write.csv(train_data, "results/stationer/train_data.csv", row.names = TRUE)
write.csv(test_data, "results/stationer/test_data.csv", row.names = TRUE)

# Save all split data
save(train_data, test_data, train_dates, test_dates, 
     train_stats, test_stats, n_train, n_test,
     file = "results/stationer/data_split.RData")
```

**Penjelasan Kode Detail:**

**A. Date Handling:**
- **`exists("dates")`**: Cek apakah variabel dates ada
- **`seq(..., by="month")`**: Buat sequence tanggal bulanan jika dates tidak ada
- **Fallback mechanism**: Pastikan selalu ada date information

**B. Split Logic:**
- **`n_test <- 12`**: Tetapkan 12 bulan untuk testing (2024)
- **`n_train <- n_total - n_test`**: Sisanya untuk training (96 bulan)
- **Index-based splitting**: `[1:n_train]` dan `[(n_train+1):n_total]`

**C. Data Extraction:**
- **`train_data <- centered_data[1:n_train, ]`**: Ambil 96 baris pertama
- **`test_data <- centered_data[(n_train+1):n_total, ]`**: Ambil 12 baris terakhir
- **Corresponding dates**: Split dates sesuai dengan data split

**D. Statistical Analysis:**
- **`colMeans()`, `apply()`**: Hitung statistik untuk train dan test terpisah
- **`data.frame()`**: Organisir statistik dalam format tabel
- **Comparison**: Bandingkan karakteristik train vs test

**E. Quality Validation:**
- **Missing values**: `sum(is.na())` - pastikan tidak ada data hilang
- **Data continuity**: `expected == actual` - pastikan tidak ada data yang hilang
- **Temporal gap**: Hitung selisih hari antara train dan test
- **Column consistency**: Pastikan kolom train dan test sama

**F. Export dan Documentation:**
- **`writeLines()`**: Buat report text file
- **`write.csv()`**: Export data ke CSV untuk analisis eksternal
- **`save()`**: Simpan R objects untuk tahap selanjutnya

**Split Configuration:**
```
Training: 96 bulan (2016-2023) = 89% data
Testing:  12 bulan (2024 only) = 11% data
```

**Hasil penting:**
```
Training period: 2016-01-01 to 2023-12-01
Testing period:  2024-01-01 to 2024-12-01
Clean temporal separation: ✅
No missing values: ✅
Data continuity: ✅
```

**Mengapa split ini optimal:**
- **Maximum training data**: 8 tahun untuk pattern learning
- **Complete seasonal cycle**: 1 tahun test untuk evaluasi musiman
- **Practical horizon**: Prediksi 1 tahun ke depan realistis
- **Clean separation**: No data leakage antara train-test

**File Output yang Dihasilkan:**
```
results/stationer/
├── data_split_report.txt          # Laporan lengkap split results
├── train_data.csv                 # Training data (96×5)
├── test_data.csv                  # Testing data (12×5)
├── train_statistics.csv           # Statistik training data
├── test_statistics.csv            # Statistik testing data
└── data_split.RData              # R objects untuk phase selanjutnya
```

**Statistical Insights:**
- **Training means**: Close to 0 (-0.0139 to -0.0030)
- **Training SDs**: Around 1.0 (0.9982 to 1.0855)
- **Testing means**: Slightly positive (0.0464 to 0.0633)
- **Testing SDs**: Lower variability (0.6133 to 0.8387)

---

### **PHASE 2: STARMA IDENTIFICATION** ✅ **COMPLETED**

#### **Step 6: STACF Analysis (05_STACF_Analysis.R)** ✅ **COMPLETED**
**Apa yang dilakukan:**
- Menghitung Space-Time AutoCorrelation Function (STACF)
- Mengidentifikasi orde Moving Average (MA) untuk model STARMA
- Menggunakan 3 jenis spatial weights untuk perbandingan
- Mendeteksi pola seasonal dan spatial dependencies

**Kode Utama yang Digunakan:**
```r
# Load required libraries
library(starma)
library(ggplot2)

# Load training data and spatial weights
load("results/stationer/data_split.RData")
load("results/stationer/spatial_weights.RData")

# Parameters for STACF
max_lag_time <- 12  # Maximum time lags
max_lag_space <- 2  # Maximum spatial lags

# Create proper wlist format (identity matrix first)
I_matrix <- diag(5)  # Identity matrix for 5 regions

# 1. STACF with Uniform Weights
wlist_uniform_stacf <- list(I_matrix, uniform_weights)
stacf_uniform <- stacf(train_data, wlist = wlist_uniform_stacf, 
                      tlag.max = max_lag_time, plot = FALSE)

# 2. STACF with Distance Weights  
wlist_distance_stacf <- list(I_matrix, distance_weights)
stacf_distance <- stacf(train_data, wlist = wlist_distance_stacf,
                       tlag.max = max_lag_time, plot = FALSE)

# 3. STACF with Correlation Weights
wlist_correlation_stacf <- list(I_matrix, correlation_weights)
stacf_correlation <- stacf(train_data, wlist = wlist_correlation_stacf,
                          tlag.max = max_lag_time, plot = FALSE)

# Extract significant lags (|STACF| > 0.1)
extract_significant_lags <- function(stacf_result, weight_name) {
  stacf_matrix <- stacf_result
  significant_lags <- which(abs(stacf_matrix) > 0.1, arr.ind = TRUE)
  
  if (nrow(significant_lags) > 0) {
    sig_df <- data.frame(
      time_lag = significant_lags[,1],
      space_lag = significant_lags[,2] - 1,
      stacf_value = stacf_matrix[significant_lags],
      weight_type = weight_name
    )
    return(sig_df)
  }
}

# Extract significant lags for each weight type
sig_uniform <- extract_significant_lags(stacf_uniform, "Uniform")
sig_distance <- extract_significant_lags(stacf_distance, "Distance")  
sig_correlation <- extract_significant_lags(stacf_correlation, "Correlation")

# Combine results
all_significant <- rbind(sig_uniform, sig_distance, sig_correlation)

# MA Order Recommendation
if (nrow(all_significant) > 0) {
  max_time_lag <- max(all_significant$time_lag)
  max_space_lag <- max(all_significant$space_lag)
  cat("Recommended MA order: (", max_time_lag, ",", max_space_lag, ")\n")
} else {
  max_time_lag <- 1
  max_space_lag <- 1
}
```

**Penjelasan Kode Detail:**

**A. Setup dan Parameter:**
- **`max_lag_time <- 12`**: Maksimal 12 lag waktu (1 tahun untuk data bulanan)
- **`max_lag_space <- 2`**: Maksimal 2 lag spasial (immediate + second neighbors)
- **`I_matrix <- diag(5)`**: Identity matrix 5×5 untuk spatial lag 0

**B. wlist Format untuk STACF:**
- **`list(I_matrix, weights)`**: Format wajib untuk fungsi `stacf()`
- **Element pertama**: Identity matrix (spatial lag 0)
- **Element kedua**: Spatial weights matrix (spatial lag 1)
- **Konsep**: `wlist[1]` = lag 0, `wlist[2]` = lag 1, dst.

**C. Fungsi stacf():**
- **`stacf(data, wlist, tlag.max, plot=FALSE)`**: Fungsi utama STACF
- **Input data**: Format matrix (time × regions) - sudah benar dari train_data
- **Output**: Matrix STACF values (time_lags × space_lags)
- **`plot=FALSE`**: Disable automatic plotting untuk kontrol manual

**D. Ekstraksi Lag Signifikan:**
- **`which(abs(stacf) > 0.1, arr.ind=TRUE)`**: Cari posisi dengan |STACF| > 0.1
- **`arr.ind=TRUE`**: Return array indices (row, col)
- **Threshold 0.1**: Standard cutoff untuk significance dalam time series

**E. Interpretasi Hasil:**
- **`time_lag`**: Lag temporal (1=1 bulan, 12=1 tahun)
- **`space_lag`**: Lag spasial (0=same location, 1=neighbors)
- **`stacf_value`**: Nilai korelasi (-1 to +1)

**Konsep STACF (Space-Time AutoCorrelation Function):**

**1. Apa itu STACF?**
```
STACF mengukur korelasi antara:
- Nilai di lokasi i pada waktu t
- Nilai di lokasi j pada waktu t-k

Formula: ρ(k,s) = Corr[Z(i,t), Z(j,t-k)]
di mana s = spatial lag antara lokasi i dan j
```

**2. Interpretasi Lag:**
- **Time lag (k)**: Berapa bulan ke belakang
- **Space lag (s)**: Berapa "step" spasial (0=same, 1=neighbor)

**3. Tujuan STACF:**
- **Identifikasi MA order**: Lag mana yang signifikan untuk Moving Average
- **Seasonal detection**: Lag 12 untuk pola tahunan
- **Spatial dependency**: Space lag > 0 untuk efek spasial

**Hasil STACF Analysis:**

**A. Significant Lags Found (60 total):**
```
Time Lag 1:  STACF = 0.452 (Strong positive - recent influence)
Time Lag 2:  STACF = 0.286 (Moderate positive)
Time Lag 3:  STACF = 0.289 (Moderate positive)
Time Lag 12: STACF = -0.363 (Strong negative - seasonal pattern!)
```

**B. Spatial Consistency:**
- **Space lag 0 vs 1**: Nilai hampir identik
- **Interpretasi**: Efek spasial minimal (sesuai korelasi tinggi)
- **Konsistensi**: Ketiga weight types memberikan hasil sama

**C. MA Order Recommendation: (12,1)**
- **Time order 12**: Seasonal Moving Average component
- **Space order 1**: Spatial lag 1 (immediate neighbors)

**Interpretasi Hasil Penting:**

**1. Seasonal Pattern Terdeteksi:**
```
Lag 12 = -0.363 (negative correlation)
Interpretasi: Curah hujan tinggi di bulan yang sama tahun lalu 
             → curah hujan rendah tahun ini (pola musiman)
```

**2. Short-term Dependencies:**
```
Lag 1-5: Positive correlations (0.45 - 0.14)
Interpretasi: Curah hujan bulan lalu mempengaruhi bulan ini
```

**3. Spatial Homogeneity:**
```
Space lag 0 ≈ Space lag 1
Interpretasi: Curah hujan di lokasi sendiri ≈ curah hujan di tetangga
             (sesuai korelasi 96-99%)
```

**4. Model Implications:**
```
MA(12,1) berarti:
- Moving Average dengan 12 time lags
- Spatial dependency hingga lag 1
- Seasonal component yang kuat
```

**File Output yang Dihasilkan:**
```
results/stationer/05_stacf_analysis/
├── stacf_results.RData              # Objek STACF lengkap
├── significant_lags.csv             # 60 lag signifikan
├── stacf_plot_data.csv             # Data untuk visualisasi
├── ma_order_recommendation.csv      # MA(12,1) recommendation
└── stacf_analysis_report.txt       # Laporan lengkap
```

**Insight untuk Model Building:**
- **Seasonal STARMA**: Model harus menangani seasonality
- **Spatial effect minimal**: Mungkin model sederhana sudah cukup
- **MA dominance**: STACF menunjukkan MA structure yang jelas
- **Next step**: STPACF untuk menentukan AR order

**Mengapa STACF Penting:**
1. **Foundation identification**: Menentukan struktur dasar model
2. **Seasonal detection**: Mengidentifikasi pola musiman
3. **Spatial assessment**: Mengukur pentingnya efek spasial
4. **Model complexity**: Menentukan kompleksitas yang dibutuhkan

#### **Step 7: STPACF Analysis (06_STPACF_Analysis.R)** ✅ **COMPLETED**
**Apa yang dilakukan:**
- Menghitung Space-Time Partial AutoCorrelation Function (STPACF)
- Mengidentifikasi orde AutoRegressive (AR) untuk model STARMA
- Menggunakan 3 jenis spatial weights untuk perbandingan
- Menentukan struktur AR temporal dan spatial

**Konsep STPACF (Space-Time Partial AutoCorrelation Function):**

**1. Perbedaan STACF vs STPACF:**
```
STACF: Mengukur total correlation (direct + indirect effects)
STPACF: Mengukur direct correlation (setelah menghilangkan efek lag sebelumnya)

Analogi:
STACF = "Berapa total pengaruh hujan 12 bulan lalu?"
STPACF = "Berapa pengaruh langsung hujan 12 bulan lalu (tanpa efek lag 1-11)?"
```

**2. Fungsi dalam Identification:**
```
STACF → Identifikasi MA order (Moving Average)
STPACF → Identifikasi AR order (AutoRegressive)
```

**Hasil STPACF Analysis:**

**A. Significant Lags Found (19 total):**
```
Time Lag 1:  STPACF = 0.458 (Strong direct AR effect)
Time Lag 3:  STPACF = 0.166 (Moderate AR component)
Time Lag 6:  STPACF = -0.211 (Negative seasonal effect)
Time Lag 12: STPACF = -0.257 (Strong seasonal AR pattern)
```

**B. Spatial Structure:**
```
Space lag 0: Semua significant lags
Space lag 1: Tidak ada yang signifikan
Interpretasi: AR effects hanya temporal, tidak ada spatial AR
```

**C. AR Order Recommendation: (12,0)**
- **Time order 12**: Seasonal AutoRegressive component
- **Space order 0**: Tidak ada spatial AR effects

**D. Complete STARMA Order: STARMA(12,0; 12,1)**
```
AR(12,0): Temporal AR hingga lag 12, no spatial AR
MA(12,1): Seasonal MA + spatial MA lag 1
```

**Interpretasi Model STARMA(12,0; 12,1):**

**1. Struktur AR (12,0):**
```
Curah hujan bulan ini dipengaruhi langsung oleh:
- Curah hujan 1 bulan lalu (0.458)
- Curah hujan 3 bulan lalu (0.166)
- Curah hujan 6 bulan lalu (-0.211)
- Curah hujan 12 bulan lalu (-0.257) ← Seasonal pattern

Tidak ada pengaruh AR dari wilayah tetangga
```

**2. Struktur MA (12,1):**
```
Error/noise bulan ini dipengaruhi oleh:
- Error 12 bulan lalu (seasonal noise)
- Error dari wilayah tetangga (spatial spillover)
```

**3. Model Complexity:**
```
Temporal: Seasonal patterns dominan (lag 12)
Spatial: Minimal AR, ada MA spillover effects
Overall: Mixed temporal-spatial model dengan seasonal components
```

---

## 📚 **STARMA ORDER NOTATION - PENJELASAN LENGKAP**

### **🔍 Perbandingan dengan ARIMA**

#### **ARIMA (Time Series Univariate)**
```
ARIMA(p, d, q)
p = AR order (autoregressive)
d = Differencing order (integrated)
q = MA order (moving average)

Contoh: ARIMA(2,1,1)
- AR(2): Dipengaruhi 2 lag waktu sebelumnya
- I(1): Data di-differencing 1 kali
- MA(1): Error dipengaruhi 1 lag error sebelumnya
```

#### **STARMA (Spatio-Temporal Multivariate)**
```
STARMA(p, s; q, r)
p = Temporal AR order
s = Spatial AR order  
q = Temporal MA order
r = Spatial MA order

Contoh: STARMA(12,0; 12,1)
- AR(12,0): Temporal AR lag 12, no spatial AR
- MA(12,1): Temporal MA lag 12 + spatial MA lag 1
```

### **📊 Struktur STARMA Order**

#### **🎯 4 Parameter Utama**
```
STARMA(p,s; q,r)
       ↑     ↑
      AR    MA
      
1. p = Temporal AR: Berapa lag waktu untuk AR
2. s = Spatial AR: Berapa lag spasial untuk AR
3. q = Temporal MA: Berapa lag waktu untuk MA
4. r = Spatial MA: Berapa lag spasial untuk MA
```

### **🔬 Contoh Interpretasi Model**

#### **Model Kita: STARMA(12,0; 12,1)**
```
p = 12: AR temporal hingga lag 12 bulan (seasonal)
s = 0:  Tidak ada AR spasial
q = 12: MA temporal hingga lag 12 bulan (seasonal)
r = 1:  MA spasial lag 1 (neighbor effect)
```

**Meaning dalam Konteks Curah Hujan:**
```
AR(12,0): Curah hujan bulan ini dipengaruhi curah hujan:
          - 1, 3, 6, 12 bulan lalu (temporal dependencies)
          - Tidak ada pengaruh langsung dari wilayah lain
          
MA(12,1): Error/noise bulan ini dipengaruhi:
          - Error 12 bulan lalu (seasonal noise pattern)
          - Error dari wilayah tetangga (spatial spillover)
```

#### **Contoh Model STARMA Lain**

**STARMA(1,1; 1,1) - Model Sederhana:**
```
p=1, s=1: AR lag 1 waktu + AR lag 1 spasial
q=1, r=1: MA lag 1 waktu + MA lag 1 spasial

Interpretasi:
- Nilai sekarang dipengaruhi nilai bulan lalu + nilai tetangga bulan lalu
- Error sekarang dipengaruhi error bulan lalu + error tetangga bulan lalu
```

**STARMA(2,0; 0,1) - AR Temporal, MA Spatial:**
```
p=2, s=0: AR temporal lag 1,2 saja
q=0, r=1: Hanya MA spasial, tidak ada MA temporal

Interpretasi:
- Nilai dipengaruhi 2 bulan sebelumnya, tidak ada efek spasial langsung
- Error hanya dipengaruhi error tetangga, tidak ada temporal MA
```

### **🎯 Perbedaan Fundamental dengan ARIMA**

#### **ARIMA: 1 Dimensi (Waktu)**
```
ARIMA(2,1,1): 
- Hanya mempertimbangkan dependensi temporal
- Satu lokasi, satu time series
- Model: Y(t) = f(Y(t-1), Y(t-2), ε(t-1))
```

#### **STARMA: 2 Dimensi (Waktu + Ruang)**
```
STARMA(2,1; 1,1):
- Mempertimbangkan dependensi temporal DAN spasial
- Multiple lokasi, multiple time series
- Model: Y(i,t) = f(Y(i,t-1), Y(j,t-1), ε(i,t-1), ε(j,t-1))
         di mana j = neighbor dari i
```

### **✅ Interpretasi Praktis Model Kita**

**STARMA(12,0; 12,1) untuk Curah Hujan Surabaya:**

**1. Komponen AR (12,0):**
```
"Curah hujan bulan ini di suatu wilayah dipengaruhi oleh:
 - Curah hujan di wilayah yang sama 1, 3, 6, dan 12 bulan lalu
 - TIDAK dipengaruhi langsung oleh curah hujan di wilayah lain"
 
Mengapa s=0? Karena korelasi antar wilayah sangat tinggi (96-99%),
efek spasial sudah "tertangkap" dalam temporal pattern.
```

**2. Komponen MA (12,1):**
```
"Error/gangguan curah hujan bulan ini dipengaruhi oleh:
 - Error 12 bulan lalu (pola seasonal dalam noise)
 - Error dari wilayah tetangga (spillover effect)"
 
Mengapa r=1? Meskipun korelasi tinggi, masih ada spillover effect
dalam komponen error/noise antar wilayah.
```

**3. Model Complexity Assessment:**
```
Temporal: High (seasonal AR + seasonal MA)
Spatial: Low (no spatial AR, minimal spatial MA)
Overall: Medium complexity dengan fokus seasonal patterns
```

---

## 🔍 **Temuan Menarik dari Analisis**

### **1. Korelasi Sangat Tinggi**
```
Barat-Utara: 99.88% - Hampir identik!
Tengah-Utara: 99.11% - Sangat mirip  
Selatan-Tengah: 99.52% - Hampir sama
```

**Interpretasi:**
- Surabaya relatif kecil geografis → cuaca homogen
- Pola curah hujan hampir identik di semua wilayah
- Efek topografi dan urbanisasi minimal

### **2. Perbandingan Spatial Weights**
| Metode | Variasi Bobot | Karakteristik |
|--------|---------------|---------------|
| Uniform | 0.25 (sama semua) | Baseline comparison |
| Distance | 0.148-0.439 | Bervariasi signifikan |
| Correlation | 0.245-0.253 | Hampir uniform |

### **3. Prediksi Performa Model**
- **Distance weights**: Kemungkinan hasil terbaik (variasi tertinggi)
- **Correlation weights**: Mungkin mirip uniform (korelasi tinggi)
- **Spatial effect**: Mungkin tidak terlalu signifikan

---

## 🎓 **Konsep Penting untuk Dipahami**

### **1. Stationarity (Kestasioneran)**
- **Definisi**: Data tidak memiliki trend atau seasonal pattern
- **Mengapa penting**: STARMA memerlukan data stasioner
- **Cara mencapai**: Box-Cox transformation + differencing

### **2. Global vs Column-wise Centering**
- **Column-wise**: Setiap wilayah di-center terpisah
- **Global**: Seluruh matrix di-center sebagai satu kesatuan
- **STARMA menggunakan global**: Konsep spatio-temporal sebagai satu proses

### **3. Spatial Weights Philosophy**
- **Uniform**: Tidak ada efek spasial khusus
- **Distance**: Efek geografis berdasarkan jarak fisik
- **Correlation**: Efek berdasarkan pola data aktual

### **4. Row Normalization**
- **Tujuan**: Setiap baris weight matrix sum = 1
- **Interpretasi**: Total pengaruh dari wilayah lain = 100%
- **Konsistensi**: Memastikan skala yang sama antar model

---

### **4. STACF Analysis Results**
```
MA Order Identified: (12,1)
Seasonal Pattern: Strong negative correlation at lag 12 (-0.363)
Spatial Effect: Minimal (space lag 0 ≈ space lag 1)
Total Significant Lags: 60 (consistent across all weight types)
```

**Interpretasi:**
- Model STARMA akan memiliki seasonal MA component
- Efek spasial ada tapi tidak dominan
- Pola curah hujan Surabaya sangat predictable

---

## 📊 **Status Progress Saat Ini**

### ✅ **COMPLETED (38% - 6/16 files)**
- **Phase 1: Data Preparation** → **100% SELESAI**
  - Environment setup ✅
  - Data loading ✅  
  - Data centering ✅
  - Spatial weights ✅
  - Data splitting ✅
- **Phase 2: STARMA Identification** → **50% SELESAI**
  - STACF Analysis ✅ → **MA(12,1) identified**

### ⏳ **NEXT STEPS**
- **Phase 2: STARMA Identification** (lanjutan)
  - STPACF Analysis (menentukan orde AR)
- **Phase 3: STARMA Estimation**
  - Model fitting dengan 3 jenis weights
- **Phase 4: STARMA Diagnostic**
  - Residual testing dan model validation
- **Phase 5: Forecasting & Evaluation**
  - Prediksi 24 bulan dan evaluasi akurasi

---

## 🎯 **Tips untuk Mengajarkan STARMA**

### **1. Mulai dengan Konsep Sederhana**
- Jelaskan time series biasa dulu (ARIMA)
- Tambahkan konsep spatial dependency
- Gunakan analogi: "Cuaca di Jakarta mempengaruhi cuaca di Bogor"

### **2. Gunakan Visualisasi**
- Peta untuk menunjukkan lokasi
- Time series plots untuk menunjukkan pola temporal
- Correlation heatmaps untuk spatial relationships

### **3. Tekankan Pentingnya Preprocessing**
- Stationarity testing dan transformation
- Data centering yang benar
- Spatial weights yang tepat

### **4. Praktik Hands-on**
- Gunakan dataset sederhana (3-4 lokasi)
- Ikuti metodologi Box-Jenkins step by step
- Bandingkan hasil dengan time series biasa

---

## 📚 **Referensi dan Sumber Belajar**

### **Paket R yang Digunakan:**
- `starma`: Paket utama untuk STARMA modeling
- `spdep`: Spatial dependence analysis
- `forecast`: Time series forecasting utilities

### **Metodologi:**
- Box-Jenkins methodology (1976)
- Pfeifer & Deutsch STARMA extension (1980)
- Felix Cheysson starma package documentation

### **Aplikasi Praktis:**
- Meteorologi dan klimatologi
- Epidemiologi spasial
- Ekonomi regional
- Analisis lingkungan

---

## 🎉 **Kesimpulan Phase 1**

**Apa yang sudah berhasil dicapai:**
1. ✅ Environment siap dengan semua library
2. ✅ Data stasioner berhasil dimuat (108×5 matrix)
3. ✅ Global centering berhasil (mean=0, SD=1)
4. ✅ 3 spatial weight matrices berhasil dibuat
5. ✅ Temuan menarik: korelasi antar wilayah sangat tinggi (96-99%)
6. ✅ Optimal data split berhasil (96 training, 12 testing)
7. ✅ Clean temporal separation tanpa data leakage

**Insight penting:**
- Curah hujan Surabaya sangat homogen antar wilayah
- Efek spasial mungkin tidak terlalu signifikan
- Distance weights kemungkinan memberikan hasil terbaik
- Split 89-11% memberikan balance optimal antara learning dan evaluation
- Data 2024 memiliki variabilitas lebih rendah (cocok untuk testing)

**Phase 2 Progress:**
- ✅ **STACF Analysis completed**: MA(12,1) identified
- ✅ **Seasonal pattern detected**: Strong negative correlation at lag 12
- ✅ **Spatial consistency confirmed**: All weight types show identical patterns
- ✅ **60 significant lags found**: Comprehensive dependency structure
- ✅ **STPACF Analysis completed**: AR(12,0) identified
- ✅ **Complete STARMA order determined**: STARMA(12,0; 12,1)

**Key Findings dari STACF & STPACF:**
- **Seasonal STARMA confirmed**: Lag 12 signifikan di STACF (-0.363) dan STPACF (-0.257)
- **AR vs MA difference**: AR hanya temporal (12,0), MA temporal+spatial (12,1)
- **Spatial structure**: Minimal AR spatial, ada MA spatial spillover
- **Model complexity**: Seasonal temporal dominan, spatial effect minimal tapi ada
- **Complete model**: STARMA(12,0; 12,1) ready untuk estimation phase

---

---

## 💻 **Tips Coding untuk Mengajarkan STARMA**

### **1. Struktur Kode yang Baik**
```r
# Selalu gunakan header yang jelas
# ============================================================================
# Nama_File.R - Deskripsi Singkat
# ============================================================================
# Purpose: Tujuan script ini
# Expected Output: Apa yang dihasilkan
# Dataset: Data apa yang digunakan
# ============================================================================
```

### **2. Error Handling yang Robust**
```r
# Selalu cek keberadaan file/variabel
if (!exists("data") || is.null(data)) {
  stop("Data tidak ditemukan!")
}

# Validasi input
if (nrow(data) < 100) {
  warning("Data terlalu sedikit untuk STARMA")
}
```

### **3. Dokumentasi dalam Kode**
```r
# Jelaskan setiap langkah penting
centered_data <- stcenter(starma_matrix)  # KUNCI: Global centering

# Berikan konteks mengapa melakukan sesuatu
diag(weights) <- 0  # Tidak ada self-influence dalam spatial weights
```

### **4. Validation dan Quality Checks**
```r
# Selalu validasi hasil
if (abs(global_mean) > 1e-10) {
  warning("Centering tidak sempurna!")
}

# Berikan feedback yang informatif
cat("✅ Data centering berhasil: mean =", global_mean, "\n")
```

### **5. Modular dan Reusable Functions**
```r
# Buat function untuk task yang berulang
validate_weights <- function(weights, name) {
  # Validation logic here
  return(validation_results)
}
```

### **6. Konsistensi Naming Convention**
```r
# Gunakan naming yang konsisten dan deskriptif
starma_matrix      # Matrix utama
centered_data      # Data yang sudah di-center
uniform_weights    # Spatial weights uniform
train_data         # Training dataset
test_data          # Testing dataset
```

### **7. Output yang Informatif**
```r
# Berikan progress feedback
cat("=== PROCESSING STEP 1/5 ===\n")
cat("✅ Data loading completed\n")
cat("📊 Matrix dimensions:", dim(data), "\n")
```

### **8. File Management yang Baik**
```r
# Organisir output files dengan baik
save(data, file = "results/stationer/step1_data.RData")
write.csv(summary, "results/stationer/step1_summary.csv")
writeLines(report, "results/stationer/step1_report.txt")
```

---

## 🎓 **Konsep Coding Penting untuk STARMA**

### **1. Matrix Operations**
```r
# STARMA bekerja dengan matrix, bukan data.frame
starma_matrix <- as.matrix(data)  # Konversi ke matrix
colnames(starma_matrix) <- regions  # Set nama kolom
```

### **2. Global vs Local Operations**
```r
# Global centering (STARMA)
global_mean <- sum(data) / (nrow(data) * ncol(data))

# Local centering (biasa)
col_means <- colMeans(data)
```

### **3. Spatial Weights Format**
```r
# STARMA butuh format wlist (list of matrices)
wlist <- list(weight_matrix)  # Bukan matrix langsung
```

### **4. Data Validation Pattern**
```r
# Pattern validasi yang konsisten
validate_data <- function(data, name) {
  cat("Validating", name, ":\n")
  cat("- Dimensions:", dim(data), "\n")
  cat("- Missing values:", sum(is.na(data)), "\n")
  cat("- Data type:", class(data), "\n")
  return(all_checks_passed)
}
```

### **5. Reproducible Research Practices**
```r
# Set seed untuk reproducibility
set.seed(123)

# Document session info
sessionInfo()  # Simpan di akhir script

# Version control friendly
# Hindari absolute paths, gunakan relative paths
```

---

*Tutorial ini dibuat berdasarkan implementasi nyata STARMA forecasting untuk curah hujan Surabaya menggunakan metodologi Box-Jenkins yang diperluas dan paket starma di R.*