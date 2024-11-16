# Carissa Fisa Azzahra (22106010034)
# UTS Analisis Data

# Nomor 2
setwd("E:/SEMESTER 5/Analisis Data")
getwd()

# b. Membaca data
students_data <- read.csv("StudentsPerformance_DataUTS_Carissa.csv", sep = ";", header = TRUE)
View(students_data)

# c. Mengecek jumlah baris pada dataset
nrow(students_data)

# Mengecek jumlah kolom pada dataset
ncol(students_data)

# Melihat struktur dari dataset
str(students_data)

# d. Melihat data pada bagian atas dan bawah untuk memastikan data sudah benar
head(students_data)
tail(students_data)

# e. Menampilkan jumlah observasi untuk setiap kategori tingkat pendidikan orang tua
head(table(students_data$parental.level.of.education))

# Memuat library dplyr
library(dplyr)

# Memfilter data untuk siswa dengan skor matematika di atas 90
filter(students_data, math.score > 90) %>%
  select(parental.level.of.education, math.score)

# Memfilter data untuk siswa dengan tingkat pendidikan orang tua "bachelor's degree"
filter(students_data, parental.level.of.education == "bachelor's degree") %>%
  select(parental.level.of.education, math.score) %>%
  as.data.frame()

# Menampilkan jumlah unik tingkat pendidikan orang tua
select(students_data, parental.level.of.education) %>% unique() %>% nrow()

# Melihat daftar tingkat pendidikan orang tua yang unik
unique(students_data$parental.level.of.education)


# f. Menampilkan ringkasan statistik untuk kolom skor matematika
summary(students_data$math.score)

# Menampilkan desil dari skor matematika siswa untuk melihat distribusi secara rinci
quantile(students_data$math.score, seq(0, 1, 0.1))


# g. Memuat package ggplot2
library(ggplot2)

# Membuat boxplot skor matematika berdasarkan tingkat pendidikan orang tua
ggplot(students_data, aes(x = parental.level.of.education, y = math.score)) +
  geom_boxplot() +
  labs(title = "Distribusi Skor Matematika Berdasarkan Tingkat Pendidikan Orang Tua",
       x = "Tingkat Pendidikan Orang Tua",
       y = "Skor Matematika") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# h. Mengelompokkan tingkat pendidikan orang tua menjadi dua kategori: "rendah" dan "tinggi"
students_data$education_level <- ifelse(students_data$parental.level.of.education %in% c("associate's degree", "high school", "some high school"), "rendah", "tinggi")

# Menghitung rata-rata dan median skor matematika berdasarkan kategori tingkat pendidikan
library(dplyr)
summary_stats <- students_data %>%
  group_by(education_level) %>%
  summarize(mean_math = mean(math.score, na.rm = TRUE),
            median_math = median(math.score, na.rm = TRUE))

print(summary_stats)

# Membuat boxplot untuk visualisasi
library(ggplot2)
ggplot(students_data, aes(x = education_level, y = math.score)) +
  geom_boxplot() +
  labs(title = "Distribusi Skor Matematika Berdasarkan Tingkat Pendidikan Orang Tua",
       x = "Tingkat Pendidikan Orang Tua",
       y = "Skor Matematika")

# h. Uji ANOVA untuk melihat perbedaan rata-rata nilai matematika berdasarkan pendidikan orang tua
anova_result <- aov(math.score ~ parental.level.of.education, data = students_data)
summary(anova_result)





# Nomor 3
# a. Data rata-rata (mean) dan standar deviasi (standard deviation)
mean_math <- mean(students_data$math.score, na.rm = TRUE)
print(mean_math) # Mean skor matematika
sd_math <- sd(students_data$math.score, na.rm = TRUE)
print(sd_math)  # Standar deviasi skor matematika

# Membuat range nilai untuk skor matematika
x <- seq(mean_math - 4 * sd_math, mean_math + 4 * sd_math, by = 0.1)

# Menghitung probabilitas untuk distribusi normal
y <- dnorm(x, mean = mean_math, sd = sd_math)

# Plot distribusi normal
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "Skor Matematika",
     ylab = "Kepadatan Probabilitas",
     main = "Kurva Distribusi Normal Skor Matematika")

# Menambahkan garis vertikal pada nilai rata-rata
abline(v = mean_math, col = "red", lty = 2, lwd = 2)

# Menambahkan legenda
legend("topright", legend = c("Distribusi Normal", "Rata-rata"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)


pnorm(80, mean = mean_math, sd = sd_math, lower.tail = FALSE)


# b. Load library
library(ggplot2)
library(gridExtra)

# Data asli: Skor Matematika Siswa
set.seed(42)  # Untuk reproducibility
math_scores <- rnorm(1000, mean = mean_math, sd = sd_math)  # Data aktual

# Data normal: Data simulasi dari distribusi normal dengan mean dan sd yang sama
normal_data <- rnorm(1000, mean = mean(math_scores), sd = sd(math_scores))

# Histogram untuk data asli
plot_real <- ggplot(data.frame(score = math_scores), aes(x = score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Histogram of Real Data",
    x = "Math Score",
    y = "Density"
  ) +
  theme_minimal()

# Histogram untuk data normal
plot_normal <- ggplot(data.frame(score = normal_data), aes(x = score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", alpha = 0.7) +
  labs(
    title = "Histogram of Normal Distribution",
    x = "Math Score",
    y = "Density"
  ) +
  theme_minimal()

# Tampilkan kedua plot berdampingan
grid.arrange(plot_real, plot_normal, ncol = 2)


# c. Load library
library(MASS)  # Untuk fungsi fitdistr

# Filter data untuk nilai positif
valid_data <- students_data$math.score[students_data$math.score > 0]
print(summary(valid_data))

# Hapus nilai NA
valid_data <- na.omit(valid_data)
print(summary(valid_data))

# Fit Gamma distribution ke data yang valid
gamma_fit <- fitdistr(valid_data, "gamma")
print(gamma_fit)

# Plot distribusi Gamma dengan data aktual
hist(students_data$math.score, freq = FALSE, breaks = 30, main = "Gamma Fit", xlab = "Math Score")
curve(dgamma(x, shape = gamma_fit$estimate['shape'], rate = gamma_fit$estimate['rate']), 
      col = "red", add = TRUE, lwd = 2)


# Pastikan kita hanya mengambil kolom skor matematika
data_math <- students_data$math.score

# Buat histogram dengan distribusi empiris
hist(data_math, 
     breaks = 15, 
     probability = TRUE, 
     col = "gray", 
     xlab = "Math Score", 
     main = "Empirical Distribution")

# Tambahkan kurva density kernel (KDE)
lines(density(data_math), 
      col = "blue", 
      lwd = 2)

# Tambahkan legend
legend("topright", 
       legend = c("Empirical Density"), 
       col = c("blue"), 
       lwd = 2)

