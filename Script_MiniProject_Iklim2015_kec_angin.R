
############################################################
# MINI PROJECT - Data Iklim 2015
# Variabel dipilih: Kecepatan Angin (kec_angin)
############################################################

rm(list = ls())
cat("\f")

# install.packages
install.packages("janitor")

# panggil packages
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)

#################################################################
# 1) Baca data (ambil file CSV secara manual di choose directory)
#################################################################
data_iklim <- read_csv(file.choose(), show_col_types = FALSE) %>%
  clean_names()
View(data_iklim)
data_iklim <- data_iklim %>% select(-any_of(c("x1", "...1")))
View(data_iklim)

# Memilih variabel iklim
var_pilih <- "kec_angin"

mydata_analisis <- data_iklim %>%
  select(provinsi, pulau, tahun, all_of(var_pilih)) %>%
  rename(nilai = all_of(var_pilih)) %>%
  mutate(
    pulau = as.factor(pulau),
    nilai = as.numeric(nilai)
  )
names(mydata_analisis)
View(mydata_analisis)
head(mydata_analisis)
glimpse(mydata_analisis)

################################################################
# 2) Visualisasi Data - BOX PLOT: Kecepatan Angin per Pulau
################################################################
v_boxplot <- ggplot(mydata_analisis, aes(x = pulau, y = nilai)) +
  geom_boxplot(outlier.alpha = 0.7) +
  labs(
    title = "Boxplot Kecepatan Angin per Pulau (Indonesia, 2015)",
    x = "Pulau",
    y = "Kecepatan angin"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

print(v_boxplot)

library(forcats)

# 1) jumlah data per pulau (n)
n_pulau <- mydata_analisis %>%
  count(pulau, name = "n")

# 2) gabung n ke data + buat pulau_label
plot_data <- mydata_analisis %>%
  left_join(n_pulau, by = "pulau") %>%
  mutate(
    pulau = fct_reorder(pulau, nilai, .fun = median, na.rm = TRUE),
    pulau_label = paste0(as.character(pulau), "\n(n=", n, ")")
  )

# 3) cek kolom 
names(plot_data)

p_box_safe <- ggplot(plot_data, aes(x = pulau_label, y = nilai)) +
  geom_boxplot(aes(fill = pulau_label), alpha = 0.75, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.8,
    size = 3.5
  ) +
  labs(
    title = "Boxplot Kecepatan Angin per Pulau (Indonesia, 2015)",
    subtitle = "Angka menunjukkan median; titik menunjukkan data provinsi",
    x = "Pulau",
    y = "Kecepatan angin"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

print(p_box_safe)

theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("boxplot_kec_angin_per_pulau_safe.png", p_box_safe,
       width = 12, height = 6, dpi = 300)

# Simpan gambar boxplot (tersimpan di working directory)
ggsave("boxplot_kec_angin_per_pulau.png", p_box_safe, width = 10, height = 5, 
       dpi = 300)

############################################################
# 3) STATISTIK DESKRIPTIF: mean, median, modus, min, max, sd
############################################################
modus <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

stat_nasional <- mydata_analisis %>%
  summarise(
    n = sum(!is.na(nilai)),
    mean = mean(nilai, na.rm = TRUE),
    median = median(nilai, na.rm = TRUE),
    mode = modus(nilai),
    min = min(nilai, na.rm = TRUE),
    max = max(nilai, na.rm = TRUE),
    sd = sd(nilai, na.rm = TRUE)
  )

cat("\n=== Statistik Deskriptif Nasional (Kecepatan Angin) ===\n")
print(stat_nasional)

stat_per_pulau <- mydata_analisis %>%
  group_by(pulau) %>%
  summarise(
    n = sum(!is.na(nilai)),
    mean = mean(nilai, na.rm = TRUE),
    median = median(nilai, na.rm = TRUE),
    mode = modus(nilai),
    min = min(nilai, na.rm = TRUE),
    max = max(nilai, na.rm = TRUE),
    sd = sd(nilai, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

cat("\n=== Statistik Deskriptif per Pulau (Kecepatan Angin) ===\n")
print(stat_per_pulau)

# Simpan tabel
write_csv(stat_nasional, "stat_deskriptif_nasional_kec_angin.csv")
write_csv(stat_per_pulau, "stat_deskriptif_per_pulau_kec_angin.csv")

############################################################
# 4) UJI INFERENSIAL: 2-sample t-test Sumatera vs Kalimantan
############################################################
dat_sk <- mydata_analisis %>%
  filter(pulau %in% c("Sumatera", "Kalimantan")) %>%
  filter(!is.na(nilai)) %>%
  droplevels()

cat("\nJumlah data per pulau (Sumatera vs Kalimantan):\n")
print(table(dat_sk$pulau))

cat("\n=== Hipotesis ===\n")
cat("H0: Rata-rata kecepatan angin Sumatera = rata-rata kecepatan angin Kalimantan\n")
cat("H1: Rata-rata kecepatan angin Sumatera ≠ rata-rata kecepatan angin Kalimantan\n")

uji_t <- t.test(nilai ~ pulau, data = dat_sk, alternative = "two.sided")

cat("\n=== Hasil 2-sample t-test (Welch) ===\n")
print(uji_t)

cat("\nRingkasan:\n")
cat("t-statistic :", unname(uji_t$statistic), "\n")
cat("df          :", unname(uji_t$parameter), "\n")
cat("p-value     :", uji_t$p.value, "\n")

alpha <- 0.05
cat("\n=== Kesimpulan (alpha = 0.05) ===\n")
if (uji_t$p.value < alpha) {
  cat("Tolak H0: terdapat perbedaan rata-rata kecepatan angin yang signifikan antara Sumatera dan Kalimantan.\n")
} else {
  cat("Gagal menolak H0: tidak ada bukti perbedaan rata-rata kecepatan angin yang signifikan antara Sumatera dan Kalimantan.\n")
}

cat("\nSelesai. File output tersimpan:\n",
    "- boxplot_kec_angin_per_pulau.png\n",
    "- stat_deskriptif_nasional_kec_angin.csv\n",
    "- stat_deskriptif_per_pulau_kec_angin.csv\n")
