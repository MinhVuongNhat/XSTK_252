# install.packages("stringr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("zoo")
# install.packages("Metrics")
# install.packages("caret")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("mltools")
# install.packages("DescTools")
# install.packages("plotly")
# install.packages("car")
# install.packages("caret")
# install.packages("effectsize")
# install.packages("boot")
# install.packages("rstatix")
# install.packages("PMwR")  
# install.packages("FSA")  
# install.packages("jsonlite")
# install.packages("mice")

# Thư viện
library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)      # mae(), mse()
library(caret)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools)
library(plotly)
library(car)          # vif(), leveneTest(), ncvTest()
library(caret)        # createDataPartition(), train()
library(effectsize)   # eta_squared()
library(boot)
library(rstatix)    # kruskal_test, dunn_test
library(PMwR)       # welch_anova, nếu dùng oneway.test
library(FSA)        # dunnTest cho Kruskal post-hoc
library(glue)
library(mice)

#--------------------------------------------------
# 1. Khám phá dữ liệu
#--------------------------------------------------

df_raw <- read.csv(
  "All_GPUs.csv", 
  header = TRUE, 
  na.strings = c("", "\n- ", "\n", "\nUnknown Release Date ")
)

print(glue("Dataframe dimension: {nrow(df_raw)} dòng x {ncol(df_raw)} cột"))
head(df_raw)

print("========= Data Summary ===========")
summary(df_raw)
print("==================================")

na_percentage <- colMeans(is.na(df_raw))
print("======== NA Percentage ========")
print(round(na_percentage, 2))

# Delete column with missing percentage over 30%
features_kept <- na_percentage <= 0.3
df_proc <- df_raw[, features_kept]
dim(df_proc)

#--------------------------------------------------
# 2. Tiền xử lí số liệu
#--------------------------------------------------
# 2.1. Định nghĩa hàm helper
# 2.1.1. Hàm unit_splitter
unit_splitter <- function(data, column_name) {
  col_value <- paste0(column_name, "_cvalue")
  col_unit  <- paste0(column_name, "_cunit")

  data %>%
    extract(
      col = all_of(column_name),
      into = c(col_value, col_unit),
      regex = "^([0-9.]+)\\s*(.*)$", 
      convert = TRUE
    )
}
# 2.1.2. Hàm to_num
to_num <- function(x) {
  cleaned <- gsub("[^0-9\\.]", "", x)
  as.numeric(cleaned)
}

# 2.2. Xử lý từng biến một
# 2.2.1. Core_Speed
df_proc <- unit_splitter(df_proc, "Core_Speed") 
df_proc <- df_proc %>%
  rename(Core_Speed = Core_Speed_cvalue)
df_proc <- df_proc %>% 
  select(-Core_Speed_cunit)


# 2.2.2. Pixel_Rate
df_proc <- unit_splitter(df_proc, "Pixel_Rate")
df_proc <- df_proc %>% 
  rename(Pixel_Rate = Pixel_Rate_cvalue) %>%
  select(-Pixel_Rate_cunit)

# 2.2.3. Memory_Speed, Memory_Bus, Memory, Process
for (col in c("Memory_Speed", "Memory_Bus", "Memory", "Process")) {
  df_proc <- unit_splitter(df_proc, col)
  df_proc[[col]] <- df_proc[[paste0(col, "_cvalue")]]
  df_proc[[paste0(col, "_cvalue")]] <- NULL
  df_proc[[paste0(col, "_cunit")]] <- NULL
}

# 2.2.4. ROPs & TMUs
df_proc$ROPs <- as.numeric(sub("^([0-9]+).*", "\\1", df_proc$ROPs))
df_proc$TMUs <- as.numeric(sub("^([0-9]+).*", "\\1", df_proc$TMUs))

# 2.2.5. Shader
df_proc$Shader <- as.numeric(df_proc$Shader)

# 2.2.6. Resolution_WxH
df_proc$Resolution_WxH <- ifelse(
  df_proc$Resolution_WxH %in% c("1024x768", "1280x1024", "1366x768"), "Low/Legacy",
  ifelse(df_proc$Resolution_WxH == "1920x1080", "Standard_FHD", "High_Resolution")
)
df_proc$Resolution_WxH <- as.factor(df_proc$Resolution_WxH)

# 2.2.7. Memory_Type
df_proc$Memory_Type <- gsub("\n", "", df_proc$Memory_Type)
df_proc$Memory_Type <- as.factor(df_proc$Memory_Type)

# 2.2.8. Manufacturer
df_proc$Manufacturer <- as.factor(df_proc$Manufacturer)

# 2.3. Loại bỏ Outlier
numerical_cols <- c("Core_Speed", "Pixel_Rate", "Memory_Speed", "Memory_Bus", 
                    "Memory", "Process", "ROPs", "TMUs", "Shader")
for (var in numerical_cols) {
  Q1 <- quantile(df_proc[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_proc[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  df_proc <- df_proc[df_proc[[var]] >= (Q1 - 1.5*IQR) & df_proc[[var]] <= (Q3 + 1.5*IQR), ]
}

# 2.4. Loại bỏ Duplicate
df_proc <- distinct(df_proc)

# 2.5. Xử lý na
## Phân loại giá trị rỗng của biến phân loại là "Missing"
df_proc <- df_proc %>%
  mutate(across(where(is.character), ~replace_na(., "Missing")))

## Xử lý na của biến liên tục với MICE
mice_processor <- mice(df_proc , m = 5, maxit = 5, method = 'pmm', seed = 123)
df_proc <- complete(mice_processor, 1)

# 2.5. Định nghĩa danh sách biến
# Danh sách các biến số
numerical_vars <- c("Core_Speed", "Pixel_Rate", "Memory", "Memory_Speed", 
                    "Memory_Bus", "Process", "ROPs", "Shader", "TMUs")

# Danh sách các biến phân loại
categorical_vars <- c("Architecture", "Manufacturer", "Memory_Type", 
                      "Direct_X", "Open_GL", "Resolution_WxH")

# 2.6. Tạo bảng log-transform
df_proc_log <- df_proc
df_proc_log[, numerical_vars] <- log(df_proc_log[, numerical_vars] + 1)

#--------------------------------------------------
# 3. Thống kê mô tả và Trực quan hóa dữ liệu
#--------------------------------------------------

#--------------------------------------------------
# 4. Phân tích tương quan
#--------------------------------------------------

#--------------------------------------------------
# 5. ANOVA
#--------------------------------------------------

#--------------------------------------------------
# 6. Kiểm định một mẫu
#--------------------------------------------------
cat("\n==========================================================\n")
cat("PHẦN 6: PHÂN TÍCH KIỂM ĐỊNH MỘT MẪU CHO PIXEL_RATE\n")
cat("Mục tiêu: Đánh giá hiệu năng thực tế so với ngưỡng 40 GPixel/s\n")
cat("==========================================================\n")

# Bước 1: Thiết lập tham số giả thuyết
# Vì dữ liệu dùng log(x + 1) nên mu_0 = log(40 + 1)
mu_0 <- log(40 + 1)
alpha <- 0.05

# Bước 2: Kiểm tra giả định phân phối chuẩn (Normality)
shapiro_p <- shapiro.test(df_proc_log$Pixel_Rate)$p.value
cat(glue("\n[1] KIỂM TRA TÍNH CHUẨN (Shapiro-Wilk test):\n"))
cat(glue("    - p-value = {format(shapiro_p, scientific = TRUE)}\n"))

if (shapiro_p > alpha) {
  # TRƯỜNG HỢP DỮ LIỆU CHUẨN
  cat("    => NHẬN ĐỊNH: p > 0.05, dữ liệu tuân theo phân phối chuẩn.\n")
  cat("    => PHƯƠNG PHÁP: Sử dụng kiểm định tham số One-sample T-test.\n\n")
  
  res <- t.test(df_proc_log$Pixel_Rate, mu = mu_0)
  p_val <- res$p.value
  method_name <- "One-sample T-test"
} else {
  # TRƯỜNG HỢP DỮ LIỆU KHÔNG CHUẨN
  cat("    => NHẬN ĐỊNH: p <= 0.05, dữ liệu vi phạm giả định phân phối chuẩn.\n")
  cat("    => PHƯƠNG PHÁP: Sử dụng kiểm định phi tham số Wilcoxon Signed-Rank Test.\n\n")
  
  res <- wilcox.test(df_proc_log$Pixel_Rate, mu = mu_0, conf.int = TRUE)
  p_val <- res$p.value
  method_name <- "Wilcoxon Signed-Rank Test"
}

# Bước 3: In kết quả kiểm định chính
cat(glue("[2] KẾT QUẢ KIỂM ĐỊNH ({method_name}):\n"))
cat(glue("    - p-value thu được: {format(p_val, scientific = TRUE)}\n"))

if (p_val < alpha) {
  cat("    => KẾT LUẬN THỐNG KÊ: Bác bỏ giả thuyết H0 ở mức ý nghĩa 5%.\n")
  cat(glue("    => Ý NGHĨA: Có sự khác biệt đáng kể giữa hiệu năng thực tế và ngưỡng 40 GPixel/s.\n"))
} else {
  cat("    => KẾT LUẬN THỐNG KÊ: Chưa đủ cơ sở bác bỏ giả thuyết H0.\n")
  cat(glue("    => Ý NGHĨA: Hiệu năng thực tế xấp xỉ đạt ngưỡng mục tiêu 40 GPixel/s.\n"))
}

# Bước 4: Đánh giá độ lớn của sự khác biệt (Effect Size)
# Chỉ định rõ thư viện effectsize để tránh xung đột
d_res <- effectsize::cohens_d(df_proc_log$Pixel_Rate, mu = mu_0)
d_val <- abs(d_res$Cohens_d)

cat(glue("\n[3] ĐÁNH GIÁ MỨC ĐỘ ẢNH HƯỞNG (Effect Size - Cohen's d):\n"))
cat(glue("    - Cohen's d = {round(d_val, 3)}\n"))

if (d_val < 0.2) {
  interpretation <- "Rất nhỏ (Negligible)"
} else if (d_val < 0.5) {
  interpretation <- "Nhỏ (Small)"
} else if (d_val < 0.8) {
  interpretation <- "Trung bình (Medium)"
} else {
  interpretation <- "Lớn (Large)"
}
cat(glue("    => NHẬN ĐỊNH: Sự khác biệt có quy mô: {interpretation}.\n"))

# Bước 5: Trực quan hóa
if(!is.null(dev.list())) dev.off() # Reset graphics device
ggplot(df_proc_log, aes(x = Pixel_Rate)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  geom_vline(aes(xintercept = mu_0, color = "Mục tiêu (40)"), linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = mean(Pixel_Rate), color = "Thực tế trung bình"), size = 1.2) +
  scale_color_manual(name = "Đường tham chiếu", values = c("Mục tiêu (40)" = "red", "Thực tế trung bình" = "darkgreen")) +
  labs(title = "BIỂU ĐỒ PHÂN PHỐI HIỆU NĂNG PIXEL_RATE",
       subtitle = glue("Kiểm định dựa trên ngưỡng mu_0 = {round(mu_0, 3)}"),
       x = "Giá trị log(Pixel_Rate + 1)", y = "Mật độ phân phối") +
  theme_minimal()
#--------------------------------------------------
# 7. Kiểm định hai mẫu
#--------------------------------------------------
cat("\n================================================================\n")
cat("PHẦN 7: PHÂN TÍCH SO SÁNH HIỆU NĂNG GIỮA NVIDIA VÀ AMD\n")
cat("Mục tiêu: So sánh Pixel_Rate giữa hai nhà sản xuất chính\n")
cat("================================================================\n")

# Lọc dữ liệu so sánh
df_compare <- df_proc_log %>% 
  filter(Manufacturer %in% c("Nvidia", "AMD")) %>%
  mutate(Manufacturer = factor(Manufacturer))

alpha <- 0.05

# --- BƯỚC 1: KIỂM TRA TÍNH CHUẨN ---
cat("\nBƯỚC 1: Kiểm tra giả định phân phối chuẩn (Normality Test)\n")
norm_results <- df_compare %>%
  group_by(Manufacturer) %>%
  summarise(p_val = shapiro.test(Pixel_Rate)$p.value)
print(norm_results)

is_normal <- all(norm_results$p_val > alpha)

if (is_normal) {
  cat("=> Ý NGHĨA: Cả hai nhóm đều có p > 0.05, thỏa mãn giả định phân phối chuẩn.\n")
  cat("=> KẾT LUẬN: Chuyển sang kiểm tra tính đồng nhất phương sai để chọn loại T-test.\n")
} else {
  cat("=> Ý NGHĨA: Có ít nhất một nhóm có p <= 0.05, vi phạm giả định phân phối chuẩn.\n")
  cat("=> KẾT LUẬN: Sử dụng kiểm định phi tham số (Mann-Whitney U) - phương pháp này không phụ thuộc vào hình dạng phân phối.\n")
}

# --- BƯỚC 2: KIỂM TRA PHƯƠNG SAI (Chỉ thực hiện nếu dữ liệu chuẩn) ---
cat("\nBƯỚC 2: Kiểm tra tính đồng nhất phương sai (Homogeneity of Variance)\n")
levene_p <- car::leveneTest(Pixel_Rate ~ Manufacturer, data = df_compare)$`Pr(>F)`[1]
cat(glue("   - Kết quả Levene's Test: p-value = {round(levene_p, 4)}\n"))

if (levene_p > alpha) {
  cat("=> Ý NGHĨA: Phương sai hai nhóm tương đồng (p > 0.05).\n")
} else {
  cat("=> Ý NGHĨA: Phương sai hai nhóm khác biệt đáng kể (p <= 0.05).\n")
}

# --- BƯỚC 3: THỰC HIỆN KIỂM ĐỊNH CHÍNH ---
cat("\nBƯỚC 3: Thực hiện phép kiểm định so sánh giá trị trung tâm\n")

if (is_normal) {
  if (levene_p > alpha) {
    cat("=> LỰA CHỌN: Student's T-test (do thỏa mãn cả tính chuẩn và đồng nhất phương sai).\n")
    res <- t.test(Pixel_Rate ~ Manufacturer, data = df_compare, var.equal = TRUE)
  } else {
    cat("=> LỰA CHỌN: Welch T-test (do dữ liệu chuẩn nhưng phương sai không đồng nhất).\n")
    res <- t.test(Pixel_Rate ~ Manufacturer, data = df_compare, var.equal = FALSE)
  }
} else {
  cat("=> LỰA CHỌN: Mann-Whitney U test (Wilcoxon Rank-Sum) (do dữ liệu không chuẩn).\n")
  res <- wilcox.test(Pixel_Rate ~ Manufacturer, data = df_compare, conf.int = TRUE)
}

cat("KẾT QUẢ:\n")
print(res)

# --- BƯỚC 4: KẾT LUẬN VỀ SỰ KHÁC BIỆT ---
cat("\nBƯỚC 4: Kết luận về ý nghĩa thống kê\n")
p_val_main <- res$p.value

if (p_val_main < alpha) {
  cat(glue("=> KẾT LUẬN: p-value ({format(p_val_main, scientific = TRUE)}) < 0.05. BÁC BỎ H0.\n"))
  cat("=> Ý NGHĨA THỰC TẾ: Có sự khác biệt đáng kể về năng lực xử lý Pixel_Rate giữa Nvidia và AMD.\n")
} else {
  cat(glue("=> KẾT LUẬN: p-value ({format(p_val_main, scientific = TRUE)}) >= 0.05. CHƯA ĐỦ CƠ SỞ BÁC BỎ H0.\n"))
  cat("=> Ý NGHĨA THỰC TẾ: Hiệu năng của hai hãng tương đương nhau trong tập dữ liệu này.\n")
}

# --- BƯỚC 5: ĐÁNH GIÁ ĐỘ LỚN (EFFECT SIZE) ---
cat("\nBƯỚC 5: Đánh giá quy mô sự khác biệt (Effect Size)\n")
d_res <- effectsize::cohens_d(Pixel_Rate ~ Manufacturer, data = df_compare)
d_abs <- abs(d_res$Cohens_d)

cat(glue("   - Chỉ số Cohen's d: {round(d_abs, 3)}\n"))

if (d_abs < 0.2) interpretation <- "Rất nhỏ" else 
  if (d_abs < 0.5) interpretation <- "Nhỏ" else 
    if (d_abs < 0.8) interpretation <- "Trung bình" else interpretation <- "Lớn"

cat(glue("=> Ý NGHĨA: Quy mô khác biệt được đánh giá là: {interpretation}.\n"))
cat("=> Điều này giúp xác định xem sự khác biệt có thực sự quan trọng trong ứng dụng thực tế hay không.\n")

# --- BƯỚC 6: TRỰC QUAN HÓA ---
if(!is.null(dev.list())) dev.off()
ggplot(df_compare, aes(x = Manufacturer, y = Pixel_Rate, fill = Manufacturer)) +
  geom_violin(alpha = 0.3, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = 16, outlier.alpha = 0.4) +
  stat_summary(fun = mean, geom = "point", color = "yellow", size = 3) +
  scale_fill_manual(values = c("Nvidia" = "#76b900", "AMD" = "#ed1c24")) +
  labs(title = "SO SÁNH PIXEL_RATE GIỮA NVIDIA VÀ AMD",
       subtitle = glue("P-value chính: {format(p_val_main, scientific=T)} \n Quy mô khác biệt: {interpretation}"),
       y = "log(Pixel_Rate + 1)", x = "Nhà sản xuất") +
  theme_minimal() + theme(legend.position = "none")
  
#--------------------------------------------------
# 8. Xây dựng mô hình
#--------------------------------------------------

#--------------------------------------------------
# 9. Đánh giá và Dự đoán
#--------------------------------------------------