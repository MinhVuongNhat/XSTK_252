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
# 4. Phân tích tương quan Pearson
#--------------------------------------------------
cat("\n==========================================================\n")
cat("PHẦN 4: PHÂN TÍCH TƯƠNG QUAN PEARSON\n")
cat("Mục tiêu: Đo lường mức độ tương quan tuyến tính giữa\n")
cat("các biến định lượng độc lập với biến mục tiêu Pixel_Rate\n")
cat("==========================================================\n")

# Chia tập train/test (2/3 – 1/3)
set.seed(42)
train_idx <- createDataPartition(df_proc_log$Pixel_Rate, p = 2/3, list = FALSE)
train <- df_proc_log[train_idx, ]
test  <- df_proc_log[-train_idx, ]
cat(glue("\nTập huấn luyện: {nrow(train)} quan sát | Tập kiểm tra: {nrow(test)} quan sát\n"))

# Biến độc lập (loại Pixel_Rate khỏi danh sách)
pred_vars  <- setdiff(numerical_vars, "Pixel_Rate")
alpha_corr <- 0.05

# --- BƯỚC 1: TÍNH HỆ SỐ TƯƠNG QUAN VÀ P-VALUE ---
cat("\nBƯỚC 1: Tính hệ số tương quan Pearson giữa từng biến số và Pixel_Rate\n")
cat("  H0: rho = 0 (không có tương quan tuyến tính)\n")
cat("  H1: rho ≠ 0 (có tương quan tuyến tính có ý nghĩa)\n")
cat("  Mức ý nghĩa: alpha = 0.05\n\n")

corr_results <- data.frame(
  Bien       = character(),
  Pearson_r  = numeric(),
  P_Value    = numeric(),
  Y_nghia    = character(),
  Muc_do     = character(),
  stringsAsFactors = FALSE
)

for (var in pred_vars) {
  ct    <- cor.test(train[[var]], train$Pixel_Rate, method = "pearson")
  r_val <- round(unname(ct$estimate), 4)
  p_val <- ct$p.value

  y_nghia <- ifelse(p_val < alpha_corr, "Co y nghia (p<0.05)", "Khong co y nghia")
  muc_do  <- ifelse(abs(r_val) >= 0.7, "Manh",
              ifelse(abs(r_val) >= 0.5, "Trung binh kha",
              ifelse(abs(r_val) >= 0.3, "Trung binh", "Yeu")))

  corr_results <- rbind(corr_results, data.frame(
    Bien      = var,
    Pearson_r = r_val,
    P_Value   = round(p_val, 6),
    Y_nghia   = y_nghia,
    Muc_do    = muc_do,
    stringsAsFactors = FALSE
  ))
}

corr_results <- corr_results[order(-abs(corr_results$Pearson_r)), ]
cat("Ket qua bang tuong quan Pearson voi Pixel_Rate (sap xep theo |r| giam dan):\n")
print(corr_results, row.names = FALSE)

# --- BƯỚC 2: MA TRẬN TƯƠNG QUAN ĐẦY ĐỦ ---
cat("\nBUOC 2: Ma tran tuong quan Pearson day du giua cac bien so\n")
cor_matrix <- cor(train[, numerical_vars], method = "pearson", use = "complete.obs")
cat("Ma tran tuong quan (lam tron 3 chu so):\n")
print(round(cor_matrix, 3))

# Tính p-value cho toàn bộ cặp biến
p_matrix <- matrix(NA, nrow = length(numerical_vars), ncol = length(numerical_vars),
                   dimnames = list(numerical_vars, numerical_vars))
for (i in numerical_vars) {
  for (j in numerical_vars) {
    p_matrix[i, j] <- if (i == j) 0 else cor.test(train[[i]], train[[j]])$p.value
  }
}

# --- BƯỚC 3: TRỰC QUAN HÓA HEATMAP TƯƠNG QUAN ---
cat("\nBUOC 3: Ve Heatmap ma tran tuong quan\n")
melted_cor <- reshape2::melt(cor_matrix)
colnames(melted_cor) <- c("Var1", "Var2", "r")
melted_p   <- reshape2::melt(p_matrix)
colnames(melted_p)   <- c("Var1", "Var2", "pval")
melted_cor <- merge(melted_cor, melted_p, by = c("Var1", "Var2"))

melted_cor$label <- ifelse(melted_cor$pval < 0.001, paste0(round(melted_cor$r, 2), "***"),
                    ifelse(melted_cor$pval < 0.01,  paste0(round(melted_cor$r, 2), "**"),
                    ifelse(melted_cor$pval < 0.05,  paste0(round(melted_cor$r, 2), "*"),
                                                     as.character(round(melted_cor$r, 2)))))

if (!is.null(dev.list())) dev.off()
print(
  ggplot(melted_cor, aes(x = Var1, y = Var2, fill = r)) +
    geom_tile(color = "white", linewidth = 0.4) +
    scale_fill_gradient2(low = "#2166ac", high = "#d73027", mid = "white",
                         midpoint = 0, limit = c(-1, 1), name = "Pearson r") +
    geom_text(aes(label = label), size = 2.5) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x   = element_text(angle = 45, hjust = 1),
          plot.title     = element_text(face = "bold", hjust = 0.5),
          plot.subtitle  = element_text(hjust = 0.5)) +
    labs(title    = "MA TRAN TUONG QUAN PEARSON (tren tap train, da log-transform)",
         subtitle = "* p<0.05  ** p<0.01  *** p<0.001",
         x = "", y = "") +
    coord_fixed()
)

# --- BƯỚC 4: SCATTER MATRIX (4 biến tương quan cao nhất với Pixel_Rate) ---
cat("\nBUOC 4: Scatter matrix – Pixel_Rate vs cac bien tuong quan cao nhat\n")
top4 <- corr_results %>%
  filter(Y_nghia == "Co y nghia (p<0.05)") %>%
  slice_head(n = 4) %>%
  pull(Bien)

if (length(top4) > 0) {
  if (!is.null(dev.list())) dev.off()
  pairs(train[, c(top4, "Pixel_Rate")],
        main = "Scatter Matrix: Pixel_Rate vs bien tuong quan cao (train set)",
        pch  = 16, cex = 0.4, col = rgb(0.2, 0.4, 0.8, 0.3))
}

# --- KẾT LUẬN PHẦN 4 ---
sig_corr_vars <- corr_results$Bien[corr_results$Y_nghia == "Co y nghia (p<0.05)"]
cat("\n[KET LUAN PHAN 4 – TUONG QUAN PEARSON]\n")
cat(glue("- Tong so bien so duoc kiem tra: {length(pred_vars)}\n"))
cat(glue("- So bien co tuong quan y nghia voi Pixel_Rate (p < 0.05): {length(sig_corr_vars)}\n"))
if (length(sig_corr_vars) > 0) {
  cat(glue("- Danh sach bien co y nghia: {paste(sig_corr_vars, collapse=', ')}\n"))
}
cat("- Cac bien nay la ung vien chinh de dua vao mo hinh hoi quy o phan sau.\n")


#--------------------------------------------------
# 5. Phân tích phương sai một yếu tố (ANOVA)
#--------------------------------------------------
cat("\n==========================================================\n")
cat("PHAN 5: PHAN TICH PHUONG SAI MOT YEU TO (ANOVA)\n")
cat("Muc tieu: Kiem dinh su khac biet trung binh Pixel_Rate\n")
cat("giua cac nhom cua tung bien phan loai (tren tap train)\n")
cat("==========================================================\n")

alpha_anova <- 0.05

# Chỉ dùng 3 biến phân loại chính (đủ dữ liệu, có ý nghĩa phân tích)
cat_vars_anova <- c("Manufacturer", "Memory_Type", "Resolution_WxH")

# Bảng tổng hợp kết quả
summary_anova <- data.frame(
  Bien_phan_loai = character(),
  F_value        = numeric(),
  p_value        = numeric(),
  Shapiro_W      = numeric(),
  Shapiro_p      = numeric(),
  Levene_F       = numeric(),
  Levene_p       = numeric(),
  Eta2           = numeric(),
  Ket_luan       = character(),
  stringsAsFactors = FALSE
)

for (cat_var in cat_vars_anova) {

  cat(glue("\n{'='*60}\n"))
  cat(glue("BIEN PHAN LOAI: {cat_var}\n"))
  cat(glue("{'='*60}\n"))

  df_sub <- train %>%
    filter(!is.na(.data[[cat_var]]), !is.na(Pixel_Rate)) %>%
    mutate(across(all_of(cat_var), as.factor)) %>%
    droplevels()

  cat(glue("\nPhan bo so luong quan sat theo tung nhom:\n"))
  print(table(df_sub[[cat_var]]))

  formula_str <- as.formula(paste("Pixel_Rate ~", cat_var))

  # ── XÂY DỰNG MÔ HÌNH ANOVA ──
  aov_mod <- aov(formula_str, data = df_sub)

  # ── BƯỚC 1: KIỂM TRA ĐIỀU KIỆN – SHAPIRO-WILK (trên phần dư) ──
  cat(glue("\nBUOC 1: Kiem tra dieu kien – Phan phoi chuan (Shapiro-Wilk tren phan du)\n"))
  cat("  Dieu kien: Phan du cua mo hinh ANOVA phai xap xi phan phoi chuan\n")
  cat("  H0: Phan du tuan theo phan phoi chuan\n")
  cat("  H1: Phan du khong tuan theo phan phoi chuan\n")

  sw_test <- shapiro.test(residuals(aov_mod))
  sw_W    <- round(sw_test$statistic, 5)
  sw_p    <- sw_test$p.value

  cat(glue("  Ket qua: W = {sw_W}, p-value = {format(sw_p, scientific=TRUE, digits=4)}\n"))
  if (sw_p > alpha_anova) {
    cat("  => p > 0.05: Khong bac bo H0. Phan du xap xi phan phoi chuan. Dieu kien THOA.\n")
  } else {
    cat("  => p <= 0.05: Bac bo H0. Phan du VI PHAM phan phoi chuan.\n")
    cat("     Luu y: Voi co mau lon, ANOVA van con tuong doi ben vung (robust).\n")
    cat("     Do do van tien hanh ANOVA va ghi nhan ket qua.\n")
  }

  # ── BƯỚC 2: KIỂM TRA ĐIỀU KIỆN – LEVENE (đồng nhất phương sai) ──
  cat(glue("\nBUOC 2: Kiem tra dieu kien – Dong nhat phuong sai (Levene's Test)\n"))
  cat("  Dieu kien: Phuong sai cua Pixel_Rate phai bang nhau giua cac nhom\n")
  cat("  H0: Phuong sai giua cac nhom bang nhau\n")
  cat("  H1: Co it nhat mot nhom co phuong sai khac\n")

  lev_res <- car::leveneTest(formula_str, data = df_sub)
  lev_F   <- round(lev_res$`F value`[1], 4)
  lev_p   <- lev_res$`Pr(>F)`[1]

  cat(glue("  Ket qua: F = {lev_F}, p-value = {format(lev_p, scientific=TRUE, digits=4)}\n"))
  if (lev_p > alpha_anova) {
    cat("  => p > 0.05: Khong bac bo H0. Phuong sai dong nhat. Dieu kien THOA.\n")
  } else {
    cat("  => p <= 0.05: Bac bo H0. Phuong sai KHONG dong nhat giua cac nhom.\n")
    cat("     Luu y: Van tien hanh ANOVA va ghi nhan ket qua.\n")
  }

  # ── BƯỚC 3: KIỂM ĐỊNH ANOVA ──
  cat(glue("\nBUOC 3: Kiem dinh ANOVA mot yeu to\n"))
  cat(glue("  H0: Trung binh Pixel_Rate bang nhau o tat ca cac nhom\n"))
  cat(glue("       mu_1 = mu_2 = ... = mu_k\n"))
  cat(glue("  H1: Ton tai it nhat mot cap nhom co trung binh khac nhau\n"))
  cat(glue("  Muc y nghia: alpha = {alpha_anova}\n\n"))

  aov_sum  <- summary(aov_mod)
  print(aov_sum)

  F_val    <- round(aov_sum[[1]]$`F value`[1], 4)
  p_val    <- aov_sum[[1]]$`Pr(>F)`[1]

  cat(glue("\n  => F-statistic = {F_val}, p-value = {format(p_val, scientific=TRUE, digits=4)}\n"))
  if (p_val < alpha_anova) {
    ket_luan_str <- "Bac bo H0: Co su khac biet co y nghia"
    cat(glue("  => KET LUAN: p < {alpha_anova} => BAC BO H0.\n"))
    cat(glue("     Trung binh Pixel_Rate khac nhau co y nghia giua cac nhom cua {cat_var}.\n"))
  } else {
    ket_luan_str <- "Chua du co so bac bo H0"
    cat(glue("  => KET LUAN: p >= {alpha_anova} => CHUA DU CO SO BAC BO H0.\n"))
    cat("     Chua co bang chung ve su khac biet trung binh giua cac nhom.\n")
  }

  # ── BƯỚC 4: TÍNH EFFECT SIZE (η²) ──
  cat(glue("\nBUOC 4: Effect Size – Eta binh phuong (eta^2)\n"))
  cat("  eta^2 do luong ty le bien thien cua Pixel_Rate duoc giai thich boi bien phan loai\n")

  ss_tab  <- aov_sum[[1]]
  eta2    <- ss_tab[1, "Sum Sq"] / sum(ss_tab[, "Sum Sq"])
  eta2    <- round(eta2, 4)

  cat(glue("  eta^2 = {eta2}\n"))
  if (eta2 < 0.01) {
    eta_interp <- "Rat nho (< 0.01)"
  } else if (eta2 < 0.06) {
    eta_interp <- "Nho (0.01 - 0.06)"
  } else if (eta2 < 0.14) {
    eta_interp <- "Trung binh (0.06 - 0.14)"
  } else {
    eta_interp <- "Lon (>= 0.14)"
  }
  cat(glue("  => Muc do anh huong: {eta_interp}\n"))

  # ── BƯỚC 5: HẬU NGHIỆM TUKEY HSD (nếu ANOVA có ý nghĩa) ──
  if (p_val < alpha_anova) {
    cat(glue("\nBUOC 5: Phan tich hau nghiem – Tukey HSD\n"))
    cat("  Muc tieu: Xac dinh cu the cap nhom nao co trung binh Pixel_Rate khac nhau\n")
    cat("  Phuong phap: Tukey Honest Significant Difference (HSD), kiem soat sai lam loai I\n\n")

    tukey_res <- TukeyHSD(aov_mod)
    print(tukey_res)

    # Trực quan hóa Tukey
    if (!is.null(dev.list())) dev.off()
    par(mar = c(5, 12, 4, 2))
    plot(tukey_res, las = 1,
         main = glue("Tukey HSD – {cat_var}"),
         col  = "steelblue")
    par(mar = c(5, 4, 4, 2))
  } else {
    cat("\nBUOC 5: Khong can hau nghiem (ANOVA khong co y nghia thong ke).\n")
  }

  # ── BƯỚC 6: BOXPLOT TRỰC QUAN HÓA ──
  cat(glue("\nBUOC 6: Bieu do Boxplot – Phan phoi Pixel_Rate theo {cat_var}\n"))
  n_grp <- nlevels(df_sub[[cat_var]])

  if (!is.null(dev.list())) dev.off()
  print(
    ggplot(df_sub, aes(x = .data[[cat_var]], y = Pixel_Rate,
                       fill = .data[[cat_var]])) +
      geom_boxplot(outlier.alpha = 0.3, outlier.size = 1.5) +
      stat_summary(fun = mean, geom = "point", shape = 18,
                   color = "red", size = 3, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "text",
                   aes(label = round(after_stat(y), 2)),
                   vjust = -1, color = "red", size = 3) +
      labs(
        title    = glue("Phan phoi log(Pixel_Rate+1) theo {cat_var}"),
        subtitle = glue("ANOVA: F = {F_val}, p-value = {format(p_val, scientific=TRUE, digits=3)} | eta^2 = {eta2} ({eta_interp})"),
        x = cat_var, y = "log(Pixel_Rate + 1)",
        caption  = "Diem do hinh thoi = trung binh nhom"
      ) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            axis.text.x     = element_text(angle = if (n_grp > 5) 30 else 0, hjust = 1),
            plot.title      = element_text(face = "bold"),
            plot.subtitle   = element_text(size = 9)) +
      scale_fill_brewer(palette = "Set2")
  )

  # Ghi vào bảng tổng hợp
  summary_anova <- rbind(summary_anova, data.frame(
    Bien_phan_loai = cat_var,
    F_value        = F_val,
    p_value        = round(p_val, 6),
    Shapiro_W      = sw_W,
    Shapiro_p      = round(sw_p, 6),
    Levene_F       = lev_F,
    Levene_p       = round(lev_p, 6),
    Eta2           = eta2,
    Ket_luan       = ket_luan_str,
    stringsAsFactors = FALSE
  ))
}

# ── BẢNG TỔNG HỢP CUỐI PHẦN 5 ──
cat("\n==========================================================\n")
cat("[KET LUAN PHAN 5 – BANG TONG HOP ANOVA]\n")
cat("==========================================================\n")
print(summary_anova, row.names = FALSE)

sig_cat_vars <- summary_anova$Bien_phan_loai[summary_anova$p_value < alpha_anova]
cat("\nCac bien phan loai co anh huong co y nghia thong ke den Pixel_Rate (p < 0.05):\n")
if (length(sig_cat_vars) > 0) {
  cat(glue("  => {paste(sig_cat_vars, collapse=', ')}\n"))
  cat("  => Cac bien nay se duoc dua vao mo hinh hoi quy o phan tiep theo.\n")
} else {
  cat("  Khong co bien nao.\n")
}

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