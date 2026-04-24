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

#--------------------------------------------------
# 7. Kiểm định hai mẫu
#--------------------------------------------------

#--------------------------------------------------
# 8. Xây dựng mô hình
#--------------------------------------------------

#--------------------------------------------------
# 9. Đánh giá và Dự đoán
#--------------------------------------------------