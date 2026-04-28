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

# Thư viện cần thiết
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

# Đọc dữ liệu
GPU = read.csv("C:/Users/Minh/Downloads/sxtk/BTL/All_GPUs.csv",header=TRUE,na.strings=c("","\n- ","\n","\nUnknown Release Date "))

# Tính N/A và lấy top 10
na_summary <- GPU %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "na_ratio") %>%
  arrange(desc(na_ratio)) %>%
  slice(1:10)

# Vẽ biểu đồ N/A
ggplot(na_summary, aes(x = reorder(variable, na_ratio), y = na_ratio)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Top 10 biến có tỷ lệ N/A cao nhất",
    x = "Biến",
    y = "Tỷ lệ N/A"
  ) +
  theme_minimal()

# Loại bỏ N/A trên 30%%
threshold <- 0.3

GPU <- GPU %>%
  select(where(~mean(is.na(.)) < threshold))
print( summary(GPU))
print( str(GPU))

# Chọn biến
GPU_new = GPU[,c("Resolution_WxH", "Manufacturer", "Core_Speed", "Memory_Speed", "Memory_Type", 
                    "ROPs", "TMUs", "Memory_Bus", "Memory", "Process", "Shader", "Pixel_Rate")]

# Kiểm tra kiểu dữ liệu và số dữ liệu trống
print( summary(GPU_new) )
print( apply(is.na(GPU_new),2,sum) )

#--------------------------------------------------
# 3. Tiền xử lí số liệu
#--------------------------------------------------
# Định nghĩa helper
helper <- function(x){ 
    if(is.na(x)) return(NA)
    as.double(strsplit(as.character(x), " ")[[1]][1])
        #strsplit return về 1 list các list: vd 7 MHz -> list(list(7),list(MHz))
        #strsplit[[1]] để truy cập vào list đầu tiên ->list(7)
        #strsplit[[1]][[1]] truy cập phần tử đầu tiên trong list -> 7
}

# Loại bỏ những kí tự không phải số
to_num <- function(x) {

  cleaned <- gsub("[^0-9\\.]", "", x)
  as.numeric(cleaned)
}

# Resolution_WxH
GPU_new$Resolution_WxH[is.na(GPU_new$Resolution_WxH)] = "4096x2160"
GPU_new$Resolution_WxH <- ifelse(GPU_new$Resolution_WxH == "4096x2160", 1, 
                        ifelse(GPU_new$Resolution_WxH == "2560x1600", 2, 3)) # Gom nhóm: 4096x2160 (39%), 2560x1600 (34%), Other (26%)
GPU_new$Resolution_WxH = factor(GPU_new$Resolution_WxH)

# Manufacturer
GPU_new$Manufacturer = factor(GPU_new$Manufacturer)

# Core_Speed
GPU_new$Core_Speed <- sapply(GPU_new$Core_Speed, helper)
GPU_new$Core_Speed[is.na(GPU_new$Core_Speed)] <- median(GPU_new$Core_Speed, na.rm = TRUE)

# Memory_Speed
GPU_new$Memory_Speed <- sapply(GPU_new$Memory_Speed, helper)
GPU_new$Memory_Speed[is.na(GPU_new$Memory_Speed)] <- median(GPU_new$Memory_Speed, na.rm = TRUE)

# ROPs
# Giả sử GPU_new$ROPs đang là character, chứa các giá trị như "24 (x4)", "48 (x2)", v.v.
GPU_new$ROPs <- as.numeric(sub("^([0-9]+).*", "\\1", GPU_new$ROPs))
GPU_new$ROPs[is.na(GPU_new$ROPs)] <- median(GPU_new$ROPs, na.rm = TRUE)

# TMUs
GPU_new$TMUs <- as.double(GPU_new$TMUs)
GPU_new$TMUs[is.na(GPU_new$TMUs)] <- median(GPU_new$TMUs, na.rm = TRUE)

# Memory_Bus
GPU_new$Memory_Bus <- sapply(GPU_new$Memory_Bus, helper)
GPU_new$Memory_Bus[is.na(GPU_new$Memory_Bus)] <- median(GPU_new$Memory_Bus, na.rm = TRUE)

# Memory
GPU_new$Memory <- sapply(GPU_new$Memory, helper)
GPU_new$Memory[is.na(GPU_new$Memory)] <- median(GPU_new$Memory, na.rm = TRUE)

# Memory_Type
GPU_new <- GPU_new[complete.cases(GPU_new$Memory_Type), ]
GPU_new$Memory_Type = gsub("[^A-Za-z]+.*","",GPU_new$Memory_Type)
GPU_new$Memory_Type = factor(GPU_new$Memory_Type)

# Process
GPU_new$Process <- as.double(gsub("[^0-9\\.]", "", as.character(GPU_new$Process)))
GPU_new$Process[is.na(GPU_new$Process)] <- median(GPU_new$Process, na.rm = TRUE)

# Shader
GPU_new$Shader <- as.double(GPU_new$Shader)
GPU_new$Shader[is.na(GPU_new$Shader)] <- median(GPU_new$Shader, na.rm = TRUE)

# Pixel_Rate 
GPU_new$Pixel_Rate <- sapply(GPU_new$Pixel_Rate, helper) 
GPU_new$Pixel_Rate[is.na(GPU_new$Pixel_Rate)] <- median(GPU_new$Pixel_Rate, na.rm = TRUE)

# Loại bỏ dòng trùng lặp
cat("Số lượng dòng trước khi loại duplicape:", nrow(GPU_new), "\n")
GPU_new <- dplyr::distinct(GPU_new)
cat("Số lượng dòng trước khi loại duplicape:", nrow(GPU_new), "\n")

# Định nghĩa lại danh sách biến số
numerical <- c(
  "Core_Speed", "Memory_Speed",
  "ROPs", "TMUs", "Memory_Bus",
  "Memory", "Process", "Shader" 
)
# Định nghĩa lại danh sách biến phân loại
categorical <- c("Manufacturer","Memory_Type","Resolution_WxH")

# Loại outlier cho từng biến numerical
cat("Số lượng dòng trước khi loại outlier:", nrow(GPU_new), "\n")
for (var in numerical) {
  if (var %in% names(GPU_new)) {
    Q1 <- quantile(GPU_new[[var]], 0.25, na.rm = TRUE)
    Q3 <- quantile(GPU_new[[var]], 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    # Giữ lại các dòng nằm trong khoảng
    GPU_new <- GPU_new[GPU_new[[var]] >= lower_bound & GPU_new[[var]] <= upper_bound, ]
    
    cat("Biến", var, ": Giữ trong khoảng [", round(lower_bound, 2), 
        ",", round(upper_bound, 2), "], Số dòng còn lại:", nrow(GPU_new), "\n")
  }
}
cat("\nTổng số dòng sau khi loại outlier:", nrow(GPU_new), "\n")

# Kiểm tra lại
str(GPU_new)
print(colSums(is.na(GPU_new)))

# Tạo dataframe log-transformed
GPU_new_log <- GPU_new
GPU_new_log[, numerical] <- log(GPU_new_log[, numerical])
GPU_new_log$Pixel_Rate <- log(GPU_new_log$Pixel_Rate)

# Xem 6 dòng đầu của GPU_new_log
head(GPU_new_log)

# Kiểm tra xem có giá trị < 0 trong dữ liệu gốc
for (var in numerical) {
  has_negative <- any(GPU_new[[var]] < 0, na.rm = TRUE)
  message(var, " in original < 0? ", has_negative)
}

# Kiểm tra xem có giá trị < 0 sau khi log-transform
for (var in numerical) {
  has_negative_log <- any(GPU_new_log[[var]] < 0, na.rm = TRUE)
  message(var, " in log-transformed < 0? ", has_negative_log)
}

# Thống kê nhanh trên GPU_new_log
print(summary(GPU_new_log))
print(colSums(is.na(GPU_new_log)))

# Xem 10 dòng đầu của GPU_new_log
head(GPU_new_log, 10)

#--------------------------------------------------
# 4. Thống kê mô tả
#--------------------------------------------------
# Lập bảng tính các giá trị thống kê mô tả cho GPU_new và GPU_new_log
mean<-apply(GPU_new[,numerical],2,mean)
sd<-apply(GPU_new[,numerical],2,sd)
min<-apply(GPU_new[,numerical],2,min)
max<-apply(GPU_new[,numerical],2,max)
median<-apply(GPU_new[,numerical],2,median)
data.frame(mean,sd,min,max,median)

mean<-apply(GPU_new_log[,numerical],2,mean)
sd<-apply(GPU_new_log[,numerical],2,sd)
min<-apply(GPU_new_log[,numerical],2,min)
max<-apply(GPU_new_log[,numerical],2,max)
median<-apply(GPU_new_log[,numerical],2,median)
data.frame(mean,sd,min,max,median)

# Chia layout thành 3 hàng và 3 cột
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Vẽ histogram cho từng biến numerical trong GPU_new
for (i in 1:length(numerical)) {
  hist_data <- GPU_new[[numerical[i]]]
  hist(hist_data, 
       xlab = names(GPU_new)[which(names(GPU_new) == numerical[i])], 
       main = paste("Histogram of", names(GPU_new)[which(names(GPU_new) == numerical[i])]), 
       labels = TRUE, 
       col = "blue")
}

# Chia layout thành 3 hàng và 3 cột
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Vẽ histogram cho từng biến numerical trong GPU_new_log
for (i in 1:length(numerical)) {
  hist_data <- GPU_new_log[[numerical[i]]]
  hist(hist_data, 
       xlab = names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])], 
       main = paste("Histogram of log(", names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])], ")"), 
       labels = TRUE, 
       col = "red")
}

# Chia layout thành 1 hàng và 2 cột
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

# Histogram Pixel_Rate
hist(GPU_new$Pixel_Rate,
     main   = "Histogram of Pixel_Rate",
     xlab   = "Pixel_Rate (GPixel/s)",
     breaks = 30,
     col    = "lightblue")

# Histogram log(Pixel_Rate)
hist(GPU_new_log$Pixel_Rate,
     main   = "Histogram of log(Pixel_Rate)",
     xlab   = "log(Pixel_Rate)",
     breaks = 30,
     col    = "lightpink")

# Chia layout thành 1 hàng và 3 cột
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

# Vẽ bảng barplot thể hiện phân phối biến định luọng
barplot(table(GPU_new$Manufacturer), xlab="Manufacturer", ylab="Frequency", main="Barplot of Manufacturer", col=c("red","green","blue"))
barplot(table(GPU_new$Memory_Type), xlab="Memory_Type", ylab="Frequency", main="Barplot of Memory_Type", col=c("red","green","blue"))
barplot(table(GPU_new$Resolution_WxH), xlab="Resolution_WxH", ylab="Frequency", main="Barplot of Resolution_WxH", col=c("red","green","blue"))

# Vẽ boxplot diễn tả 5 vị trí phân bố dữ liệu của biến định lượng
par(mfrow=c(1,2))
boxplot(Pixel_Rate~Manufacturer, data=GPU_new, main="boxplot of Pixel_Rate for Manufacturer", col="blue")
boxplot(Pixel_Rate~Manufacturer, data=GPU_new_log, main="boxplot of log(Pixel_Rate) for Manufacturer", col="red")

par(mfrow=c(1,2))
boxplot(Pixel_Rate~Memory_Type, data=GPU_new, main="boxplot of Pixel_Rate for Memory_Type", col="blue")
boxplot(Pixel_Rate~Memory_Type, data=GPU_new_log, main="boxplot of log(Pixel_Rate) for Memory_Type", col="red")

par(mfrow=c(1,2))
boxplot(Pixel_Rate~Resolution_WxH, data=GPU_new, main="boxplot of Pixel_Rate for Resolution_WxH", col="blue")
boxplot(Pixel_Rate~Resolution_WxH, data=GPU_new_log, main="boxplot of log(Pixel_Rate) for Resolution_WxH", col="red")

# Chia layout thành 3 hàng và 2 cột
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# Vẽ scatter plot
# Core_Speed vs Pixel_Rate
plot(GPU_new$Core_Speed, GPU_new$Pixel_Rate,
     xlab = "Core_Speed", ylab = "Pixel_Rate",
     main = "Pixel_Rate and Core_Speed", col = "blue", pch = 20)
fit1 <- lm(Pixel_Rate ~ Core_Speed, data = GPU_new)
abline(fit1, col = "red")

# log(Core_Speed) vs log(Pixel_Rate)
plot(GPU_new_log$Core_Speed, GPU_new_log$Pixel_Rate, 
     xlab = "log(Core_Speed)", ylab = "log(Pixel_Rate)", 
     main = "log(Pixel_Rate) and log(Core_Speed)", col = "red", pch = 20)
fit2 <- lm(Pixel_Rate ~ Core_Speed, data = GPU_new_log)
abline(fit2, col = "blue")

# ROPs vs Pixel_Rate
plot(GPU_new$ROPs, GPU_new$Pixel_Rate,
     xlab = "ROPs", ylab = "Pixel_Rate",
     main = "Pixel_Rate and ROPs", col = "blue", pch = 20)
fit3 <- lm(Pixel_Rate ~ ROPs, data = GPU_new)
abline(fit3, col = "red")

# log(ROPs) vs log(Pixel_Rate)
plot(GPU_new_log$ROPs, GPU_new_log$Pixel_Rate, 
     xlab = "log(ROPs)", ylab = "log(Pixel_Rate)", 
     main = "log(Pixel_Rate) and log(ROPs)", col = "red", pch = 20)
fit4 <- lm(Pixel_Rate ~ ROPs, data = GPU_new_log)
abline(fit4, col = "blue")

# Shader vs Pixel_Rate
plot(GPU_new$Shader, GPU_new$Pixel_Rate,
     xlab = "Shader", ylab = "Pixel_Rate",
     main = "Pixel_Rate and Shader", col = "blue", pch = 20)
fit5 <- lm(Pixel_Rate ~ Shader, data = GPU_new)
abline(fit5, col = "red")

# log(Core_Speed) vs log(Pixel_Rate)
plot(GPU_new_log$Core_Speed, GPU_new_log$Pixel_Rate, 
     xlab = "log(Shader)", ylab = "log(Pixel_Rate)", 
     main = "log(Pixel_Rate) and log(Shader)", col = "red", pch = 20)
fit6 <- lm(Pixel_Rate ~ Shader, data = GPU_new_log)
abline(fit6, col = "blue")

# Reset layout
par(mfrow = c(1, 1))

#--------------------------------------------------
# 5. Thống kê suy diễn
#--------------------------------------------------
# Chia dữ liệu thành tập huấn luyện và tập kiểm tra (2/3 - 1/3)
set.seed(123)
index <- createDataPartition(GPU_new_log$Pixel_Rate, p = 2/3, list = FALSE)
train <- GPU_new_log[index, ]
test  <- GPU_new_log[-index, ]

# 5.1 Đánh giá mối quan hệ giữa các biến
# 5.1.1 Ma trận tương quan Pearson
correlation_results <- data.frame(
  Variable = character(),
  Pearson_r = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (var in numerical) {
  test_result <- cor.test(train[[var]], train$Pixel_Rate)
  correlation_results <- rbind(
    correlation_results,
    data.frame(
      Variable = var,
      Pearson_r = unname(test_result$estimate),
      P_Value = test_result$p.value
    )
  )
}
print(correlation_results)

# 5.1.2 Scatter plot và Scatter matrix
pairs(train[numerical], main = "Scatter matrix (Train set)")

for (v in numerical) {
  ggplot(train, aes_string(x = v, y = "Pixel_Rate")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, col = "red") +
    ggtitle(paste("Pixel_Rate ~", v, "(Train set)")) +
    theme_minimal() -> p
  print(p)
}

par(mfrow = c(2, 2), mar = c(6, 6, 4, 4))
# 5.2 Phân tích phương sai (ANOVA) cho 3 nhóm trở lên
for (cat in categorical) {
  cat("\nANOVA for", cat, "on Train set\n")
  aov_mod <- aov(as.formula(paste("Pixel_Rate ~", cat)), data = train)
  print(summary(aov_mod))
  
  # 5.2.1 Kiểm định giả thiết (Shapiro, Levene)
  print(shapiro.test(residuals(aov_mod)))  # Normality
  levene_result <- leveneTest(as.formula(paste("Pixel_Rate ~", cat)), data = train)
  print(levene_result)  # Homogeneity
  
  # 5.2.2 Hậu nghiệm Tukey & Effect size (η²)
  tukey_result <- TukeyHSD(aov_mod)
  print(tukey_result)
  plot(TukeyHSD(aov_mod), las = 1)
  tab <- summary(aov_mod)[[1]]
  eta2 <- tab[,"Sum Sq"] / sum(tab[,"Sum Sq"])
  print(cbind(tab, Eta2 = eta2))
}
par(mfrow = c(1, 1))

# 5.3. KIỂM ĐỊNH MỘT MẪU (One-sample test)
# Chuẩn bị giá trị mu (giá trị giả thuyết) trên thang log nếu dùng dữ liệu log
mu_pixel <- log(40)      # cho Pixel_Rate = 40 GPixel/s
mu_core  <- log(1200)    # cho Core_Speed = 1200 MHz
mu_process <- log(10)    # cho Process = 10 nm

cat("KIỂM ĐỊNH MỘT MẪU CHO Pixel_Rate\n")

# 1. Kiểm tra normality trước (rất quan trọng)
shapiro.test(train$Pixel_Rate)

# 2. Parametric: One-sample t-test
t_test_pixel <- t.test(train$Pixel_Rate, 
                       mu = mu_pixel, 
                       alternative = "two.sided", 
                       conf.level = 0.95)
print(t_test_pixel)

# 3. Non-parametric: Wilcoxon signed-rank test (nếu không chuẩn)
wilcox_pixel <- wilcox.test(train$Pixel_Rate, 
                            mu = mu_pixel, 
                            alternative = "two.sided", 
                            conf.int = TRUE)
print(wilcox_pixel)

# Effect size (Cohen's d) cho one-sample t-test
library(effectsize)
cohens_d_one <- cohens_d(train$Pixel_Rate, mu = mu_pixel)
print(cohens_d_one)

# 5.4. KIỂM ĐỊNH HAI MẪU (Two-sample test)
cat("KIỂM ĐỊNH HAI MẪU: Pixel_Rate ~ Manufacturer (Nvidia vs AMD)\n")

# Lọc dữ liệu (nếu cần)
data_compare <- train[train$Manufacturer %in% c("Nvidia", "AMD"), ]

# 1. Kiểm tra normality cho từng nhóm
shapiro.test(data_compare$Pixel_Rate[data_compare$Manufacturer == "Nvidia"])
shapiro.test(data_compare$Pixel_Rate[data_compare$Manufacturer == "AMD"])

# 2. Kiểm tra đồng nhất phương sai (homogeneity of variance)
var.test(Pixel_Rate ~ Manufacturer, data = data_compare)

# 3. Parametric: Two-sample t-test
t_test_two <- t.test(Pixel_Rate ~ Manufacturer, 
                     data = data_compare, 
                     alternative = "two.sided", 
                     var.equal = FALSE)   # Welch t-test (không giả định var.equal)
print(t_test_two)

# 4. Non-parametric: Mann-Whitney U test (Wilcoxon rank-sum)
wilcox_two <- wilcox.test(Pixel_Rate ~ Manufacturer, 
                          data = data_compare, 
                          alternative = "two.sided", 
                          exact = FALSE)   # exact = FALSE nếu sample lớn
print(wilcox_two)

# Effect size cho two-sample t-test
library(effectsize)
cohens_d_two <- cohens_d(Pixel_Rate ~ Manufacturer, data = data_compare)
print(cohens_d_two)

# Memory_Speed ~ Memory_Type (eDRAAM vs GDDR)
cat("\nKIỂM ĐỊNH HAI MẪU: Memory_Speed ~ Memory_Type\n")

data_mem <- train[train$Memory_Type %in% c("eDRAM", "GDDR"), ]

var.test(Memory_Speed ~ Memory_Type, data = data_mem)
t.test(Memory_Speed ~ Memory_Type, data = data_mem, var.equal = FALSE)
wilcox.test(Memory_Speed ~ Memory_Type, data = data_mem, exact = FALSE)

# 5.3 Xây dựng mô hình hồi quy đa biến
# 5.3.1 Tiền xử lý & lựa chọn biến (stepwise / Lasso / VIF)
# Tạo công thức với tất cả biến
full_form <- as.formula(
  paste("Pixel_Rate ~", paste(c(numerical, categorical), collapse = " + "))
)

# Huấn luyện mô hình đầy đủ
full_mod <- lm(full_form, data = train)
print(vif(full_mod))  # Kiểm tra cộng tuyến multicollinearity

# Lựa chọn biến bằng stepwise selection
step_mod <- step(full_mod, direction = "both", trace = 0)
print(summary(step_mod))

# 5.3.2 Xây dựng mô hình đa biến
model_final <- step_mod
print(summary(model_final))

# 5.3.3 Diagnostic plots
par(mfrow = c(2, 2))
plot(model_final)
par(mfrow = c(1, 1))

# 5.3.4 Cross-validation (k-fold)
set.seed(123)
cv <- train(full_form, data = train,
            method = "lm",
            trControl = trainControl(method = "cv", number = 5))
print(cv)

#--------------------------------------------------
# 6. Đánh giá và dự báo
#--------------------------------------------------
# 6.1 Dự báo trên tập test: MAE, MSE, R², RMSE
pred <- predict(model_final, newdata = test)
test$predicted_value <- pred

# Tính toán các chỉ số
mae_value <- mae(test$Pixel_Rate, test$predicted_value)
mse_value <- mse(test$Pixel_Rate, test$predicted_value)
rmse_value <- rmse(test$Pixel_Rate, test$predicted_value)
r2_value <- cor(test$Pixel_Rate, test$predicted_value)^2

cat("MAE trên tập kiểm tra:", mae_value, "\n")
cat("MSE trên tập kiểm tra:", mse_value, "\n")
cat("RMSE trên tập kiểm tra:", rmse_value, "\n")
cat("R² trên tập kiểm tra:", r2_value, "\n")

# 6.2 Density plot/Scatter plot thực vs dự đoán
# Density plot
test_long <- test %>%
  select(Pixel_Rate, predicted_value) %>%
  rename(Thuc_te = Pixel_Rate, Du_doan = predicted_value) %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Value")

ggplot(test_long, aes(x = Value, fill = Type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_bw() +
  labs(title = "Thực tế (red) vs Dự đoán (blue) trên tập kiểm tra", x = "Pixel_Rate")

# Scatter plot
ggplot(test, aes(x = Pixel_Rate, y = predicted_value)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  theme_minimal() +
  labs(title = "Thực tế vs Dự đoán", x = "Thực tế", y = "Dự đoán")

# 6.3 Prediction intervals cho tương lai
new_data <- test[1, , drop = FALSE]  # Lấy một hàng từ test làm ví dụ
pred_interval <- predict(model_final, newdata = new_data, interval = "prediction")
print(pred_interval)
print(new_data)

# 6.4 Kịch bản dự báo (giả định tăng/giảm biến X)
scenario_data <- test
scenario_data$Core_Speed <- scenario_data$Core_Speed * 1.1
pred_scenario <- predict(model_final, newdata = scenario_data)
cat("Dự đoán với Core_Speed tăng 10%:\n")
print(summary(pred_scenario))