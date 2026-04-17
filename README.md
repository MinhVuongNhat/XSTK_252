# 1. Cài đặt & Load thư viện
install.packages chỉ cần chạy 1 lần, sau đó comment lại để không bị chạy lại
Load thư viện library(tên_package) 

# 2. Đọc dữ liệu & Chọn biến ban đầu
## 2.1. Đọc file CSV
Mã code:
RGPU = read.csv("C:/.../All_GPUs.csv", 
               header = TRUE, 
               na.strings = c("", "\n- ", "\n", "\nUnknown Release Date "))

header = TRUE: dòng đầu tiên là tên cột.
na.strings: chỉ rõ tất cả ký tự sẽ được coi là N/A, vì file gốc có thể chứa nhiều ký tự lạ.

## 2.2. Chọn biến quan trọng (7-12 biến)
Mã code:
GPU_new = GPU[, c("Resolution_WxH", "Manufacturer", ..., "Pixel_Rate")]
Chỉ giữ những biến có ý nghĩa với mục tiêu dự báo Pixel_Rate nhằm giảm kích thước dữ liệu, tránh nhiễu. 
Có thể skip bước này để sử lý hết dữ liệu rồi sẽ chọn biến sau khi tiền xử lý và xem xét tương quan ở sau

# 3. Kiểm tra dữ liệu ban đầu (Exploratory)
Bước này nhằm kiểm tra dữ liệu, kiểu dữ liệu hoặc phát hiện sớm vấn đề (kiểu dữ liệu sai, N/A nhiều, min/max bất thường)
## 3.1. Xem tóm tắt
print(summary(GPU_new))
## 3.2. Đếm N/A từng cột
apply(is.na(GPU_new), 2, sum)
## 3.3. Xử lý N/A
Loại bỏ các cột có tỷ lệ trống quá cao (>70%) hoặc các dòng không thể lấp đầy.

# 4. Tiền xử lý số liệu (Preprocessing)
## 4.1. Định nghĩa 2 hàm helper
### 4.1.1. Hàm helper để trích xuất đặc trưng
Xử lý chuỗi kiểu “1200 MHz” thành chỉ lấy 1200, loại bỏ đơn vị đi.
helper <- function(x){ 
  if(is.na(x)) return(NA)
  as.double(strsplit(as.character(x), " ")[[1]][1])
}

Chuyển x thành character.
strsplit(..., " ") -> tách theo khoảng trắng.
Lấy phần tử đầu tiên [1][1] và ép double.

### 4.1.2. Hàm to_num
Loại bỏ hoàn toàn ký tự không phải số.
to_num <- function(x) {
  cleaned <- gsub("[^0-9\\.]", "", x)
  as.numeric(cleaned)
}

## 4.2. Xử lý từng biến một
Ví dụ cho từng biến trong code mẫu:
Biến,               Code thực hiện,                                                     Mục đích ,                              Lý do
Resolution_WxH,     "is.na(...) = ""4096x2160"" -> ifelse gom nhóm 1/2/3 -> factor",    Gom 3 nhóm chính (39% - 34% - 26%),     Giảm chiều categorical
Manufacturer,       factor(GPU_new$Manufacturer),                                       Chuyển thành factor,                    Phân tích ANOVA sau
Core_Speed,         sapply(helper) -> median imputation,                                Chuyển chuỗi -> số + xử N/A,            Median robust với outlier
Memory_Speed,       Tương tự Core_Speed,                                                -,                                      -
ROPs,               "sub(""^([0-9]+).*"", ""\\1"", ...) -> as.numeric -> median",       Lấy số trước dấu ngoặc “24 (x4)”,       Regex capture group
TMUs,               as.double -> median,                                                -,                                      -
Memory_Bus,         sapply(helper) -> median,                                           -,                                      -
Memory,             sapply(helper) -> median,                                           -,                                      -
Memory_Type,        complete.cases -> gsub -> factor,                                   "Xóa NA + giữ chữ (GDDR5, HBM…)",       Làm sạch hoàn toàn
Process,            gsub -> as.double -> median,                                        “7 nm” -> 7,                            -
Shader,             as.double -> median,                                                -,                                      -
Pixel_Rate,         sapply(helper) -> median,                                           Biến target Y,                          -

## 4.3. Loại bỏ Outlier
Tính Q1, Q3, IQR cho từng biến numerical.
Giữ lại chỉ những giá trị nằm trong khoảng [Q1 - 1.5*IQR, Q3 + 1.5*IQR].
Outliers có thể làm lệch đường hồi quy, dẫn đến mô hình dự báo sai lệch đáng kể cho đa số dữ liệu còn lại.

numerical <- c("Core_Speed", "Memory_Speed", "ROPs", "TMUs", 
               "Memory_Bus", "Memory", "Process", "Shader", "Pixel_Rate")

for (var in numerical) {
  Q1 <- quantile(GPU_new[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(GPU_new[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  GPU_new <- GPU_new[GPU_new[[var]] >= lower & GPU_new[[var]] <= upper, ]
}

## 4.4. Loại duplicate & Kiểm tra lại
GPU_new <- dplyr::distinct(GPU_new)
str(GPU_new)
print(colSums(is.na(GPU_new)))

## 4.5. Định nghĩa danh sách biến
các biến số: numerical <- c(...)
các biến phân loại: categorical <- c(...)

## 4.6. Tạo bảng log-transform
Đưa dữ liệu về gần phân phối chuẩn hơn, giúp đáp ứng giả định của mô hình hồi quy tuyến tính và giảm hiện tượng phương sai thay đổi
Làm phân phối gần normal -> thỏa mãn giả định linear regression & ANOVA.
Kiểm tra negative: 2 vòng for kiểm tra trước và sau log.
GPU_new_log <- GPU_new
GPU_new_log[, numerical] <- log(GPU_new_log[, numerical])
GPU_new_log$Pixel_Rate <- log(GPU_new_log$Pixel_Rate)

# 5. Thống kê mô tả
## 5.1. Bảng thống kê (mean, sd, min, max, median) cho cả hai bảng (original + log) nhằm kiểm tra lại sau tiền xử lý.
## 5.2. Vẽ histogram original + histogram log (par(mfrow=c(3,3))).
## 5.3. Histogram riêng Pixel_Rate vs log(Pixel_Rate).
## 5.4. barplot cho categorical.
## 5.4. boxplot original vs log.
## 5.6. scatter plot + đường hồi quy original vs log.

# 6. Thống kê suy diễn
## 6.1. Ma trận tương quan Pearson cho tất cả numerical.
Sử dụng hệ số tương quan Pearson (r) để đo lường mức độ liên kết tuyến tính giữa các biến số độc lập và biến phụ thuộc.
Loại bỏ các biến có tương quan quá thấp (p-value > 0.05) vì chúng không có ý nghĩa thống kê trong việc giải thích biến phụ thuộc.

## 6.2. ANOVA + kiểm định giả định + Tukey + η² cho từng categorical.
Các điều kiện cần kiểm tra trước khi thực hiện ANOVA:
 - Độc lập của các quan sát
 - Biến phụ thuộc là biến liên tục
 - Phân phối chuẩn trong mỗi nhóm (dùng Shapiro–Wilk)
 - Đồng nhất phương sai giữa các nhóm (Dùng Levene’s test)
Nếu tất cả điều kiện thõa -> Dùng tiếp ANOVA, không thì sẽ dùng Kruskal–Wallis và Dunn’s test để thay thế.
Tránh xử dụng ANOVA tiếp vì không còn ý nghĩa khoa học.

# 7. Xây dựng mô hình và đánh giá
## 7.1. Chia train/test (70/30 hoặc 80/20) bằng createDataPartition.
Sử dụng K-fold Cross-validation (k=5 hoặc 10) để đảm bảo mô hình không bị Overfitting (học vẹt trên tập train).

## 7.2. Kiểm tra Đa cộng tuyến (Multicollinearity - VIF)
Sử dụng hàm vif() từ thư viện car loại bỏ các biến có hệ số VIF > 5 (hoặc 10).
Khi các biến độc lập tương quan quá mạnh với nhau, mô hình sẽ không thể xác định được tác động riêng lẻ của từng biến, làm mất tính ổn định của hệ số hồi quy.
Trực quan hóa mối quan hệ. Các điểm dữ liệu hội tụ quanh đường thẳng hồi quy đỏ cho thấy việc áp dụng mô hình hồi quy tuyến tính là phù hợp.

## 7.3. Lựa chọn biến tự động (Stepwise Selection)
Sử dụng hàm step() với tiêu chí AIC (Akaike Information Criterion).
AIC cân bằng giữa độ khớp của mô hình và độ phức tạp (số lượng biến). Bước này sẽ tự động loại bỏ các biến thừa để tối ưu hóa mô hình.

## 7.4. Xây dựng mô hình hồi quy đa biến
Xây dựng nhiều mô hình (2-4 mô hình) để so sánh kết quả giữa các môi hình

## 7.5. Kiểm định giả thuyết hồi quy (Diagnostics)
Trước khi tin tưởng mô hình, ta phải kiểm tra:
 - Tính tuyến tính: Residuals vs Fitted plot. Phần dư phân tán tương đối đều, dù có hiện tượng "phễu" nhẹ ở các giá trị cao (phương sai thay đổi).
 - Phân phối chuẩn của sai số: Q-Q plot. Phần lớn các điểm nằm trên đường thẳng, xác nhận sai số xấp xỉ phân phối chuẩn ở vùng trung tâm.
 - Phương sai không đổi: Scale-Location plot. Phát hiện một vài điểm có sức ảnh hưởng lớn (như điểm 188), cần lưu ý khi áp dụng dự báo cho các dòng GPU cực kỳ đặc biệt.

## 7.6. 5-fold Cross-validation.
Chia dữ liệu thành 5 phần (folds). Lần lượt huấn luyện trên 4 phần và kiểm tra trên 1 phần còn lại.
Để đảm bảo mô hình không bị "học vẹt" (overfitting).

# 8. Dự báo VÀ Kịch bản
## 8.1. Dự báo trên test set + tính MAE, MSE, RMSE, R².
## 8.2. Density plot & Scatter thực tế vs dự đoán.
## 8.3. Prediction interval cho quan sát mới.
## 8.4. Kịch bản what-if (Core_Speed tăng 10%).
