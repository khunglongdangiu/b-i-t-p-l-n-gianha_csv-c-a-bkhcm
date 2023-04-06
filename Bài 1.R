#1.Đọc dữ liệu
gia_nha <- read.csv("D:/btl/gia_nha.csv")
#2a.Trích ra dữ liệu con New_DF
names(gia_nha)
new_DF = data.frame(gia_nha[,c(6,23,11,14,16,9)])
#2b.Kiểm tra giá trị bị khuyết trong tập tin
apply(is.na(new_DF),2,which)
colSums(is.na(new_DF))
colMeans(is.na(new_DF))
#2b.Phương pháp thay thế
new_DF$price[is.na(new_DF$price)]=mean(new_DF$price,na.rm=T)
#3a.Chuyển đổi biến sang log
new_DF[,c(1,2,5,6)]=log(new_DF[,c(1,2,5,6)])
#3b.Tính các giá trị thống kê
trungbinh=apply(new_DF[,c(1,2,5,6)],2,mean)
trungvi=apply(new_DF[,c(1,2,5,6)],2,median)
dolechchuan=apply(new_DF[,c(1,2,5,6)],2,sd)
giatrinhonhat=apply(new_DF[,c(1,2,5,6)],2,min)
giatrilonnhat=apply(new_DF[,c(1,2,5,6)],2,max)
bangmota=data.frame(trungbinh, trungvi, dolechchuan, giatrilonnhat, giatrinhonhat)
bangmota
#3c.Lập bảng thống kê số lượng từng chủng loại
table(new_DF$floors)
table(new_DF$condition)
#3d. Vẽ đồ thị bằng hàm hist
hist(new_DF$price,xlab="price",main="do thi phan phoi",labels=T)
#3e.Vẽ đồ thị bằng hàm boxplot
boxplot(price~floors,main="Bieu do boxplot cua bien price cho nhom phan loai cua bien floors", new_DF)
boxplot(price~floors,main="Bieu do boxplot cua bien price cho nhom phan loai cua bien condition", new_DF)
#3f. Vẽ đồ thị bằng hàm pairs
pairs(price~sqft_living15,main="Phan phoi cua bien price theo bien sqft_living15", new_DF)
pairs(price~sqft_above,main="Phan phoi cua bien price theo bien sqft_above",new_DF)
pairs(price~sqft_living,main = "Phan phoi cua bien price theo bien sqft_living",new_DF)
#4a. Xây dựng mô hình tuyến tính bội
tt1=lm(price~sqft_living15+ floors+condition+sqft_above+sqft_living, data=new_DF)
summary(tt1)
tt2 = lm(price ~ sqft_living15 + floors + sqft_above + sqft_living,data = new_DF)
summary(tt2)
#4c. Mô hình hồi quy annova
anova(tt1,tt2)
#4e. Vẽ đồ thị plot
plot(tt1,which = 1)
plot(tt1,which = 1)
plot(tt1,which = 2)
plot(tt1,which = 3)
plot(tt1,which = 5)
#5a. Dự báo giá nhà theo 2 thuộc tính X1 và X2
X1 = data.frame(sqft_living15 = mean(new_DF$sqft_living15), sqft_above =mean(new_DF$sqft_above), sqft_living = mean(new_DF$sqft_living), floors = 2,condition = 3)
predict_X1 = predict(tt1, X1, interval = "confidence")
predict_X1
X2 = data.frame(sqft_living15 = max(new_DF$sqft_living15), sqft_above =max(new_DF$sqft_above), sqft_living = max(new_DF$sqft_living), floors = 2,condition = 3)
predict_X2 = predict(tt1, X2, interval = "confidence")
predict_X2
#5b. Tạo bảng so sánh khoảng tin cậy
pred = data.frame(rbind(predict_X1 , predict_X2))
rownames(pred) = c("X1","X2")
pred$range = pred$upr - pred$lwr
pred