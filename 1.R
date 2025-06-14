dt <- read.csv("F:/Training/Project/M.csv")
summary(dt)
str(dt)
#factor
for (i in colnames(dt[,c("sex","diabetes","eGFR_g","HbA1C_g","HF")])){dt[[i]] <- as.factor(dt[[i]])}

#TableStack
library(epiDisplay)          
table<-tableStack(c(age,sex,mbp,bmi,diabetes,hdl,ldl,eGFR_g,HbA1C_g),by=HF,data=dt,total.column=T)
print(table)
write.csv(table,"table.csv")


#Logistic regression
data_clean <- na.omit(dt)  
model_base <- glm(HF ~ age + sex + mbp + bmi + diabetes + hdl + ldl + eGFR_g + HbA1C_g, data = data_clean,family = binomial())
summary(model_base)
or<-logistic.display(model_base)
write.csv(or,"OR.csv")

model_final <- step(model_base, direction = "both")
model_base1 <- glm(HF ~eGFR_g+hdl+HbA1C_g, data = dt, family = binomial)
summary(model_base1)
logistic.display(model_base1)


model_base11 <- glm(HF ~age + bmi+sex+diabetes+ hdl + ldl + eGFR, data = dt, family = binomial)
summary(model_base11)
logistic.display(model_base11)

model_base2 <- glm(HF ~chol + ldl + eGFR, data = dt, family = binomial)
summary(model_base2)
logistic.display(model_base2)

######Univariate
md <- glm(HF ~age, data = dt, family = binomial)
summary(md)
logistic.display(md)

md1 <- glm(HF ~sex, data = dt, family = binomial)
summary(md1)
logistic.display(md1)

md2 <- glm(HF ~dbp, data = dt, family = binomial)
summary(md2)
logistic.display(md2)

md3 <- glm(HF ~mbp, data = dt, family = binomial)
summary(md3)
logistic.display(md3)

md4 <- glm(HF ~bmi, data = dt, family = binomial)
summary(md4)
logistic.display(md4)

md5 <- glm(HF ~diabetes, data = dt, family = binomial)
summary(md5)
logistic.display(md5)

md7 <- glm(HF ~hdl, data = dt, family = binomial)
summary(md7)
logistic.display(md7)

md8 <- glm(HF ~ldl, data = dt, family = binomial)
summary(md8)
logistic.display(md8)

md9 <- glm(HF ~eGFR_g, data = dt, family = binomial)
summary(md9)
logistic.display(md9)

md10 <- glm(HF ~ckd, data = dt, family = binomial)
summary(md10)
logistic.display(md10)

md11 <- glm(HF ~HbA1C_g, data = dt, family = binomial)
summary(md10)
logistic.display(md11)



#################################ลดขนาดข้อมูล
library(dplyr)

# แยกข้อมูลออกเป็น 2 กลุ่ม
positive <- dt %>% filter(HF == 1)
negative <- dt %>% filter(HF == 0)

# สุ่มเลือกกลุ่มปกติให้มีขนาดเท่ากับกลุ่มป่วย (69 คน)
set.seed(123)  # เพื่อให้ผลลัพธ์สามารถทำซ้ำได้
negative_sample <- negative %>% sample_n(4*nrow(positive))

# รวมข้อมูลใหม่ที่สมดุล
balanced_data <- bind_rows(positive, negative_sample)
summary(balanced_data)
write.csv(balanced_data,"2.csv")
#
table<-tableStack(c(age,sex,sbp,dbp,bmi,palse,diabetes,ckd,chol,hdl,ldl,eGFR,HbA1C),by=HF,data=balanced_data,total.column=T)
print(table)


data_clean <- na.omit(dt)  # ลบแถวที่มี NA ทั้งหมดออก
model_base <- glm(HF ~ age + sex + mbp + bmi + diabetes + hdl + ldl + eGFR_g + HbA1C_g, 
                  data = data_clean, 
                  family = binomial())
summary(model_base)

model_final <- step(model_base, direction = "both")
model_base1 <- glm(HF ~ hdl + ldl + HbA1C_g, data = balanced_data, family = binomial)
summary(model_base1)
 
model_base2 <- glm(HF ~chol + ldl + eGFR, data = balanced_data, family = binomial)
summary(model_base2)
############################################################################################




####################Graph
# ติดตั้งแพ็กเกจถ้ายังไม่ได้ติดตั้ง
install.packages("ggcorrplot")

# โหลดแพ็กเกจ
library(ggcorrplot)


numeric_dt <- dt %>%
  select(where(is.numeric)) %>%
  select(-c(BP_DIASTOLIC,BP_SYSTOLIC))

# คำนวณค่า correlation
cor_matrix <- cor(numeric_dt, use = "complete.obs")


# สร้างกราฟแบบสามเหลี่ยมล่าง + ตัวเลข + สี
ggcorrplot(cor_matrix,
           method = "square",       # รูปทรงช่อง
           type = "lower",          # แสดงเฉพาะสามเหลี่ยมล่าง
           lab = TRUE,              # แสดงค่าตัวเลข
           tl.cex = 10,             # ขนาดตัวอักษร label
           lab_size = 3,            # ขนาดตัวเลข
           colors = c("#B2182B", "white", "#2166AC"),  # สีแดง-ขาว-น้ำเงิน
           outline.color = "gray")  # เส้นกรอบ


ggplot(dt, aes(x = eGFR_g, y = hdl, fill = HF)) +
  geom_boxplot() +
  labs(
    title = "Comparison of HDL Cholesterol Levels by eGFR and Heart Failure Status",
    x = "eGFR Group",
    y = "HDL Cholesterol Level", fill = "Heart Failure Status"
  ) +
  scale_fill_manual(values = c("0" = "palegreen", "1" = "#FF6666"),labels = c("0 = No Heart Failure", "1 = Heart Failure")) +
  theme_minimal() +
  scale_x_discrete(
    labels = c(
      "1" = "G2 (≥60)",
      "2" = "G3a (45–59)",
      "3" = "G3b (30–44)",
      "4" = "G4 (15–29)",
      "5" = "G5 (<15)"
    ))+
  coord_cartesian(ylim = c(30, 65))+
                    theme(
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())



library(reshape2)
library(ggplot2)

dt_long <- melt(dt, id.vars = "HF", measure.vars ="eGFR")

ggplot(dt_long, aes(x = factor(HF), y = value, fill = factor(HF))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "Heart Failure", y = "Variable Values", fill = "Heart Failure Status") +
  scale_fill_manual(
    values = c("0" = "palegreen", "1" = "#FF6666"),
    labels = c("0 = No Heart Failure", "1 = Heart Failure")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


install.packages("pROC")
library(pROC)
prob <- predict(model_base2, type = "response")
roc_obj <- roc(dt$HF, prob)

plot(roc_obj, col = "blue", lwd = 3, main = "ROC Curve for Heart Failure Prediction")
auc_value <- auc(roc_obj)
text(0.6, 0.4, paste("AUC =", round(auc_value, 3)), cex = 1.5)


install.packages("ggridges")
library(ggridges)

ggplot(dt, aes(x = age, y = factor(HF), fill = factor(HF))) +
  geom_density_ridges(alpha = 0.6) +
  scale_fill_manual(values = c("0" = "#00BFC4", "1" = "#F8766D")) +
  labs(
    title = "Age Distribution by Heart Failure Status",
    x = "Age",
    y = "Heart Failure",fill = "Heart Failure Status"
  ) +
  scale_fill_manual(
    values = c("0" = "palegreen", "1" = "#FF6666"),
    labels = c("0 = No Heart Failure", "1 = Heart Failure"))+
  theme_minimal()


