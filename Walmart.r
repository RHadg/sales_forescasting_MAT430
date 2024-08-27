feature <- read.csv(file = "features.csv")

clean_data <- read.csv(file = 'out.csv')

head(clean_data, 10)

clean_data <- within(clean_data, {
    IsHoliday <- factor(IsHoliday)
    })

cor_lm <- lm(Weekly_Sales~ Size + Dept + IsHoliday + WeekOfYear + MarkDown, data= clean_data)
summary(cor_lm)

fitted_val_cor <- fitted.values(cor_lm)
residual_val_cor <- residuals(cor_lm)

plot(fitted_val_cor, residual_val_cor,
    main = "Residuals against Fitted Values Model",
    xlab = "Fitted Values", ylab = "Residuals",
    col = "red",
    pch = 19, frame = TRUE)

qqnorm(main = "QQ Plot for Model", residual_val_cor, pch = 19, col = "red", frame = TRUE)
qqline(residual_val_cor, col = "blue", lwd = 2)

linearmodel <- lm(Weekly_Sales~., data = clean_data)

summary(linearmodel)

#rse computation
sigma(linearmodel)/mean(clean_data$Weekly_Sales)

#take out IsHoliday (all values with p-value less than 0.05)
sw_lm <- lm(Weekly_Sales ~ Store + Dept + Temperature + Fuel_Price + CPI + Unemployment + Size + Year + Month + Day + Quarter + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm)

#take out IsHoliday and Temperature
sw_lm_2 <- lm(Weekly_Sales ~ Store + Dept + Fuel_Price + CPI + Unemployment + Size + Year + Month + Day + Quarter + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm_2)

#take out IsHoliday and Temperature and Fuel Price
sw_lm_3 <- lm(Weekly_Sales ~ Store + Dept + CPI + Unemployment + Size + Year + Month + Day + Quarter + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm_3)

#take out IsHoliday/Temperature/Fuel Price/Month/Day/Quarter
sw_lm_4 <- lm(Weekly_Sales ~ Store + Dept + CPI + Unemployment + Size + Year + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm_4)

#take out IsHoliday/Temperature/Fuel Price/Month/Day/Quarter/WeekOfYear
sw_lm_5 <- lm(Weekly_Sales ~ Store + Dept + CPI + Unemployment + Size + Year + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm_5)

#take out IsHoliday/Temperature/Fuel Price/Month/Day/Quarter/WeekOfYear/Unemployment
sw_lm_6 <- lm(Weekly_Sales ~ Store + Dept + CPI + Size + Year + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm_6)

#take out IsHoliday/Temperature/Fuel Price/Month/Day/Quarter/WeekOfYear/Unemployment/Dept
sw_lm_7 <- lm(Weekly_Sales ~ Store + CPI + Size + Year + WeekOfYear + MarkDown, data = clean_data)
summary(sw_lm_7)

#Feature Importance from random forest
sw_lm_rf <- lm(Weekly_Sales ~ Store + Dept + Dept + Size + Year + WeekOfYear + IsHoliday, data = clean_data)
summary(sw_lm_rf)

#Revers StepWise
sw_lm_rev <- lm(Weekly_Sales ~ Store, data = clean_data)
summary(sw_lm_rev)

#Revers StepWise2
sw_lm_rev2 <- lm(Weekly_Sales ~ Store + Dept, data = clean_data)
summary(sw_lm_rev2)

#Revers StepWise3
sw_lm_rev3 <- lm(Weekly_Sales ~ Store + Dept + IsHoliday, data = clean_data)
summary(sw_lm_rev3)

#Revers StepWise4
sw_lm_rev4 <- lm(Weekly_Sales ~ Store + Dept + IsHoliday + Temperature, data = clean_data)
summary(sw_lm_rev4)

print("Fitted Values Final Linear Regression Model")
fitted_val <- fitted.values(sw_lm_rf)

print("Residual Values Final Linear Regression Model")
residual_val <- residuals(sw_lm_rf)

plot(fitted_val, residual_val,
    main = "Residuals against Fitted Values Model",
    xlab = "Fitted Values", ylab = "Residuals",
    col = "red",
    pch = 19, frame = TRUE)

qqnorm(main = "QQ Plot for Model", residual_val, pch = 19, col = "red", frame = TRUE)
qqline(residual_val, col = "blue", lwd = 2)
