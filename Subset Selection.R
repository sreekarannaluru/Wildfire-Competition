# Forward/Backward subset selection!
library(leaps)
library(psych)
df_d <- read_excel("Denver.xlsx")
df_c <- read_excel("Columbus.xlsx")
df_d <- df_d[, -40]
df_c<- df_c[, -40]
attach(df_d)
regfit.fwd <- regsubsets(brightness ~ . - (Year + Month + Lat + Lon + Occurences + bright_t31), data = df_d, nvmax = 40, method = "forward")
coef(regfit.fwd, 21)
regfit.back <- regsubsets(brightness ~ . - (Year + Month + Lat + Lon + Occurences + bright_t31), data = df_d, nvmax = 40, method = "backward")
coef(regfit.back, 21)


# Let's check that for Columbus region!
df_c <- read_excel("Columbus.xlsx")
df_c <- df_c[, -40]
attach(df_c)
regfit.fwd <- regsubsets(brightness ~ . - (Year + Month + Lat + Lon + Occurences + bright_t31), data = df_c, nvmax = 40, method = "forward")
coef(regfit.fwd, 11)

regfit.back <- regsubsets(brightness ~ . - (Year + Month + Lat + Lon + Occurences + bright_t31), data = df_c, nvmax = 40, method = "backward")
coef(regfit.fwd, 11)

# Please note we used the optimal solution of Lasso for Forward/Backward coefficients.























