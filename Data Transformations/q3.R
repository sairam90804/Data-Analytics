library(mgcv)
library(visreg)
library(glmnet)
library(readxl)

concrete_data <- read_excel('Z://IIT/DPA/Assignment3/Concrete_Data.xls')

concrete <- data.frame(concrete_data)
colnames(concrete) <- c("cem", "bfs", "fa", "water", "sp", "cagg", "fagg", "age", "ccs")

gam.linear <- gam(ccs~cem+fa+water+sp+cagg+fagg+age, data=concrete)
# r2
summary(gam.linear)$r.sq

gam.smooth<- gam(ccs~s(cem)+s(fa)+s(water)+s(sp)+s(cagg)+s(fagg)+s(age), data=concrete)
# r2
summary(gam.smooth)$r.sq

visreg(gam.smooth, scale = "response", alpha = 0.2, rug = TRUE)
