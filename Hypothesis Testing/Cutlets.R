cutlets <- read.csv (choose.files()) 
# Y is continueous and X is descrete in two columns# 
library(nortest) 
ad.test(cutlets$Unit.A) 
ad.test(cutlets$Unit.B) 
# P high null fly # > # Unit A and unit B data are normal # 
t.test(cutlets$Unit.A,cutlets$Unit.B,alternative = "two.sided",conf.level = 0.95,paired = TRUE) 
# There is not significance difference in the diameter of the cutlet between two units #
  