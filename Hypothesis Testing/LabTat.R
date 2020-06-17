LabTat <- read.csv(choose.files())
ad.test(LabTat$Laboratory.1)
ad.test(LabTat$Laboratory.2)
ad.test(LabTat$Laboratory.3)
ad.test(LabTat$Laboratory.4)
 # P high null fly. Data of all four laboratories is normal#
stacked_LabTat <- stack(LabTat)
leveneTest(stacked_LabTat$values~stacked_LabTat$ind,stacked_LabTat)
 # P high null fly. Variences of all four laboratories is equal#
Anova_Result<-aov(stacked_LabTat$values~stacked_LabTat$ind)
summary(Anova_Result)
 # P low null go. There is a difference in average TAT among the different laboratories #
