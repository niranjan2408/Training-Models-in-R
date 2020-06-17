BuyerRatio <- read.csv(choose.files())
#Inputs are 4 discrete variables(east,west,north,south).
#Output is also discrete. We are trying to find out if proportions of male and female are similar or not across the regions
#We proceed with chi-square test
#Ho= Proportions of Male and Female are same
#Ha= Proportions of Male and Female are not same
df<-data.frame(BuyerRatio$East,BuyerRatio$West,BuyerRatio$North, BuyerRatio$South)
chisq.test(df)