#Call the file location
COF<-read.csv(choose.files())
#Convert data into binary 0,1
Binary_COF<-ifelse(COF=="Error Free",0,1)
#Convert into data frame
df<-data.frame(Binary_COF)
#stack the data
Stacked_Binary_COF<-stack(df)
#Run Chi Squ Test
chisq.test(Stacked_Binary_COF$values,Stacked_Binary_COF$ind)
#P high null fly
#Ho = the defective %  does not varies by centre
#Ha = the defective %  varies by centre
