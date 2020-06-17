#Call the file location
Fantaloons<-read.csv(choose.files())
View(Fantaloons)
summary(Fantaloons)
prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = TRUE,alternative = "two.sided")
#Ho= Males versus females walking in to the store does not differ based on day of the week
#Ha= Males versus females walking in to the store differ based on day of the week

#P low null go. Males versus females walking in to the store differ based on day of the week