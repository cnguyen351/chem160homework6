setwd("D:/Chem 160/chem160homework6")
secher<-read.table("secher.txt",header=T)
model1<-lm(bwt~bpd, data=secher)
sum.model1<-summary(model1)
R2<-sum.model1$r.squared
f<-sum.model1$fstatistic 
p.value1<-pf(f[1],f[2],f[3],lower.tail=F)
intercept1<-model1$coefficients[1]
slope1<-model1$coefficients[2] 
output1<-sprintf("bwt~bpd R2 = %f and p-value=%e", R2, p.value1)
output2<-sprintf("bwt~bpd slope=%f and intercept=%f",slope1, intercept1)
cat(output1)
cat(output2)
png("bwt_bpd.png")
plot(bwt~bpd, data=secher)
abline(model1)
dev.off()

model2<-lm(bwt~ad, data=secher)
sum.model2<-summary(model2)
R22<-sum.model2$r.squared
f2<-sum.model2$fstatistic 
p.value2<-pf(f[1],f[2],f[3],lower.tail=F)
intercept2<-model2$coefficients[1]
slope2<-model2$coefficients[2] 
output3<-sprintf(" bwt~ad R2 = %f and p-value=%e", R22, p.value2)
output4<-sprintf(" bwt~ad slope=%f and intercept=%f",slope2, intercept2,'\n')
cat(output3)
cat(output4)
png("bwt_ad.png")
plot(bwt~ad, data=secher)
abline(model2)
dev.off()

model3<-lm(bwt~bpd+ad, data=secher)
sum.model3<-summary(model3)
R222<-sum.model3$r.squared
f3<-sum.model3$fstatistic
p.value3<-pf(f3[1],f3[2],f3[3],lower.tail=F)
output5<-sprintf(" bwt~bpd+ad R2 = %f and p-value=%e", R222, p.value3,'\n')
cat(output5,'\n')
