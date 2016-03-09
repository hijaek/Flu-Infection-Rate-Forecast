attach(flu)
length(week)
length(season)
 
#######Plot all WILI#######
plot(week-20, wili, main = "wili vs week")
 
 
#######draw avg of mean of wili####
onea.graph = NULL
wili.graph = NULL
for(i in 21:72)
{
  onea.graph[i]= mean(wili[week==i & season != 11])
}
 
plot(week-20, wili, main = "wili vs week")
points((21:72)-20, onea.graph[21:72], xlab="weeks", ylab="average wili", col="red", lwd=3, pch = 16)
lines((21:72)-20, onea.graph[21:72], xlab="weeks", ylab="average wili", col="red", lwd=2)
#plot((21:72)-20, onea.graph[21:72], xlab="weeks", ylab="average wili", main="10yr avg wili vs week")
#this is also a model, a really bad one
 
 
#######Forecasting season 11######
 
smooth.df=numeric(10)
vec<-numeric(0)
err.cv=matrix(0)
err.cv.newdf=matrix(0)
 
for(i in 1:10)
{
  smooth.flua = smooth.spline(week[season==1], wili[season==i], cv = TRUE)
  flu.pred = predict(smooth.flua, week[season==i][1:29])
  err.cv[i]= mean((wili[season==11][1:29]-flu.pred$y)^2)
 
  smooth.flua.newdf= smooth.spline(week[season==1], wili[season==i], df =0.75*smooth.flua$df)
  flu.pred.newdf = predict(smooth.flua.newdf, week[season==i][1:29])
  err.cv.newdf[i] = mean((wili[season==11][1:29]-flu.pred.newdf$y)^2)
}
 
########which season's spline best fits season 11?######
err.cv
which(err.cv == min(err.cv))
# 0.01001347 - SEASON8
 
err.cv.newdf
which(err.cv.newdf == min(err.cv.newdf))
# 0.009079415 - SEASON8
 
 
 
 
#############give me the forecast between 50 to 72#########
season.eight = smooth.spline(week[season==8], wili[season==8], cv = TRUE)
season.eight.new = smooth.spline(week[season==8],wili[season==8], df=0.75 * season.eight$df)
one = predict(season.eight.new, week[30:52])
one
 
 
####checking the error again
####qqq = predict(season.eight.new, week[1:29])
####asdf = mean((wili[season==11][1:29]-qqq$y)^2)
####season3 with the lowest err = 0.008668962 - MATCHES YESSS##
 
 
 
 
##############what is an estimate of the test error of your forecast?#################
j=i=0
sf.cv =matrix()
sf.newdf.cv=matrix()
flu.pred.newdf.cv=matrix()
err.newdf.cv=matrix(0,10,10)
cvnum =matrix()
cv.testpredict=matrix()
cv.testerr=matrix()
 
 
for(j in (1:10))
{
  for (i in (1:10)[-c(j)])
  {
    sf.cv = smooth.spline(week[season==1], wili[season==i], cv = TRUE)
    sf.newdf.cv= smooth.spline(week[season==1], wili[season==i], df =0.75*sf.cv$df)
    flu.pred.newdf.cv = predict(sf.newdf.cv, week[season==i][1:29])
   
    
    err.newdf.cv[j,i] = mean((wili[season==j][1:29]-flu.pred.newdf.cv$y)^2)
    #cvnum[j]=which(err.newdf.cv[j,] == min(err.newdf.cv[j,][err.newdf.cv[j,]!=min(err.newdf.cv[j,])]))
    cvnum[j]=which(err.newdf.cv[j,] == min(err.newdf.cv[j,][err.newdf.cv[j,]!=0]))
   
    cv.testpredict = predict(sf.newdf.cv, week[season==i][30:52])
    cv.testerr[j] = mean((wili[season==j][30:52]-cv.testpredict$y)^2)
   
    #flu.pred.test = predict(smooth.test, week[season==i][30:52])
    #err.cv.test[i] = mean((flu.pred.test$y - one$y)^2)
  }
}
 
cv.testerr
mean(cv.testerr)
 
 
#####testing. sth's wrong....#####
#plot(wili[season==7][1:52])
#sf.cv = smooth.spline(week[season==1], wili[season==10], cv = TRUE)
#sf.newdf.cv= smooth.spline(week[season==1], wili[season==10], df =0.75*sf.cv$df)
#cv.testpredict = predict(sf.newdf.cv, week[season==10])
#lines((cv.testpredict$y))
#mean((wili[season==7][30:52]-cv.testpredict$y[30:52])^2)
 
 
 
 
####draw a predcited line for 30:52#####
plot(week-20, wili, main = "Season 11 wili forecast and its peak", ylab="WILI", xlab="Week")
lines(week[season==1][30:52]-20, one$y, xlab="week", ylab="wili forecast", col = "red", lwd=3)
 
#########What's the maximum of the predicted year?##
 
max(one$y)
which(one$y==max(one$y))+29
#week 38 is the peak.
points(which(one$y==max(one$y))+29, max(one$y), lwd=5, col="red", pch = 19)
 
 
##only compare with the 8th season
plot(week[season==8]-20, wili[season==8], main = "Season 8 and Season 11's forecast", ylab="WILI", xlab="Week")
points(week[season==11]-20,wili[season==11], lwd=1, col="red")
lines(week[season==1][30:52]-20, one$y, xlab="week", ylab="wili forecast", col = "red", lwd=3)
 
 
 
 
 
 
 
############bootstrap for estimting point estimate variability#######
 
 
#######create datasets for 500 times############right now it's 10
final = data.frame()
 
#####bootstrap size
bootsize=500
##make the sample
for (i in 1:bootsize) {
 
newwili=matrix(nrow=52,ncol=10)
 
for (w in 1:52)
  {
    newwili[w,]=sample(wili[week==w+20], 10, replace=TRUE)
  }
# first 10 years
newwili = data.frame(newwili)
newwili_ten = data.frame(matrix(0,ncol=3,nrow=520))
for (c in 0:9) {
  newwili_ten[(52*c+1):(52*c+52),1] = newwili[,c+1]
  newwili_ten[(52*c+1):(52*c+52),2] = rep(c+1,52)
  newwili_ten[(52*c+1):(52*c+52),3] = c(21:72)
}
 
colnames(newwili_ten) = c('wili2','season2','week2')
# 11th year
newwili_ele = data.frame(matrix(0,ncol=3,nrow=29))
for (w in 1:29)
  {
    newwili_ele[w,] = sample(wili[week==w+20], 1, replace=TRUE)
    newwili_ele[,2] = rep(11,29)
    newwili_ele[,3] = c(21:49)
  }
colnames(newwili_ele) = c('wili2','season2','week2')
#we combine
newdb = rbind(newwili_ten, newwili_ele)
#View(newdb)
newdb = cbind(newdb, rep(i,nrow(newdb)))
names(newdb)[4] = 'sample'
final = rbind(final,newdb)
print(i)
}
 
#View(final)
 
############################# Run the model with bootstrap data set##############################
 
#terror=matrix()
#View(final)
#attach(final)
attach(final)
finalpeak=matrix()
 
 
for(m in 1:bootsize){
 
smooth.df=numeric(10)
vec<-numeric(0)
err.cv=matrix(0)
err.cv.newdf=matrix(0)
 
for(i in 1:10)
{
  smooth.flua = smooth.spline(week2[sample==1][season2==1][1:52], wili2[sample==m][season2==i][1:52], cv = TRUE)
  flu.pred = predict(smooth.flua, week2[season2==i][1:29])
  err.cv[i]= mean((wili2[sample==m][season2==11][1:29]-flu.pred$y)^2)
 
  smooth.flua.newdf= smooth.spline(week2[sample==1][season2==1][1:52], wili2[sample==m][season2==i][1:52], df =0.75*smooth.flua$df)
  flu.pred.newdf = predict(smooth.flua.newdf, week2[sample==m][season2==i][1:29])
  err.cv.newdf[i] = mean((wili2[sample==m][season2==11][1:29]-flu.pred.newdf$y)^2)
}
 
 
 
##
bestseason = smooth.spline(week2[sample==m][season2[sample==m]==which(err.cv == min(err.cv))], wili2[sample==m][season2[sample==m]==which(err.cv == min(err.cv))], cv = TRUE)
##
bestseason.new = smooth.spline(week2[sample==m][season2[sample==m]==which(err.cv.newdf == min(err.cv.newdf))],wili2[sample==m][season2[sample==m]==which(err.cv.newdf == min(err.cv.newdf))], df=0.75 * bestseason$df)
two = predict(bestseason.new, week2[sample==m][30:52])
 
print(m)
finalpeak[m]=max(two$y)
 
}
 
 
#the end. happy ever after
mean(finalpeak)
sd(finalpeak)
