library(prevtoinc)
?epmf
epmf(y)
#### Second Practical ####

#1.	Draw 1000 random samples from X~ Exp(£f=6) using R function ¡§rexp¡¨ 
#and transform the variable by its own cdf FX(x)=1-e^-lambda*x, s.t. Y=FX(x). 
#Comment on the distribution of Y.

set.seed(1001)
N <- 1000
lambda <- 6
x <- rexp(N, lambda)
x
hist(x)
#Transforming the variable by its own cdf
y <- 1-exp(-lambda*x)
emp
par(mfrow=c(1,2))
hist(y, freq = FALSE)
lines(seq(0,1,0.01),dunif(seq(0,1,0.01),0,1),col = "blue",lwd=2)
plot(ecdf(y),col='red',pch=19,xlab='y',ylab='ECDF(y)', lwd = 2)        #empirical cdf
lines(seq(0,1,0.01),punif(seq(0,1,0.01),0,1),col = "blue",lwd=2)

#from the plot of Fy we can see that it is a U[0,1] distribution.


























#2.	Draw 1000 samples from a Y ~ Exp (lambda = 1) distribution, 
#using just the samples drawn from X ~ U (0,1) with the R function "runif" 
#using probability integral transform. The steps involved are given below:
#	First draw 1000 samples from X ~ U (0,1)
#	Transform Y=F^(-1)X(X) with the inverse CDF F^(-1)X(X) =-lambdaf ln(1-x)
#	Now Y has the same distribution as F^(-1)Y(y)=1-e^-lambda*y


set.seed(1001)
x1 <- runif(1000,0,1)
x1
lambda1 <- 1
y1 <- -(1/lambda1)*log(1-x1)

#log(1-x1,base = exp(1))
par(mfrow=c(2,2))

hist(x1,col="orange")
hist(y1,probaility=TRUE, col='blue')

#empirical and actual pdf
plot(density(y1),col = "blue", lwd=2)

lines(seq(0,20,0.01),dexp(seq(0,20,0.01),1),col = "red",lwd=2)


#empirical and actual cdf of x
plot(ecdf(y1),col='red',pch=19,x1lab='y1',y1lab='ECDF(y1)',
     main="CDF of -lambda1*log(1-x1)")
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),1),col = "green",lwd=2)

#
































#Third Question

#For the above example use λ= 2,3,5. 
#Plot the histogram of x, PDF and cdf of Y 
#and comment on the empirical and actual distribution.

?pexp
#question 3
set.seed(1001)
x2 <- runif(1000,0,1)
x2
lambda2 <- 2
1/lambda2
y2 <- -(1/lambda2)*log(1-x2)

par(mfrow=c(2,2))

hist(x2,col="orange")
hist(y2,probaility=TRUE, col='blue')

#empirical and actual pdf
plot(density(y2),col = "blue", lwd=2)

lines(seq(0,20,0.01),dexp(seq(0,20,0.01),lambda2),col = "red",lwd=2)


#empirical and actual cdf of x
plot(ecdf(y2),col='red',pch=19,x1lab='y2',y1lab='ECDF(y2)',
     main="CDF of -lambda2*log(1-x2)")
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),lambda2),col = "green",lwd=2)

########Practise
set.seed(1001)
x2 <- runif(1000,0,1)
x2
lambda2 <- 1/2
1/lambda2
y2 <- -(1/lambda2)*log(1-x2)

par(mfrow=c(2,2))

hist(x2,col="orange")
hist(y2,probaility=TRUE, col='blue')

#empirical and actual pdf
plot(density(y2),col = "blue", lwd=2)

lines(seq(0,20,0.01),dexp(seq(0,20,0.01),lambda2),col = "red",lwd=2)


#empirical and actual cdf of x
plot(ecdf(y2),col='red',pch=19,x1lab='y2',y1lab='ECDF(y2)',
     main="CDF of -lambda2*log(1-x2)")
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),lambda2),col = "green",lwd=2)














lambda3 <- 1/3
y3 <- -(1/lambda3)*log(1-x2)

par(mfrow=c(2,2))

hist(x2,col="orange")
hist(y3,probability=TRUE, col='blue')

#empirical and actual pdf
plot(density(y3),col = "blue", lwd=2)

lines(seq(0,20,0.01),dexp(seq(0,20,0.01),lambda3),col = "red",lwd=2)


#empirical and actual cdf of x
plot(ecdf(y3),col='red',pch=19,x1lab='y3',y1lab='ECDF(y3)',
     main="CDF of -lambda3*log(1-x2)")
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),lambda3),col = "green",lwd=2)


lambda4 <- 1/5
y3 <- -(1/lambda4)*log(1-x2)

par(mfrow=c(2,2))

hist(x2,col="orange")
hist(y3,probaility=TRUE, col='blue')

#empirical and actual pdf
plot(density(y3),col = "blue", lwd=2)

lines(seq(0,20,0.01),dexp(seq(0,20,0.01),lambda4),col = "red",lwd=2)


#empirical and actual cdf of x
plot(ecdf(y3),col='red',pch=19,x1lab='y3',y1lab='ECDF(y3)',
     main="CDF of -lambda3*log(1-x2)")
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),lambda4),col = "green",lwd=2)


####Writing the statements in one loop####
par(mfrow=c(1,1))
set.seed(123)
x=runif(1000,0,1)
hist(x)

par(mfrow=c(3,2))
l=c(2,3,5)
for (i in 1:3)
  {
  set.seed(123)
  y=(-1/l[i])*(log(1-x)) #transformation of uniform to exp
    plot(density(y),main="PDF",xlab="y")
    lines(seq(0,10,0.01),dexp(seq(0,10,0.01),l[i]),col="red")
    legend("topright",c("Theoretical","Empirical"),col=c("Red","Black"),
         lty=1,cex=0.5)
  
  
  plot(ecdf(y),main="CDF",ylab="CDF",xlab="y")
  lines(seq(0,10,0.01),pexp(seq(0,10,0.01),l[i]),col="red")
  legend("bottomright",c("Theoretical","Empirical"),col=c("Red","Black"),
           lty=1,cex=0.5)
}

mtext("Comparison for lambda=2,3,5 respectively",outer=T,cex=1,line=-1.5)



#Fourth Question 

#U ~ U (0,1) distribution. Show that both -log U and -log(1-U)
#are exponential random variables.
#State the transformation required to generate data from exponential 
#distribution with mean λ (≠1). 
#Check equivalence of actual and empirical distribution graphically.

?log
set.seed(1001)
x <- runif(1000,0,1)
x
y <- -log(x)
#We take lambda value as 1.
par(mfrow=c(2,2))
#empirical and actual pdf
plot(density(y),col = "blue", lwd=2)

lines(seq(0,20,0.01),dexp(seq(0,20,0.01),1),col = "red",lwd=2)

#empirical and actual cdf of x
plot(ecdf(y),col='red',pch=19,xlab='y',ylab='ECDF(y)',
     main="CDF of -log(x)")
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),1),col = "green",lwd=2)

y1 <- -log(1-x)

#empirical pdf
plot(density(y1),col = "blue", lwd=2)
# actual pdf
lines(seq(0,20,0.01),dexp(seq(0,20,0.01),1),col = "red",lwd=2)

#empirical cdf
plot(ecdf(y1),col='red',pch=19,xlab='y1',ylab='ECDF(y1)',
     main="CDF of -log(1-x)")
#actual cdf
lines(seq(0,40,0.01),pexp(seq(0,40,0.01),1),col = "green",lwd=2)

#general lambda

lambda <- 5
y2 <- -lambda*log(1-x)


#empirical pdf
plot(density(y2),col = "blue", lwd=2)
#actual pdf
lines(seq(0,20,0.01),dexp(seq(0,20,0.01),1/lambda),col = "red",lwd=2)


## Question 5

# Draw 1000 samples from U(0,1)

x<-runif(1000,0,1)
x

# Transformation y=tan(??(x-0.5))

y<- tan(pi*(x-0.5))
y

hist(y, col="blue")
par(mfrow=c(1,2))

# Empirical pdf of transformed variable (y)
plot(density(y), col="green",  xlim = c(-10,10))

# Actual pdf 
lines(seq(-10,10,0.01),dcauchy(seq(-10,10,0.01),location = 0,scale = 1), col='black', lwd=2)

# Empirical cdf of transformed variable(y)
plot(ecdf(y),col='blue', pch=19, xlab='x', ylab = 'ECDF(x)', lwd = 2)

# Actual cdf of y
lines(seq(-150,1000,0.01), pcauchy(seq(-150,1000,0.01),location = 0,scale = 1), col='red', lwd=2)
#UPDATED CODE:
  ################################ OR ###################################
## Question 5

# Draw 1000 samples from U(0,1)
set.seed(111)
x<-runif(1000)
x

# Transformation y=tan(??(x-0.5))

y<- tan(pi*(x-0.5))
y
max(y)
min(y)
plot(density(y))
bins<-seq(-10,10,by=1)
Scores=cut(y,bins)
freq_table<-transform(table(Scores))
Emp_pdf<-transform(freq_table,Rel_Freq=prop.table(Freq))$Rel_Freq

Emp_pdffff<-transform(freq_table,Rel_Freq=prop.table(Freq))
?transform
#PDF
plot(seq(-9.5,9.5,1),Emp_pdf,type="l",ylim=c(0,0.5))
lines(seq(-10,10,0.01),dcauchy(seq(-10,10,0.01),0,1),col="red")


#############################
plot(density(y),col = "blue", lwd=2,xlim = c(-10,10))
lines(seq(-10,10,0.01),dcauchy(seq(-10,10,0.01),0,1),col="red")

?plot

#CDF
plot(ecdf(y),xlim=c(-10,10))
lines(seq(-10,10,0.01),pcauchy(seq(-10,10,0.01),0,1),col="green")


cot(1)

tan(1)
?Trig


####6
set.seed(1000)
u=runif(600)
x=-1/tan(u*pi)



#empirical pdf
plot(density(x),col='blue',lwd=2,xlim=c(-10,10))
#actual pdf
lines(seq(-10,10,0.01),dcauchy(seq(-10,10,0.01),1),col='red',lwd=2)






#empirical pdf



plot(ecdf(x),col='red',pch=19,xlab='Y',ylab="ECDF(Y)",
     main="CDF of Y",lwd=3,xlim=c(-10,10))
#actual cdf
lines(seq(-10,10,0.01),pcauchy(seq(-10,10,0.01),1),col='green',lwd=2)



bins<-seq(-10,10,by=1)
Scores=cut(x,bins)
freq_table<-transform(table(Scores))
Emp_pdf<-transform(freq_table,Rel_Freq=prop.table(Freq))$Rel_Freq

Emp_pdffff<-transform(freq_table,Rel_Freq=prop.table(Freq))
?transform
#PDF
plot(seq(-9.5,9.5,1),Emp_pdf,type="l",ylim=c(0,0.5))
lines(seq(-10,10,0.01),dcauchy(seq(-10,10,0.01),0,1),col="red")
  