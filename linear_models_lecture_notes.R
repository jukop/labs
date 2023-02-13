
set.seed(40)
n <- 8
x <- 0+runif(n,0,10)
y <- 4.0 + 5.3*x + rnorm(n,sd=5)
dat <- data.frame(x=round(x,2),y=round(y))
knitr::kable(dat)

plot(dat$x,dat$y,xlim=c(0,10),xlab="x",ylab="y")
m1 <- lm(y~x,data=dat)
abline(coef(m1))

m1 <- lm(y ~ x, data=dat)
m1

fitted(m1)

predict(m1,newdata = data.frame(x=c(10,11)))

dat <- read.csv("femaleMiceWeights.csv")
stripchart(Bodyweight ~ Diet, data=dat, vertical=TRUE,
method="jitter", pch=1, main="Mice weights")

m1 <- lm(Bodyweight ~ Diet, data=dat)
summary(m1)

t.test(Bodyweight ~ Diet, data=dat, var.equal=T)

A <- matrix(c(1,2,
              3,4,
              5,6),nrow=3,ncol=2,byrow=T)
A
t(A)

d <- c(4,5,6)
d
t(d)
t(t(d))
c(1,2,3) %*% d

diag(3)

# Let's create a square matrix
B <- t(A) %*% A
B
det(B) #determinant is not zero => invertible
solve(B)

A <- matrix(c(1, 1, 1,
              3,-2, 1,
              2, 1,-1),nrow=3,ncol=3,byrow=T)
d <- c(6,2,1)
solve(A) %*% d

## model.matrix(~ Diet, data=dat)
## # or
## model.matrix(m1)

spider <- read.csv("spider_wolff_gorb_2013.csv", skip=1)

## View(spider)

head(spider)

boxplot(spider$friction ~ spider$type * spider$leg, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs")

X <- model.matrix(~ type + leg, data=spider)
## View(X)

fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)
coefs <- coef(fitTL)
coefs

Y <- spider$friction
X <- model.matrix(~ type + leg, data=spider)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y
t(beta.hat)

spider$group <- factor(paste0(spider$leg, spider$type))
stripchart(split(spider$friction, spider$group), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,11), ylim=c(0,2))
a <- -0.25
lgth <- .1
library(RColorBrewer)
cols <- RColorBrewer::brewer.pal(5,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")

coef(fitTL)

library(contrast) #Available from CRAN
L3vsL2 <- contrast(fitTL,list(leg="L3",type="pull"),list(leg="L2",type="pull"))
L3vsL2

coefs[4] - coefs[3]
(cT <- L3vsL2$X)
cT %*% coefs
