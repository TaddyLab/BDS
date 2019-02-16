a <- 1
b <- 1
qgrid <- seq(0,1,length=500)
plot(qgrid, dbeta(qgrid, a, b), ylim=c(0,10), col=8,
	type="l", bty="n", lwd=1, xlab="q", ylab="posterior density")

K <- 20
lcols <- terrain.colors(K*1.1)[K:1]
lg <- c(1,5,10,15,20)

for(k in 1:K){
n <- 5
x <- rbinom(n, p=1/3, size=1)
a <- a + sum(x)
b <- b + n - sum(x)
print(c(a,b))
lines(qgrid, dbeta(qgrid,a,b), col=lcols[k], lwd=1)
}
legend("topright", bty="n", title="sample size", 
	legend=c(0,n*lg), col=c(8,lcols[lg]), lwd=1)
