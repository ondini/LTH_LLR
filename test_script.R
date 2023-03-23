# test script

x <- c(3, 5, 7, 11, 13)

x + 3
exp(x)
y <- c(17, 19, 23, 29, 31)
z <- c(x, y)
z
seq(1, 102, 9)
seq(from = 1, to= 100, length.out = 12)
1:3
3:1
rep(c(1, 2, 3), times = 3)
rep(1:3, each = 4)
rep(1:3, t = 3, e = 4)
rep(1:3, length.out = 20)
?seq
help(seq)

# getting only some values
myvalues <- 21:30
myvalues
myvalues[1]
myvalues[c(1, 3, 5)]
myvalues[1:3]

# excluding values
myvalues[-1]
myvalues[-c(1, 3, 5)]
myvalues[-(2:4)]

x <- rnorm(100)
mean(x)
var(x)^2
sd(x)
hist(x, freq=TRUE)

ls() # list environment
remove(list = ls()) # clear environment


# matrices
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
B <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
C <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
t(A)

AB <- A %*% B # matrix mult
BA <- B %*% A

solve(AB) #matrix inverse - in this case singular
solve(BA)

qr(AB)
qr(AB)$rank

eigen(AB)$values

plot(AB[,2] ~ AB[,1])
abline(lm(AB[,2] ~ AB[,1]))


I3 <- diag(3)
4+I3