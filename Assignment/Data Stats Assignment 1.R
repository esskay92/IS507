
# Question 5 part (a)
1 - pnorm(-1.13,mean = 0, sd = 1)

# Question 5 part (b)
pnorm(0.18,mean = 0, sd = 1)

# Question 5 part (c)
1 - pnorm(8, mean = 0, sd = 1)

# Question 5 part (d)
p1 <- pnorm(-0.5, mean = 0, sd = 1)
p2 <- pnorm(0.5, mean = 0, sd = 1)
p2 - p1

# Question 6 part (b)
curve(dnorm, from = -5, to=5)
abline(v=1.285714, col="blue")
abline(v=0.5215124, col="red")
text(1.285714+1, 0.3, "Verbal: 1.29",col="blue") 
text(0.5215124-1.5, 0.1, "Quantitative: 0.52", col="red") 

# Question 6 part (e)
pnorm(1.2857)
pnorm(0.5215)