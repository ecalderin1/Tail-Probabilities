# Tail-Probabilities
Tail probabilities Compound Poisson

upper_incomplete_gamma <- function(s, x) {
  gamma_s <- gamma(s)  # Complete gamma function
  lower_gamma <- gamma(s)*pgamma(x, shape = s,1)  # Lower incomplete gamma function
  return(gamma_s - lower_gamma)
}


###################Total Probabilities Compound Poisson




comp_prob<- function(n,x,lambda,theta){

  p0=exp(-lambda) 
  sum_prob <- 0
  
  for(k in 1:n) {
    
    
    compute_sum <- function(k, x,theta) {
      sum_value <- 0
      for (i in 0:k) {
        sum_value <- sum_value + ((theta^i) / (1 + theta)^k) *  choose(k,i) *(upper_incomplete_gamma(2*k-i,x*theta)/gamma(2*k-i))
      }
      return(1-sum_value)
    }
    
  
  sum_prob<-sum_prob+(exp(-lambda)*lambda^k/factorial(k))* compute_sum(k,x,theta)
  
  }
  return(p0+sum_prob)
  
}
x <- seq(0,10,1)

1-comp_prob(85,x,1,0.75)


x <- seq(0.001,13,0.001)
y1=1-comp_prob(85,x,1,0.5)
y2=1-comp_prob(85,x,1,1.0)
y3=1-comp_prob(85,x,1,1.5)
y4=1-comp_prob(85,x,1,2.0)
par(mar = c(5, 7, 4, 2))  # Increase space on the left margin
plot(x,y1,ylab = expression( bar(F)[S](x)),main="Compound Poisson",col="white",lwd = 0.25,  cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
lines(x, y1,  lwd = 2, lty = 1) 
lines(x, y2,  lwd = 2, lty = 2) 
lines(x, y3, lwd = 2, lty = 3) 
lines(x, y4, lwd = 2, lty = 4)  
legend(10.25,0.60,legend = c(expression(paste( theta, " = 0.5")),expression(paste( theta, " = 1.0")),expression(paste( theta, " = 1.5")), expression(paste( theta, " = 2.0")) ), lty = c(1,2,3,4), lwd = 2,bty = "n")



