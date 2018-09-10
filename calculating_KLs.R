
#the above package wasn't being super transparent to me to be honest, so I had a look around some more
#calc k-l divergence based on this equation in an answer to this SO question:
#https://stats.stackexchange.com/questions/7440/kl-divergence-between-two-univariate-gaussians :
#log(σ2/σ1) + ((σ1^2 + (μ1−μ2)2^2) / (2*σ2^2) )− 1/2
#Note: this says it's for Gaussian so should think about how this might not be right... (for later)

calc_KL <- function(prior, posterior) {
  return(log(sd(unlist(posterior))/sd(unlist(prior))) +
           (((sd(unlist(prior))^2 + (mean(unlist(prior)) - mean(unlist(posterior)))^2) /(2*sd(unlist(posterior))^2))) - 1/2)
}

calc_KL2 <- function(prior, posterior) {

  s = 0

  # print(posterior)
  #  print(prior)
  for (i in 1:length(unlist(prior))) {
    if(unlist(posterior)[i] > 0){
      thing <- (unlist(prior)[i] * log( unlist(prior)[i]/unlist(posterior)[i]))
      print(paste(unlist(prior)[i], unlist(posterior)[i], log( unlist(prior)[i]/unlist(posterior)[i])))
      if (!is.nan(thing)) {
        s <- s + thing

        print (s)
      }
    }
  }
  return(s)

}
calc_KL2(nc2015gor$priordist[1], nc2015gor$posteriordist[1])


nc2015gor$kl2 <- mapply(calc_KL2, nc2015gor$priordist, nc2015gor$posteriordist)



calc_KL3 <- function(prior, posterior) {

  s = 0

  # print(posterior)
  #  print(prior)
  for (i in 1:length(unlist(prior))) {
    if(unlist(posterior)[i] > 0){
      thing <- (unlist(prior)[i] * log( unlist(prior)[i]/unlist(posterior)[i]))
      print(paste(unlist(prior)[i], unlist(posterior)[i], log( unlist(prior)[i]/unlist(posterior)[i])))
      if (!is.nan(thing)) {
        s <- s + thing

        print (s)
      }
    }
  }
  return(s)

}



#make plot with KL score
pdf( "klplots5.pdf", width = 10, height = 7)
par(mfrow=c(3,3))
for(i in 1:9){
  plot(th,unlist(nc2015gor$priordist[i]),
       type="l",
       ylab="Density",
       main = nc2015gor$rgn15nm[i],
       sub = paste0("KL divergence: ",round(nc2015gor$kl2[i], 3)),
       lty="longdash",
       lwd=2,
       col = "blue",
       xlab = expression(theta),
       xlim = c(0.001, 0.01),
       ylim = c(0, 2))
  lines(th, unlist(nc2015gor$likelidist[i]), col = 'red', lty = "dotted", lwd=2)
  lines(th, unlist(nc2015gor$posteriordist[i]), col = 'green', lwd=2)
  legend(x = "topleft",
         legend=c("prior", "likelihood", "posterior"),
         col=c("blue", "red", "green"),
         lty=c("longdash", "dotted", "solid"))
}
dev.off()

