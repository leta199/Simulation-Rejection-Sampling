#Monte Carlo Integration- Integration by Simulation-------------------------
#Defining our target pdf(density function to integrate) --------------------------

target_pdf<-function(x){
  (x*exp(-x))/(6*exp(-5))
}

#Defining porposal pdf (density that we sample from a compare to target)--
proposal_pdf<- function(x){
  5+ rexp(1, rate = 1)
}
#what curve looks like 
curve(target_pdf(x), from = 5, to =10)

sim_gamma<-function(){
  set.seed(123)
  n<-5000
  sample_y<-numeric(n)
  count<-0
  while(count<n) {
    y<-proposal_pdf(1)
    #uniform comparison
    u<- runif(1,0,1)
    #accept or reject criteria
    
    if( u <target_pdf(y)/proposal_pdf(y))
      count<- count +1
    sample_y_9[count]<-y
    
  }
  return(sample_y_9)
}

hist(sim_gamma_9(), freq = FALSE)
curve(target_9(x), add = TRUE)