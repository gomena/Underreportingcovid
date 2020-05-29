

fill_between <-function(caselist, ndays){
  caselist = pmax(t$new_cases_symptom,0)
  ndays = num_days
  
  newcases = matrix(NA, length(caselist), ndays)
  prob = matrix(NA, length(caselist), ndays)
  
  
  for(i in 1:length(caselist)){
    prob[i,]=rep(1/ndays, ndays)
  }
  for(i in 1:ndays){
    prob[dim(prob)[1]-ndays+i,1:i-1]=0
    prob[dim(prob)[1]-ndays+i,] =prob[dim(prob)[1]-ndays+i,]/sum(prob[dim(prob)[1]-ndays+i,]) 
  }
  for(i in 1:length(caselist)){
    newcases[i,]=rowSums(rmultinom(caselist[i],1, prob[i,]))
  }
  return(newcases)
}
