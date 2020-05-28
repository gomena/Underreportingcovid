if (!require(MCMCpack)) {
  install.packages("MCMCpack")
  require(MCMCpack)
}
#by gonzalo contador
fill2 <- function(col1, col2, dcases, dsym,  distribution="FCFC", parameter=1) {
  col1[is.na(col1)]=0
  col2[is.na(col2)]=0
  nsem = length(col1)
  ninf = length(col2)
  j=1
  ma = matrix(0, nrow = nsem, ncol = ninf)
  if(distribution=="FCFC"){
    if(sum(col1, na.rm=TRUE)==0 | sum(col2, na.rm=TRUE)==0){return(ma)}
    for(i in 1:nsem){
      if(col1[i]==0){
        next
      }else{
        if(j<=ninf){
          #print(c("hola",i,col1[i],j,col2[j]))
          flag=1
          while(flag==1){
            if(col1[i]<=col2[j]){
              ma[i,j] = col1[i]
              col2[j] = col2[j]-col1[i]
              flag=0
              
            }
            else{ ma[i,j] = col2[j]
            col1[i] = col1[i]-col2[j]
            j=j+1
            if(j>ninf){
              flag=0
            }}
          }}}}}
  else{
    while(j<ninf){
      for(w in 1:col2[j]){
        datestosample<-which(dsym <= dcases[j])
        datestosample<-datestosample[col1[datestosample]>0]
        if(distribution=="uniform"){probs = col1[datestosample]}
        if(distribution=="poisson"){probs = dpois(as.numeric(dcases[j]-dsym[datestosample]),parameter)}
        if(distribution=="inverse"){probs=1/col1[datestosample]}
        if(distribution=="olderfirst"){probs=as.numeric(dcases[j]-dsym[datestosample])*col1[datestosample]}
        if(distribution=="other"){probs<-rev(parameter)
        if(length(probs)<length(datestosample)){probs<-probs[1:length(probs)]}
        else{datestosample<-datestosample[1:length(probs)]}}
        if(distribution=="dirichlet"){
          alpha<-rep(0,length(parameter))
          for(z in 1:length(parameter)){
            index<-which(as.numeric(dcases[j]-dsym[datestosample])==z)
            if(length(z)>0){alpha(z)<-parameter[z]}
          }
          probs<-rdirichlet(1,alpha)
          datestosample<-datestosample[(length(datestosample)-length(parameter)+1):length(datestosample)]
        }
        if(length(datestosample)>0){
          aux<-sample(datestosample,1, replace = TRUE, probs)
          ma[aux,j]<-ma[aux,j]+1
          col2[j]<-col2[j]-1
          col1[aux]<-col1[aux]-1
        }
      }
      j<-j+1
    }
  }
  return(ma)
}


col1 = delay_ini$new_cases_symptom
wh = !is.na(delay_ini$new_cases_daily)
col2 = delay_ini$new_cases_daily[wh]
dcases = delay_ini$date_symp[wh]
dsym= delay_ini$date_symp
filled=fill_old(symp,cases)

filled2=fill2(col1,col2,datescases,datessym, distribution="olderfirst", parameter=1)


