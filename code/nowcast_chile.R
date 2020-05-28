library(data.table)
library(jsonlite)
library(dplyr)
library(purrr)
library(janitor)
library(binr)
library(tidyverse)
library(hrbrthemes)
library(NLP)
library(readxl)
library(naniar)
library(forcats)
library(ggplot2)
library(scales)
library(readr)
library(NobBS)
Sintomas_Notificacion <- read_csv("Documents/Datos-COVID19/Sintomas-Notificacion.csv")
#View(Sintomas_Notificacion)

Xdf = Sintomas_Notificacion[ , grepl( "X" , names( Sintomas_Notificacion ) ) ]

reporteddf = Sintomas_Notificacion[ , grepl( "Reportado" , names( Sintomas_Notificacion ) ) ]

reportednames = colnames((reporteddf))
day = str_sub(colnames(reporteddf), start=14,end=15)
month = str_sub(colnames(reporteddf), start=17,end=18)
dates = paste("2020", paste(month, day,sep="-"), sep="-")
dates2 = c('date', dates)
setnames(Xdf %>% as_tibble(), old=colnames(Xdf), new = dates2)
setnames(reporteddf %>% as_tibble(), old=colnames(reporteddf), new = dates)
dates = Xdf$date
a=str_locate(pattern='2020',dates)
day = str_pad(str_sub(dates, start=3, end=a[,1]-2), 2, pad="0")
month = paste("0", str_sub(dates, start=1, end=1), sep="")
dates = paste("2020", paste(month, day, sep='-'),sep='-')
Xdf = Xdf[-1,] %>% mutate_all(funs(replace(., .=='-', NA))) %>% mutate(date = dates[-1]) %>% pivot_longer(-date, names_to = 'date_reported', values_to='new_cases')

reporteddf = reporteddf[-1,] %>% mutate_all(funs(replace(., .=='-', NA))) %>% mutate(date = dates[-1]) %>% 
  pivot_longer(-date, names_to = 'date_reported', values_to='new_cases_symptom')

delay = Xdf %>% full_join(reporteddf) %>% mutate(date = as.Date(date), date_reported = as.Date(date_reported), 
                                                 new_cases = as.integer(new_cases), new_cases_symptom = as.integer(new_cases_symptom)) %>%
  mutate_at(vars(new_cases, new_cases_symptom),funs(replace(., is.na(.), 0)))  %>%rename(date_symp = date) %>% 
  mutate(difnum =as.integer(date_reported-date_symp) ) %>% mutate(dif = difnum>=25)

#delay %>% group_by(date,date_reported) %>% summarize_at(new_cases_symptom_old) mutate_at(new_cases_symptom = funs())

delay = delay %>% full_join(delay %>% group_by(date_symp)  %>% summarise(med = median(new_cases_symptom[dif==TRUE]), 
                                                                         maxi = max(new_cases_symptom[dif==FALSE])), by=c("date_symp"))%>% 
  mutate(new_cases_symptom = replace(new_cases_symptom, which(dif==TRUE), med[dif==TRUE])) %>%
  mutate(new_cases_symptom = replace(new_cases_symptom, which(dif==TRUE), pmax(new_cases_symptom[dif==TRUE], maxi[dif==TRUE]))) %>%
  mutate(new_cases_symptom = replace(new_cases_symptom, which(is.na(med)), new_cases_symptom[is.na(med)])) %>%
  dplyr::select(c("date_symp",'date_reported', 'new_cases_symptom')) %>% group_by(date_symp) %>%
  mutate(new_cases_symptom= c(new_cases_symptom[1], (new_cases_symptom - lag(new_cases_symptom))[-1])) %>%
  mutate(date_reported_num = as.integer(date_reported)) %>% 
    mutate(date_reported_num_dif= c(date_reported_num[1], (date_reported_num - lag(date_reported_num))[-1])) 

dates_missed =unique(delay$date_reported[delay$date_reported_num_dif>1 & delay$date_reported_num_dif<100])
delay2 = delay
for(i in 1:length(dates_missed)){
  t = delay %>% filter(date_reported == dates_missed[i] & date_symp<=dates_missed[i])
  num_days = t$date_reported_num_dif[1]
  f = fill_between(pmax(t$new_cases_symptom,0), num_days) %>% as_tibble() 
  setnames(f, old=colnames(f), new =as.character(dates_missed[i]-seq(num_days-1,0))) 
  f = f%>% mutate(date_symp= t$date_symp) %>% 
    pivot_longer(-c('date_symp'), names_to ='date_reported', values_to='new_cases_symptom') %>% mutate(date_reported = as.Date(date_reported))
  delay2 = delay2 %>% filter(date_reported!=dates_missed[i]) %>% full_join(f)
}
delay2 = delay2 %>%  mutate(difnum =as.integer(date_reported-date_symp) ) 



CasosTotales <- fread("Documents/Datos-COVID19/output/producto5/TotalesNacionales.csv") %>% 
  pivot_longer(-Fecha, names_to='date_reported', values_to = 'cases') %>%
  filter(Fecha=='Casos totales') %>%
  mutate(new_cases_daily = c(cases[1], (cases - lag(cases))[-1])) %>% dplyr::select(-Fecha) %>% 
  mutate(date_reported= as.Date(date_reported)-1) 

date_ini= as.Date("2020-04-13")
delay_ini = delay2 %>% filter(date_reported==date_ini ) %>% 
  full_join(CasosTotales %>% rename(date_symp = date_reported) %>% 
              mutate(date_symp =date_symp)) %>% filter(date_symp<=date_ini)

symp = delay_ini$new_cases_symptom
wh = !is.na(delay_ini$new_cases_daily)
cases = delay_ini$new_cases_daily[wh]
datescases = delay_ini$date_symp[wh]
datessym= delay_ini$date_symp
filled_old=fill_old(symp,cases)
filled=fill2(symp,cases,datescases,datessym, distribution="uniform", parameter=1)
d=as_tibble(filled_old) 
setnames(d,old=colnames(d), new = as.character(datescases))
d = d %>% mutate(date_symp = datessym) %>% pivot_longer(-date_symp, names_to='date_reported', 
                                                        values_to = 'new_cases_symptom') %>% 
  filter(new_cases_symptom>0)%>% mutate(date_reported = as.Date(date_reported))

delay2_symptom = delay2 %>% filter(date_reported>date_ini) %>% full_join(d) %>% 
  mutate_at(vars(new_cases_symptom),funs(replace(., .<0, 0))) 

today = as.Date("2020-05-10")

#nowcast2 <- NobBS(data.frame(delay2_symptom %>% filter(date_reported<=today)  %>% uncount(new_cases_symptom) ), today,units="1 day",
#                 onset_date="date_symp",report_date="date_reported", max_D =25, specs=list(dist='NB',nChains=2))

#alphat.shape.prior = 100, 
#alphat.rate.prior = 0.0001
#alpha1.prec.prior=100000000
#specs$
nowcast2 <- NobBS(data.frame(delay2_symptom %>% filter(date_reported<=today)  %>% 
                               uncount(new_cases_symptom) ), today,units="1 day",
                  onset_date="date_symp",
                  report_date="date_reported", max_D =25, 
                  specs=list(dist='NB',nSamp=10000, nThin=1, nBurnin=5000,
                             alpha1.prec.prior=0.001, alphat.shape.prior = 0.001,alphat.rate.prior=0.01,
                             dispersion.prior <- c(0.001, 0.001)))

#nowcast2 <- NobBS(data.frame(delay2_symptom %>% filter(date_reported<=today)  %>% 
 #                              uncount(new_cases_symptom) ), today,units="1 day",
  #                onset_date="date_symp",
   #               report_date="date_reported", max_D =25, 
    #              specs=list(dist='Poisson',nSamp=10000, nThin=1, nBurnin=5000))

nowcast_df = data.frame(date_symp = nowcast2$estimates$onset_date, 
                        lower=nowcast2$estimates$lower, upper=nowcast2$estimates$upper, 
                        estimate = nowcast2$estimates$estimate)

total_cases_past=delay2_symptom %>% filter(date_reported<=today) %>% group_by(date_symp) %>% summarize_at(vars(new_cases_symptom),funs(sum(., na.rm=TRUE))) %>% rename(new_cases_symptom_past =new_cases_symptom)
total_cases=delay2_symptom %>% group_by(date_symp) %>% summarize_at(vars(new_cases_symptom), funs(sum(., na.rm=TRUE)))

total_cases_all = total_cases %>% full_join(total_cases_past) %>% 
  inner_join(nowcast_df) %>% inner_join(CasosTotales %>% rename(date_symp = date_reported) %>% dplyr::select(-cases)) %>%
  pivot_longer(c('new_cases_symptom_past','new_cases_symptom', 'new_cases_daily'), names_to='time', values_to='cases') %>% 
  filter(date_symp<=today)
total_cases_all$date_symp = as.POSIXct(total_cases_all$date_symp)
date_ini2 = today-30
total_cases_all = total_cases_all %>% filter(date_symp>=date_ini2)
total_cases_all %>% ggplot(aes(x=date_symp, y=cases, fill=time)) + geom_bar(stat = "identity",position='dodge') + 
  geom_line(aes(x=date_symp, y=estimate),label='Nowcasting')+
  geom_ribbon(aes(x=date_symp,ymin=lower,ymax=upper), alpha=0.3) + 
  scale_x_datetime(date_breaks = "2 day", labels = date_format("%b %d")) +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        plot.subtitle = element_text(color="black", size=12, face="bold"),
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 8, angle = 30),
        axis.text.y = element_text(color = "black", 
                                   size = 8),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12)) +ylab('Casos') +xlab('Fecha de inicio de síntomas') + 
  labs(title="Incidencia diaria por fecha de inicio de síntomas en Chile, ",
       subtitle=paste("hasta", today, sep=" "),
       caption="datos: https://github.com/MinCiencia/Datos-COVID19", fill='Fecha de reporte', size=3)+
  scale_fill_discrete(labels = c(paste("hasta", today, "(por diagnóstico PCR)", sep=" "),
                                 paste("Hasta", max(delay2$date_reported), "(por síntoma)",sep=" "),
                                 paste("Hasta", today, "(por síntoma)",sep=" ")))

posterior_lambda=nowcast2$params.post[,lambdanames]
betanames = grep("Beta", colnames(nowcast2$params.post), value=TRUE)

posterior_beta=nowcast2$params.post[,betanames]
m=exp(colMeans(posterior_beta))
m=m/sum(m)

#histogram plot
delay2 %>% filter(date_symp>=as.Date("2020-04-13") & date_symp<=as.Date("2020-04-27") & date_reported<=as.Date("2020-04-27") & difnum<=12 &difnum>=0) %>% ggplot(aes(x=date_symp, y=new_cases_symptom)) +geom_bar(stat = "identity",position='dodge') +facet_wrap(~difnum)
histo =delay2 %>% filter(date_symp>=as.Date("2020-04-13") & date_symp<=as.Date("2020-04-27") & date_reported<=as.Date("2020-04-27") & difnum<=12 &difnum>=0) %>% group_by(difnum) %>% summarize(count = sum(new_cases_symptom))
histo$count = histo$count/sum(histo$count)

delay2 %>% filter( difnum<=15 &difnum>=0) %>% group_by(date_reported,difnum) %>% summarize(count = sum(new_cases_symptom)) 
histo2$count=histo2$count/sum(histo2$count)
