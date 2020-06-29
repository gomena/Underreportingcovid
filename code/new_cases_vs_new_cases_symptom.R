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


# Copyright 2020 By Gonzalo Mena, gomena.github.io, @mena_gonzalo, github.com/gomena
CasosNuevosConSintomas  = fread("https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto26/CasosNuevosConSintomas.csv") %>%  clean_names() %>% as_tibble() %>% filter(region!='Total') %>% 
  pivot_longer(-c('region'), names_to='date', values_to='new_cases_with_symptoms') %>% mutate(date = as.Date(sub('_','-',sub('_','-',str_remove(date, 'x'))))) %>%
  group_by(date) 

CasosNuevosSinSintomas  = fread("https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto27/CasosNuevosSinSintomas.csv") %>%  clean_names() %>% as_tibble() %>% filter(region!='Total') %>% 
  pivot_longer(-c('region'), names_to='date', values_to='new_cases_without_symptoms') %>% mutate(date = as.Date(sub('_','-',sub('_','-',str_remove(date, 'x'))))) %>%
  group_by(date) 

CasosNuevos  = fread("https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto13/CasosNuevosCumulativo.csv") %>%  clean_names() %>% as_tibble() %>% filter(region!='Total') %>% 
  pivot_longer(-c('region'), names_to='date', values_to='new_cases_total') %>% mutate(date = as.Date(sub('_','-',sub('_','-',str_remove(date, 'x'))))) %>%
  group_by(date) 


case_test_df = CasosNuevosConSintomas %>% full_join(CasosNuevosSinSintomas) %>% full_join(CasosNuevos) 
 
case_test_df_pivoted = case_test_df %>% pivot_longer(-c('region','date'), names_to = 'type_metric', values_to = 'metric')



dates1 = c('2020-04-15','2020-04-17','2020-04-20','2020-04-24','2020-04-27','2020-05-01','2020-05-04','2020-05-08','2020-05-11','2020-05-15')
list = c("FechaInicioSintomas-16-04","FechaInicioSintomas-18-04","FechaInicioSintomas-22-04","FechaInicioSintomas-25-04", "FechaInicioSintomas-29-04","FechaInicioSintomas-01-05","FechaInicioSintomas-04-05","FechaInicioSintomas-08-05")
archivos_csv = paste("https://github.com/gomena/Underreportingcovid/tree/master/data/historysintom/",list,'.txt',sep="")

listcols = c()
for(i in 7:19){
  listcols = append(listcols, paste("se", as.character(i), sep=""))
}
tsem=tibble('se7'=0,'se8'=0,'se9'=0,'se10'=0,'se11'=0,'se12'=0,'se13'=0,'se14'=0,'se15'=0,'se16'=0,'se17'=0,'se18'=0,'se19'=0)
datereportlist=c("2020-04-15","2020-04-17","2020-04-20","2020-04-24","2020-04-27","2020-05-01","2020-05-04","2020-05-08")
sintom_history <- map(
  seq_along(archivos_csv),
  function(x) {
    y <- fread(archivos_csv[x]) %>% 
      clean_names() %>% 
      as_tibble() %>%  mutate(date = datereportlist[x])
    y = add_column(y, !!!tsem[setdiff(names(tsem), names(y))])  %>% 
      pivot_longer(-c("region", "codigo_region","comuna","codigo_comuna","poblacion","date"), names_to = 'senum', values_to ="cases") 
  })


n=5
sumary =case_test_df %>% filter(date<=dates1[n+1] &date>dates1[n]) %>%  group_by(region) %>% 
  summarize_at(vars(new_cases_total, new_cases_with_symptoms,new_cases_without_symptoms),funs(sum(., na.rm=TRUE))) %>% 
  mutate(period=paste(dates1[n], dates1[n+1],sep=" a ")) %>% mutate(date=as.Date(dates1[n+1]))

for(n in 2:9){
  sumary = sumary %>% full_join(case_test_df %>% filter(date<=dates1[n+1] &date>dates1[n]) %>%  group_by(region) %>% 
                                  summarize_at(vars(new_cases_total, new_cases_with_symptoms,new_cases_without_symptoms),funs(sum(., na.rm=TRUE))) %>% 
                                  mutate(period=paste(dates1[n], dates1[n+1],sep=" a ")) %>% mutate(date=as.Date(dates1[n+1])))
}





sintom_history_all = sintom_history[[1]]

for(i in 2:length(sintom_history)){
  sintom_history_all = sintom_history_all %>% full_join(sintom_history[[i]])
  
}
sintom_history_all = sintom_history_all %>% rename(senumsin=senum) %>%mutate_at(vars(senumsin, date), factor) %>% 
  group_by(comuna, senumsin) %>% mutate(new_cases_sint = c(cases[1], (cases - lag(cases))[-1])) %>%  group_by(region, date) %>% summarize_at(vars(new_cases_sint),funs(sum(., na.rm=TRUE))) %>%
  mutate(date=as.Date(date))

sumary = sumary %>% full_join(sintom_history_all)
sumary = sumary %>% filter(date>="2020-04-25")



theme_set(theme_classic())
sumary %>% ggplot(aes(x=new_cases_sint)) + 
  geom_point(aes(y=new_cases_with_symptoms, color='Con síntomas'),size=4) +
  geom_point(aes(y=new_cases_total,color='Totales'),size=4) + 
  geom_segment(aes(x=0, 
                   xend=4500, 
                   y=0, 
                   yend=4500), 
               linetype="dashed", 
               size=0.5) +
  facet_wrap("period") +
  labs(title="Casos nuevos por diagnóstico vs Casos por inicio de síntomas",
       caption="Codigo: https://github.com/gomena/UnderreportingCovid/. fuente: minsal/ministerio de ciencia", color='Tipo de confirmacion') +  
  ylab('Casos nuevos por diagnóstico') + 
  xlab('Casos nuevos por inicio de síntomas') +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
  plot.subtitle = element_text(color="black", size=12, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  axis.text.x = element_text(face = "bold", color = "black", 
                             size = 10, angle = 15),
  axis.text.y = element_text(face = "bold", color = "black", 
                             size = 8))



