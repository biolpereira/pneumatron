####################################################
##### PNEUMATRON - DATA ANALYSIS               #####
##### VERSION 1.1                              #####
##### 09/04/2020                               #####
##### Luciano Pereira, biolpereira@gmail.com   #####
##### function sigmoidal curve by:             #####
##### Paulo Bittencourt, paulo09d@gmail.com    #####
####################################################

############################## change here according to your data
#select folder
setwd('C:/Users/biolp/OneDrive/Documentos/Projetos/Pneumatica_automatizado/Laurus_nobilis/laurus4')
#put the correct file name 
#raw <- read.delim("log.txt", header=T, sep=' ', as.is=T)
raw <- read.csv('laurus4.csv', header=T, sep=',', as.is=T)
psy <- read.csv('laurus4_id.csv', header=T, sep=',', as.is=T)
# define initial (pi_s) and final (pf_s) pressures time desired (usually 1.5 and 30 seconds, respectively)
pi_s <- 1.5
pf_s <- 30
# define tubing volume (in mL)
reservoir <- 1.305
# define atmospheric pressure (in kPa)
p_atm <- 100
############################### ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# ## remove data point if needed (using rep number)
#raw <- raw[raw$rep > 2 , ]
#  raw <- raw[raw$rep < 150 , ]
# raw <- raw[ !(raw$rep %in% c(11,12,13,14,15,16, 90,96)), ] #delete specific data points noisy

library(lubridate)
raw$datetime = with(raw, dmy(date) + hms(hour))
raw$n_time = as.numeric(raw$datetime)
raw$i_time = raw$n_time - min(raw$n_time)
raw$min = raw$i_time/60
raw$step_min = round(raw$min, 0)
head(pf)

pi <- raw[raw$time== pi_s*2, ]
pf <- raw[raw$time==pf_s*2, ]

pres = data.frame(pi$pressure, pi$step_min, pf$pressure, pf$rep)
names(pres)[names(pres) == "pi.step_min"] <- "step_min"

# water potential extrapolated for every min
psy$datetime = with(psy, dmy(date) + hm(hour))
psy$datetime = as.numeric(psy$datetime)
psy$i_time = psy$datetime - min(psy$datetime)
psy$min = psy$i_time/60
psy$step_min = round(psy$min, 0)

library(tidyverse)
psy = psy %>% mutate(t2 = lag(step_min))
psy$int = psy$step_min - psy$t2
psy = psy %>% mutate(psy_lag = lag(psy))
psy$psy_int = psy$psy - psy$psy_lag
psy$rate = psy$psy_int/psy$int
max_step_min = max(psy$step_min)
step_min = 0:max_step_min
step_min = data.frame(step_min)
psy1 = data.frame(psy$step_min, psy$step_min, psy$psy, psy$rate)
names(psy1)[names(psy1) == "psy.step_min"] <- "step_min"
names(psy1)[names(psy1) == "psy.psy"] <- "psy"
names(psy1)[names(psy1) == "psy.rate"] <- "rate"

psy1 <- merge(step_min, psy1, by="step_min", all.x = TRUE)
psy1 = psy1 %>%  fill(psy.step_min.1, psy) %>% fill(rate, .direction = "up")
psy1$est_psy = ((psy1$step_min-psy1$psy.step_min.1)*psy1$rate)+psy1$psy

#select by rep every 15 min (if needed)
library(dplyr)
psy2 = psy1  %>% slice(which(row_number() %% 15 == 1))
psy2$pf.rep = seq(1, nrow(psy2), )

#### merge tables

pres <- merge(pres, psy2, by="pf.rep")

#calculate air discharged (AD) in mols, uL, and the percentage of air discharged (PAD)
pres$ad_mol <- (((p_atm-pres$pf.pressure*1000)*(reservoir*10^-6))/(8.3144621*293.15))-(((p_atm-pres$pi.pressure*1000)*(reservoir*10^-6))/(8.3144621*293.15))
pres$ad_ul <- (pres$ad_mol*8.3144621*293.15/(p_atm*1000))*1000*1000*1000
pres$pad <- (100*(pres$ad_ul-(min(pres$ad_ul))))/((max(pres$ad_ul))-(min(pres$ad_ul)))

#### if PAD curve form a continuous "line" (not noisy), than it is possible to estimate P50 from below:
library(dplyr)

## P50 from nearest values
near50 <- pres[pres$pad >=50, ]
near50 <-near50 %>% filter(pad == min(pad)) 
near50b <- pres[pres$pad <=50, ]
near50b <-near50b %>% filter(pad == max(pad)) 
near50 <- bind_rows(near50, near50b)
lm<- lm(near50$pad~near50$est_psy)
int_50 = summary(lm)$coefficients[1] 
slp_50 = summary(lm)$coefficients[2]       
p50_near = (50-int_50)/slp_50

## P88 from nearest values
near88 <- pres[pres$pad >=88, ]
near88 <-near88 %>% filter(pad == min(pad)) 
near88b <- pres[pres$pad <=88, ]
near88b <-near88b %>% filter(pad == max(pad)) 
near88 <- bind_rows(near88, near88b)
lm<- lm(near88$pad~near88$est_psy)
int_88 = summary(lm)$coefficients[1] 
slp_88 = summary(lm)$coefficients[2]       
p88_near = (88-int_88)/slp_88

## P12 from nearest values
near12 <- pres[pres$pad >=12, ]
near12 <-near12 %>% filter(pad == min(pad)) 
near12b <- pres[pres$pad <=12, ]
near12b <-near12b %>% filter(pad == max(pad)) 
near12 <- bind_rows(near12, near12b)
lm<- lm(near12$pad~near12$est_psy)
int_12 = summary(lm)$coefficients[1] 
slp_12 = summary(lm)$coefficients[2]       
p12_near = (12-int_12)/slp_12

########### Paulo Bittencout's solution for curve estimation
try.nls <- function(work.table,model,start.values,try.times=100){
  for(times in 1:try.times){ #try the fit "try.times" times
    start = list() #empty list to save store values
    for(n in 1:nrow(start.values)){ #get store values in the min-max interval in the start.values data frame
      start[n] = sample(seq(start.values$min[n],start.values$max[n],abs(start.values$min[n]-start.values$max[n])/try.times),1)
      names(start)[n] = as.character(start.values$parameter[n]) #makes it a names list (necessary for nls function)
    }
    fit<-NA 
    try(fit<-nls(model,work.table,start=start)) #tries the nls model
    if(any(!is.na(fit)))break #if the model was suscesfully fit break out of the loop
  }
  return(fit) #returns nls fit. If fit was not sucesfull returns NA
}
fit.pad = try.nls(work.table=pres,model=pad~100/(1+exp(a*(est_psy-p50))),start=data.frame(parameter=c("a","p50"),min=c(0,-10),max=c(5,0)))
summary(fit.pad)
a.pad = summary(fit.pad)$coefficients[1] 
p50.pad = summary(fit.pad)$coefficients[2]
p88.pad = log(12/88,exp(1))/a.pad + p50.pad
p12.pad = log(88/12,exp(1))/a.pad + p50.pad

##### save results
results = data.frame(p50_near, p88_near, p12_near, p50.pad, p88.pad, p12.pad)
write.csv(results, './results.csv')
write.csv(pres, './pres.csv')

##### plot graph
library(ggplot2)
fig<-ggplot (data= pres, aes(x = est_psy, y = pad))+geom_point(shape = 21, size = 3) +
  stat_function(fun = function(x) 100/(1+exp(a.pad*(x-p50.pad))), color= "royalblue", size=1)+
# you may comment (put # before) the next lines if you do not want text in your graph
  annotate("point", x = p50_near, y = 50, colour = "red", shape = 17, size = 5)+
  annotate(geom="text", x=-8, y=30, label= expression(psi[50]), color="black")+
  annotate(geom="text", x=-8, y=25, label= round(p50.pad, 2), color="royalblue")+
  annotate(geom="text", x=-8, y=20, label= round(p50_near, 2), color="red")+
  annotate(geom="text", x=-7, y=30, label= expression(psi[88]), color="black")+
  annotate(geom="text", x=-7, y=25, label= round(p88.pad, 2), color="royalblue")+
  annotate(geom="text", x=-7, y=20, label= round(p88_near, 2), color="red")+
  annotate(geom="text", x=-6, y=30, label= expression(psi[12]), color="black")+
  annotate(geom="text", x=-6, y=25, label= round(p12.pad, 2), color="royalblue")+
  annotate(geom="text", x=-6, y=20, label= round(p12_near, 2), color="red")+
  annotate("pointrange", x = p50.pad, y = 50, ymin = 0, ymax = 100, colour = "royalblue", size = 1, linetype = "dashed")+
# axis and theme
  scale_x_continuous("Water potential (MPa)") + 
  scale_y_continuous("PGD (%)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), aspect.ratio = 1) + # formato final
  theme(axis.title.x = element_text(vjust = - 1.0),axis.title.y = element_text(vjust = + 3.0))# +
fig

fig_gd<-ggplot (data= pres, aes(x = est_psy, y = ad_ul))+geom_point(shape = 21, size = 3) +
  scale_x_continuous("Water potential (MPa)") + 
  scale_y_continuous("GD (uL)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), aspect.ratio = 1) + # formato final
  theme(axis.title.x = element_text(vjust = - 1.0),axis.title.y = element_text(vjust = + 3.0))# +
fig_gd

fig_gd_time<-ggplot (data= pres, aes(x = step_min.x, y = ad_ul))+geom_point(shape = 21, size = 3) +
  scale_x_continuous("Time (min)") + 
  scale_y_continuous("GD (uL)") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"), aspect.ratio = 1) + # formato final
  theme(axis.title.x = element_text(vjust = - 1.0),axis.title.y = element_text(vjust = + 3.0))# +
fig_gd_time

# save figure
ggsave("fig_curve.pdf", fig, width=4.5, height=4.5, units = "in")
ggsave("fig_GD.pdf", fig_gd, width=4.5, height=4.5, units = "in")
ggsave("fig_GD_time.pdf", fig_gd_time, width=4.5, height=4.5, units = "in")
