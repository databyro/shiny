# libraries
library(tidyverse)
library(ggplot2)
library(extrafont)
library(reshape)
library(SDMTools)

pollster <- "Yle/Taloustutkimus 11.5. - 2.6.2020, 1625 vastaajaa" # pollster information

# setting the poll results for parties
SDPraw <- 0.232
PSraw <- 0.181
Kokraw <- 0.175
Keskraw <- 0.107
Vihrraw <- 0.119
Vasraw <- 0.078
SFPraw <- 0.039
KDraw <- 0.035
Liikraw <- 0.023
N <- 1660 # poll sample size (respondents)
z <- 1.96 # setting critical value for alpha = 0.05 so that confidence level is 0.95

# setting colours
keskColor = '#01954B'
kokColor = '#006288'
psColor = '#FFDE55'
sdpColor = '#E11931' 
vihrColor = '#61BF1A'
vasColor = '#F00A64'
sfpColor = '#FFDD93'
kdColor = '#18359B'
sinColor = '#003680'
liikColor = '#ae2375'
keskColor2 = '#01954B99'
kokColor2 = '#00628899'
psColor2 = '#FFDE5599'
sdpColor2 = '#E1193199' 
vihrColor2 = '#61BF1A99'
vasColor2 = '#F00A6499'
sfpColor2 = '#FFDD9399'
kdColor2 = '#18359B99'
sinColor2 = '#00368099'
liikColor2 = '#ae237599'
colours <- c(psColor, kokColor, sdpColor, keskColor, vihrColor, vasColor, sfpColor, kdColor, liikColor)
colours2 <- c(psColor2, kokColor2, sdpColor2, keskColor2, vihrColor2, vasColor2, sfpColor2, kdColor2, liikColor2,"white")
parties <- c("SDP", "PS", "Kok", "Kesk", "Vihr", "Vas", "SFP", "KD", "Liik")

# calculating standard error for each party and rolling the result 100 000 times with the SE
PS_SE <- sqrt((PSraw*(1-PSraw))/N)
PS_MoE <- z*PS_SE
PS <- rnorm(100000, PSraw, PS_SE)
Kok_SE <- sqrt((Kokraw*(1-Kokraw))/N)
Kok_MoE <- z*Kok_SE
Kok <- rnorm(100000, Kokraw, Kok_SE)
SDP_SE <- sqrt((SDPraw*(1-SDPraw))/N)
SDP_MoE <- z*SDP_SE
SDP <- rnorm(100000, SDPraw, SDP_SE)
Kesk_SE <- sqrt((Keskraw*(1-Keskraw))/N)
Kesk_MoE <- z*Kesk_SE
Kesk <- rnorm(100000, Keskraw, Kesk_SE)
Vihr_SE <- sqrt((Vihrraw*(1-Vihrraw))/N)
Vihr_MoE <- z*Vihr_SE
Vihr <- rnorm(100000, Vihrraw, Vihr_SE)
Vas_SE <- sqrt((Vasraw*(1-Vasraw))/N)
Vas_MoE <- z*Vas_SE
Vas <- rnorm(100000, Vasraw, Vas_SE)
SFP_SE <- sqrt((SFPraw*(1-SFPraw))/N)
SFP_MoE <- z*SFP_SE
SFP <- rnorm(100000, SFPraw, SFP_SE)
KD_SE <- sqrt((KDraw*(1-KDraw))/N)
KD_MoE <- z*KD_SE
KD <- rnorm(100000, KDraw, KD_SE)
Liik_SE <- sqrt((Liikraw*(1-Liikraw))/N)
Liik_MoE <- z*Liik_SE
Liik <- rnorm(100000, Liikraw, Liik_SE)

# melting the party results to a dataframe
totals <- "totals"
densitydf <- data.frame(totals, PS, Kok, SDP, Kesk, Vihr, Vas, SFP, KD, Liik)
densitydf2 <- melt(densitydf, id="totals")

# drawing the actual graph
violin <- ggplot(densitydf2, aes(x=reorder(variable, value), y=value))+
  geom_violin(aes(fill=variable), alpha=0.5, bw=0.002, scale="width", colour="darkgrey")+
  stat_summary(aes(label=scales::percent(..y.., accuracy=0.1, scale=100, suffix=" %")),fun.y = median, geom = "text", size=6, family="Philosopher")+
  labs(title="Puolueiden kannatus", subtitle=pollster, x="", y="Kannatus", caption = "Databyro.fi")+
  scale_fill_manual(values = colours)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 100), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), limits=c(0,0.3),
                     minor_breaks = c(0.01,0.02,0.03,0.04, 0.05, 0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.2,
                                      0.21,0.22,0.23,0.24,0.25,0.26,0.27,0.28,0.29,0.3))+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=40, family="Philosopher"))+
  theme(plot.subtitle = element_text(color="darkgrey", size=30, family="Philosopher"))+
  theme(text=element_text(color="black", family="Philosopher", size=30))+
  theme(legend.position = "none")+
  theme(plot.caption=element_text(hjust = 0.98, vjust = 1))+
  theme(plot.margin = margin(10,10,10,10))+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.grid.major.x = element_line(colour = "darkgrey"))

#printing the graph
violin
