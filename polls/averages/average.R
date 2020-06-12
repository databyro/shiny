#libraries
library(plotly)
library(zoo)
library(readxl)

#Get your own API key by making an account here: https://chart-studio.plotly.com/Auth/login/?action=signup#/
Sys.setenv("plotly_username"="xxx")
Sys.setenv("plotly_api_key"="xxx")

#party colours
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

#Reading the poll data file and assigning objects
X2019_23 <- read_excel("/2019-23.xlsx")
data = X2019_23
date = X2019_23$date
kok = X2019_23$Kok
kesk = X2019_23$Kesk
ps = X2019_23$PS
sdp = X2019_23$SDP
vihr = X2019_23$Vihr
vas = X2019_23$Vas
sfp = X2019_23$SFP
kd = X2019_23$KD
liik = X2019_23$Liik
id = X2019_23$id
hallitus = X2019_23$hallitus
oppositio = X2019_23$oppositio

avgKesk = X2019_23$avgKesk
avgKok = X2019_23$avgKok
avgPs = X2019_23$avgPs
avgSdp = X2019_23$avgSdp
avgVihr = X2019_23$avgVihr
avgVas = X2019_23$avgVas
avgSfp = X2019_23$avgSfp
avgLiik = X2019_23$avgLiik
avgHall = X2019_23$avgHall
avgOpp = X2019_23$avgOpp

#drawing the actual party plot
fichart <- plot_ly(data, x = ~as.Date(date)) %>%
  
  add_lines(y = ~avgSdp, line = list(color = sdpColor), name = "SDP (S&D)") %>%
  add_trace(y = ~sdp, mode = 'markers', marker = list(color = sdpColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgSdp-moeSDP, ymax = ~avgSdp+moeSDP, showlegend = F, hoverinfo= 'skip', line = list(color = sdpColor), fillcolor=sdpColor, opacity=0.1)%>%
  
  add_lines(y = ~avgPs, line = list(color = psColor), name = "PS (ID)") %>%
  add_trace(y = ~ps, mode = 'markers', marker = list(color = psColor, opacity = 0.4, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgPs-moePS, ymax = ~avgPs+moePS, showlegend = F, hoverinfo= 'skip', line = list(color = psColor), fillcolor=psColor, opacity=0.2)%>%
  
  add_lines(y = ~avgKok, line = list(color = kokColor), name = "Kok (EPP)") %>%
  add_trace(y = ~kok, mode = 'markers', marker = list(color = kokColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgKok-moeKok, ymax = ~avgKok+moeKok, showlegend = F, hoverinfo= 'skip', line = list(color = kokColor), fillcolor=kokColor, opacity=0.1)%>%
  
  add_lines(y = ~avgKesk, line = list(color = keskColor), name = "Kesk (RE)") %>%
  add_trace(y = ~kesk, mode = 'markers', marker = list(color = keskColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgKesk-moeKesk, ymax = ~avgKesk+moeKesk, showlegend = F, hoverinfo= 'skip', line = list(color = keskColor), fillcolor=keskColor, opacity=0.1)%>%
  
  add_lines(y = ~avgVihr, line = list(color = vihrColor), name = "Vihr (G/EFA)") %>%
  add_trace(y = ~vihr, mode = 'markers', marker = list(color = vihrColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgVihr-moeVihr, ymax = ~avgVihr+moeVihr, showlegend = F, hoverinfo= 'skip', line = list(color = vihrColor), fillcolor=vihrColor, opacity=0.1)%>%
  
  add_lines(y = ~avgVas, line = list(color = vasColor), name = "Vas (GUE/NGL)") %>%
  add_trace(y = ~vas, mode = 'markers', marker = list(color = vasColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgVas-moeVas, ymax = ~avgVas+moeVas, showlegend = F, hoverinfo= 'skip', line = list(color = vasColor), fillcolor=vasColor, opacity=0.1)%>%
  
  add_lines(y = ~avgSfp, line = list(color = sfpColor), name = "SFP (RE)") %>%
  add_trace(y = ~sfp, mode = 'markers', marker = list(color = sfpColor, opacity = 0.4, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgSfp-moeSFP, ymax = ~avgSfp+moeSFP, showlegend = F, hoverinfo= 'skip', line = list(color = sfpColor), fillcolor=sfpColor, opacity=0.2)%>%
  
  add_lines(y = ~avgKd, line = list(color = kdColor), name = "KD (EPP)") %>%
  add_trace(y = ~kd, mode = 'markers', marker = list(color = kdColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgKd-moeKD, ymax = ~avgKd+moeKD, showlegend = F, hoverinfo= 'skip', line = list(color = kdColor), fillcolor=kdColor, opacity=0.1)%>%
  
  add_lines(y = ~avgLiik, line = list(color = liikColor), name = "Liik (*)") %>%
  add_trace(y = ~liik, mode = 'markers', marker = list(color = liikColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgLiik-moeLiik, ymax = ~avgLiik+moeLiik, showlegend = F, hoverinfo= 'skip', line = list(color = liikColor), fillcolor=liikColor, opacity=0.1)%>%
  
  layout(title= "Puolueiden kannatus", titlefont=list(size=24,family="Philosopher"), annotations = list(text = "Databyro.fi",
      font = list(size = 12),showarrow = FALSE, xref = 'paper', x =1, yref = 'paper', y = -0.3), margin = list(l = 50, r = 50, t = 50, b = 50),
      xaxis=list(fixedrange=TRUE, tickformat = "%d.%m.%Y", title = "",titlefont = list(family="Philosopher"),
      range=c(as.numeric(as.Date("2019-04-14")) * 24 * 60 * 60 * 1000, as.numeric(as.Date("2021-04-15")) * 24 * 60 * 60 * 1000)),
      yaxis=list(fixedrange=TRUE, title = "Kannatus", titlefont=list(family="Philosopher"), tickformat = "%", range=c(0,0.3)),
      hovermode = 'compare', legend = list(y = -0.18, x = 0, orientation = 'h')) %>%
  
  config(displayModeBar = F)
fichart
api_create(fichart, filename = "partychart")

#drawing the government support plot
govtchart <- plot_ly(data, x = ~as.Date(date)) %>%
  add_lines(y = ~avgHall, line = list(color = sdpColor), name = "Hallituspuolueet") %>%
  add_trace(y = ~hallitus, mode = 'markers', marker = list(color = sdpColor, opacity = 0.2, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgHall-moeHall, ymax = ~avgHall+moeHall, showlegend = F, hoverinfo= 'skip', line = list(color = sdpColor), fillcolor=sdpColor, opacity=0.1)%>%
  
  add_lines(y = ~avgOpp, line = list(color = psColor), name = "Oppositiopuolueet") %>%
  add_trace(y = ~oppositio, mode = 'markers', marker = list(color = psColor, opacity = 0.4, size = 5), showlegend = FALSE, hoverinfo = 'skip') %>%
  #add_ribbons(ymin = ~avgOpp-moeOpp, ymax = ~avgOpp+moeOpp, showlegend = F, hoverinfo= 'skip', line = list(color = psColor), fillcolor=psColor, opacity=0.2)%>%
  
  layout(title= "Puolueiden kannatus", titlefont=list(size=24,family="Philosopher"), annotations = list(text = "Databyro.fi",
      font = list(size = 12),showarrow = FALSE, xref = 'paper', x =1, yref = 'paper', y = -0.3), margin = list(l = 50, r = 50, t = 50, b = 50),
      xaxis=list(fixedrange=TRUE, tickformat = "%d.%m.%Y", title = "",titlefont = list(family="Philosopher"),
      range=c(as.numeric(as.Date("2019-04-14")) * 24 * 60 * 60 * 1000, as.numeric(as.Date("2021-04-15")) * 24 * 60 * 60 * 1000)),
      yaxis=list(fixedrange=TRUE, title = "Kannatus", titlefont=list(family="Philosopher"), tickformat = "%", range=c(0.35,0.65)),
      hovermode = 'compare', legend = list(y = -0.2, x = 0, orientation = 'h')) %>%
  
  config(displayModeBar = F)
govtchart
api_create(govtchart, filename = "governmentchart")
