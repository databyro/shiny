#libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(extrafont)
library(reshape)
library(jsonlite)
library(fresh)
library(formattable)
library(forcats)
library(profvis)

options(scipen=999)

datecolour <- "#fb9a99"

#ui
ui <- fluidPage(theme="style.css",
                use_googlefont("Philosopher"),
                titlePanel("Vahvistetut koronavirustartunnat"),
                

                    tabsetPanel(type = "tabs",
                                tabPanel("Suomi",
                                         sidebarLayout(
                                             sidebarPanel(
                                                 
                                                 selectInput("datatyyppi", h4("Valitse tyyppi"),
                                                             choices = list(
                                                                 "Yhteenveto" ="Yhteenveto",
                                                                 "Jakaumat" = "Jakaumat"),
                                                             selected = "Yhteenveto"),
                                                 
                                                 conditionalPanel(
                                                     condition = "input.datatyyppi == 'Yhteenveto'",
                                                     selectInput("data3", h4("Valitse data"),
                                                                 choices = list(
                                                                     "Kokonaistartunnat" = "Kokonaistartunnat",
                                                                     "Uusien tartuntojen keskiarvo" = "Uusien tartuntojen keskiarvo",
                                                                     "Kasvu suhteessa tartuntoihin (%)" = "Kasvu suhteessa tartuntoihin (%)"),
                                                                 selected = "Tartunnat sairaanhoitoalueittain")),
                                                 
                                                 conditionalPanel(
                                                     condition = "input.datatyyppi == 'Jakaumat'",
                                                     selectInput("data2", h4("Valitse data"),
                                                                  choices = list(
                                                                      "Tartunnat sairaanhoitoalueittain" = "Tartunnat sairaanhoitoalueittain",
                                                                      "Tartunnat ikäluokittain" = "Tartuntojen jakauma (%)"),
                                                                  selected = "Tartunnat sairaanhoitoalueittain")),
                                                 
                                                 conditionalPanel(
                                                     condition = "input.datatyyppi == 'Yhteenveto' && input.data3 == 'Kokonaistartunnat'",
                                                     radioButtons("scale2", h4("Valitse asteikko"),
                                                                  choices = list(
                                                                      "Lineaarinen" = "Lineaarinen",
                                                                      "Logaritminen" = "Logaritminen"),
                                                                  selected = "Logaritminen")),
                                                 
                                                 helpText(HTML("Datalähteet: <a href='https://github.com/HS-Datadesk/koronavirus-avoindata' target='_blank'>Helsingin Sanomat</a> ja <a href='https://experience.arcgis.com/experience/d40b2aaf08be4b9c8ec38de30b714f26' target='_blank'>THL</a>"))),
                                             
                                             mainPanel(
                                                 
                                                     conditionalPanel(
                                                     condition = "input.datatyyppi == 'Yhteenveto'",
                                                     htmlOutput("Overall"),
                                                     #htmlOutput("selected_scale2"),
                                                     plotOutput("Yhteenveto")),
                                                 
                                                       conditionalPanel(
                                                           condition = "input.datatyyppi == 'Jakaumat'",
                                                           htmlOutput("selected_data2"),
                                                           plotOutput("suomi"))
                                                    
                                                       )
                                         )
                                ),
                                tabPanel("Maailma",
                                         sidebarLayout(
                                             sidebarPanel(
                                                 
                                                 selectInput("datatyyppi2", h4("Valitse tyyppi"),
                                                             choices = list("Graafi" = "Graafi",
                                                                            "Taulukko" = "Taulukko"),
                                                             selected = "Graafi"),

                                                 conditionalPanel(
                                                     condition = "input.datatyyppi2 == 'Graafi'",
                                                     
                                                     selectInput("country", 
                                                             h4("Valitse maat (max. 12)"), multiple = T,
                                                             choices = list(
                                                             "Afganistan" = "Afghanistan",
                                                             "Albania" = "Albania",
                                                             "Algeria" = "Algeria",
                                                             "Andorra" = "Andorra",
                                                             "Angola" = "Angola",
                                                             "Antigua ja Barbuda" = "Antigua and Barbuda",
                                                             "Argentiina" = "Argentina",
                                                             "Armenia" = "Armenia",
                                                             "Australia" = "Australia",
                                                             "Azerbaidžan" = "Azerbaijan",
                                                             "Bahama" = "Bahamas, The",
                                                             "Bahrain" = "Bahrain",
                                                             "Bangladesh" = "Bangladesh",
                                                             "Barbados" = "Barbados",
                                                             "Belgia" = "Belgium",
                                                             "Benin" = "Benin",
                                                             "Bhutan" = "Bhutan",
                                                             "Bolivia" = "Bolivia",
                                                             "Bosnia ja Hertsegovina" = "Bosnia and Herzegovina",
                                                             "Brasilia" = "Brazil",
                                                             "Brunei" = "Brunei",
                                                             "Bulgaria" = "Bulgaria",
                                                             "Burkina Faso" = "Burkina Faso",
                                                             "Chad" = "Chad",
                                                             "Chile" = "Chile",
                                                             "Costa Rica" = "Costa Rica",
                                                             "Djibouti" = "Djibouti",
                                                             "Dominica" = "Dominica",
                                                             "Dominikaaninen tasavalta" = "Dominican Republic",
                                                             "Ecuador" = "Ecuador",
                                                             "Egypti" = "Egypt",
                                                             "El Salvador" = "El Salvador",
                                                             "Eritrea" = "Eritrea",
                                                             "Espanja" = "Spain",
                                                             "Eswatini (Swazimaa)" = "Eswatini",
                                                             "Etelä-Afrikka" = "South Africa",
                                                             "Etelä-Korea" = "Korea, South",
                                                             "Etiopia" = "Ethiopia",
                                                             "Fidži" = "Fiji",
                                                             "Filippiinit" = "Philippines",
                                                             "Gabon" = "Gabon",
                                                             "Gambia" = "Gambia, The",
                                                             "Georgia" = "Georgia",
                                                             "Ghana" = "Ghana",
                                                             "Grenada" = "Grenada",
                                                             "Grönlanti" = "Greenland",
                                                             "Guatemala" = "Guatemala",
                                                             "Guinea" = "Guinea",
                                                             "Guyana" = "Guyana",
                                                             "Haiti" = "Haiti",
                                                             "Hollanti" = "Netherlands",
                                                             "Honduras" = "Honduras",
                                                             "Indonesia" = "Indonesia",
                                                             "Intia" = "India",
                                                             "Irak" = "Iraq",
                                                             "Iran" = "Iran",
                                                             "Irlanti" = "Ireland",
                                                             "Islanti" = "Iceland",
                                                             "Iso-Britannia" = "United Kingdom",
                                                             "Israel" = "Israel",
                                                             "Italia" = "Italy",
                                                             "Itä-Timor" = "East Timor",
                                                             "Itävalta" = "Austria",
                                                             "Jamaika" = "Jamaica",
                                                             "Japani" = "Japan",
                                                             "Jordania" = "Jordan",
                                                             "Kambodža" = "Cambodia",
                                                             "Kamerun" = "Cameroon",
                                                             "Kanada" = "Kanada",
                                                             "Kap Verde" = "Cabo Verde",
                                                             "Kazakstan" = "Kazakhstan",
                                                             "Kenia" = "Kenya",
                                                             "Kiina" = "China",
                                                             "Kirgisia" = "Kyrgyzstan",
                                                             "Kolumbia" = "Colombia",
                                                             "Kongon demokraattinen tasavalta" = "Congo (Kinshasa)",
                                                             "Kongon tasavalta" = "Congo (Brazzaville)",
                                                             "Kosovo" = "Kosovo",
                                                             "Kreikka" = "Kreikka",
                                                             "Kroatia" = "Croatia",
                                                             "Keski-Afrikan tasavalta" = "Central African Republic",
                                                             "Kuuba" = "Cuba",
                                                             "Kuwait" = "Kuwait",
                                                             "Kypros" = "Cyprus",
                                                             "Latvia" = "Latvia",
                                                             "Libanon" = "Lebanon",
                                                             "Liberia" = "Liberia",
                                                             "Liechtenstein" = "Liechtenstein",
                                                             "Liettua" = "Lithuania",
                                                             "Luxemburg" = "Luxembourg",
                                                             "Macau" = "Macau",
                                                             "Madagaskar" = "Madagascar",
                                                             "Malediivit" = "Maldives",
                                                             "Malesia" = "Malaysia",
                                                             "Malta" = "Malta",
                                                             "Marokko" = "Morocco",
                                                             "Mauritania" = "Mauritania",
                                                             "Mauritius" = "Mauritius",
                                                             "Meksiko" = "Mexico",
                                                             "Moldova" = "Moldova",
                                                             "Mongolia" = "Mongolia",
                                                             "Montenegro" = "Montenegro",
                                                             "Mosambik" = "Mozambique",
                                                             "Namibia" = "Namibia",
                                                             "Nepal" = "Nepal",
                                                             "Nicaragua" = "Nicaragua",
                                                             "Niger" = "Niger",
                                                             "Nigeria" = "Nigeria",
                                                             "Norja" = "Norway",
                                                             "Norsunluurannikko" = "Cote d'Ivoire",
                                                             "Oman" = "Oman",
                                                             "Pakistan" = "Pakistan",
                                                             "Palestiina" = "occupied Palestinian territory",
                                                             "Panama" = "Panama",
                                                             "Papua Uusi-Guinea" = "Papua New Guinea",
                                                             "Paraguay" = "Paraguay",
                                                             "Peru" = "Peru",
                                                             "Pohjois-Makedonia" = "North Macedonia",
                                                             "Portugali" = "Portugal",
                                                             "Puola" = "Poland",
                                                             "Päiväntasaajan Guinea" = "Equatorial Guinea",
                                                             "Qatar" = "Qatar",
                                                             "Ranska" = "France",
                                                             "Ranskan Guayana" = "French Guiana",
                                                             "Romania" = "Romania",
                                                             "Ruanda" = "Rwanda",
                                                             "Ruotsi" = "Sweden",
                                                             "Saksa" = "Germany",
                                                             "San Marino" = "San Marino",
                                                             "Saudi-Arabia" = "Saudi Arabia",
                                                             "Sambia" = "Zambia",
                                                             "Senegal" = "Senegal",
                                                             "Serbia" = "Serbia",
                                                             "Seychellit" = "Seychelles",
                                                             "Slovakia" = "Slovakia",
                                                             "Slovenia" = "Slovenia",
                                                             "Singapore" = "Singapore",
                                                             "Somalia" = "Somalia",
                                                             "Sri Lanka" = "Sri Lanka",
                                                             "Sudan" = "Sudan",
                                                             "Suomi" = "Finland",
                                                             "Suriname" = "Suriname",
                                                             "Sveitsi" = "Switzerland",
                                                             "Syyria" = "Syria",
                                                             "Taiwan" = "Taiwan*",
                                                             "Tansania" = "Tanzania",
                                                             "Tanska" = "Denmark",
                                                             "Thaimaa" = "Thailand",
                                                             "Togo" = "Togo",
                                                             "Trinidad ja Tobago" = "Trinidad and Tobago",
                                                             "Tšekin tasavalta" = "Czechia",
                                                             "Tunisia" = "Tunisia",
                                                             "Turkki" = "Turkey",
                                                             "Uganda" = "Uganda",
                                                             "Ukraina" = "Ukraine",
                                                             "Unkari" = "Hungary",
                                                             "Uruguay" = "Uruguay",
                                                             "Uusi-Seelanti" = "New Zealand",
                                                             "Uzbekistan" = "Uzbekistan",
                                                             "Valko-Venäjä" = "Belarus",
                                                             "Vatikaani" = "Holy See",
                                                             "Venezuela" = "Venezuela",
                                                             "Venäjä" = "Russia",
                                                             "Vietnam" = "Vietnam",
                                                             "Viro" = "Estonia",
                                                             "Yhdistyneet Arabiemiirikunnat" = "United Arab Emirates",
                                                             "Yhdysvallat" = "US",
                                                             "Zimbabwe" = "Zimbabwe"),
                                                         selected = c("Finland", "US", "Germany", "France", "Italy", "Sweden", "Norway", "Denmark", "Denmark", "Czechia", "Austria"))),
                           
                                                 conditionalPanel(
                                                     condition = "input.datatyyppi2 == 'Graafi'",
                                                     
                                                     selectInput("data", h4("Valitse data"),
                                                                 choices = list(
                                                                                "Kokonaistartunnat" = "Kokonaistartunnat",
                                                                                "Tartunnan saaneiden osuus populaatiosta (%)" = "Tartunnan saaneiden osuus populaatiosta (%)",
                                                                                #"Aktiiviset tartunnat" ="Aktiiviset tartunnat",
                                                                                #"Parantuneet" = "Parantuneet",
                                                                                "Kuolleet" = "Kuolleet",
                                                                                "Kuolleiden osuus kaikista sairastuneista (%)" = "Kuolleiden osuus kaikista sairastuneista (%)",
                                                                                "Kuolleiden osuus populaatiosta (%)" = "Kuolleiden osuus populaatiosta (%)"
                                                                                ),
                                                                 selected = "Kokonaistartunnat")),
                                                 
                                                 conditionalPanel(
                                                     condition = "input.datatyyppi2 == 'Graafi'",
                                                     
                                                     radioButtons("scale", h4("Valitse asteikko"),
                                                                  choices = list("Lineaarinen" = "Lineaarinen",
                                                                                 "Logaritminen" = "Logaritminen"),
                                                                  selected = "Logaritminen")),
                           
                           helpText(HTML("Datalähde: <a href='https://github.com/CSSEGISandData/COVID-19' target='_blank'>Johns Hopkins -yliopisto</a>"))),
                           
                    mainPanel(
                        conditionalPanel(
                            condition = "input.datatyyppi2 == 'Graafi'",
                        htmlOutput("selected_data"),
                        htmlOutput("selected_scale"),
                        plotOutput("maailma")),
                        
                        conditionalPanel(
                            condition = "input.datatyyppi2 == 'Taulukko'",
                            htmlOutput("selected_all2"),
                            tableOutput("tartunnat2"))
                            )
                )
    )
    ))

#server
server <- function(input, output) {
    
##text inputs
    output$Overall <- renderText({ 
        paste("<h3>", input$data3, "–", format(Sys.Date()-1, "%d.%m.%Y"), "</h3>")
    })
    
    output$selected_all2 <- renderText({ 
        paste("<h3>Kokonaistartunnat", format(Sys.Date()-1, "%d.%m.%Y"),"</h3>")
    })
    
    output$selected_data2 <- renderText({ 
        paste("<h3>", input$data2, "–", format(Sys.Date()-1, "%d.%m.%Y"),"</h3>")
    })
    
    output$selected_scale2 <- renderText({ 
        paste("<h4>", input$scale2, "asteikko</h4>")
    })
    
    output$selected_data <- renderText({ 
        paste("<h3>", input$data, "–", format(Sys.Date()-1, "%d.%m.%Y"),"</h3>")
    })
    
    output$selected_scale <- renderText({ 
        paste("<h4>", input$scale, "asteikko</h4>")
    })
    
##actual graphs
    json <- fromJSON(url("https://srv.databyro.fi/2020-corona/HSraw.json"), flatten = T)
    
    jsonconfirmed <- json$confirmed
    jsonconfirmed$date <- as.Date(jsonconfirmed$date)
    df <- table(jsonconfirmed$date)
    df <- as.data.frame(df)
    colnames(df) <- c("date", "cases")
    df$date <- as.Date(df$date, format="%Y-%m-%d")
    
    jsondeaths <- json$deaths
    jsondeaths$date <- as.Date(jsondeaths$date)
    df3 <- table(jsondeaths$date)
    df3 <- as.data.frame(df3)
    colnames(df3) <- c("date", "deaths")
    df3$date <- as.Date(df3$date, format="%Y-%m-%d")
    
    merged <- merge(df, df3, by="date", all = T)
    merged[is.na(merged)] <- 0
    merged$cumsumcases <- cumsum(merged$cases)
    merged$cumsumdeaths <- cumsum(merged$deaths)
    
    cumsumcases <- merged$cumsumcases
    cumsumdeaths <- merged$cumsumdeaths
    
    av <- function(x,n){stats::filter(x,rep(1/n,n), sides=1)}
    
    merged <- tail(merged, -1)
    
    rollmean <- av(merged$cases, 7)
    rollmean <- na.omit(rollmean)
    #rollmean <- head(rollmean, -1)
    merged3 <- tail(merged, -6)
    #merged3 <- head(merged3, -1)
    
    rollmean2 <- av((merged$cases/merged$cumsumcases*100), 3)
    rollmean2 <- na.omit(rollmean2)
    rollmean2 <- tail(rollmean2, -3)
    #rollmean2 <- head(rollmean2, -1)
    merged2 <- tail(merged, -5)
    #merged2 <- head(merged2, -1)
    
#finnish chart
    output$Yhteenveto <- renderPlot({
        
        scale2 <- switch(input$scale2,
                         "Logaritminen" = scale_y_log10(labels = scales::format_format(big.mark = " ", decimal.mark = ",", drop0trailing=TRUE), breaks=c(0,1,3,10,30,100,300,1000,3000), minor_breaks=NULL),
                         "Lineaarinen" = scale_y_continuous(labels = scales::format_format(big.mark = " ", decimal.mark = ",", drop0trailing=TRUE), breaks = seq(0,100000,500), minor_breaks = seq(0,100000,100))
        )
        
        graafi <- switch (input$data3,
                        "Kokonaistartunnat" =         
                            ggplot(merged, aes(x=date))+
                            geom_line(aes(y=cumsumcases, colour="#fb6a4a"), size=1)+
                            geom_line(aes(y=cumsumdeaths, colour="#969696"), size=1.5)+
                            labs(caption="Databyro.fi\nData: Helsingin Sanomat")+
                            ylab("")+
                            xlab("")+
                            scale_color_identity(guide="legend", breaks=c("#fb6a4a", "#969696"), labels=c("Tartunnat", "Kuolemat"), name="")+
                            scale2+
                            scale_x_date(date_breaks = "14 days", date_minor_breaks = "7 days", date_labels = "%d.%m.")+
                            theme_minimal()+
                            theme(plot.title = element_text(color="black", size=30, family="Philosopher"))+
                            theme(plot.subtitle = element_text(color="black", size=14, family="Philosopher"))+
                            theme(text=element_text(color="black", family="Philosopher", size=12))+
                            theme(plot.caption=element_text(size=14, family="Philosopher"))+
                            theme(plot.margin = margin(10,10,10,10))+
                            theme(panel.grid.major.y = element_line(colour = "darkgrey")),
                        
                        "Uusien tartuntojen keskiarvo" =
                            ggplot(merged3, aes(x=date))+
                            geom_line(aes(y=rollmean), colour="#fb6a4a", size=1)+
                            geom_bar(aes(y=cases), stat="identity", alpha = 0.3, fill="#756bb1")+
                            labs(subtitle = "Keskiarvokasvu laskettu kuluneiden seitsemän päivän osalta kunakin ajankohtana\nData voi etenkin kolmen viimeisimmän päivän osalta päivittyä reilusti", caption="Databyro.fi\nData: Helsingin Sanomat")+
                            ylab("")+
                            xlab("")+
                            scale_y_continuous(breaks = seq(0,1000,20))+
                            scale_x_date(date_breaks = "14 days", date_minor_breaks = "7 days", date_labels = "%d.%m.")+
                            theme_minimal()+
                            theme(plot.title = element_text(color="black", size=30, family="Philosopher"))+
                            theme(plot.subtitle = element_text(color="black", size=14, family="Philosopher"))+
                            theme(text=element_text(color="black", family="Philosopher", size=12))+
                            theme(plot.caption=element_text(size=14, family="Philosopher"))+
                            theme(plot.margin = margin(10,10,10,10))+
                            theme(panel.grid.major.y = element_line(colour = "darkgrey")),
                        
                        "Kasvu suhteessa tartuntoihin (%)" =
                            ggplot(merged2, aes(x=date))+
                            geom_line(aes(y=rollmean2), colour="#fb6a4a", size=1)+
                            labs(subtitle="Data voi etenkin kolmen viimeisimmän päivän osalta päivittyä reilusti", caption="Databyro.fi\nData: Helsingin Sanomat")+
                            ylab("")+
                            xlab("")+
                            scale_y_continuous(limits=c(0,40))+
                            scale_x_date(date_breaks = "14 days", date_minor_breaks = "7 days", date_labels = "%d.%m.")+
                            theme_minimal()+
                            theme(plot.title = element_text(color="black", size=30, family="Philosopher"))+
                            theme(plot.subtitle = element_text(color="black", size=14, family="Philosopher"))+
                            theme(text=element_text(color="black", family="Philosopher", size=12))+
                            theme(plot.caption=element_text(size=14, family="Philosopher"))+
                            theme(plot.margin = margin(10,10,10,10))+
                            theme(panel.grid.major.y = element_line(colour = "darkgrey"))
        )
            
graafi
        
    })
    
    pops <- read.csv(url("https://databyro.fi/wp-content/uploads/2020/04/pops.csv"), check.names =F)
    confirmed <- read.csv(url("https://srv.databyro.fi/2020-corona/JHUconfsglobal.csv"), check.names = F)
    deaths <- read.csv(url("https://srv.databyro.fi/2020-corona/JHUdeathsglobal.csv"), check.names = F)
    recovered <- read.csv(url("https://srv.databyro.fi/2020-corona/JHUrecosglobal.csv"), check.names = F)
    
##world chart
    output$maailma <- renderPlot({
        
        confirmed <- merge(confirmed, pops, by = "Country/Region")
        deaths <- merge(deaths, pops, by = "Country/Region")
        #recovered <- merge(recovered, pops, by = "Country/Region")
        
        country <- confirmed$"Country/Region"
        
        selectedconfirmed <- subset(confirmed, country==input$country[1] | country==input$country[2] | country==input$country[3] | country==input$country[4] | country==input$country[5] | country==input$country[6] | country==input$country[7] | country==input$country[8] | country==input$country[9] | country==input$country[10] | country==input$country[11] | country==input$country[12])
        selecteddeaths <- subset(deaths, country==input$country[1] | country==input$country[2] | country==input$country[3] | country==input$country[4] | country==input$country[5] | country==input$country[6] | country==input$country[7] | country==input$country[8] | country==input$country[9] | country==input$country[10] | country==input$country[11] | country==input$country[12])
        #selectedrecovered <- subset(recovered, country==input$country[1] | country==input$country[2] | country==input$country[3] | country==input$country[4] | country==input$country[5] | country==input$country[6] | country==input$country[7] | country==input$country[8] | country==input$country[9] | country==input$country[10] | country==input$country[11] | country==input$country[12])
        
        moltenconfirmed <- melt(selectedconfirmed, id=c("Province/State", "Country/Region", "Lat", "Long", "Population"))
        moltendeaths <- melt(selecteddeaths, id=c("Province/State", "Country/Region", "Lat", "Long", "Population"))
        #moltenrecovered <- melt(selectedrecovered, id=c("Province/State", "Country/Region", "Lat", "Long", "Population"))
        
        moltenconfirmed$variable <- as.Date(moltenconfirmed$variable, format="%m/%d/%y")
        moltendeaths$variable <- as.Date(moltendeaths$variable, format="%m/%d/%y")
        #moltenrecovered$variable <- as.Date(moltenrecovered$variable, format="%m/%d/%y")
        
        aggconfirmed <-  aggregate(moltenconfirmed$value, by=list(moltenconfirmed$"Country/Region", moltenconfirmed$variable, moltenconfirmed$Population), FUN=sum)
        aggdeaths <-  aggregate(moltendeaths$value, by=list(moltendeaths$"Country/Region", moltendeaths$variable, moltendeaths$Population), FUN=sum)
        #aggrecovered <-  aggregate(moltenrecovered$value, by=list(moltenrecovered$"Country/Region", moltenrecovered$variable, moltenrecovered$Population), FUN=sum)
        
        colourconfirmed <- aggconfirmed$Group.1
        colourdeaths <- aggdeaths$Group.1
        #colourrecovered <- aggrecovered$Group.1
        
        dateconfirmed <- aggconfirmed$Group.2
        datedeaths <- aggdeaths$Group.2
        #daterecovered <- aggrecovered$Group.2
        
        popsconfirmed <- aggconfirmed$Group.3
        popsdeaths <- aggdeaths$Group.3
        #popsrecovered <- aggrecovered$Group.3
        
        valueconfirmed <- aggconfirmed$x
        valuedeaths <- aggdeaths$x
        #valuerecovered <- aggrecovered$x
        
        #active <- (valueconfirmed - valuedeaths - valuerecovered)
        deaths <- valuedeaths
        #recovered <- valuerecovered
        cumulative <- valueconfirmed
        deathpercentage <- ((valuedeaths/valueconfirmed) *100)
        proportionconfirmed <- ((valueconfirmed/popsconfirmed) *100)
        proportiondead <- ((valuedeaths/popsdeaths) *100)
        
        data <-  switch(input$data,
                       "Kokonaistartunnat" = cumulative,
                       #"Aktiiviset tartunnat" = active,
                       #"Parantuneet" = recovered,
                       "Kuolleet" = deaths,
                       "Kuolleiden osuus kaikista sairastuneista (%)" = deathpercentage,
                       "Tartunnan saaneiden osuus populaatiosta (%)" = proportionconfirmed,
                       "Kuolleiden osuus populaatiosta (%)" = proportiondead)
        
        scale <- switch(input$scale,
                       "Logaritminen" = scale_y_log10(labels = scales::format_format(big.mark = " ", decimal.mark = ",", drop0trailing=TRUE)),
                       "Lineaarinen" = scale_y_continuous(labels = scales::format_format(big.mark = " ", decimal.mark = ",", drop0trailing=TRUE))
        )
    
   ggplot(aggconfirmed, aes(x=dateconfirmed, y=data, colour=reorder(colourconfirmed, -data)))+
        geom_line(size=1)+
        labs(caption="Databyro.fi\nData: Johns Hopkins University")+
        ylab("")+
        xlab("")+
        scale+
        scale_colour_brewer(name="", palette = "Paired")+
        scale_x_date(date_breaks = "14 days", date_minor_breaks = "7 days", date_labels = "%d.%m.")+
        theme_minimal()+
        theme(plot.title = element_text(color="black", size=30, family="Philosopher"))+
        theme(plot.subtitle = element_text(color="black", size=20, family="Philosopher"))+
        theme(text=element_text(color="black", family="Philosopher", size=12))+
        theme(plot.caption=element_text(size=14, family="Philosopher"))+
        theme(plot.margin = margin(10,10,10,10))+
       theme(panel.grid.major.y = element_line(colour = "darkgrey"))
   
    })
    
##finnish chart
    json <- fromJSON(url("https://srv.databyro.fi/2020-corona/HSraw.json"), flatten = T)
    json <- json$confirmed
    json$date <- as.Date(json$date)
    json$date <- format(json$date, "%d.%m.")
    colnames(json) <- c("Potilas-id", "Tartunnan päivämäärä", "Sairaanhoitopiiri" , "Tartunnan lähdemaa" , "Tartunnan lähde")
    
    agegroups <- fromJSON(url("https://srv.databyro.fi/2020-corona/agegroups.json"), flatten = T)
    agegroups <- agegroups$dataset
    agegroups <- agegroups$value
    
    agegroups <- as.data.frame(agegroups)
    agegroups <- agegroups[,-10]
    
    colnames(agegroups) <- c("0-9v", "10-19v", "20-29v", "30-39v", "40-49v", "50-59v", "60-69v", "70-79v", "80-89v")
    dummy <- c("dummy")
    agegroups <- cbind(agegroups, dummy)
    
    agemelt <- melt(agegroups, id.vars = dummy)
    agemelt$value <- as.character(agemelt$value)
    agemelt$value <- as.numeric(agemelt$value)
    
    output$suomi <- renderPlot({
        
        data2 <- switch(input$data2,
                        "Tartunnat sairaanhoitoalueittain" =         
                            ggplot(json, aes(x=fct_rev(fct_infreq(json$Sairaanhoitopiiri))))+
                            geom_bar(fill="#756bb1")+
                            labs(caption = "Databyro.fi\nData: Helsingin Sanomat")+
                            xlab("")+
                            ylab("")+
                            scale_y_continuous(labels = scales::format_format(big.mark = " ", decimal.mark = ",", drop0trailing=TRUE), breaks = seq(0,100000,500), minor_breaks = seq(0,100000,100))+
                            coord_flip()+
                            theme_minimal()+
                            theme(plot.title = element_text(color="black", size=36, family="Philosopher"))+
                            theme(plot.subtitle = element_text(color="darkgrey", size=30, family="Philosopher"))+
                            theme(text=element_text(color="black", family="Philosopher", size=12))+
                            theme(plot.caption=element_text(size=14))+
                            theme(plot.margin = margin(10,10,10,10))+
                            theme(panel.grid.major.y = element_blank())+
                            theme(panel.grid.major.x = element_line(colour = "darkgrey")),
                        
                        "Tartuntojen jakauma (%)" =         
                            ggplot(agemelt, aes(x=fct_rev(agemelt$variable), y=((agemelt$value/sum(agemelt$value)*100))))+
                            geom_col(fill="#756bb1")+
                            labs(caption = "Databyro.fi\nData: THL")+
                            xlab("")+
                            ylab("")+
                            scale_y_continuous(labels = scales::format_format(big.mark = " ", decimal.mark = ",", drop0trailing=TRUE), minor_breaks = seq(0,100,1))+
                            coord_flip()+
                            theme_minimal()+
                            theme(plot.title = element_text(color="black", size=36, family="Philosopher"))+
                            theme(plot.subtitle = element_text(color="darkgrey", size=30, family="Philosopher"))+
                            theme(text=element_text(color="black", family="Philosopher", size=12))+
                            theme(plot.caption=element_text(size=14))+
                            theme(plot.margin = margin(10,10,10,10))+
                            theme(panel.grid.major.y = element_blank())+
                            theme(panel.grid.major.x = element_line(colour = "darkgrey"))
                            
                        )
        
        data2
        
    })
    
##table
    confirmed <- read.csv(url("https://srv.databyro.fi/2020-corona/JHUconfsglobal.csv"), check.names = F)
    deaths <- read.csv(url("https://srv.databyro.fi/2020-corona/JHUdeathsglobal.csv"), check.names = F)
    recovered <- read.csv(url("https://srv.databyro.fi/2020-corona/JHUrecosglobal.csv"), check.names = F)
    
    confirmed2 <- (confirmed[,ncol(confirmed)])
    country <- confirmed$`Country/Region`
    
    latest <- data.frame(country, confirmed2)
    latest[is.na(latest)]<- 0
    
    latest <- aggregate(latest$confirmed2, by=list(latest$country), FUN=sum)
    latest <- latest[order(-latest$x),]
    colnames(latest) <-  c("Valtio", "Kokonaistartuntoja")
    latest[latest==-1]<- 0
    latest$`Kokonaistartuntoja` <- format(round(latest$`Kokonaistartuntoja`, digits=0), nsmall = 0, big.mark = " ")
    
    output$tartunnat2 <- renderTable(
    
    formattable(latest, align= c("l", "l", "l", "l", "l"))
    )
}

shinyApp(ui = ui, server = server)
