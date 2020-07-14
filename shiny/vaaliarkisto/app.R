# declaring libraries
library(extrafont)
library(reshape)
library(plotly)
library(shiny)
library(fresh)
library(ggplot2)
library(reactable)
library(scales)

#setting up custom font directory
dir.create('~/.fonts')
file.copy("www/Philosopher.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# declaring colours
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
rrColor = '#FFD600'
nsColor = '#3399FF'
smpColor = '#053170'
ekoColor = 'darkgreen '
lkpColor = '#F1AF44'
skdlColor ='#BF1E24'
devaColor = '#8B0000'
tpslColor = '#DA2300'
kpColor = '#47C7E6'
keColor = '#ffd700'
iklColor = '#000008'
iklkokColor = 'black'

colours <- c("Sosialidemokraattinen Puolue" = sdpColor,
             "Kokoomus" = kokColor,
             "Keskusta" = keskColor,
             "Perussuomalaiset" = psColor,
             "Vihr" = vihrColor,
             "Vasemmistoliitto" = vasColor,
             "Ruotsalainen Kansanpuolue" = sfpColor,
             "Kristillisdemokraatit" = kdColor,
             "Liike Nyt" = liikColor,
             "Remontti" = rrColor,
             "Nuorsuomalaiset" = nsColor,
             "Suomen Maaseudun Puolue" = smpColor,
             "Kirjava Puolue" = ekoColor,
             "Liberaalinen Kansanpuolue" = lkpColor,
             "Suomen Kansan Demokraattinen Liitto" = skdlColor,
             "Demokraattinen Vaihtoehto" = devaColor,
             "Perustuslaillinen Oikeistopuolue" = "#BEBEBE",
             "SKYP" = "#BEBEBE",
             "TPSL" = tpslColor,
             "Suomen Kansanpuolue" = kpColor,
             "Vapaamielisten Liitto" = "#BEBEBE",
             "Kansallinen Edistyspuolue" = keColor,
             "IKL" = iklColor,
             "IKL + Kok." = iklkokColor,
             "PMP" = "#BEBEBE",
             "SPP" = "grey",
             "Kansanpuolue" = "grey",
             "Ruotsalainen Vasemmisto" = sfpColor,
             "STPV" = "#BEBEBE",
             "KrTL" = "#BEBEBE"
)

# declaring labels
labs <- c("Sosialidemokraattinen Puolue" = "SDP",
          "Kokoomus" = "Kok.",
          "Keskusta" = "Kesk.",
          "Perussuomalaiset" = "PS",
          "Vihr" = "Vihr.",
          "Vasemmistoliitto" = "Vas.",
          "Ruotsalainen Kansanpuolue" = "RKP",
          "Kristillisdemokraatit" = "KD",
          "Liike Nyt" = "Liik.",
          "Remontti" = "Remontti",
          "Nuorsuomalaiset" = "NuSu",
          "Suomen Maaseudun Puolue" = "SMP",
          "Kirjava Puolue" = "KiPu",
          "Liberaalinen Kansanpuolue" = "LKP",
          "Suomen Kansan Demokraattinen Liitto" = "SKDL",
          "Demokraattinen Vaihtoehto" = "DeVa",
          "Perustuslaillinen Oikeistopuolue" = "POP",
          "SKYP" = "SKYP",
          "TPSL" = "TPSL",
          "Suomen Kansanpuolue" = "KP",
          "Vapaamielisten Liitto" = "VL",
          "Kansallinen Edistyspuolue" = "KE",
          "IKL" = "IKL",
          "IKL + Kok." = "IKL + Kok.",
          "PMP" = "PMP",
          "SPP" = "SPP",
          "Kansanpuolue" = "Kans.",
          "Ruotsalainen Vasemmisto" = "RV",
          "STPV" = "STPV",
          "KrTL" = "KrTL"
          )

# user interface
ui <- fluidPage(theme="style.css",

    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    use_googlefont("Philosopher"),

    titlePanel(h1("Suomen vaaliarkisto"), windowTitle = "Suomen vaaliarkisto"),

    sidebarLayout(
        sidebarPanel(
            
            conditionalPanel(
                condition = "input.datamuoto == 'graph'",
            radioButtons("grafiikkamuoto",
                         h3("Grafiikan muoto"),
                         choices = list(
                             "Kannatushistoria" = "history",
                             "Vaaleittain" = "election"
                             ),
                         selected = c("history")
                         )
            ),
            
            radioButtons("resulttype",
                         h3("Tulokset"),
                         choices = list(
                             "Kannatus" = "Kannatus",
                             "Paikat" = "Paikat"
                             ),
                         selected = c("Kannatus")
                         ),
            
            radioButtons("vaalityyppi",
                         h3("Vaalityyppi"),
                         choices = list(
                             "Eduskuntavaalit" = "Eduskuntavaalit",
                             "Europarlamenttivaalit" = "Europarlamenttivaalit"
                             ),
                         selected = c("Eduskuntavaalit")
                         ),
            
            conditionalPanel(
                condition = "input.datamuoto == 'graph' && input.grafiikkamuoto == 'election' && input.vaalityyppi == 'Eduskuntavaalit'",
                selectInput("electionyear", h3("Vaalivuosi"),
                                               choices = list(
                                                   "2019" = 2019,
                                                   "2015" = 2015,
                                                   "2011" = 2011,
                                                   "2007" = 2007,
                                                   "2003" = 2003,
                                                   "1999" = 1999,
                                                   "1995" = 1995,
                                                   "1991" = 1991,
                                                   "1987" = 1987,
                                                   "1983" = 1983,
                                                   "1979" = 1979,
                                                   "1975" = 1975,
                                                   "1972" = 1972,
                                                   "1970" = 1970,
                                                   "1966" = 1966,
                                                   "1962" = 1962,
                                                   "1958" = 1958,
                                                   "1954" = 1954,
                                                   "1951" = 1951,
                                                   "1948" = 1948,
                                                   "1945" = 1945,
                                                   "1939" = 1939,
                                                   "1936" = 1936,
                                                   "1933" = 1933,
                                                   "1930" = 1930,
                                                   "1929" = 1929,
                                                   "1927" = 1927,
                                                   "1924" = 1924,
                                                   "1922" = 1922,
                                                   "1919" = 1919
                                               ),
                            selected = 2019
                            )
                ),
            
            conditionalPanel(
                condition = "input.datamuoto == 'graph' && input.grafiikkamuoto == 'election' && input.vaalityyppi == 'Europarlamenttivaalit'",
                selectInput("electionyear2", h3("Vaalivuosi"),
                            choices = list(
                                "2019" = 2019,
                                "2014" = 2014,
                                "2009" = 2009,
                                "2004" = 2004,
                                "1999" = 1999,
                                "1996" = 1996
                            ),
                            selected = 2019
                )
            ),

            
            conditionalPanel(
                condition = "input.vaalityyppi == 'Eduskuntavaalit' && input.grafiikkamuoto == 'history'",
                sliderInput("Vuosi",
                            h3("Vuodet"),
                            min=1919,
                            max=2019,
                            value=c(1919, 2019),
                            sep=""
                            )
                ),
            
            conditionalPanel(
                condition = "input.vaalityyppi == 'Europarlamenttivaalit' && input.grafiikkamuoto == 'history'",
                sliderInput("Vuosi2",
                            h3("Vuodet"),
                            min=1996,
                            max=2019,
                            value=c(1996, 2019),
                            sep=""
                            )
                ),
            
            radioButtons("datamuoto",
                         h3("Datan muoto"),
                         choices = list(
                             "Graafi" = "graph",
                             "Taulukko" = "table"
                             ),
                         selected = c("graph")
                         ),
            
            helpText(HTML("Avoin lähdekoodi <a href='https://github.com/Julleht/databyro/tree/master/shiny/vaaliarkisto' target='_blank'>GitHubissa</a>"))
            
            ),

        mainPanel(
            conditionalPanel(condition = "input.datamuoto == 'graph' && input.grafiikkamuoto == 'history'",
                             plotlyOutput("history")
                             ),
            
            conditionalPanel(condition = "input.datamuoto == 'graph' && input.grafiikkamuoto == 'election'",
                             conditionalPanel(condition = "input.vaalityyppi == 'Eduskuntavaalit'",
                                              htmlOutput("selected_elections")
                             ),
                             conditionalPanel(condition = "input.vaalityyppi == 'Europarlamenttivaalit'",
                                              htmlOutput("selected_elections2")
                             ),
                             htmlOutput("selected_results"),
                             plotOutput("election")
                             ),
            
            conditionalPanel(condition = "input.datamuoto == 'table'",
                             reactableOutput("taulukko")
                             )
        )
    )
)

# backend
server <- function(input, output) {
    
    output$selected_elections <- renderText({ 
        paste("<h2>", input$vaalityyppi, input$electionyear,"</h2>")
    })
    
    output$selected_elections2 <- renderText({ 
        paste("<h2>", input$vaalityyppi, input$electionyear2,"</h2>")
    })
    
    output$selected_results <- renderText({ 
        paste("<h3>", input$resulttype,"</h3>")
    })

    # party history graph
    output$history <- renderPlotly({

        df <- as.data.frame(read.csv("data/vaaliarkisto.csv", dec = ".", check.names = F, encoding = "UTF-8"))
        
        df2 <- subset(df, type==input$vaalityyppi[1])
        df2 <- subset(df2, result==input$resulttype[1])
        
        df2 <- switch(input$vaalityyppi,
                      "Eduskuntavaalit" = subset(df2, Vuosi>=input$Vuosi[1]),
                      "Europarlamenttivaalit" = subset(df2, Vuosi>=input$Vuosi2[1])
                      )
        
        df2 <- switch(input$vaalityyppi,
                      "Eduskuntavaalit" = subset(df2, Vuosi<=input$Vuosi[2]),
                      "Europarlamenttivaalit" = subset(df2, Vuosi<=input$Vuosi2[2])
        )
        
        molten.df <- melt(df2, id=c("Vuosi", "type", "result"))
        
        Vuosi <- switch(input$vaalityyppi,
            "Eduskuntavaalit" = c(input$Vuosi[1]-1, input$Vuosi[2]+1),
            "Europarlamenttivaalit" = c(input$Vuosi2[1]-1, input$Vuosi2[2]+1)
            )
        
        vaalit <- switch(input$vaalityyppi,
                      "Eduskuntavaalit" = c(0,90),
                      "Europarlamenttivaalit" = c(0,5)
        )
        
        range <- switch(input$resulttype,
                        "Kannatus" = list(ticksuffix = " %", title="", fixedrange=T),
                        "Paikat" = list(title="", range=vaalit, fixedrange=T)
        )
    
        
        plot_ly(data=molten.df, mode="lines+markers", type="scatter", x = ~Vuosi, y = ~value, color= ~variable, colors=colours, text = ~variable, marker=list(size=8), height=650) %>%
            layout(hovermode="closest", dragmode=F,
                   margin = list(l=0, r=0, t= 0, b=0, pad=5),
                   showlegend = T,
                   title = list(text = "",
                                font = list(family="Philosopher", size=40), x=0),
                   legend = list(y=-0.1, orientation = "h",
                                 font=list(family="Philosopher"),
                                 title = list(text = "",
                                              font = list(size=20, family="Advent Pro"))),
                   yaxis = range,
                   xaxis = list(title="", range=Vuosi, fixedrange=T)
                   ) %>%
            config(displayModeBar = F)
    })
    
    # election graph
    output$election <- renderPlot({
        
        df <- as.data.frame(read.csv("data/vaaliarkisto.csv", dec = ".", check.names = F, encoding = "UTF-8"))
        
        df2 <- subset(df, type==input$vaalityyppi[1])
        df2 <- subset(df2, result==input$resulttype[1])
        
        df2 <- switch(input$vaalityyppi,
                      "Eduskuntavaalit" = subset(df2, Vuosi==input$electionyear[1]),
                      "Europarlamenttivaalit" = subset(df2, Vuosi==input$electionyear2[1])
        )
        
        molten.df <- melt(df2, id=c("Vuosi", "type", "result"))
        newdf <- cast(molten.df, variable~Vuosi, value="value")
        colnames(newdf) <- c("variable", "year")
        newdf <- subset(newdf, year!=0)
        newdf <- na.omit(newdf)
        
        scale <- switch(input$resulttype,
                      "Kannatus" = scale_y_continuous(breaks = seq(0,100,5), minor_breaks = seq(0,100,1), limits=c(-1,(max(newdf$year)+2)), labels=scales::percent_format(accuracy = 1, suffix=" %", scale=1)),
                      "Paikat" = scale_y_continuous(breaks = seq(0,100,10), minor_breaks = seq(0,100,2))
        )
        
        label <- switch(input$resulttype,
                        "Kannatus" = geom_label(aes(label=scales::percent(year, accuracy = 0.01, suffix=" %", scale=1)), size=5),
                        "Paikat" = geom_label(aes(label=year), size=5)
        )
        
        ggplot(newdf, aes(x=reorder(variable, year), y=year))+
            geom_col(aes(fill=variable), alpha=0.9)+
            label+
            xlab("")+
            ylab("")+
            scale+
            scale_x_discrete(labels=labs)+
            scale_fill_manual(values = colours)+
            coord_flip()+
            theme_minimal()+
            theme(plot.title = element_text(color="black", size=40, family="Philosopher"))+
            theme(plot.subtitle = element_text(color="darkgrey", size=30, family="Philosopher"))+
            theme(text=element_text(color="black", family="Philosopher", size=26))+
            theme(legend.position = "none")+
            theme(plot.caption=element_text(hjust = 0.98, vjust = 1))+
            theme(plot.margin = margin(10,10,10,10))+
            theme(panel.grid.major.y = element_blank())+
            theme(panel.grid.major.x = element_line(colour = "darkgrey"))
    })
    
    # table
    output$taulukko <- renderReactable({
        
        df <- as.data.frame(read.csv("data/vaaliarkisto.csv", dec = ".", check.names = F, na.strings = c(""), encoding = "UTF-8"))
        head(df)
        names(df)[4] <- "Sosialidemo-kraattinen Puolue"
        names(df)[5] <- "Perus-suomalaiset"
        names(df)[8] <- "Vihreät"
        names(df)[9] <- "Vasemmisto-liitto"
        names(df)[10] <- "Ruotsalainen Kansan-puolue"
        names(df)[11] <- "Kristillis-demokraatit"
        names(df)[13] <- "Remontti-ryhmä"
        names(df)[14] <- "Nuor-suomalaiset"
        names(df)[16] <- "Kirjava ”Puolue” – Elonkehän Puolesta"
        names(df)[17] <- "Liberaalinen Kansan-puolue"
        names(df)[18] <- "Suomen Kansan-demokraat-tinen Puolue"
        names(df)[19] <- "Demokraat-tinen Vaihtoehto"
        names(df)[20] <- "Perustus-laillinen Oikeisto-puolue"
        names(df)[21] <- "Suomen Kansan Yhtenäi-syyden Puolue"
        names(df)[22] <- "Työväen ja Pienviljelijäin Sosialidemo-kraattinen Liitto"
        names(df)[23] <- "Suomen Kansan-puolue"
        names(df)[24] <- "Vapaa-mielisten Liitto"
        names(df)[25] <- "Kansallinen Edistys-puolue"
        names(df)[26] <- "Isänmaallinen kansanliike"
        names(df)[27] <- "Isänmaallinen kansanliike + Kokoomus"
        names(df)[28] <- "Pienviljelijäin ja maalais-kansan puolue"
        names(df)[29] <- "Suomen pienviljelijäin puolue"
        names(df)[30] <- "Kansan-puolue"
        names(df)[32] <- "Sosialistinen työväen ja pien-viljelijöiden vaalijärjestö"
        names(df)[33] <- "Kristillisen Työväen Liitto"
        head(df)
        
        df2 <- subset(df, type==input$vaalityyppi[1] | type==input$vaalityyppi[2])
        df2 <- subset(df2, result==input$resulttype[1] | result==input$resulttype[2])
        
        df2 <- switch(input$vaalityyppi,
                      "Eduskuntavaalit" = subset(df2, Vuosi >= input$Vuosi[1] & Vuosi <= input$Vuosi[2]),
                      "Europarlamenttivaalit" = subset(df2, Vuosi >= input$Vuosi2[1] & Vuosi <= input$Vuosi2[2])
                      )
        
        df2 <- df2[,c(1,4:32)]
        df2 <- df2[, colSums(is.na(df2)) != nrow(df2)]
        
        coldefs <- switch(input$resulttype,
            "Kannatus" = colDef(sortNALast = TRUE, align = "center", format = colFormat(suffix = " %")),
            "Paikat" = colDef(sortNALast = TRUE, align = "center")
        )
        
        reactable(df2,
                  defaultColDef = coldefs,
                  bordered = T,
                  height = "auto",
                  pagination = F,
                  striped = T,
                  compact = T,
                  columns = list(
                      Vuosi = colDef(
                          style = list(position = "sticky", left=0, background = "#fff", zIndex = 1, borderRight = "1px solid #eee"),
                          headerStyle = list(position = "sticky", left=0, background = "#fff", zIndex = 1, borderRight = "1px solid #eee"),
                          format = colFormat(suffix = "")
                          )
                      )
                  )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
