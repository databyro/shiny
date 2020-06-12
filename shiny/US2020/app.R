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

#read the polling data and mutate it a bit
pressanvaali <- read.csv(url("https://srv.databyro.fi/2020-US/president_polls.csv"))

colours= c(Biden = "#3333FF", Trump = "#E81B23")
pressanvaali <- pressanvaali[c(2,4,6,13,14,20,21,34,37)]
pressanvaali <- subset(pressanvaali, answer == "Biden" | answer == "Trump")
pressanvaali <- subset(pressanvaali, state == "")
pressanvaali$start_date <- as.Date(pressanvaali$start_date, format= "%m/%d/%y")
pressanvaali$end_date <- as.Date(pressanvaali$end_date, format= "%m/%d/%y")
pressanvaali$mid_date <- as.Date(pressanvaali$start_date + ((pressanvaali$end_date - pressanvaali$start_date) / 2), format="%y-%m-%d")



#ui
ui <- fluidPage(
    
    theme="style.css",
    use_googlefont("Philosopher"),
    titlePanel("Yhdysvaltain presidentinvaalit 2020"),
    titlePanel(h3("Mielipidemittuskeskiarvo")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            h4(strong("Rajaa mittausjoukkoa")),
            
            radioButtons("population", "Otantajoukko",
                         choices = list(
                             "Rekisteröityneet äänestäjät" = "rv",
                             "Todennäköiset äänestäjät" = "lv",
                             "Aikuiset" = "a")),
            
            sliderInput("sample_size", "Otannan vähimmäiskoko",
                        min = 300,
                        max = 2000,
                        value = 500),
            
            dateRangeInput("mid_date", "Mittausten päiväys", separator = "-", start = "2019-01-01", end = Sys.Date()),
            
            helpText(HTML("Datalähde: <a href='https://projects.fivethirtyeight.com/polls/' target='_blank'>FiveThirtyEight</a>"))),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("loess")
        )
    )
)

#server
server <- function(input, output) {

##simple scatterplot with LOESS smoothing
    output$loess <- renderPlot({
        
        pressanvaali <- subset(pressanvaali, population == input$population)
        pressanvaali <- subset(pressanvaali, sample_size > input$sample_size)
        pressanvaali <- subset(pressanvaali, mid_date > input$mid_date[1] & mid_date < input$mid_date[2])
        
        ggplot(pressanvaali, aes(x=mid_date, colour=answer))+
          geom_point(aes(y=pct), alpha=0.6)+
          geom_smooth(aes(y=pct), method = "loess", size=2, alpha=0.6, se=F, span=0.3)+
          scale_color_manual(values=colours, name="")+
          scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 months", date_labels = "%d.%m.%Y")+
          labs(caption="Databyro.fi\nData: FiveThirtyEight")+
          xlab("")+
          ylab("Kannatus (%)")+
          theme_minimal()+
          theme(plot.title = element_text(color="black", size=30, family="Philosopher"))+
          theme(plot.subtitle = element_text(color="black", size=20, family="Philosopher"))+
          theme(text=element_text(color="black", family="Philosopher", size=12))+
          theme(plot.caption=element_text(size=14, family="Philosopher"))+
          theme(plot.margin = margin(10,10,10,10))+
          theme(panel.grid.major.y = element_line(colour = "darkgrey"))
    })
}

shinyApp(ui = ui, server = server)
