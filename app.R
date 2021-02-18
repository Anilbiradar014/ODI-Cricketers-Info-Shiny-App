library(readr)
library(magrittr)
library(shiny)
library(plotly)
library(shinythemes)
odiBatsmanDetails <-
    read_csv("Year_wise_batting_stats_all_ODI_players(20-12-19).csv")
odiBatsmanDetails %>% head()
dim(odiBatsmanDetails)
sum(is.na(odiBatsmanDetails))
odiBatsmanDetailsUpdated <- na.omit(odiBatsmanDetails)
sum(is.na(odiBatsmanDetailsUpdated))
dim(odiBatsmanDetailsUpdated)
country_List <-  sort(unique(odiBatsmanDetailsUpdated$country))
countryFullName <- c()
# To display the full name of the country, instead of 3 Letters
for (country in country_List) {
    if (country == 'AFG') {
        countryFullName <- c(countryFullName, "AFGHANISTAN")
    } else if (country == 'AUS') {
        countryFullName <- c(countryFullName, "AUSTRALIA")
    } else if (country == 'BAN') {
        countryFullName <- c(countryFullName, "BANGLADESH")
    } else if (country == 'BER') {
        countryFullName <- c(countryFullName, "BERMUDA")
    } else if (country == 'CAN') {
        countryFullName <- c(countryFullName, "CANADA")
    } else if (country == 'EAF') {
        countryFullName <- c(countryFullName, "EAST AFRICA")
    } else if (country == 'ENG') {
        countryFullName <- c(countryFullName, "ENGLAND")
    } else if (country == 'HOK') {
        countryFullName <- c(countryFullName, "HONG KONG")
    } else if (country == 'IND') {
        countryFullName <- c(countryFullName, "INDIA")
    } else if (country == 'IRE') {
        countryFullName <- c(countryFullName, "IRELAND")
    } else if (country == 'KEN') {
        countryFullName <- c(countryFullName, "KENYA")
    } else if (country == 'NAM') {
        countryFullName <- c(countryFullName, "NAMIBIA")
    } else if (country == 'NED') {
        countryFullName <- c(countryFullName, "NETHERLANDS")
    } else if (country == 'NEP') {
        countryFullName <- c(countryFullName, "NEPAL")
    } else if (country == 'NZL') {
        countryFullName <- c(countryFullName, "NEW ZEALAND")
    } else if (country == 'OMA') {
        countryFullName <- c(countryFullName, "OMAN")
    } else if (country == 'PAK') {
        countryFullName <- c(countryFullName, "PAKISTAN")
    } else if (country == 'PNG') {
        countryFullName <- c(countryFullName, "PAPUA NEW GUINEA")
    } else if (country == 'SAF') {
        countryFullName <- c(countryFullName, "SOUTH AFRICA")
    } else if (country == 'SCO') {
        countryFullName <- c(countryFullName, "SCOTLAND")
    } else if (country == 'SRL') {
        countryFullName <- c(countryFullName, "SRI LANKA")
    } else if (country == 'UAE') {
        countryFullName <- c(countryFullName, "UNITED ARAB EMIRATES")
    } else if (country == 'USA') {
        countryFullName <- c(countryFullName, "UNITED STATES")
    } else if (country == 'WIN') {
        countryFullName <- c(countryFullName, "WEST INDIES")
    } else if (country == 'ZIM') {
        countryFullName <- c(countryFullName, "ZIMBABWE")
    }
}
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    
    # Application title
    headerPanel(
        fluidRow(
            titlePanel("One-Day International(ODI) players year wise batting analysis from their Debut")
           )
    ),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            p("From the two drop downs below, first, select the country of the player and then the player name."),
            p("You will get corresponding 3 plots, which will give information about :"
            ),
            tags$ol(
                tags$li(
                    "Average runs scored by the Player each year from their debut to 2019 (or retirement) and Overall average"
                ),
                tags$li("Strike rate each year from their debut to retirement"),
                tags$li(
                    "Number of hundreds scored by the selected player and Sachin Tendulkar"
                )
            ),
            p(
                'Click on the corresponding header to view the plot. Hovering over the graph will give details about X and Y-axis Values.'
            ),
            sliderInput(
                "inningsSlider",
                label = h4("Minimum Number of Innings Played in thier Career:"),
                min = 0,
                max = 100,
                value = 20
            ),
            
            selectInput("countryList", "Select Player Country", choices = countryFullName),
            selectInput("PlayerName", "Select Player", choices = NULL),
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tab",
                tabPanel("Batting Average", plotlyOutput("PlayerRunplot")),
                tabPanel("Strike Rate", plotlyOutput("PlayerStrikeRate")),
                tabPanel(
                    "Number of Hundreds In comparision with Sachin Tendulkar",
                    plotlyOutput("PlayerVsTendulkar")
                    
                ),
                tabPanel(
                    "References",
                    tags$h3("Data Reference:"),
                    tags$ul(
                        tags$li(
                            "seshadri. ODI players year wise batting stats. Retrieved June 11, 2020, from Kaggle:"
                        ),
                        tags$a(
                            href = "https://www.kaggle.com/seshadri95/odi-players-year-wise-batting-stats",
                            "https://www.kaggle.com/seshadri95/odi-players-year-wise-batting-stats"
                        ),
                    ),
                    tags$h3("Reference:"),
                    tags$ul(
                        tags$li("ShinyfromRstudio. Retrieved from June 11, 2020, Shinyapps.io:"),
                        tags$a(
                            href = "https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/#t=30m35s",
                            "https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/#t=30m35s"
                        )
                    )
                )
            )
        )
    ),
    tags$br(),
    tags$h4("Student ID: S3798024"),
    tags$h4("Student Name : Anilkumar Lingraj Biradar")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observe({
        minimunInnings <- input$inningsSlider
        minInningsDataSet <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$Year == 'Overall', ]
        playersWithMinInningsDataSet <-
            minInningsDataSet[minInningsDataSet$Inns >= minimunInnings,]
        odiBatsmanDetailsUpdated <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$player_name
                                     %in%  playersWithMinInningsDataSet$player_name, ]
        
        country <- input$countryList
        # In data there are only 3 characters , since we are displaying full name
        if (country == 'AFGHANISTAN') {
            country_selected <- 'AFG'
        } else if (country == 'AUSTRALIA') {
            country_selected <- 'AUS'
        } else if (country == 'BANGLADESH') {
            country_selected <- 'BAN'
        } else if (country == 'BERMUDA') {
            country_selected <- 'BER'
        } else if (country == 'CANADA') {
            country_selected <- 'CAN'
        } else if (country == 'EAST AFRICA') {
            country_selected <- 'EAF'
        } else if (country == 'ENGLAND') {
            country_selected <- 'ENG'
        } else if (country == 'HONG KONG') {
            country_selected <- 'HOK'
        } else if (country == 'INDIA') {
            country_selected <- 'IND'
        } else if (country == 'IRELAND') {
            country_selected <- 'IRE'
        } else if (country == 'KENYA') {
            country_selected <- 'KEN'
        } else if (country == 'NAMIBIA') {
            country_selected <- 'NAM'
        } else if (country == 'NETHERLANDS') {
            country_selected <- 'NED'
        } else if (country == 'NEPAL') {
            country_selected <- 'NEP'
        } else if (country == 'NEW ZEALAND') {
            country_selected <- 'NZL'
        } else if (country == 'OMAN') {
            country_selected <- 'OMA'
        } else if (country == 'PAKISTAN') {
            country_selected <- 'PAK'
        } else if (country == 'PAPUA NEW GUINEA') {
            country_selected <- 'PNG'
        } else if (country == 'SOUTH AFRICA') {
            country_selected <- 'SAF'
        } else if (country == 'SCOTLAND') {
            country_selected <- 'SCO'
        } else if (country == 'SRI LANKA') {
            country_selected <- 'SRL'
        } else if (country == 'UNITED ARAB EMIRATES') {
            country_selected <- 'UAE'
        } else if (country == 'UNITED STATES') {
            country_selected <- 'USA'
        } else if (country == 'WEST INDIES') {
            country_selected <- 'WIN'
        } else if (country == 'ZIMBABWE') {
            country_selected <- 'ZIM'
        }
        
        countryWisePlayer <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$country ==
                                         country_selected, ]
        uniq_player <- sort(unique(countryWisePlayer$player_name))
        if (length(uniq_player) == 0) {
            uniq_player <- "None"
        }
        
        updateSelectInput(session, "PlayerName", "Select Player", choices = uniq_player)
        
    })
    
    
    output$PlayerRunplot <- renderPlotly({
        selectedPlayer <- input$PlayerName
        playerData <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$player_name == selectedPlayer, ]
        Year_Data <- playerData$Year
        Average_Score <- playerData$Avg
        lengthOfYear <- length(Year_Data)
        TotalNumberOfYears <- length(Year_Data) - 1
        colorList <- c()
        for (i in 1:TotalNumberOfYears) {
            colorList <- c(colorList, "lightskyblue")
        }
        colorList <- c(colorList, "steelblue")
        plot_ly(
            x = ~ Year_Data,
            y = ~ Average_Score,
            marker = list(color = colorList)
        ) %>% add_bars() %>%
            layout(
                yaxis = list(zeroline = FALSE, title = "Average Runs"),
                xaxis = list(title = "Year")
            )
    })
    output$PlayerStrikeRate <- renderPlotly({
        selectedPlayer <- input$PlayerName
        playerData <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$player_name == selectedPlayer &
                                         odiBatsmanDetailsUpdated$Year != 'Overall', ]
        Year_Data <- playerData$Year
        Average_StrikeRate <- playerData$`S/R`
        data <- data.frame(Year_Data, Average_StrikeRate)
        
        fig <-
            plot_ly(
                data,
                x = ~ Year_Data,
                y = ~ Average_StrikeRate,
                type = 'scatter',
                mode = 'lines+markers'
            ) %>%
            layout(yaxis = list(title = "Strike Rate"),
                   xaxis = list(title = "Year"))
        
        fig
        
    })
    output$PlayerVsTendulkar <- renderPlotly({
        selectedPlayer <- input$PlayerName
        sachinOverAllData <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$player_name == 'Sachin Tendulkar'
                                     &
                                         odiBatsmanDetailsUpdated$Year == 'Overall', ]
        playerData <-
            odiBatsmanDetailsUpdated[odiBatsmanDetailsUpdated$player_name == selectedPlayer
                                     &
                                         odiBatsmanDetailsUpdated$Year == 'Overall', ]
        if(playerData$player_name ==  sachinOverAllData$player_name) {
            PlayersName <-
                  sachinOverAllData$player_name
            NumberOfHundres <- playerData$`100s`
            dataComparison <- data.frame(PlayersName, NumberOfHundres)
            colorList <- c("lightskyblue")
            fig = plot_ly(
                dataComparison,
                x = ~ PlayersName,
                y = ~ NumberOfHundres,
                marker = list(color = colorList)
            ) %>% add_bars() %>%
                layout(
                    yaxis = list(zeroline = FALSE, title = "Number of Hundred(s)"),
                    xaxis = list(title = "Player")
                )
        } else {
            PlayersName <-
                c(playerData$player_name,
                  sachinOverAllData$player_name)
            NumberOfHundres <-
                c(playerData$`100s`, sachinOverAllData$`100s`)
            dataComparison <- data.frame(PlayersName, NumberOfHundres)
            dataComparison$PlayersName <-
                factor(
                    dataComparison$PlayersName,
                    levels = c(
                        playerData$player_name,
                        sachinOverAllData$player_name
                    )
                )
            colorList <- c("lightskyblue")
            colorList <- c(colorList, "steelblue")
            fig = plot_ly(
                dataComparison,
                x = ~ PlayersName,
                y = ~ NumberOfHundres,
                marker = list(color = colorList)
            ) %>% add_bars() %>%
                layout(
                    yaxis = list(zeroline = FALSE, title = "Number of Hundred(s)"),
                    xaxis = list(title = "Players")
                )
        }
       
          
       
        
        
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
