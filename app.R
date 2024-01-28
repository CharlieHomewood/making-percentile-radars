library(shiny)
library(tidyverse)
library(kableExtra)
library(here)

# sets wd to the parent directory of this file. Please ensure all relevant files 
# are contained within the same directory as this file in order to work
here::i_am("app.R")

source("Fbref Big5 data cleaning.R")
source("percentile radars data.R")
source("percentile radars plots.R")

options(shiny.autoreload = TRUE)

# Code for the UI
ui <- fluidPage(
    
    tags$head(
      tags$script(
        HTML("
        $(document).ready(function() {
          $(window).on('resize', function() {
            var fontSize = Math.min(0.015 * $(window).width(), 0.015 * $(window).height());
            Shiny.setInputValue('fontSize', fontSize);
          }).resize();
        });
        ")
      )
    ),
  
    tags$style('
               html {
                  min-height: 100%;
               }
               
               body {
                  color: black;
                  background-color: #d7ddfa;
                  min-height: 100vh;
               }
               
               .box {
                  border-top: 0px;
               }
               
               #percentileRadar {
                  margin: 0px;
               }
               
               .col-sm-3, .col-sm-6 {
                  height: fit-content;
                  padding: 0;
                  margin: 0;
               }
               
               .col-sm-3 .well {
                  height: fit-content;
                  padding: 1vh;
                  margin: 0;
                  margin-top: 1vh;
                  background-color: #b6c2fc;
                  border: solid 0.25vh;
               }
               
               .selectize-input.items.full.has-options.has-items {
                  background-color: #d7ddfa;
               }
               
               .selectize-dropdown-content {
                  background-color: #d7ddfa;
               }
                
               ."option active" {
                  background-color: #b6c2fc;
               }

               '),
      
        sidebarPanel(
        
        h2("Select a player...", style = "font-weight: bold; font-size: 4.5vh; margin-bottom: 1.5vh;"),
        
        div(id = "dataSelection", selectInput("data", "Select Data Frame", choices = c("Goalkeepers", "Defenders", "Midfielders", "Forwards")), style = "font-size: 2vh;"),
        
        div(id = "playerSelection", selectInput("player", "Select Row by Name", choices = NULL, multiple = FALSE, ), style = "font-size: 2vh;"),
        
        div(tableOutput("playerInfo"), style = "font-size: 1.75vh;"),
        
        width = 3

      ),
      
      mainPanel(
        
        h1(
          strong(textOutput("playerName")), 
          align = "center", 
          style = "font-size: 7.5vh; margin-bottom: 1.5vh;"
          ),
        
        div(
          plotOutput("percentileRadar", height = "80vh", width = "80vh"), 
          style = "height: fit-content; width: fit-content; border: solid 0.5vh;"
          ),
        
        h1(
          
          strong("Data from", a("FBref", href = "https://fbref.com/en/", style = "color: black; text-decoration: underline")), 
          style = "margin-top: 2vh;"
          
        ),
        
        width = 6,
        
        align = "center"
        
      ),
    
      sidebarPanel(
        
        id = "rightSidebarPanel",
        
        div(id = "playerDemos", tableOutput("playerDemos"), style = "font-size: 1.75vh;"),
        
        width = 3
        
      ),
    
    style = "min-height: 100vh;"

)

# Server logic
server <- function(input, output, session) {
  
  # A reactive function to store the currently selected data frame as an object
  # called "selected_df".
  selected_df <- reactive({
    switch(input$data,
           "Goalkeepers" = percentile_plot_data_GK,
           "Defenders" = percentile_plot_data_DF,
           "Midfielders" = percentile_plot_data_MF,
           "Forwards" = percentile_plot_data_FW)
  })
  
  # this observe() function detects any changes to the current data frame
  # selection and updates the player search box to only include players from
  # the currently selected data frame.
  observe({
    df <- selected_df()
    updateSelectizeInput(session, "player", choices = df$Player)
  })
  
  # A reactive function to store the currently selected player as an object 
  # called "selected_player".
  selected_player <- reactive({
    
    df <- selected_df()
    name <- input$player
    columns <- c("Player", grep("Percentile", colnames(df), value = TRUE))
    
    # If the current value in the search box is blank, return nothing (i.e.
    # return a NULL value - which technically isn't "nothing" ;) )
    if (name == "") {
      return(NULL)
    }
    
    # This selects the row from the currently selected data frame that 
    # corresponds to the player name that is searched for in the search box.
    return(df[df$Player == name, columns, drop = FALSE])
    
  })
  
  # A reactive function to store the non-statistic player info
  selected_player_info <- reactive({
    
    df <- selected_df()
    name <- input$player
    columns <- c(
      "Player",
      grep("Percentile", colnames(df), value = TRUE, invert = TRUE)
      )
    
    # If the current value in the search box is blank, return nothing (i.e.
    # return a NULL value - which technically isn't "nothing" ;) )
    if (name == "") {
      return(NULL)
    }
    
    # This selects the row from the currently selected data frame that 
    # corresponds to the player name that is searched for in the search box.
    return(df[df$Player == name, columns, drop = FALSE])
    
  })
  
  # A reactive function to display the current selected player name above the
  # percentile radar.
  output$playerName <- reactive({
    
    df <- selected_player()
    
    df$Player
    
  })
  
  # Player demographc info table
  output$playerDemos <- renderTable({
    
    df <- selected_player_info() %>% 
      mutate_all(as.character) %>% 
      pivot_longer(
        .,
        cols = -Player,
        names_to = "Player Info",
        values_to = " "
      )
    
    if (is.null(df)) {
      return(NULL)
    }
    
    df <- df %>% 
      select(-Player) %>% 
      slice(-c(1:2, 8:nrow(df)))
    
  }, spacing = "l", width = "100%")
  
  # The raw stats table
  output$playerInfo <- renderTable({
    
    df <- selected_player_info() %>% 
      mutate_all(as.character) %>% 
      pivot_longer(
        .,
        cols = -Player,
        names_to = "Raw Stats",
        values_to = " "
      )
    
    if (is.null(df)) {
      return(NULL)
    }
    
    df$` ` <-  lapply(df$` `, as.numeric)
    
    df <- df %>%
      select(-Player) %>% 
      slice(-c(1:7)) %>% 
      arrange(-desc(`Raw Stats`))
    
  }, digits = 2, spacing = "l", width = "100%")
  
  # Renders the percentile radar of the currently selected player.
  output$percentileRadar <- renderPlot({
    
    df <- selected_player()
    
    if (is.null(df)) {
      return(NULL)
    }
    
    df_long <- pivot_longer(df, cols = -Player, names_to = "Attribute", values_to = "Percentile")
    
    df_long$Attribute <- gsub("Percentile", "", as.character(df_long$Attribute))
    
    custom_order <- unique(c(percentile_radar_bar_orders[[input$data]], df_long$Attribute))
    
    df_long <- df_long %>% 
      mutate(Attribute = factor(Attribute, levels = custom_order))
    
    bar_colour <- percentile_radar_bar_colours[[input$data]]
    
    ggplot(
      data = df_long, 
      aes(x = Attribute, y = Percentile)
      ) +
    
    labs(caption = str_wrap("Note: Percentiles are based on comparisons with players from Europe's Big 5 leagues who play in the same position.", width = 45)) + 
    
    geom_bar(stat = "identity", width = 1, fill = bar_colour, color = "black", linewidth = 0.5) +
    
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      
    geom_label(
      aes(label = Percentile),
      size = 8,
      fontface = "bold",
      label.r = unit(0.55, "lines"), 
      ) +
      
    ylim(-10, 100) +
    
    theme(
      
      panel.grid.major = element_line(colour = "#787878", linetype = "dotdash"),
      panel.background = element_blank(),
      
      plot.background = element_rect("white"),
      plot.caption = element_text(size = 16, face = "italic"),
      
      axis.title = element_blank(),
      axis.text.x = element_text(face = "bold", color = "black", size = input$fontSize),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
      
    ) +
      
    coord_polar(
      start = 0
    )
    
  })
  
}

# Run the app
shinyApp(ui, server)





