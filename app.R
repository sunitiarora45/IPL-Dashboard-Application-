
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fontawesome))
suppressPackageStartupMessages(library(emojifont))

setwd(getwd())

matches <- read.csv("Dataset/matches.csv")
deliveries <- read.csv("Dataset/deliveries.csv")


my_color <- c("#d8a47f","#ffb997", "#f67e7d", "#d34e5b", "#74546a", "#843b62")

my_color_teams <- c("#d8a47f","#ffb997", "#f67e7d", "#d34e5b", "#843b62")


df <- deliveries %>% inner_join(matches, by = c('match_id' = 'id'))
season_filter_players <- sort(as.character(unique(df$season)), decreasing = F)
season_filter <- c("All", sort(as.character(unique(df$season)), decreasing = F))


# Define toss winner filter options
toss_winner_filter <- c("All", unique(df$toss_winner))
toss_winner_filter <- sort(toss_winner_filter)

# Create a summary table that counts the number of times each team chose to bat or field first after winning the toss
summary_table <- matches %>%
  filter(!is.na(toss_winner)) %>%
  group_by(toss_winner, toss_decision) %>%
  summarize(count = n()) %>%
  ungroup()

# Calculate winning percentage for each team
win_percentages <- matches %>%
  filter(!is.na(winner)) %>% # Remove null value team
  group_by(winner) %>%
  summarize(wins = n()) %>%
  mutate(win_percentage = round((wins / sum(wins)) * 100, 2)) %>%
  arrange(desc(win_percentage)) %>% # Sort in descending order color-wise
  top_n(14, win_percentage)


# Calculate wins by team
wins <- reactive({
  matches %>%
    group_by(winner) %>%
    summarise(total_wins = n()) %>%
    arrange(desc(total_wins)) %>%  # Sort by total_wins in descending order
    top_n(10, total_wins) # Keep only the top 10 teams by total_wins
})

# Find team with maximum wins
max_wins <- reactive({
  wins() %>%
    slice(1)
})


# Calculate wins by fielding first and venue

venue_field_wins <- matches %>%
  filter(toss_decision == "field") %>%
  group_by(venue) %>%
  summarise(total_win = sum(ifelse(winner == toss_winner, 1, 0))) %>%
  arrange(desc(total_win)) %>%
  top_n(10, total_win)

# Calculate wins by fielding first and venue

venue_bat_wins <- matches %>%
  filter(toss_decision == "bat") %>%
  group_by(venue) %>%
  summarise(total_win = sum(ifelse(winner == toss_winner, 1, 0))) %>%
  arrange(desc(total_win)) %>%
  top_n(10, total_win)


ui <- dashboardPage(
  dashboardHeader(title = 'IPL Analysis'
  ),
  dashboardSidebar( tags$style(HTML('.main-sidebar{width: 220px;}')),
                    sidebarMenu(
                      # menuItem('About', tabName = 'about', icon = icon('pen')),
                      menuItem('IPL - Summary', tabName = 'summary', icon = icon('trophy')),
                      menuItem('Winning Toss Analysis', tabName = 'toss', icon = icon('id-card')),
                      menuItem('Most Successful Teams', tabName = 'teams', icon = icon('chart-bar')),
                      menuItem('Top Performances - By Season', tabName = 'season', icon = icon('chart-bar'))
                    )
  ),
  dashboardBody(
    tabItems(
      
      # Overview -------------------------------------------------------------------
      tabItem(tabName = 'summary',
              h2('Indian Premier League', align = 'center'),
              h3('Objective', align = 'center'),
              tags$p('The objectives of this analysis are to explore various aspects of the Indian Premier League (IPL) using the provided dataset. This will involve identifying the team that won the most and lost the most number of matches in a season. Additionally, we will examine the impact of winning the toss on the match outcome to gain insight into its importance. Moreover, we will analyze how the trend of winning and losing teams who fielded and batted first varies across venues. Lastly, we will identify top-performing players in batting and bowling analysis. These objectives will provide insight into the performance of teams and players in the IPL and aid in future predictions and decision-making.', style = 'font-size: 120%;margin-left:2.5em;'),
              
              h3('Overview', align = 'center'),
              
              fluidRow(
                column(6, shinydashboard::box(title = '', width = 15,solidHeader = T, background = 'black',
                                              
                                              fluidRow(
                                                
                                                infoBoxOutput('matches'),tags$style('#matches {width:300px;}'),
                                                infoBoxOutput('teams'),tags$style('#teams {width:300px;}'),
                                                infoBoxOutput('runs'),tags$style('#runs {width:300px;}'),
                                                infoBoxOutput('wickets'),tags$style('#wickets {width:300px;}'),
                                                infoBoxOutput('seasons'),tags$style('#seasons {width:300px;}'),
                                                infoBoxOutput('mom'),tags$style('#mom {width:300px;}'),
                                                infoBoxOutput('sixes'),tags$style('#sixes {width:300px;}'),
                                                infoBoxOutput('fours'),tags$style('#fours {width:300px;}')
                                              ))
                       
                ),
                
                column(6,  shinydashboard::box(title = 'Winning Percentage', width = 12,solidHeader = T, background = 'black',
                                               
                                               plotOutput("pie_chart")
                ))
                
              )),
      
      
      # Winning Toss Analysis ---------------------------------------------------------
      
      tabItem(tabName = 'toss',
              
              fluidRow(
                shinydashboard::box(title = 'Winning Toss Analysis',width = 12, solidHeader = T, background = 'black',
                                    
                                    box(title = "Total Matches Played", width = 3, solidHeader = TRUE, status = "primary",
                                        infoBoxOutput("total_matches")),
                                    box(title = "Matches Won by Toss Winner", width = 3, solidHeader = TRUE, status = "success",
                                        infoBoxOutput("toss_winner_wins")),
                                    box(title = "Matches Won by Non-Toss Winner", width = 3, solidHeader = TRUE, status = "warning",
                                        infoBoxOutput("non_toss_winner_wins")),
                                    box(title = "Winning Percentage", width = 3, solidHeader = TRUE, status = "info",
                                        infoBoxOutput("winning_percentage"))
                                    
                )),
              
              
              column(6, shinydashboard::box(title = 'Toss Decision Plot', width = 12, solidHeader = T, background = 'black',
                                            
                                            plotOutput('toss_decision_plot', height = 450))),
              
              
              column(6, shinydashboard::box(title = 'Toss Win vs Match Win by Each Season', width = 12,solidHeader = T, background = 'black', 
                                            
                                            fluidRow(
                                              column(6, selectInput('season_filter', 'Select Season:',
                                                                    season_filter)),
                                              column(6, selectInput('toss_winner_filter', 'Select Toss Winner:',
                                                                    toss_winner_filter)),
                                              
                                              plotOutput("toss_vs_match_plot", height = 450))
                                            
              ))
      ),
      
      # Successful Teams Analysis ---------------------------------------------------------
      
      tabItem(tabName = 'teams',
              
              fluidRow(
                shinydashboard::box(title = 'Winning Toss Analysis',width = 12, solidHeader = T, background = 'black',
                                    
                                    
                                    plotOutput("wins_plot", height = 350)),
                
                shinydashboard::box(title = 'Winning matches by Fielding first at venues',width = 12, solidHeader = T, background = 'black', collapsible = T, collapsed = T,
                                    plotOutput("venue_field_plot")),
                
                shinydashboard::box(title = 'Winning matches by Batting first at venues',width = 12, solidHeader = T, background = 'black', collapsible = T, collapsed = T,
                                    plotOutput("venue_bat_plot"))
                
              )),
      
      
      # Season Analysis ---------------------------------------------------------
      
      tabItem(tabName = 'season',
              fluidPage(
                selectInput('season_filter_players', 'Select Season:',
                            season_filter_players),
                fluidRow(
                  shinydashboard::box(title = 'Batting Analysis',width = 12,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset1', height = '270px',
                                             tabPanel(tags$p('Most Runs', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_batsman', height = 210)),
                                             tabPanel(tags$p('Most Hundreds', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('most_100s', height = 210)),
                                             tabPanel(tags$p('Most Fifties', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('most_50s', height = 210)),
                                             tabPanel(tags$p('Most Sixes', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_6s', height = 210)),
                                             tabPanel(tags$p('Most Fours', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_4s', height = 210))
                                      )
                  ),
                  
                  shinydashboard::box(title = 'Bowling Analysis',width = 12,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset2', height = '270px',
                                             tabPanel(tags$p('Most Wickets', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_bowlers', height = 210)),
                                             tabPanel(tags$p('Most Maidens', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('maiden', height = 210)),
                                             tabPanel(tags$p('Most Dot Balls', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('dot_balls', height = 210)),
                                             tabPanel(tags$p('Most 4 Wickets', style = 'color:black;font-weight:bold;'),
                                                      plotOutput('wickets_4', height = 210))
                                      )
                  )
                )
              )
      )
    )
  )
)



# server ------------------------------------------------------------------

server <- function(input, output) { 
  
  # Overview Outputs --------------------------------------------------------
  
  output$pie_chart <- renderPlot({
    # Create pie chart for all teams
    ggplot(data = win_percentages, aes(x = "", y = win_percentage, fill = winner)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      # labs(title = "Winning percentage for all teams",
      labs (fill = "Team",
            x = NULL,
            y = NULL) +
      theme_void() 
    
    # theme(legend.text = element_text(color = "white")) # Change color of text in legends
  })
  
  
  output$matches <- renderInfoBox({
    infoBox('# Matches', df %>% summarise(matches = n_distinct(match_id)),
            icon = icon('handshake'), color = 'yellow', fill = T, width = 1)
  })
  output$seasons <- renderInfoBox({
    infoBox('# Season', df %>% summarise(matches = n_distinct(season)),
            icon = icon('trophy'), color = 'yellow', fill = T, width = 1)
  })
  
  output$runs <- renderInfoBox({
    infoBox('# Runs', df %>% summarise(runs = sum(total_runs)),
            icon = icon('walking'), color = 'yellow', fill = T, width = 1)
  })
  
  output$teams <- renderInfoBox({
    infoBox('# Teams', df %>% summarise(teams = n_distinct(batting_team)), 
            icon = icon('users'), color = 'yellow', fill = T, width = 1)
  })
  output$wickets <- renderInfoBox({
    infoBox('# Wickets', df %>% filter(dismissal_kind %in% 
                                         c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped'))%>%  summarise(wickets = n()), 
            icon = icon('hand-pointer'), color = 'yellow', fill = T, width = 1)
  })
  output$fours <- renderInfoBox({
    infoBox('# Fours', filter(df, batsman_runs == 4) %>% summarise(fours = n()), 
            icon = icon('dice-four'), color = 'yellow', fill = T, width = 1)
  })
  output$sixes <- renderInfoBox({
    infoBox('# Sixes', filter(df, batsman_runs == 6) %>% summarise(fours = n()), 
            icon = icon('dice-six'), color = 'yellow', fill = T, width = 1)
  })
  output$mom <- renderInfoBox({
    infoBox('Most MOM', tags$p((df %>% group_by(player_of_match) %>% summarise(num = n_distinct(match_id)) %>% 
                                  arrange(desc(num)) %>% head(1)), style = 'font-size: 90%;'),
            icon = icon('user-plus'), color = 'yellow', fill = T, width = 1)
  })
  
  
  
  # Toss Decision
  
  
  output$toss_decision_plot <- renderPlot({
    ggplot(summary_table, aes(x = "", y = count, fill = toss_decision)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      scale_fill_manual(values = c("yellow", "#E74C3C")) +
      # scale_fill_manual(values = c("skyblue", "palegreen")) +
      labs(title = "Decision taken after winning tosses")
    # labs(title = "Proportion of Teams Choosing to Bat or Field First After Winning Toss")
  })
  
  
  
  # Total Matches Played
  
  output$total_matches <- renderInfoBox({
    # infoBox("Total Matches Played", nrow(df), icon = icon("futbol"), color = "purple", width = 10, style = "color: black;")
    infoBox("Total Matches Played", tags$span(style = "color: black;", nrow(df)), icon = icon("futbol"), color = "purple", width = 10)
  })
  
  # Matches Won by Toss Winner
  output$toss_winner_wins <- renderInfoBox({
    if(input$season_filter!="All"){
      df_toss_winners <- df %>% filter(season == input$season_filter) %>%
        filter(toss_decision != "bat" | toss_winner == winner) %>% # Toss winner chose to field OR bat and won the match
        filter(toss_winner != "") # Filter out the matches where toss winner is not present in the dataset
      
      if (input$toss_winner_filter != "All") {
        df_toss_winners <- df_toss_winners %>% filter(toss_winner == input$toss_winner_filter)
      }
      
      infoBox("Matches Won by Toss Winner", tags$span(style = "color: black;", nrow(df_toss_winners)), icon = icon("check"), color = "green", width = 10)
    }
    else
    {
      df_toss_winners <- df %>% 
        filter(toss_decision != "bat" | toss_winner == winner) %>% # Toss winner chose to field OR bat and won the match
        filter(toss_winner != "") # Filter out the matches where toss winner is not present in the dataset
      
      if (input$toss_winner_filter != "All") {
        df_toss_winners <- df_toss_winners %>% filter(toss_winner == input$toss_winner_filter)
      }
      
      infoBox("Matches Won by Toss Winner", tags$span(style = "color: black;", nrow(df_toss_winners)), icon = icon("check"), color = "green", width = 10)
      
    }
  })
  
  # Matches Won by Non-Toss Winner
  output$non_toss_winner_wins <- renderInfoBox({
    if(input$season_filter!="All"){
      df_non_toss_winners <- df %>% filter(season == input$season_filter) %>%
        filter(toss_decision == "bat" & toss_winner != winner) %>% # Toss winner chose to bat and lost the match
        filter(toss_winner != "") # Filter out the matches where toss winner is not present in the dataset
      
      if (input$toss_winner_filter != "All") {
        df_non_toss_winners <- df_non_toss_winners %>% filter(toss_winner != input$toss_winner_filter)
      }
      
      infoBox("Matches Won by Non-Toss Winner", tags$span(style = "color: black;", nrow(df_non_toss_winners)), icon = icon("times"), color = "red", width = 10)
    }
    else
    {
      df_non_toss_winners <- df %>% 
        filter(toss_decision == "bat" & toss_winner != winner) %>% # Toss winner chose to bat and lost the match
        filter(toss_winner != "") # Filter out the matches where toss winner is not present in the dataset
      
      if (input$toss_winner_filter != "All") {
        df_non_toss_winners <- df_non_toss_winners %>% filter(toss_winner != input$toss_winner_filter)
      }
      
      infoBox("Matches Won by Non-Toss Winner", tags$span(style = "color: black;", nrow(df_non_toss_winners)), icon = icon("times"), color = "red", width = 10)
    }
  })
  
  # Winning Percentage
  output$winning_percentage <- renderInfoBox({
    if(input$season_filter!="All"){
      
      df_season <- df %>% filter(season == input$season_filter)
      
      total_matches <- nrow(df_season)
      toss_winner_wins <- df_season %>% filter(toss_decision != "bat" | toss_winner == winner) %>% # Toss winner chose to field OR bat and won the match
        filter(toss_winner != "") %>% # Filter out the matches where toss winner is not present in the dataset
        nrow()
      non_toss_winner_wins <- df_season %>% filter(toss_decision == "bat" & toss_winner != winner) %>% # Toss winner chose to bat and lost the match
        filter(toss_winner != "") %>% # Filter out the matches where toss winner is not present in the dataset
        nrow()
      
      winning_percentage <- round(((toss_winner_wins + non_toss_winner_wins) / total_matches) * 100, 2)
      
      
      # infoBox("Winning Percentage", paste0(winning_percentage, "%"), icon = icon("trophy"), color = "blue", width = 10)
      
      infoBox("Winning Percentage", tags$span(style = "color: black;", paste0(winning_percentage, "%")), icon = icon("trophy"), color = "blue", width = 10)
    }
    else
    {
      df_season <- df 
      
      total_matches <- nrow(df_season)
      toss_winner_wins <- df_season %>% filter(toss_decision != "bat" | toss_winner == winner) %>% # Toss winner chose to field OR bat and won the match
        filter(toss_winner != "") %>% # Filter out the matches where toss winner is not present in the dataset
        nrow()
      non_toss_winner_wins <- df_season %>% filter(toss_decision == "bat" & toss_winner != winner) %>% # Toss winner chose to bat and lost the match
        filter(toss_winner != "") %>% # Filter out the matches where toss winner is not present in the dataset
        nrow()
      
      winning_percentage <- round(((toss_winner_wins + non_toss_winner_wins) / total_matches) * 100, 2)
      
      
      # infoBox("Winning Percentage", paste0(winning_percentage, "%"), icon = icon("trophy"), color = "blue", width = 10)
      
      infoBox("Winning Percentage", tags$span(style = "color: black;", paste0(winning_percentage, "%")), icon = icon("trophy"), color = "blue", width = 10)
    }
    
  })
  
  
  
  # Toss Win vs Match Win Plot
  output$toss_vs_match_plot <- renderPlot({
    
    if(input$season_filter!="All"){
      df_toss_winners <- df %>% 
        filter(season == input$season_filter) %>%
        filter(toss_winner != "") # Filter out the matches where toss winner is not present in the dataset
      
      
      if (input$toss_winner_filter != "All") {
        df_toss_winners <- df_toss_winners %>% filter(toss_winner == input$toss_winner_filter)
      }
      
      df_toss_winners$toss_match <- ifelse(as.character(df_toss_winners$toss_winner) == as.character(df_toss_winners$winner), "Won", "Lost")
      
      ggplot(df_toss_winners[!is.na(df_toss_winners$toss_match), ], aes(x = toss_match, fill = toss_match)) +
        geom_bar() +
        xlab("Toss") + ylab("Number of Matches Won") + ggtitle("How Much of an Advantage is Winning the Toss")
    }
    else
    {
      df_toss_winners <- df %>% 
        filter(toss_winner != "") # Filter out the matches where toss winner is not present in the dataset
      
      if (input$toss_winner_filter != "All") {
        df_toss_winners <- df_toss_winners %>% filter(toss_winner == input$toss_winner_filter)
      }
      
      df_toss_winners$toss_match <- ifelse(as.character(df_toss_winners$toss_winner) == as.character(df_toss_winners$winner), "Won", "Lost")
      
      ggplot(df_toss_winners[!is.na(df_toss_winners$toss_match), ], aes(x = toss_match, fill = toss_match)) +
        geom_bar() +
        xlab("Toss") + ylab("Number of Matches Won") + ggtitle("How Much of an Advantage is Winning the Toss")
      
      
    }
    
  })
  
  
  # Teams and Venue Analysis
  # Render plot
  output$wins_plot <- renderPlot({
    ggplot(wins(), aes(x = reorder(winner, desc(total_wins)), y = total_wins)) +
      geom_bar(stat = "identity", aes(fill = as.numeric(total_wins)), width=0.6) +
      xlab("Team") +
      ylab("Total Wins") +
      ggtitle("Total Wins by Team in IPL") +
      geom_text(aes(label=total_wins), vjust = 2) +
      scale_fill_gradient(low = my_color_teams, high = my_color_teams[length(my_color_teams)]) +
      theme(axis.text.x = element_text(angle = 25, hjust=1))
  })
  
  # Render plot for venue analysis for fielding first
  output$venue_field_plot <- renderPlot({
    ggplot(venue_field_wins, aes(x = reorder(venue, desc(total_win)), y = total_win)) +
      geom_bar(stat = "identity", aes(fill = as.numeric(total_win)), width = 0.6) +
      xlab("Venue") +
      ylab("Total Matches Won by Fielding First") +
      ggtitle("Winning matches by fielding first across venues") +
      geom_text(aes(label = total_win), vjust = 2) +
      scale_fill_gradient(low = my_color_teams, high = my_color_teams[length(my_color_teams)]) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  }) 
  
  # Render plot for venue analysis for batting first
  output$venue_bat_plot <- renderPlot({
    ggplot(venue_bat_wins, aes(x = reorder(venue, desc(total_win)), y = total_win)) +
      geom_bar(stat = "identity", aes(fill = as.numeric(total_win)), width = 0.6) +
      xlab("Venue") +
      ylab("Total Matches Won by Fielding First") +
      ggtitle("Winning matches by batting first across venues") +
      geom_text(aes(label = total_win), vjust = 2) +
      scale_fill_gradient(low = my_color_teams, high = my_color_teams[length(my_color_teams)]) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  
  
  # Season Outputs --------------------------------------------------------
  
  output$top_10_batsman <- renderPlot({
    filter(df %>% group_by(batsman, season) %>% summarise(runs = sum(batsman_runs)), 
           season == input$season_filter_players) %>% arrange(desc(runs)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-runs), y = runs)) + geom_bar(stat="identity", width=0.6, aes(fill = as.numeric(runs))) +
      geom_text(aes(label = as.numeric(runs), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Batsman', y = 'Total Runs Scored') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$most_100s <- renderPlot({
    filter(df %>% group_by(batsman, season, match_id) %>% summarise(runs = sum(batsman_runs)),
           runs >= 100 & season == input$season_filter_players) %>% group_by(batsman) %>% summarise(hundreds = n()) %>% 
      arrange(desc(hundreds)) %>% 
      ggplot(aes(x = reorder(batsman,-hundreds), y = hundreds)) + geom_bar(stat="identity", width=0.6, aes(fill = as.numeric(hundreds))) + geom_text(aes(label = as.numeric(hundreds), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$most_50s <- renderPlot({
    filter(df %>% group_by(batsman, season, match_id) %>% summarise(runs = sum(batsman_runs)),
           runs >= 50 & season == input$season_filter_players) %>% group_by(batsman) %>% summarise(fifties = n()) %>% 
      arrange(desc(fifties)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-fifties), y = fifties)) + geom_bar(stat="identity", width=0.6, aes(fill = as.numeric(fifties))) +
      geom_text(aes(label = as.numeric(fifties), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_6s <- renderPlot({
    filter(as.data.frame(filter(df, batsman_runs == 6) %>% group_by(batsman, season) %>% summarise(sixes = n())), 
           season == input$season_filter_players) %>% arrange(desc(sixes)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-sixes), y = sixes)) + geom_bar(stat="identity", width=0.6, aes(fill = as.numeric(sixes))) +
      geom_text(aes(label = as.numeric(sixes), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Batsman', y = 'No. of 6s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_4s <- renderPlot({
    filter(as.data.frame(filter(df, batsman_runs == 4) %>% group_by(batsman, season) %>% summarise(fours = n())), 
           season == input$season_filter_players) %>% arrange(desc(fours)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-fours), y = fours)) + geom_bar(stat="identity", width=0.6, aes(fill = as.numeric(fours))) +
      geom_text(aes(label = as.numeric(fours), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Batsman', y = 'No. of 4s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_bowlers <- renderPlot({
    filter(df %>% filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped')) %>%
             group_by(bowler, season) %>% summarise(wickets = n()), 
           season == input$season_filter_players) %>% arrange(desc(wickets)) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -wickets), y = wickets)) + geom_bar(stat = 'identity', width = 0.6 , aes(fill = as.numeric(wickets))) +
      geom_text(aes(label = as.numeric(wickets), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Bowler', y = 'Total Wickets Taken') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$maiden <- renderPlot({
    data.frame(filter(filter(df, season== input$season_filter_players)%>% group_by(match_id,inning, over,bowler) %>% 
                        summarise(runs = sum(batsman_runs)-sum(bye_runs)), runs ==0) %>% group_by(bowler) %>% summarise(maiden =n())) %>%
      inner_join( data.frame(filter(df, season== input$season_filter_players)%>% group_by(match_id,inning, over,ball,bowler) %>% 
                               summarise(runs_given = sum(batsman_runs)-sum(bye_runs)-sum(legbye_runs)) %>% group_by(bowler) %>% 
                               summarise(runs_given= sum(runs_given), overs = n_distinct(match_id,over))) %>% mutate(econ = round(runs_given/overs,2)),
                  by = "bowler") %>% arrange(desc(maiden), econ) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -maiden), y = maiden)) + geom_bar(stat = 'identity', width = 0.6 , aes(fill = as.numeric(maiden))) +
      geom_text(aes(label = as.numeric(maiden), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$dot_balls <- renderPlot({
    filter(df, season== input$season_filter_players & total_runs == 0)%>% group_by(bowler) %>% summarise(dot = n())  %>% 
      arrange(desc(dot)) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -dot), y = dot)) + 
      geom_bar(stat = 'identity', width = 0.6 , aes(fill = as.numeric(dot))) +
      geom_text(aes(label = as.numeric(dot), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Bowler', y = '# of dot balls') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$wickets_4 <- renderPlot({
    filter(filter(df, season== input$season_filter_players) %>% 
             filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                          'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% summarise(wickets = n()), 
           wickets == 4) %>% group_by(bowler) %>% summarise(Four_wickets = n()) %>% 
      ggplot(aes(x = reorder(bowler, -Four_wickets), y = Four_wickets)) + 
      geom_bar(stat = 'identity', width = 0.6 , aes(fill = as.numeric(Four_wickets))) +
      geom_text(aes(label = as.numeric(Four_wickets), vjust = 2)) +
      scale_fill_gradient(low = my_color, high = my_color[length(my_color)]) +
      labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
}

shinyApp(ui, server)
