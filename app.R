library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Load helper functions
# Load your player data
df_age <<- readRDS("df_age.RDS")  # wide format, one row per player
age_prec <<- readRDS("age_prec.RDS")
level_scores <<- readRDS("level_scores.RDS")

############################################################
# Prepare matrices used by Functions.R
############################################################

age_cols <<- grep("_age$", names(age_prec), value = TRUE)

age_cols <<- intersect(age_cols, names(level_scores))

age_mat   <<- as.matrix(age_prec[, age_cols])
score_mat <<- as.matrix(level_scores[, age_cols])

player_names <<- age_prec$Name
############################################################
# Compute final career score
############################################################

final_score <<- apply(score_mat, 1, function(x) max(x, na.rm = TRUE))

############################################################
# Training set (players born before 1996)
############################################################

age_prec$Birth <<- as.Date(age_prec$Birth)  # convert POSIXct -> Date
train_idx <<- which(age_prec$Birth < as.Date("1996-01-01"))
source("Functions.R")

# Milestone mapping
milestone_labels <- c(
  "CHF" = "CH Final",
  "CHT" = "CH Title",
  "FT" = "Tour Title",
  "FF" = "Tour Final",
  "MT" = "MS Title",
  "MF" = "MS Final",
  "GSW" = "GS Title",
  "GSF" = "GS Final",
  "GSSF" = "GS Semis",
  "GSQF" = "GS Quarters",
  "GSR4" = "GS R4",
  "YECW" = "YEC Title",
  "YECF" = "YEC Final",
  "OGM" = "OG Medal",
  "OGG" = "OG Gold",
  "CGS" = "Career Grand Slam / 10 Slams",
  "T300" = "Top 300",
  "T200" = "Top 200",
  "T100" = "Top 100",
  "T50"  = "Top 50",
  "T30"  = "Top 30",
  "T20"  = "Top 20",
  "T10"  = "Top 10",
  "T5"   = "Top 5",
  "T1"   = "No.1"
)

# --- UI ---
# --- UI ---
ui <- fluidPage(
  titlePanel("ATP Career Milestones V2.0 (Data Updated 2026-02-09)"),
  
  sidebarLayout(
    sidebarPanel(
      # First player search
      selectizeInput(
        "search_player", "Search Player Name 1:",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        options = list(
          placeholder = 'Type to search player...',
          maxOptions = 100
        )
      ),
      
      # Side-by-side checkboxes
      fluidRow(
        tags$div(
          style = "display: flex; justify-content: space-between; font-size: 16px; padding-top: 5px;",
          checkboxInput("show_potential", "Potential Plot", value = FALSE),
          checkboxInput("show_similar", "Close Comparisons", value = FALSE),
          checkboxInput("show_compare", "Twin Trajectories", value = FALSE)
        )
      ),
      
      # Conditional panels for Potential / Similar
      conditionalPanel(
        condition = "input.show_potential == true",
        plotOutput("potential_plot", height = "300px", width = "100%")
      ),
      
      conditionalPanel(
        condition = "input.show_similar == true",
        sliderInput(
          "similar_age", "Select Age:", min = 15, max = 40, value = 25, step = 0.1
        ),
        tableOutput("similar_table")
      ),
      
      # Milestone dropdown (only show if Compare is not checked)
      conditionalPanel(
        condition = "input.show_compare == false",
        uiOutput("milestone_ui"),
        verbatimTextOutput("milestone_info")
      )
    ),
    
    mainPanel(
      # Timeline plot shown only if Compare is not checked
      conditionalPanel(
        condition = "input.show_compare == false",
        div(
          style = "overflow-x: auto; width: 100%;",
          plotOutput("timeline_plot", height = "700px", width = "1000px")
        )
      ),
      
      # Compare UI: second player dropdown and age slider (top right)
      conditionalPanel(
        condition = "input.show_compare == true",
        fluidRow(
          column(6,
                 selectizeInput(
                   "search_player2", "Search Player Name 2:",
                   choices = NULL,
                   selected = NULL,
                   multiple = FALSE,
                   options = list(
                     placeholder = 'Type to search second player...',
                     maxOptions = 100
                   )
                 )
          ),
          column(6,
                 sliderInput(
                   "compare_age", "Select Age Limit:", min = 15, max = 40, value = 25, step = 0.1
                 )
          )
        ),
        # Comparison plot below
        plotOutput("compare_plot", height = "600px", width = "100%")
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Player name updates for both dropdowns
  observe({
    updateSelectizeInput(
      session,
      "search_player",
      choices = df_age$Name,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "search_player2",
      choices = df_age$Name,
      server = TRUE,
      selected = "Carlos ALCARAZ"
    )
  })
  
  # Mutually exclusive behavior for first two checkboxes
  observeEvent(input$show_potential, {
    if(input$show_potential) updateCheckboxInput(session, "show_similar", value = FALSE)
  })
  observeEvent(input$show_similar, {
    if(input$show_similar) updateCheckboxInput(session, "show_potential", value = FALSE)
  })
  # Reference date for "current age"
  # Reference date for "current age"
  reference_date <- as.Date("2026-03-12")
  
  observeEvent(input$search_player, {
    
    req(input$search_player)
    
    birth <- age_prec$Birth[age_prec$Name == input$search_player][1]
    
    # compute age in years
    age_now <- as.numeric(
      difftime(reference_date, as.Date(birth), units = "days")
    ) / 365.25
    
    age_now <- round(age_now, 1)
    
    # enforce slider bounds
    if(age_now < 15) age_now <- 15
    if(age_now > 40) age_now <- 25
    
    # update BOTH sliders
    updateSliderInput(session, "similar_age", value = age_now)
    updateSliderInput(session, "compare_age", value = age_now)
    
  })
  
  # Selected player row
  player_row <- reactive({
    req(input$search_player)
    df_age %>% filter(Name == input$search_player)
  })
  
  # Dynamic milestone dropdown
  output$milestone_ui <- renderUI({
    req(player_row())
    p_row <- player_row()
    
    all_milestones <- names(milestone_labels)
    available_codes <- all_milestones[!is.na(unlist(p_row[1, paste0(all_milestones, "_age")]))]
    available_labels <- milestone_labels[available_codes]
    
    choices_list <- c("Select a milestone..." = "", setNames(available_codes, available_labels))
    
    selectizeInput(
      "search_milestone",
      "Milestone:",
      choices = choices_list,
      selected = "",
      multiple = FALSE,
      options = list(
        placeholder = 'Select milestone...',
        maxOptions = 20
      )
    )
  })
  
  output$milestone_info <- renderText({
    req(player_row(), input$search_milestone)
    
    p_row <- player_row()
    milestone_code <- input$search_milestone
    age_col  <- paste0(milestone_code, "_age")
    date_col <- milestone_code
    
    age_val  <- p_row[[age_col]][1]
    date_val <- p_row[[date_col]][1]
    
    # --- Compute ranking robustly ---
    # all players who have a non-NA age for this milestone
    ages_all <- age_prec[[age_col]]
    achieved_idx <- which(!is.na(ages_all))
    ages_achieved <- ages_all[achieved_idx]
    players_achieved <- age_prec$Name[achieved_idx]
    
    # rank of current player (smaller age = better)
    # handles ties correctly
    rank_vec <- rank(ages_achieved, ties.method = "min")
    player_rank <- rank_vec[players_achieved == p_row$Name[1]]
    
    total_N <- length(ages_achieved)
    
    paste0(
      "Age: ", round(age_val, 2), "\n",
      "Date: ", date_val, "\n",
      "Youngest Rank: ", player_rank, " / ", total_N
    )
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlot({
    req(player_row())
    p_row <- player_row()
    
    level <- classify_level(p_row)
    timeline_df <- build_timeline_age(p_row)
    
    plot_timeline_age(p_row, timeline_df, p_row$Name[1], level)
  })
  
  # Potential plot
  output$potential_plot <- renderPlot({
    req(input$show_potential)
    req(input$search_player)
    plot_potential(input$search_player)
  })
  
  # Similar players table
  output$similar_table <- renderTable({
    req(input$show_similar)
    req(input$search_player)
    req(input$similar_age)
    
    df <- find_similar_players(input$search_player, input$similar_age, lambda = 0.15)
    df %>% mutate(Distance = round(Distance, 2))
  })
  
  # Compare plot
  output$compare_plot <- renderPlot({
    req(input$show_compare)
    req(input$search_player, input$search_player2, input$compare_age)
    
    # Attempt the plot safely
    if(nrow(age_prec[age_prec$Name == input$search_player, ]) == 0 ||
       nrow(age_prec[age_prec$Name == input$search_player2, ]) == 0){
      plot.new()
      text(0.5, 0.5, "Player not found.", cex = 1.2, col = "red", adj = 0.5)
      return()
    }
    
    # Check if either player has no milestones before input_age
    p1_ages <- age_prec[age_prec$Name == input$search_player, grep("_age$", names(age_prec))]
    p2_ages <- age_prec[age_prec$Name == input$search_player2, grep("_age$", names(age_prec))]
    
    if(all(is.na(p1_ages) | p1_ages > input$compare_age) ||
       all(is.na(p2_ages) | p2_ages > input$compare_age)){
      plot.new()
      text(0.5, 0.5, paste0("Error: One or both players have no milestones before age ", 
                            input$compare_age, "."), 
           cex = 1.2, col = "red", adj = 0.5)
      return()
    }
    
    # Otherwise, plot normally
    plot_two_player_trajectories(input$search_player, input$search_player2, input$compare_age)
  })
}

shinyApp(ui, server)