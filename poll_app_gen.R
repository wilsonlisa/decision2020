options(scipen = 999)

# load libraries
library(tidyverse)
library(shiny)
library(plotly)
library(shinyWidgets)
library(scales)
library(RColorBrewer)
library(Cairo)

# data
url <- "https://projects.fivethirtyeight.com/polls-page/"
poll_file <- "president_polls.csv"
download.file(paste0(url, poll_file), poll_file, 
              mode = "wb")
# download.file(paste0(url, poll_file), paste(poll_file, Sys.Date(), sep="_"),
# mode = "wb")

polls <- read_csv(poll_file, 
                  col_types = cols_only(poll_id = col_double(),
                                        state = col_character(),
                                        display_name = col_character(),
                                        fte_grade = col_character(),
                                        sample_size = col_double(),
                                        population = col_character(),
                                        population_full = col_character(),
                                        methodology = col_character(),
                                        start_date = col_character(),
                                        end_date = col_character(),
                                        # sponsor_candidate = col_logical(),
                                        internal = col_logical(),
                                        tracking = col_logical(),
                                        nationwide_batch = col_logical(),
                                        stage = col_character(),
                                        candidate_party = col_character(),
                                        answer = col_character(),
                                        candidate_name = col_character(),
                                        pct = col_double())
)

# a: all adults; lv: likely voters; rv: registered voters; v: voters

# data cleaning
polls$start_date <- as.Date(polls$start_date, "%m/%d/%y")
polls$end_date <- as.Date(polls$end_date, "%m/%d/%y")
polls$fte_grade <- as.factor(polls$fte_grade)
polls$state <- as.factor(polls$state)
polls$methodology <- as.factor(polls$methodology)
polls$population <- as.factor(polls$population)
polls$population_full <- as.factor(polls$population_full)
polls$stage <- as.factor(polls$stage)
polls$candidate_party <- as.factor(polls$candidate_party)

#####

# polls <- filter(polls, party == "DEM" & sample_size >= 100)
# poll_count <- polls %>%
    # group_by(answer) %>%
    # summarize(count = n()) %>%
    # ungroup()
# polls <- data.frame(left_join(polls, poll_count))

# only include polling for candidates who have been included
# in more than 100 polls
# polls_sig <- filter(polls, count > 100)
# eliminate head-to-head polling (e.g., Biden vss Warren, Biden v Sanders)
# polls_sig_field <- filter(polls_sig, display_name != "Echelon Insights")

#####

polls_apr <- polls %>%
    filter(end_date >= "2020-04-01",
           candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump"))

# make this nicer
# polls %>%
# filter(pct > 5 & count > 100 & end_date > "2019-10-01") %>%
ggplot(polls_apr, aes(end_date, pct)) +
    geom_step(aes(color = candidate_name)) +
    # geom_smooth(aes(color = candidate_name), se = FALSE) +
    geom_point(aes(color = candidate_name), size = 1) +
    scale_x_date(date_labels = "%b \n%d", date_breaks = "1 week") +
    labs(x = "Date", y = "Percentage of support", color = "Candidate") +
    theme_light() +
    theme(panel.background = element_rect(fill = "aliceblue"))
# scale_color_viridis_d()

poll_step = function(data) {
    # scalelen <- length(unique(polls_sig$candidate_name))
    # timepal <- hue_pal(direction = -1)(scalelen)
    # names(timepal) <- unique(polls_sig$answer[order(polls_sig$answer)])
    # names(timepal) <- levels(reorder(stringr::str_wrap(polls_sig$candidate_name, 10), seasondat$season_outcome))
    
    ggplot(data, aes(end_date, pct)) +
        geom_step(aes(color = answer)) +
        geom_point(aes(color = answer, text = sprintf("Candidate: %s <br>Percentage: %.2f", candidate_name, pct)), size = 1) +
        # scale_color_manual(values = timepal) +
        scale_x_date(date_labels = "%b \n%d", date_breaks = "1 week") +
        labs(x = "Date", y = "Percentage of support", color = "Candidate") +
        theme_light() +
        theme(panel.background = element_rect(fill = "aliceblue"))
}

poll_smooth = function(data) {
    # scalelen <- length(unique(polls_sig$candidate_name))
    # timepal <- hue_pal(direction = -1)(scalelen)
    # names(timepal) <- unique(polls_sig$answer[order(polls_sig$answer)])
    # names(timepal) <- levels(reorder(stringr::str_wrap(polls_sig$candidate_name, 10), seasondat$season_outcome))
    
    ggplot(data, aes(end_date, pct)) +
        geom_smooth(aes(color = answer), se = FALSE) +
        geom_point(aes(color = answer, text = sprintf("Candidate: %s <br>Percentage: %.2f", candidate_name, pct)), size = 1) +
        # scale_color_manual(values = timepal) +
        scale_x_date(date_labels = "%b \n%d", date_breaks = "1 week") +
        labs(x = "Date", y = "Percentage of support", color = "Candidate") +
        theme_light() +
        theme(panel.background = element_rect(fill = "aliceblue"))
}

blank = function(data) {
    ggplot(data, aes(end_date, pct)) +
        geom_blank() +
        labs(x = "Date", y = "Percentage of support", color = "Candidate") +
        # scale_x_date(date_labels = "%b \n%d", breaks = seq(from = start, to = end, by = "week")) +
        theme_light() +
        theme(legend.position = "none", panel.background = element_rect(fill = "aliceblue"))
}

# User interface ----
ui <- fluidPage(
    titlePanel("PollTracker 2020: General Election"),
    
    sidebarLayout(
        sidebarPanel(
            # p(em("What happens on the show is only half the story.")),
            
            # p(strong("Which queens most successfully translated a reality television run into an expanded online fanbase? Explore this and more below!")),
            
            # br(),
            
            helpText("Hover over points for more information, such as candidate name and percentage."),
            
            br(),
            
            # helpText("Data available for seasons 4-10 only. If a contestant is not listed in the dropdown menus, they did not have Twitter data available during their season."),
            
            # br(),
            
            dateInput(inputId = "start", label = "Select a start date",
                      value = "2020-04-01"),
            
            dateInput(inputId = "end", label = "Select an end date"),
            
            uiOutput("candidate"),
            
            pickerInput(
                inputId = "candidate", 
                label = "Select/deselect candidates", 
                # choices = unique(polls_sig[polls_sig$start_date>input$start & polls_sig$end_date<input$end,]$candidate_name), 
                choices = unique(polls_apr$candidate_name),
                selected = c("Joseph R. Biden Jr.", "Donald Trump"),
                options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3"
                ), 
                multiple = TRUE
            ),
            
            radioButtons("switch", label="Select a plot type", 
                         choices=c("Stairstep plot" = "step",
                                   "Smoothing curve" = "smooth"),
                         selected = "step"),
            
            # br(),
            
            br(),
            
            helpText("Source: FiveThirtyEight")
            
        ),
        
        mainPanel(plotlyOutput("plot"))
    )
)

# Server logic
server <- function(input, output) {
    
    #output$candidate <- renderUI({
    #cands <- unique(polls_sig_field[polls_sig_field$start_date>=input$start & polls_sig_field$end_date<=input$end,]$candidate_name)
    #pickerInput(
    #inputId = "candidate", 
    #label = "Select/deselect candidates", 
    # choices = unique(polls_sig_field[polls_sig_field$start_date>input$start & polls_sig_field$end_date<input$end,]$candidate_name), 
    #choices = cands,
    #selected = c("Elizabeth Warren", "Bernard Sanders"),
    #options = list(
    #`actions-box` = TRUE, 
    #size = 10,
    #`selected-text-format` = "count > 3"
    #), 
    #multiple = TRUE
    #)
    #})
    
    choices <- reactive({
        input$candidate
    })
    
    selectedData <- reactive({
        polls_apr %>% 
            filter(start_date >= input$start & end_date <= input$end & candidate_name %in% choices()) 
    })
    
    output$plot <- renderPlotly({
        if (is.null(choices())){
            ggplotly(blank(input$start, input$end)
            )
        }
        else {
            if (input$switch == "smooth"){
                ggplotly(poll_smooth(selectedData()), 
                         # height = 600,
                         tooltip = "text")
            }
            else{
                ggplotly(poll_step(selectedData()), 
                         # height = 600,
                         tooltip = "text")
            }
        }
    })
}

# Run app
shinyApp(ui, server)
