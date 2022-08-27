library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(scales)
library(RColorBrewer)
library(Cairo)

to_spread <- function(x) {
    100*(exp(x)-1)/(exp(x+1))
}

# data
url <- "https://projects.fivethirtyeight.com/polls-page/"
poll_file <- "president_polls.csv"
download.file(paste0(url, poll_file), poll_file, 
              mode = "wb")

polls <- read_csv(poll_file,
                  col_types = cols(.default = col_character(), 
                                   question_id = col_double(),
                                   poll_id = col_double(),
                                   cycle = col_double(),
                                   pollster_id = col_double(),
                                   sponsor_ids = col_number(),
                                   pollster_rating_id = col_double(),
                                   sample_size = col_double(),
                                   internal = col_logical(),
                                   tracking = col_logical(),
                                   nationwide_batch = col_logical(),
                                   # candidate_id = col_double(),
                                   pct = col_double()))

# a: all adults; lv: likely voters; rv: registered voters; v: voters

# data cleaning
polls$start_date <- as.Date(polls$start_date, "%m/%d/%y")
polls$end_date <- as.Date(polls$end_date, "%m/%d/%y")
polls$fte_grade <- as.factor(polls$fte_grade)
polls$methodology <- as.factor(polls$methodology)
polls$population <- as.factor(polls$population)
polls$population_full <- as.factor(polls$population_full)
polls$stage <- as.factor(polls$stage)
polls$candidate_party <- as.factor(polls$candidate_party)

polls <- polls %>%
    mutate(mid_date = end_date - (1 + as.numeric(end_date - start_date)) %/% 2,
           state = as.factor(replace_na(state, "Nationwide")))

polls_apr_usa <- polls %>%
    filter(mid_date >= "2020-04-01",
           candidate_name %in% c("Joseph R. Biden Jr.", "Donald Trump"))

polls_apr <- polls_apr_usa %>%
    filter(state != "Nationwide")

# create ratio variable
q_id <- polls_apr %>%
    dplyr::select(question_id, start_date, end_date) %>%
    distinct() %>%
    pull(question_id)

biden <- rep(NA, length(q_id))
trump <- rep(NA, length(q_id))
ratio <- rep(NA, length(q_id))
spred <- rep(NA, length(q_id))
for (i in 1:length(q_id)){
    temp <- as.data.frame(filter(polls_apr, question_id == q_id[i]))
    if (any(!c("Biden", "Trump") %in% temp$answer)){
        next
    } 
    else{
        biden[i] <- temp[temp$answer == "Biden", "pct"]
        trump[i] <- temp[temp$answer == "Trump", "pct"]
        ratio[i] <- biden[i]/trump[i]
        spred[i] <- biden[i] - trump[i]
    }
}

tr_vals <- data.frame("biden" = biden, "trump" = trump, "ratio" = ratio, "spred" = spred)

polls_re <- polls_apr %>%
    dplyr::select(question_id, poll_id, start_date, end_date, pollster_id, pollster, state) %>%
    distinct() %>%
    bind_cols(tr_vals) %>%
    drop_na()

election_day <- as.Date("2020-11-03")
polls_re <- polls_re %>%
    mutate(days_til_elec = as.numeric(end_date - election_day),
           logratio = log(ratio),
           mid_date = end_date - (1 + as.numeric(end_date - start_date)) %/% 2,
           days_til_elec_mid = as.numeric(mid_date - election_day))

#####
# PollTracker
#####

cands <- c("Biden" = "navyblue", "Trump" = "firebrick4")

poll_line = function(data) {
    ggplot(data, aes(mid_date, pct)) +
        geom_line(aes(color = answer)) +
        geom_point(aes(color = answer, 
                       text = sprintf("Candidate: %s <br>Percentage: %.2f <br>Region: %s", candidate_name, pct, state)), size = 1) +
        scale_color_manual(values = cands) +
        scale_x_date(date_labels = "%b \n%d", date_breaks = "1 week") +
        labs(x = "Date", y = "Percentage of support", color = "Candidate") +
        theme_light() +
        theme(panel.background = element_rect(fill = "aliceblue"))
}

poll_smooth = function(data) {
    ggplot(data, aes(mid_date, pct)) +
        geom_smooth(aes(color = answer), se = FALSE) +
        geom_point(aes(color = answer, 
                       text = sprintf("Candidate: %s <br>Percentage: %.2f <br>Region: %s", candidate_name, pct, state)), size = 1) +
        scale_color_manual(values = cands) +
        scale_x_date(date_labels = "%b \n%d", date_breaks = "1 week") +
        labs(x = "Date", y = "Percentage of support", color = "Candidate") +
        theme_light() +
        theme(panel.background = element_rect(fill = "aliceblue"))
}

blank = function(data) {
    ggplot(data, aes(mid_date, pct)) +
        geom_blank() +
        labs(x = "Date", y = "Percentage of support", color = "Candidate") +
        # scale_x_date(date_labels = "%b \n%d", breaks = seq(from = start, to = end, by = "week")) +
        theme_light() +
        theme(legend.position = "none", panel.background = element_rect(fill = "aliceblue"))
}

#####
# Forecasts
theta_cur_ec <- read.csv("theta_cur_ec.csv")

dem_likely <- c("Delaware", "District of Columbia", "Hawaii", "Illinois", "Oregon", "Rhode Island", "Vermont")
rep_likely <- c("Idaho", "Louisiana", "Nebraska", "North Dakota", "South Dakota", "West Virginia", "Wyoming")

rep_votes <- sum(filter(drop_na(theta_cur_ec), est < 0)$electoralvotes) + sum(filter(theta_cur_ec, is.na(est) & state %in% rep_likely)$electoralvotes)
dem_votes <- sum(filter(drop_na(theta_cur_ec), est > 0)$electoralvotes) + sum(filter(theta_cur_ec, is.na(est) & state %in% dem_likely)$electoralvotes)

tossup <- filter(theta_cur_ec, 0 > lb & 0 < ub)$state

### Get forecasts for today
theta_cur_gen_df <- read.csv("theta_cur_gen.csv")
state_gen <- sort(theta_cur_gen_df$state)

plot_lograt = function(data) {
    ggplot(data, aes(x=state_gen)) +
        geom_hline(yintercept = 0, color = "grey75") +
        geom_point(aes(y = est)) +
        geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2) +
        # geom_point(aes(y = res), color = "blue", shape = 17, size = 2) +
        labs(x = "State", y = expression(hat(theta)), 
             title = "Log-ratio estimates: if the election were held today") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_spread = function(data){
    ggplot(data, aes(x=state_gen, y = to_spread(est), ymin = to_spread(lb), ymax = to_spread(ub))) +
        geom_hline(yintercept = 0, color = "grey60") +
        geom_hline(yintercept = 10, color = "darkcyan") +
        geom_hline(yintercept = -10, color = "darkcyan") +
        geom_errorbar(width = 0.8, color = ifelse(0 > data$lb & 0 < data$ub, "darkorchid4", ifelse(data$lb > 0, cands[1], cands[2]))) +
        geom_point(aes(text = sprintf("Net Biden estimate: %.2f points <br>State: %s", to_spread(est), state)), 
                   size = 1) +
        # geom_point(aes(y = res), color = "blue", shape = 17, size = 2) +
        labs(x = "State", y = "Net Biden", 
             title = "Net Biden estimates: if the election were held today") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#####

shinyApp(
    ui = navbarPage("Decision 2020: General Election", theme = shinytheme("flatly"),
                    tabPanel("Forecasts",
                             sidebarLayout(
                                 sidebarPanel(
                                     helpText(strong("Electoral College Tally")),
                                     
                                     helpText(paste0("Biden: ", dem_votes)),
                                     
                                     helpText(paste0("Trump: ", rep_votes)),
                                     
                                     helpText(paste("Current toss-up states: ", paste(tossup, collapse = ", "))),
                                     
                                     br(),
                                     
                                     helpText("Estimates based on smoothing mixed-effects model proposed in Wright 2018."),
                                     
                                     br(),
                                     
                                     helpText("Updated November 3, 2020.")
                                 ),
                                 mainPanel(plotlyOutput("plot2")
                                           
                                 ),
                             ),
                    ),
                    tabPanel("PollTracker",

                             sidebarLayout(
                                 
                                 sidebarPanel(
                                     
                                     helpText("Hover over points for more information, such as candidate name and percentage."),
                                     
                                     br(),
                                     
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
                                                  choices=c("Smoothing curve" = "smooth",
                                                            "Line plot" = "line"),
                                                  selected = "smooth"),
                                                                        
                                     br(),
                                     
                                     helpText("Source: FiveThirtyEight")
                                     
                                 ),
                                 
                                 mainPanel(plotlyOutput("plot"))
                             )
                    )
    ),
    server = function(input, output) {
        choices <- reactive({
            input$candidate
        })
        
        selectedData <- reactive({
            polls_apr_usa %>% 
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
                    ggplotly(poll_line(selectedData()), 
                             # height = 600,
                             tooltip = "text")
                }
            }
        })
        
        output$plot2 <- renderPlotly({
            ggplotly(plot_spread(theta_cur_gen_df),
                     tooltip = "text")
        })
    }
)
