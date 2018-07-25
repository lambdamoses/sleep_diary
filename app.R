library(shiny)
library(shinyTime)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(DT)

# Define the fields we want to save from the form
fields_m <- c("date", "time_bed", "time_slp", "if_woke", "reasons_woke",
              "reasons_awake", "time_ob", "feel_wake")
fields_e <- c("n_coffee", "n_btea", "n_gtea", "t_caffeine", "if_nap", "nap_start", "nap_end",
              "time_dinner", "if_snack", "medication", "doze", "mood_day", "n_steps",
              "activities_3h", "activities_1h", "sl_hygiene", "comments")
tzone <- Sys.timezone()
saveData <- function(data, part) {
    if (part == "morning") {
      data <- as.data.frame(data)
        names(data) <- fields_m
        if (hour(data$time_bed) > hms("12:00:00")) {
            date(data$time_bed) <- date(data$time_bed) - 1
        }
        if (hour(data$time_slp) > hms("12:00:00")) {
            date(data$time_slp) <- date(data$time_slp) - 1
        }
        eve <- rep(NA, length(fields_e))
        names(eve) <- fields_e
        eve <- as.data.frame(t(eve))
        dat_use <- cbind(data, eve)
        if (exists("responses")) {
          if (sum(responses$date == Sys.Date()) == 1) {
            ind <- responses$date == Sys.Date()
            responses <<- responses[-ind,]
            responses <<- rbind(responses, dat_use)
          } else if (!any(responses$date == Sys.Date())) {
            responses <<- rbind(responses, dat_use)
          } else {
            stop("Each day should only have one row")
          }
        } else {
            responses <<- dat_use
        }
    } else if (part == "evening") {
        names(data) <- fields_e
        data$sl_hygiene <- str_c(data$sl_hygiene, collapse = ", ")
        data <- as.data.frame(data)
        morn <- rep(NA, length(fields_m))
        names(morn) <- fields_m
        morn <- as.data.frame(t(morn))
        morn[1,1] <- Sys.Date()
        morn[,1] <- as.Date(morn[,1], origin = "1970-01-01")
        if (exists("responses")) {
            if (sum(responses$date == Sys.Date()) == 1) {
                ind <- responses$date == Sys.Date()
                dat_use <- cbind(responses[ind, 1:8, drop = FALSE],
                                 data)
                responses <<- responses[-ind,]
                responses <<- rbind(responses, dat_use)
            } else if (!any(responses$date == Sys.Date())) {
                dat_use <- cbind(morn, data)
                responses <<- rbind(responses, dat_use)
            } else {
                stop("Each day should only have one row")
            }
        } else {
            dat_use <- cbind(morn, data)
            responses <<- dat_use
        }
    } else {
        stop("part must be either 'morning' or 'evening'")
    }
}

loadData <- function() {
    if (exists("responses")) {
        responses
    }
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
    ui = fluidPage(theme = shinytheme("flatly"),
        navbarPage(title = "Sleep Diary",
        tabPanel(
        title = "Data Collection", 
          splitLayout(
            ##### 
            # Morning section, to be filled in the morning
            wellPanel(tags$h2("Morning"),
                       dateInput("date", "Date"),
                       tags$hr(),
                       tags$p(strong("Last night's sleep")),
                       timeInput("time_bed", "When did I go to bed last night?", seconds = FALSE),
                       timeInput("time_slp", "When did I feel sleepy last night?", seconds = FALSE),
                       radioButtons("if_woke", "Did I wake up after falling asleep last night?",
                                    choices = c("Yes", "No")),
                       textAreaInput("reasons_woke", "What (if anything) woke me up?"),
                       textAreaInput("reasons_awake", 
                                     "What (if anything) kept me up if I couldn't fall asleep?"),
                       tags$hr(),
                       tags$p(strong("This morning")),
                       timeInput("time_ob", "When did I get out of bed in the morning?", seconds = FALSE),
                       radioButtons("feel_wake", "How do I feel after waking up?",
                                    choices = c("Refreshed", "Somewhat refreshed", "Fatigued")),
                       actionButton("submit_m", "Submit for morning")),
            #####
            # Evening section, to be filled in the evening
            wellPanel(tags$h2("Evening"),
                       tags$p(strong("Caffeine Consumption")),
                       numericInput("n_coffee", 
                                    "How much coffee did I consume (approx. in ml)?",
                                    value = 0),
                       numericInput("n_btea", "How much black tea did I consume?",
                                    value = 0),
                       numericInput("n_gtea", "How much green tea did I consume?",
                                    value = 0),
                       timeInput("t_caffeine", "When was the latest time when I consumed caffeine?",
                                 seconds = FALSE),
                       tags$hr(),
                       tags$p(strong("Naps")),
                       radioButtons("if_nap", "Did I take a nap?", choices = c("Yes", "No")),
                       timeInput("nap_start", "Start of nap, if I took a nap:", seconds = FALSE),
                       timeInput("nap_end", "End of nap:", seconds = FALSE),
                      tags$hr(),
                       tags$p(strong("Food")),
                       timeInput("time_dinner", "When did I have dinner?", seconds = FALSE),
                       radioButtons("if_snack", "Did I snack within 2 hours of bed?",
                                    choices = c("Yes", "No")),
                       textInput("medication", "What medication (if any) did I take?"),
                      tags$hr(),
                       tags$p(strong("Daytime summary")),
                       radioButtons("doze", "How likely was it to doze off during the day?",
                                    choices = c("No chance", "Slight chance", "Moderate chance", "High chance")),
                       textInput("mood_day", "How was my mood during the day?"),
                      tags$hr(),
                       tags$p(strong("Activities")),
                       numericInput("n_steps", "How many steps did I walk today?", value = 0),
                       textAreaInput("activities_3h", "What did I do 1-3 hours before bed?"),
                       textAreaInput("activities_1h", "What did I do 1 hour before bed?"),
                      checkboxGroupInput("sl_hygiene", 
                                         "Which of following have I done last night?",
                                         choices = c("No blue light", "Hot shower",
                                                     "Keep room dark", "Meditation")),
                       textAreaInput("comments", "Other comments"),
                       actionButton("submit_e", "Submit for evening"))
                 )
    ),
    ##### 
    # View data
    tabPanel(title = "View Data",
             DT::dataTableOutput("responses")),
    #####
    # Plot data
    tabPanel(title = "Plot Data"))),
    
    server = function(input, output, session) {
        
        # Whenever a field is filled, aggregate all form data
        formMorning <- reactive({
            data <- lapply(fields_m, function(x) input[[x]])
            data
        })
        formEvening <- reactive({
            data <- lapply(fields_e, function(x) input[[x]])
            data
        })
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit_m, {
            saveData(formMorning(), "morning")
        })
        observeEvent(input$submit_e, {
            saveData(formEvening(), "evening")
        })
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$responses <- DT::renderDataTable({
            input$submit
            loadData() %>% 
              datatable() %>% 
              formatDate(c(2,3,7,11,13,14,15), "toLocaleString")
        })     
    }
)