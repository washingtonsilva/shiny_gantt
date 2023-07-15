


# packages used
library(tidyverse)
library(ggplot2)
library(DT)
library(shiny)


# user interface
ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    tags$h3("R Shiny Task Scheduling"),
    tags$hr(),
    textInput(
      inputId = "inTaskName",
      label = "Task:",
      placeholder = "e.g., Marketing"
    ),
    dateInput(
      inputId = "inStartDate",
      value = Sys.Date(),
      min = Sys.Date(),
      label = "Start Date:"
    ),
    dateInput(
      inputId = "inEndDate",
      value = Sys.Date() + 10,
      min = Sys.Date() + 1,
      label = "End Date:"
    ),
    actionButton(inputId = "btn", label = "Add Task")
  ),
  mainPanel(
    tags$h3("Task Table View"),
    tags$hr(),
    DTOutput(outputId = "tableTasks"),
    tags$h3("Task Chart View"),
    tags$hr(),
    plotOutput(outputId = "plotTasks")
  )
))


# server function
server <- function(input, output) {
  df <- reactiveValues(data = data.frame(
    Task = c("Research", "Clinical Trials", "Regulatory Approval"),
    StartDate = as.Date(c(
      "2023-05-01", "2023-07-01", "2024-01-01"
    )),
    EndDate = as.Date(c(
      "2023-06-30", "2023-12-31", "2024-06-30"
    ))
  ))
  
  # ADD TASK
  observeEvent(input$btn, {
    task_name <- input$inTaskName
    task_start_date <- input$inStartDate
    task_end_date <- input$inEndDate
    
    # Check if not null
    if (!is.null(task_name) &&
        !is.null(task_start_date) && !is.null(task_end_date)) {
      # Make a new row
      new_row <-
        data.frame(
          Task = task_name,
          StartDate = task_start_date,
          EndDate = task_end_date,
          stringsAsFactors = FALSE
        )
      # Add row to the existing dataframe
      df$data <- rbind(df$data, new_row)
      # Sort the dataframe by StartDate
      df$data <- df$data[order(df$data$StartDate),]
    }
  })
  
  output$tableTasks <- renderDT({
    datatable(
      data = df$data,
      colnames = c("Task", "Start Date", "End Date"),
      filter = "top"
    )
  })
  
  output$plotTasks <- renderPlot({
    ggplot(df$data,
           aes(
             x = StartDate,
             xend = EndDate,
             y = fct_rev(fct_inorder(Task)),
             yend = Task
           )) +
      geom_segment(linewidth = 10, color = "#0198f9") +
      labs(title = "Pharma Company Gantt Chart",
           x = "Duration",
           y = "Task") +
      theme_bw() +
      theme(legend.position = "none") +
      theme(
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12, angle = 45)
      )
  })
}

# run app
shinyApp(ui = ui, server = server)