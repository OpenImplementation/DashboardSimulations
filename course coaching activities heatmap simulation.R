### Creates a heatmap for coaching activities over time ###
### X = time, Y = action, cell color = number of actions ###
### In the second version, you can hover over a color to see individual teacher contributions ###

#### SIMULATE DATA: WEEKLY COACHING ACTIVITIES ####
# Load necessary library
library(tidyr)
library(dplyr)

# Define the timeframe (26 weeks for 6 months)
n_weeks <- 1:26
n_courses <- 100
n_teachers_per_course <- 10

# Generate course names
generate_course_names <- function(n) {
  letters <- c(LETTERS, do.call(paste0, expand.grid(LETTERS, LETTERS)))
  return(letters[1:n])
}
course_names <- generate_course_names(n_courses)

# Predefined list of teacher names
teacher_names <- c("Adam", "Brian", "Chris", "David", "Eric", 
                   "Fiona", "Grace", "Heather", "Irene", "Julia")

# Expand to weekly
weekly_coaching_simulation <- expand.grid(
  Course = course_names,
  Teacher = teacher_names,
  Week = n_weeks
)

### Generate random data for individual activity
set.seed(123)  # For reproducibility
weekly_coaching_simulation$Individual_Checkin <- sample(0:7, nrow(weekly_coaching_simulation), replace = TRUE)
weekly_coaching_simulation$Individual_Resource_Sharing <- sample(0:7, nrow(weekly_coaching_simulation), replace = TRUE)
weekly_coaching_simulation$Individual_Debrief <- sample(0:7, nrow(weekly_coaching_simulation), replace = TRUE)

# For group activity
group_checkin_values <- rep(sample(0:7, n_courses, replace = TRUE), each = n_teachers_per_course * length(n_weeks))
group_resource_sharing_values <- rep(sample(0:7, n_courses, replace = TRUE), each = n_teachers_per_course * length(n_weeks))
group_debriefs_values <- rep(sample(0:7, n_courses, replace = TRUE), each = n_teachers_per_course * length(n_weeks))

weekly_coaching_simulation$Group_Checkin <- group_checkin_values
weekly_coaching_simulation$Group_Resource_Sharing <- group_resource_sharing_values
weekly_coaching_simulation$Group_Debrief <- group_debriefs_values

### Fix week to date (starting January 1 2020)
library(lubridate)
start_date <- ymd("2020-01-01")
weekly_coaching_simulation$Week_Start_Date <- start_date + weeks(weekly_coaching_simulation$Week - 1)
weekly_coaching_simulation$Week_Start_Date <- format(weekly_coaching_simulation$Week_Start_Date, "%Y-%m-%d")



### Manually add a course
manual_individual_data <- data.frame(
  Teacher = rep(c("Adam", "Brian", "Chris", "David", "Eric", "Fiona", "Grace", "Heather", "Irene", "Julia"), length(n_weeks)),
  Week = rep(n_weeks, each = 10),
  Individual_Checkin = rep(c(2, 7, 2, 2, 7, 7, 2, 2, 7, 7), length(n_weeks)),
  Individual_Resource_Sharing = rep(c(3, 4, 3, 3, 4, 4, 3, 3, 4, 4), length(n_weeks)),
  Individual_Debrief = rep(c(5, 1, 5, 5, 1, 1, 5, 5, 1, 1), length(n_weeks))
)

# Add group activity data, which is the same for all teachers in MANUAL course
manual_group_data <- data.frame(
  Group_Checkin = rep(3, nrow(manual_individual_data)),
  Group_Resource_Sharing = rep(5, nrow(manual_individual_data)),
  Group_Debrief = rep(2, nrow(manual_individual_data))
)

# Combine individual and group data
manual_course_data <- cbind(Course = rep("MANUAL", nrow(manual_individual_data)), manual_individual_data, manual_group_data)

# Fix week to start date
start_date <- ymd("2020-01-01")
manual_course_data$Week_Start_Date <- start_date + weeks(manual_course_data$Week - 1)
manual_course_data$Week_Start_Date <- format(manual_course_data$Week_Start_Date, "%Y-%m-%d")

# Append the MANUAL course data to the existing dataset
weekly_coaching_simulation_FULL <- rbind(weekly_coaching_simulation, manual_course_data)


#### SIMULATE DASHBOARD: COURSE COACHING ACTIVITIES HEATMAP ####
library(shiny)
library(ggplot2)
library(reshape2)

# Assuming 'weekly_coaching_simulation_FULL' is your dataframe name
# Load or prepare your dataframe here if it's not already loaded

# Define UI
ui <- fluidPage(
  titlePanel("Course Actions Heatmap"),
  sidebarLayout(
    sidebarPanel(
      selectInput("course",
                  "Select a Course:",
                  choices = levels(weekly_coaching_simulation_FULL$Course))
    ),
    mainPanel(
      plotOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$heatmap <- renderPlot({
    # Filter data for the selected course
    filtered_data <- subset(weekly_coaching_simulation_FULL, Course == input$course)
    
    # Melt the data to have variables as factor levels
    melted_data <- melt(filtered_data, id.vars = c("Week", "Week_Start_Date"),
                        measure.vars = c("Individual_Checkin", "Individual_Resource_Sharing", 
                                         "Individual_Debrief", "Group_Checkin", 
                                         "Group_Resource_Sharing", "Group_Debrief"))
    
    # Creating the heatmap
    ggplot(melted_data, aes(x = Week, y = variable, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = "Week", y = "Action Type", fill = "Number of Actions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)



#### SIMULATE DASHBOARD: COURSE COACHING ACTIVITIES HEATMAP WITH HOVER ####
library(shiny)
library(plotly)
library(dplyr) # For data manipulation


# Define UI
ui <- fluidPage(
  titlePanel("Interactive Heatmap with Week Information on Hover"),
  sidebarLayout(
    sidebarPanel(
      selectInput("course",
                  "Select a Course:",
                  choices = levels(weekly_coaching_simulation_FULL$Course)) # Ensure your data is pre-loaded to get levels
    ),
    mainPanel(
      plotlyOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$heatmap <- renderPlotly({
    # Filter data for the selected course
    filtered_data <- weekly_coaching_simulation_FULL %>%
      filter(Course == input$course) %>%
      select(Week, Teacher, Individual_Checkin, Individual_Resource_Sharing,
             Individual_Debrief, Group_Checkin, Group_Resource_Sharing, Group_Debrief) %>%
      melt(id.vars = c("Week", "Teacher"), variable.name = "ActionType") %>%
      group_by(Week, ActionType, Teacher) %>%
      summarise(Value = sum(value), .groups = "drop")
    
    # Prepare hover text with week at the top and line breaks
    hover_data <- filtered_data %>%
      group_by(Week, ActionType) %>%
      summarise(HoverText = paste("Week:", Week, "\n", paste(Teacher, "=", Value, collapse = "\n"), sep=""), .groups = "drop")
    
    # Ensure only one week label appears
    hover_data <- hover_data %>%
      mutate(HoverText = gsub("Week: \\d+\\nWeek: ", "", HoverText, perl = TRUE))
    
    # Merge hover text with original data to get total values
    plot_data <- filtered_data %>%
      group_by(Week, ActionType) %>%
      summarise(TotalValue = sum(Value), .groups = "drop") %>%
      left_join(hover_data, by = c("Week", "ActionType"))
    
    # Create the heatmap
    plot_ly(data = plot_data, x = ~Week, y = ~ActionType, z = ~TotalValue, type = "heatmap",
            colors = colorRamp(c("white", "blue")),
            hoverinfo = "text",
            text = ~HoverText) %>%
      layout(yaxis = list(title = "Action Type"), xaxis = list(title = "Week"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

