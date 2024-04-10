### Creates a stacked bar graph for course online engagement over time ###
### Each bar = 1 week; Each color = different actions; y = total actions by all teachers ###

#### WEEKLY COACHING ACTIVITIES SIMULATION ####
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


#### WEEKLY COACHING ACTIVITIES DASHBOARD ####
library(shiny)
library(ggplot2)
library(dplyr)

data = weekly_coaching_simulation_FULL

### Define UI
ui <- fluidPage(
  titlePanel("Weekly Coaching Activities Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_course", "Select a Course:", 
                  choices = unique(data$Course))
    ),
    
    mainPanel(
      plotOutput("weeklyActivitiesPlot")
    )
  )
)

### Define server logic
server <- function(input, output) {
  
  output$weeklyActivitiesPlot <- renderPlot({
    # Filter data based on selected course
    filtered_data <- data %>%
      filter(Course == input$selected_course) %>%
      group_by(Week_Start_Date) %>%
      summarise(
        Total_Individual_Checkin = sum(Individual_Checkin),
        Total_Individual_Resource_Sharing = sum(Individual_Resource_Sharing),
        Total_Individual_Debrief = sum(Individual_Debrief),
        Total_Group_Checkin = sum(Group_Checkin) / n_teachers_per_course, # Average per teacher
        Total_Group_Resource_Sharing = sum(Group_Resource_Sharing) / n_teachers_per_course,
        Total_Group_Debrief = sum(Group_Debrief) / n_teachers_per_course
      ) %>%
      gather("Activity", "Value", -Week_Start_Date) # Corrected to Week_Start_Date
    
    # Generate stacked bar graph
    ggplot(filtered_data, aes(fill = Activity, y = Value, x = Week_Start_Date)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste("Sum of Coaching Activities for Course", input$selected_course),
           y = "Total Activities",
           x = "Week") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate dates to be readable
  })
}

### Run the application
shinyApp(ui = ui, server = server)

