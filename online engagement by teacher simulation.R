### Creates a stacked bar graph for total teacher online engagement ###
### Each bar = 1 teacher; Each color = different actions; y = total actions ###

# Load necessary library
library(dplyr)
library(tidyr)

#### SIMULATE DATA: TOTAL ONLINE ENGAGEMENT ####
# Set how many courses you want to simulate and how many teachers per course
n_courses <- 100
n_teachers_per_course <- 10

# Generate course names 
generate_course_names <- function(n) {
  letters <- c(LETTERS, do.call(paste0, expand.grid(LETTERS, LETTERS)))
  return(letters[1:n])
} # This makes course names A-Z, then AA, AB, AC... forever.
courses <- rep(generate_course_names(n_courses), each = n_teachers_per_course) #Add course names 

### Generate teacher names 
# This will make names Teacher_1, Teacher_2, etc.
#teachers <- paste("Teacher", seq(1, n_courses * n_teachers_per_course), sep = "_")
# This is predefined to give every course the same 10 names
teacher_names <- c("Adam", "Brian", "Chris", "David", "Eric", 
                   "Fiona", "Grace", "Heather", "Irene", "Julia")
teachers <- rep(teacher_names, n_courses)

# Create a dataframe
set.seed(123) # Setting the seed makes sure that everyone gets the same "random" result
online_engagement_simulation <- data.frame(
  Course = courses,
  Teacher = teachers,
  Sessions = sample(0:20, n_courses * n_teachers_per_course, replace = TRUE),
  Bookmarks = sample(0:20, n_courses * n_teachers_per_course, replace = TRUE),
  Reactions = sample(0:20, n_courses * n_teachers_per_course, replace = TRUE),
  Ratings = sample(0:20, n_courses * n_teachers_per_course, replace = TRUE),
  Downloads = sample(0:20, n_courses * n_teachers_per_course, replace = TRUE),
  Posts = sample(0:20, n_courses * n_teachers_per_course, replace = TRUE)
)

### Manually add courses
# For pedagogical reasons I imagine you don't want to sift through 6 billion courses
# And maybe you just want to make up some data?
# So here's how to do that for a single course.
manual_data <- data.frame(
  Course = rep("MANUAL", 10),
  Teacher = c("Adam", "Brian", "Chris", "David", "Eric", "Fiona", "Grace", "Heather", "Irene", "Julia"),
  Sessions = c(10,9,8,7,6,5,4,3,2,1),
  Bookmarks = c(10,9,8,7,6,5,4,3,2,1),
  Reactions = c(10,9,8,7,6,5,5,5,5,5),
  Ratings = c(2,3,4,5,6,6,5,4,3,2),
  Downloads = c(10,10,10,10,10,5,5,5,5,5),
  Posts = c(8,8,8,8,8,6,6,6,6,6)
)

# Combine the manual data with the existing dataset
# I am making a new dataset so that if you repeat this code by mistake
# You don't end up with "manual" in there 60 times
online_engagement_simulation_FULL <- rbind(online_engagement_simulation, manual_data)


#### SIMULATE DATA: TEACHER ONLINE ENGAGEMENT BAR CHART ####
library(shiny)
library(ggplot2)
library(dplyr)

### Shiny apps need 4 things:
# The user interface (what it should look like)
# The server logic (what it should do)
# The data (what it should use to do the thing)
# A function to combine those

data = online_engagement_simulation_FULL # Specifying the data here let's you reuse this code regardless of data source
### Define UI
# We want users to select one of the simulated courses
# And then see a cool plot
ui <- fluidPage(
  titlePanel("Online Engagement Indicators by Teacher (Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_course", "Select a Course:", 
                  choices = unique(data$Course))
    ),
    
    mainPanel(
      plotOutput("engagementPlot")
    )
  )
)

### Define server logic
# We need to make the customizable plot
server <- function(input, output) {
  
  output$engagementPlot <- renderPlot({
    # Filter data based on selected course
    filtered_data <- data %>%
      filter(Course == input$selected_course) %>%
      select(Teacher, Sessions, Bookmarks, Reactions, Ratings, Downloads, Posts) %>%
      gather("Category", "Value", -Teacher)
    
    # Generate stacked bar graph
    ggplot(filtered_data, aes(fill = Category, y = Value, x = Teacher)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Online Engagement Indicators for", input$selected_course),
           y = "Engagement Points",
           x = "Teacher") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)







