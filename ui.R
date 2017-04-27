source("HW3.R")

shinyUI(
        
        navbarPage("Data Visualization Homework 3",
        
        tabPanel("Heat Map",
                 sidebarPanel(
                   checkboxGroupInput(inputId = "fb.dow",
                                    label="Days of the Week",
                                    choices=unique(fb$Post.Weekday),
                                    selected=unique(fb$Post.Weekday)),
                   sliderInput(inputId = "no.likes", 
                               label   = "Number of Likes", 
                               min     = min(fb$page.total.likes), 
                               max     = max(fb$page.total.likes),
                               value   = max(fb$page.total.likes) )
                   ),
                 mainPanel(plotOutput("heatmap"))
                 ),
        
        tabPanel("Small Multiples",
                 mainPanel(plotOutput("smallmult"),
                           checkboxGroupInput(inputId = "admit.type",
                                              label="Admission Type",
                                              choices=unique(df$admission.name),
                                              selected=unique(df$admission.name))
                 )),
        tabPanel("Parallel Coordinates",
                 sidebarPanel(
                   checkboxGroupInput(inputId = "post.type",
                                      label="Media Content in Post",
                                      choices=unique(fb$Type),
                                      selected="Link"    )
                 ),
                 mainPanel(plotOutput("parallel"))
        )
  )
)
