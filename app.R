# We can load in packages and files here, before we design the user interface and server side
#  of our shiny app.

library(shiny)
library(tidyverse)
library(sf)
library(readxl)
library(sortable)

dat = data.frame(a = runif(100, min = 1, max = 3))

# R Shiny apps can be designed in (at least) two ways: 
#   1. With a ui and a server (which can be in one file, like this, or separate files)
#   2. With a cool package called flexdashboard, which works with R Markdown documents
#      and makes setting up a typical dashboard a breeze!

# Here, we will be looking at option 1: setting up a UI and a server.

# The UI is what the user sees. We build it by selecting one of the types of 'pages'
# that shiny (or shinydashboard) package offers. Most common are fluidPage, navbarPage 
#    and dashboardPage (from shinydashboard).

# Most / all of these page options are made up of 3 or 4 sections: title, sidebar (optional), main page, etc.
# 
# An optional addition is a sidebarLayout. This gives us a permanent sidebar, which is handy.
#   We call this function inside of our Page, just after the title. Inside this layout, we have
#   two panels: the sidebarPanel, and the mainPanel.

# Finally, we can also have more than one page. We could use a navbarPage to get this functionality,
# or we can put a "tabset" inside the main Panel of a fluidPage.
#
# For useful examples, check this website out: https://shiny.rstudio.com/gallery/widget-gallery.html?msclkid=0398a0c0d14011ec85779a469dcacca1
#   There you can find all of the 'widgets' that you can use in your shiny page, as 
#   well as examples of really nice shiny apps that come with their code. This is 
#   a nice way to start a new app if you are searching for inspiration.

#  Note that for the UI we must use commas between each argument, while on the server side, 
#  we don't need those. The server side kind of looks like a regular R script,
#  while the UI looks like one very long and complicated function.
ui <- fluidPage(
    
    # Here's our title
    titlePanel("Hello Shiny!"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "filter",
                        label = "Range filter",
                        min = 0,
                        max = 3,
                        value = c(0,2.5)),
            selectInput(inputId = "varselect",
                        label = "Select Variable to take logarithm",
                        choices = names(iris))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(title = "Unmodified Plot",
                         plotOutput('iris_plot')
                ),
                tabPanel(title = "Filtered",
                         plotOutput('iris_filtered')
                ),
                tabPanel(title = "New Variable",
                         plotOutput('iris_newvar')
                ),
                tabPanel(title = "Reactive UI",
                         numericInput(inputId = 'number_vars',
                                      label = "Number of Variables",
                                      value = 1,
                                      min = 1, 
                                      max = 4),
                         inputPanel("Here's an input panel.",
                                    verbatimTextOutput('logbook')),
                         tags$div(id = 'variable_selectors'),
                         dataTableOutput('iris_dynamictable'))
            )
        )
    )
)

#  Shiny works by having a conversation between the UI (the 'inputs') and the 
#  server side (which generates the 'outputs'). You can have varied kinds of inputs
#  (number input, drop-down menus, check-boxes, sliders, users can even upload files!)
#  and then you use those in the server side to do the nitty gritty.

server <- function(input, output) {
    
    ##### FIRST LEVEL OF COMPLEXITY ######
    
    # To make a plot, a table, or even just some text in the server side and show it
    #   in the UI, we have to use one of the 'render*' functions on the server side, 
    #   and the corresponding '*Output' functions on the UI side. Note that 
    #   many packages (e.g. leaflet, plotly) have their own versions of the 'render'
    #   and 'output' functions. For example:
    output$iris_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        iris %>% 
            ggplot() + 
            geom_histogram(aes(Petal.Width, fill = Species)) + 
            ggtitle("Unmodified Iris Plot") + 
            theme_minimal()
    })
    
    #  A key concept used in shiny is reactivity. This is typically done using the 
    #  function 'reactive({ })' if you want to make a dataset reactive, 
    #  or by using 'observe({ })', if you want to cause some effect (e.g. update the UI).
    #  You can make objects/variables reactive, meaning their value is recalculated 
    #  whenever the input data they depend on is changed. 
    #
    #  Once you define a variable 'reactively', it becomes
    #  a kind of function, and you'll need to use parentheses after its name to use it 
    #  in the future. For example:
    
    IrisFiltered = reactive({
        iris %>% 
            filter(Petal.Width >= input$filter[1],
                   Petal.Width < input$filter[2])
        #the input 'filter' is from our range filter that we defined in the UI above.
    })
    
    #Now we can use this 'IrisBinned' reactive object to make a new ggplot.
    
    output$iris_filtered = renderPlot({
        IrisFiltered() %>% 
            ggplot() + 
            geom_histogram(aes(Petal.Width, fill = Species)) + 
            ggtitle("Reactively Filtered Iris Plot") + 
            theme_minimal()
    })
    
    ##### SECOND LEVEL OF COMPLEXITY ######
    
    # One thing that is quite important to know how to do, but gets a little
    # complicated, is how to use 'tidyeval' to pass in variable names that are, themselves, reactive.
    # In other words, how can we use an input from the UI side on the *left* side of things like
    # filter / mutate functions?
    # If a user selects a variable, this input is typically in the form of a string ("hello!"), not
    # in the form of a 'symbol' (i.e. a column or variable name). There are two ways to convert a string into 
    # a symbol.
    #
    # For example, Let's ask the user to choose a variable to be modified (logged). To use the variable
    # name they've chosen, we can either use this: !!sym(x)
    #                                    or this: .data[[x]]
    
    IrisLogged = reactive({
        
        varname = input$varselect
        
        iris %>% 
            #mutate(varname = log(varname)) #Note: this doesn't work
            mutate(!!sym(varname) := log(!!sym(varname))) #But this does!
    })
    
    output$iris_newvar = renderPlot({
        
        varname = input$varselect
        
        IrisLogged() %>% 
            ggplot() + 
            geom_histogram(aes(x = .data[[input$varselect]], fill = Species)) + 
            geom_density(aes(x = !!sym(varname), col = Species, size = 3)) +
            scale_color_brewer(palette = "Dark2") +
            ggtitle("New Variable (the log of user input) Iris Plot") + 
            theme_minimal()
    })
    
    ##### THIRD LEVEL OF COMPLEXITY ######
    
    # There may be times when you want the user interface itself to be reactive!
    # This draws on the fact that the shiny library converts your R code into HTML (and CSS) - 
    # website stuff. You can actually dig down and directly access the HTML components to get
    # even greater customization of a shiny app.
    
    # For example, you may want the user to be able to choose the number of 
    # variables to be worked with. In this case, you can generate the UI elements
    # on the server side, and call them on the UI side by directing calling the HTML 
    # id that you set. 
    
    # I found a great stackoverflow post about how to dynamically render tables based 
    # on user input; I modified the answer a bit so that we dynamically render inputs.
    # Render the variable selection drop-downs based on the user's number input.
    
    #This object is kind of like a log book - what are the id's of the inputs we've
    #  added so far?
    inserted_variableselections <- c()
    
    #The following line says: "every time 'number_vars' changes, do the following".
    observeEvent(input$number_vars, {
        
        variable_number <- input$number_vars
        
        id <- paste0('variable_', variable_number)
        
        if (input$number_vars > length(inserted_variableselections)) {
            #User 'insertUI' to dynamically create UI elements in the server side.
            insertUI(selector = '#variable_selectors', #This selector is the HTML tag id we will use.
                     
                     #In the 'ui' section, we define the actual UI element.
                     ui = tags$div(tags$p(
                         selectInput(
                             inputId = paste0("variable_",variable_number), #setting the UI's input ID dynamically!
                             label = paste0("Variable ",variable_number),
                             choices = c(names(iris),"nothing"), #Note: we could use a reactive object/dataset here too!
                             multiple = F,
                             width = "200%"
                         )),
                         id = id))
            inserted_variableselections <<- c(id, inserted_variableselections) #Update the "logbook". This 
            #double arrow thing updates a variable OUTSIDE the scope of a function. I don't use it much.
        }
        else {
            inserted_variableselections <- sort(inserted_variableselections)
            removeUI(selector = paste0('#', inserted_variableselections[length(inserted_variableselections)]))          
            inserted_variableselections <<- inserted_variableselections[-length(inserted_variableselections)]
        }
        #This 'else' section actually REMOVES ui elements if the user reduces the number in the input.
    })
    
    #Let's take a look at this 'logbook'...
    output$logbook = renderText(paste0(inserted_variableselections, collapse = ", "))
    
    #Let's render the iris dataframe, applying a modification in a loop to each selected variable.
    output$iris_dynamictable = renderDataTable({
        
        #For each of the variables the user has input...
        for(i in 1:input$number_vars){
            
            #Get the variable name from the 'input' object.
            variable_name = input[[paste0("variable_", i)]]
            
            #Modify the corresponding variable in the dataset!
            if(variable_name != "nothing"){
                iris = iris %>% 
                    mutate(!!sym(variable_name) := log(!!sym(variable_name)))
            }
        }
        
        #After that modification loop, print out the table.
        iris  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
