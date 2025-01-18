ui <- 
  fluidPage(
    titlePanel("Barley Yield"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "gen",  # Give the input a name "genotype"
                    label = "1. Select genotype",  # Give the input a label to be displayed in the app
                    choices = c("A" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"), selected = "a"),  # Create the choices that can be selected. e.g. Display "A" and link to value "a"
        selectInput(inputId = "colour", 
                    label = "2. Select histogram colour", 
                    choices = c("blue","green","red","purple","grey"), selected = "grey"),
        sliderInput(inputId = "bin", 
                    label = "3. Select number of histogram bins", 
                    min=1, max=25, value= c(10)),
        textInput(inputId = "text", 
                  label = "4. Enter some text to be displayed", "")
      ),
      mainPanel()
    )
  )
  )