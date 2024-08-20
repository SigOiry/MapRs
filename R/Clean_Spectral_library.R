clean_spectra <- function(input_data) {
  
  # Define your start and end colors
  start_color <- "#F1D00A" # Red
  end_color <- "#7B0101"   # Blue
  
  
  num_colors <- length(unique(input_data$ID))
  
  # Create a color palette function
  color_palette <- colorRampPalette(c(start_color, end_color))
  
  # Generate the vector of colors
  color_vector <- color_palette(num_colors)
  
  
  # Define the number of colors you want in the sequence
 
  
  
  # Initialize a variable to store the edited data
  edited_data <<- NULL  # Assign globally to ensure it is accessible after app closes
  # Define UI
  ui <- shiny::fluidPage(
    tags$head(
      tags$style(HTML("
        body, html, #spectra_plot {
          height: 100%;
          width: 100%;
          margin: 0;
          padding: 0;
        }
        #control-panel {
          position: absolute;
          top: 10px;
          left: 10px;
          z-index: 100;
          background-color: rgba(255, 255, 255, 0.8);
          padding: 10px;
          border-radius: 8px;
          box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
        }
      "))
    ),
    div(
      id = "control-panel",
      shiny::selectizeInput("spectrum_select", "Select Spectrum to Delete:",
                            choices = NULL, selected = NULL, 
                            options = list(maxOptions = 1000)),
      shiny::actionButton("delete", "Delete Selected Spectrum"),
      shiny::actionButton("save_exit", "Save and Exit")
    ),
    plotly::plotlyOutput("spectra_plot", height = "900", width = "1800")
  )
  # Define server logic
  server <- function(input, output, session) {
    
    # Create a reactive dataframe
    spectra_data <- shiny::reactiveVal(input_data)

    
    # Update the selectizeInput with the spectrum choices dynamically
    shiny::updateSelectizeInput(session, "spectrum_select", 
                                choices = unique(input_data$ID), server = TRUE)
    
    output$spectra_plot <- plotly::renderPlotly({
      plotly::plot_ly(spectra_data(), x = ~Wavelength, y = ~Value, color = ~ID, colors = color_vector,
                      type = 'scatter', mode = 'lines', hoverinfo = 'text',
                      text = ~paste("Spectrum:", ID)) %>%
        plotly::layout(showlegend = F)
    })
    
    shiny::observeEvent(input$delete, {
      if (!is.null(input$spectrum_select) && input$spectrum_select != "") {
        spectrum_to_delete <- input$spectrum_select
        spectra_data(spectra_data() %>% dplyr::filter(ID != spectrum_to_delete))
        
        # Update the dropdown list to reflect the remaining spectra
        shiny::updateSelectInput(session, "spectrum_select",
                                 choices = unique(spectra_data()$ID), selected = NULL)
      }
    })
    
    shiny::observeEvent(input$save_exit, {
      # Save the edited dataframe to the edited_data variable
      edited_data <<- spectra_data()  # Assign it globally to capture after app closes
      shiny::stopApp()  # Stop the Shiny app
    })
  }
  
  # Run the Shiny app
  shiny::runApp(list(ui = ui, server = server))
  
  # Return the edited dataframe after the app stops
  return(edited_data)
}
