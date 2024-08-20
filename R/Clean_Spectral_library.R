clean_spectra <- function(input_data, batch_size = 100) {
  
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
    shinyjs::useShinyjs(),
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
      shiny::actionButton("prev_batch", "Previous Batch"),
      shiny::actionButton("next_batch", "Next Batch"),
      shiny::actionButton("save_exit", "Save and Exit")
    ),
    plotly::plotlyOutput("spectra_plot", height = "900", width = "1800")
  )
  # Define server logic
  server <- function(input, output, session) {
    
    # Create a reactive dataframe
    spectra_data <- shiny::reactiveVal(input_data)
    
    # Initialize reactive values to manage batches
    batch_start <- reactiveVal(1)
    batch_end <- reactiveVal(min(batch_size, dplyr::n_distinct(input_data$ID)))
    
    # Update the selectizeInput with the spectrum choices dynamically
    shiny::updateSelectizeInput(session, "spectrum_select", 
                                choices = unique(input_data$ID), server = TRUE)
    
    output$spectra_plot <- plotly::renderPlotly({
      
      plot_data <- spectra_data()
      
      # Get the current batch of spectra
      unique_spectra <- unique(plot_data$ID)
      current_batch <- unique_spectra[batch_start():batch_end()]
      
      plot_data <- plot_data %>% dplyr::filter(ID %in% current_batch)
      
      
      plotly::plot_ly(plot_data, x = ~Wavelength, y = ~Value, color = ~ID,
                      type = 'scatter', mode = 'lines', hoverinfo = 'text',
                      text = ~paste("Spectrum:", ID)) %>%
        plotly::layout(showlegend = F)
    })
    
    # Hide or show the "Next Batch" button based on the current batch
    observe({
      total_spectra <- n_distinct(spectra_data()$Spectra)
      if (batch_end() >= total_spectra) {
        shinyjs::hide("next_batch")
      } else {
        shinyjs::show("next_batch")
      }
    })
    
    shiny::observeEvent(input$prev_batch, {
      if (batch_start() > 1) {
        new_start <- max(1, batch_start() - batch_size)
        new_end <- max(batch_size, batch_end() - batch_size)
        batch_start(new_start)
        batch_end(new_end)
      }
    })
    
    shiny::observeEvent(input$next_batch, {
      if (batch_end() < n_distinct(spectra_data()$ID)) {
        new_start <- batch_start() + batch_size
        new_end <- min(n_distinct(spectra_data()$ID), batch_end() + batch_size)
        batch_start(new_start)
        batch_end(new_end)
      }
    })
    
    shiny::observeEvent(input$delete, {
      if (!is.null(input$spectrum_select) && input$spectrum_select != "") {
        spectrum_to_delete <- input$spectrum_select
        spectra_data(spectra_data() %>% dplyr::filter(ID != spectrum_to_delete))
        # 
        # # Update the dropdown list to reflect the remaining spectra
        shiny::updateSelectInput(session, "spectrum_select",
                                 choices = unique(spectra_data()$ID), selected = NULL)

        # Adjust batch indices if needed
        if (batch_start() > n_distinct(spectra_data()$ID)) {
          batch_start(max(1, n_distinct(spectra_data()$ID) - batch_size + 1))
          batch_end(n_distinct(spectra_data()$ID))
        }
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
