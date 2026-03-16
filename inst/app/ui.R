# At the top of ui.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(shinycssloaders)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Species Distribution Modelling"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Species Distribution", tabName = "distribution", icon = icon("map")),
      menuItem("Environmental Data", tabName = "environment", icon = icon("globe")),
      menuItem("Model Training", tabName = "model", icon = icon("brain")),
      menuItem("Predictions", tabName = "predictions", icon = icon("chart-area")),
      menuItem("Future Projections", tabName = "future", icon = icon("temperature-high"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .content-wrapper, .right-side {
      background-color: #f4f4f4;
    }
    .box {
      border-radius: 8px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    /* Style for processing notifications - target by their type */
    .shiny-notification-notification {
      background-color: #ffa500 !important;
      color: white !important;
      font-weight: bold;
      border-left: 5px solid #ff8c00 !important;
    }
    .img-container {
        position: fixed;
        bottom: 20px;
        left: 20px;
        z-index: 1000;
        padding: 10px;
    }
  ")),
      tags$script(HTML("
    Shiny.addCustomMessageHandler('show_processing', function(message) {
      // Create a custom notification element
      var notificationId = 'processing_' + message.id;
      var notification = $('<div>')
        .attr('id', notificationId)
        .addClass('processing-notification')
        .addClass('shiny-notification')
        .css({
          'position': 'fixed',
          'bottom': '20px',
          'right': '20px',
          'z-index': '9999',
          'padding': '10px 15px',
          'border-radius': '5px',
          'max-width': '300px',
          'box-shadow': '0 2px 5px rgba(0,0,0,0.2)'
        })
        .text(message.text);

      // Remove any existing notification with same ID
      $('#' + notificationId).remove();

      // Add to notification panel
      $('#shiny-notification-panel').append(notification);

      // Auto-remove after timeout if specified
      if (message.duration) {
        setTimeout(function() {
          $('#' + notificationId).remove();
        }, message.duration);
      }
    });

    Shiny.addCustomMessageHandler('remove_processing', function(message) {
      var notificationId = 'processing_' + message.id;
      $('#' + notificationId).remove();
    });
  "))
    ),

    tabItems(
      # Tab 1: Data Upload - MODIFIED VERSION with pseudo-absence generation
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Upload Species Data", width = 6, status = "primary", solidHeader = TRUE,
            fileInput("species_file", "Choose CSV File",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            helpText("CSV file should contain columns: 'species' (1=presence, 0=absence), 'lon', 'lat'"),

            hr(),
            h4(HTML("<span style='color: red; font-weight: bold;'>Study Area Selection</span>")),
            radioButtons("study_area_type",
                         "Select study area method:",
                         choices = c(
                           "Custom coordinates (min/max longitude/latitude)" = "custom",
                           "Indonesia boundary (precise country shape)" = "indonesia",
                           "Upload custom boundary (Shapefile)" = "custom_boundary"
                         ),
                         selected = "custom"),

            conditionalPanel(
              condition = "input.study_area_type == 'custom'",
              fluidRow(
                column(6, numericInput("xmin", "Longitude Min", value = 95, min = -180, max = 180, step = 1)),
                column(6, numericInput("xmax", "Longitude Max", value = 141, min = -180, max = 180, step = 1))
              ),
              fluidRow(
                column(6, numericInput("ymin", "Latitude Min", value = -11, min = -90, max = 90, step = 1)),
                column(6, numericInput("ymax", "Latitude Max", value = 6, min = -90, max = 90, step = 1))
              )
            ),

            conditionalPanel(
              condition = "input.study_area_type == 'indonesia'",
              helpText(tags$span("Using Indonesia country boundary from rnaturalearth data",
                                 style = "color: #008000; font-weight: bold;")),
              br(),
              radioButtons("indonesia_scale", "Boundary resolution:",
                           choices = c(
                             "Small (1:110M) - Coarse, fast" = "small",
                             "Medium (1:50M) - Balanced" = "medium",
                             "Large (1:10M) - Detailed, slower" = "large"
                           ),
                           selected = "medium",
                           inline = FALSE),
              br(),
              # Display the Indonesia extent for information
              verbatimTextOutput("indonesia_extent_info"),
              br(),
              # Add a status indicator
              div(style = "padding: 10px; background-color: #e8f5e9; border-left: 5px solid #4CAF50; margin-bottom: 10px;",
                  h5(HTML("<span style='color: #2E7D32; font-weight: bold;'>✓ Indonesia Boundary Active</span>")),
                  p(style = "margin: 5px 0;",
                    HTML(paste(
                      "<strong>Current study area:</strong> Indonesia (precise boundary)<br>",
                      "<strong>Environmental data will be cropped</strong> to Indonesia's bounding box<br>",
                      "<strong>Points will be filtered</strong> to Indonesia land area"
                    ))
                  )
              )
            ),

            conditionalPanel(
              condition = "input.study_area_type == 'custom_boundary'",
              helpText(tags$span("Upload your own boundary as a shapefile",
                                 style = "color: #0000FF; font-weight: bold;")),
              br(),
              fileInput("custom_boundary_files",
                        "Choose Shapefile (select all files: .shp, .shx, .dbf, .prj, .sbn, .sbx, .cpg)",
                        multiple = TRUE,
                        accept = c(".shp", ".shx", ".dbf", ".prj", ".sbn", ".sbx", ".cpg")),
              helpText("Please select ALL files associated with your shapefile (at minimum: 6-7 type files)"),
              br(),
              conditionalPanel(
                condition = "output.custom_boundary_loaded",
                verbatimTextOutput("custom_boundary_info"),
                br(),
                div(style = "padding: 10px; background-color: #e3f2fd; border-left: 5px solid #2196F3; margin-bottom: 10px;",
                    h5(HTML("<span style='color: #1565C0; font-weight: bold;'>✓ Custom Boundary Loaded</span>")),
                    p(style = "margin: 5px 0;",
                      HTML(paste(
                        "<strong>Current study area:</strong> Custom uploaded boundary<br>",
                        "<strong>Environmental data will be cropped</strong> to boundary's bounding box<br>",
                        "<strong>Points will be filtered</strong> to boundary area"
                      ))
                    )
                )
              )
            ),

            # Add hidden inputs to store the actual coordinates
            # These will be updated when Indonesia is selected
            numericInput("current_xmin", label = NULL, value = 95, min = -180, max = 180, step = 1),
            numericInput("current_xmax", label = NULL, value = 141, min = -180, max = 180, step = 1),
            numericInput("current_ymin", label = NULL, value = -11, min = -90, max = 90, step = 1),
            numericInput("current_ymax", label = NULL, value = 6, min = -90, max = 90, step = 1),

            tags$head(tags$style(HTML("
              #current_xmin, #current_xmax, #current_ymin, #current_ymax {
                display: none;
              }
            "))),
            hr(),
            #h4("Pseudo-Absence Generation (for Presence-Only Data)"),
            h4(HTML("<span style='color: red; font-weight: bold;'>Pseudo-Absence Generation (for Presence-Only Data)</span>")),
            radioButtons("pseudo_absence_option",
                         "Does your data have absence (0) records?",
                         choices = c("Yes, my data has both presence and absence" = "has_absence",
                                     "No, I have presence-only data" = "presence_only"),
                         selected = "has_absence",
                         inline = FALSE), # KALAU TRUE, nanti pilihannya segaris

            conditionalPanel(
              condition = "input.pseudo_absence_option == 'presence_only'",
              h5(HTML("<span style='color: #ff471a; font-weight: bold;'>Generate pseudo-absence points:</span>")),
              radioButtons("pseudo_ratio",
                           "Pseudo-absence ratio",
                           choices = c("1× (equal to presence count)" = "1x",
                                       "2× (recommended)" = "2x"),
                           selected = "2x"),

              # Action button with better styling
              actionButton("generate_pseudo", "Generate Pseudo-Absences",
                           class = "btn-warning", icon = icon("random"),
                           style = "width: 100%; margin-bottom: 10px;"),
              # Status indicator
              uiOutput("pseudo_status"),
              helpText(tags$span("After generating pseudo-absences, you can download the data in CSV format' ", style = "color: #ff471a; font-weight: bold;")),
              hr(),
              conditionalPanel(
                condition = "output.pseudo_data_available",
                downloadButton("download_pseudo_data", "Download Pseudo-Absence Data (CSV)",
                               class = "btn-success",
                               style = "width: 100%;")
              ),
              helpText(tags$span("After download Pseudo-Absence Data, go back to 'Choose CSV File' ", style = "color: #008000; font-weight: bold;")),
            ),
            # Add this in the UI section, after the study area selection radio buttons
            h4("Or use example data:"),
            actionButton("load_example", "Load Example Data", class = "btn-success"),
            hr(),
            h4("Select Environmental Variables to Load"),
            h6(HTML("<span style='color: red; font-weight: bold;'>Climate Variables Must be checked as base predictors</span>")),

            # Climate category with descriptive labels
            div(class = "well",
                style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px;",
                checkboxInput("select_climate",
                              HTML("<strong>Climate Variables (19 bioclimatic)</strong>"),
                              value = TRUE),
                conditionalPanel(
                  condition = "input.select_climate == true",
                  div(style = "margin-left: 20px; padding: 10px; background-color: #ffffff; border-left: 3px solid #337ab7;",
                      h5("Select individual Bioclimatic variables:", style = "font-weight: bold; color: #337ab7;"),
                      div(style = "max-height: 400px; overflow-y: auto; padding: 10px; background-color: #f5f5f5; border-radius: 5px;",
                          checkboxGroupInput("selected_bio_vars",
                                             label = NULL,
                                             choices = c(
                                               "bio01 (Annual Mean Temperature)" = "bio01",
                                               "bio02 (Mean Diurnal Range: Mean of monthly (max temp - min temp))" = "bio02",
                                               "bio03 (Isothermality: (bio02/bio07) × 100)" = "bio03",
                                               "bio04 (Temperature Seasonality: Standard deviation × 100)" = "bio04",
                                               "bio05 (Max Temperature of Warmest Month)" = "bio05",
                                               "bio06 (Min Temperature of Coldest Month)" = "bio06",
                                               "bio07 (Temperature Annual Range: bio05 - bio06)" = "bio07",
                                               "bio08 (Mean Temperature of Wettest Quarter)" = "bio08",
                                               "bio09 (Mean Temperature of Driest Quarter)" = "bio09",
                                               "bio10 (Mean Temperature of Warmest Quarter)" = "bio10",
                                               "bio11 (Mean Temperature of Coldest Quarter)" = "bio11",
                                               "bio12 (Annual Precipitation)" = "bio12",
                                               "bio13 (Precipitation of Wettest Month)" = "bio13",
                                               "bio14 (Precipitation of Driest Month)" = "bio14",
                                               "bio15 (Precipitation Seasonality: Coefficient of Variation)" = "bio15",
                                               "bio16 (Precipitation of Wettest Quarter)" = "bio16",
                                               "bio17 (Precipitation of Driest Quarter)" = "bio17",
                                               "bio18 (Precipitation of Warmest Quarter)" = "bio18",
                                               "bio19 (Precipitation of Coldest Quarter)" = "bio19"
                                             ),
                                             selected = c("bio01", "bio12", "bio07", "bio15"),
                                             inline = FALSE,
                                             width = '100%')
                      ),
                      br(),
                      # Fixed alignment for action links
                      div(style = "display: flex; justify-content: flex-end; gap: 15px;",
                          actionLink("select_all_bio", "Select All",
                                     icon = icon("check-square"),
                                     style = "color: #337ab7;"),
                          actionLink("clear_all_bio", "Clear All",
                                     icon = icon("square-o"),
                                     style = "color: #d9534f;")
                      )
                  )
                )
            ),

            # Elevation (single variable, no sub-options needed)
            div(class = "well",
                style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px;",
                checkboxInput("select_elevation",
                              HTML("<strong>Elevation</strong>"),
                              value = TRUE)
            ),

            # Soil variables with more descriptive labels
            div(class = "well",
                style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px;",
                checkboxInput("select_soil",
                              HTML("<strong>Soil Variables (19 variables)</strong>"),
                              value = TRUE),
                conditionalPanel(
                  condition = "input.select_soil == true",
                  div(style = "margin-left: 20px; padding: 10px; background-color: #ffffff; border-left: 3px solid #5cb85c;",

                      # Categorical Soil Variables section
                      h5("Categorical Soil Variables:", style = "font-weight: bold; color: #5cb85c;"),
                      checkboxGroupInput("selected_soil_cat_vars",
                                         label = NULL,
                                         choices = c(
                                           "WRB4_CODE (World Reference Base soil classification - 4th level)" = "WRB4_CODE",
                                           "WRB2_CODE (World Reference Base soil classification - 2nd level)" = "WRB2_CODE",
                                           "FAO90_CODE (FAO-90 soil classification)" = "FAO90_CODE",
                                           "ROOT_DEPTH (Root depth limitation class)" = "ROOT_DEPTH",
                                           "DRAINAGE (Soil drainage class)" = "DRAINAGE"
                                         ),
                                         selected = c("WRB4_CODE", "WRB2_CODE", "FAO90_CODE", "ROOT_DEPTH", "DRAINAGE"),
                                         inline = FALSE),

                      # Fixed alignment for categorical soil action links
                      div(style = "display: flex; justify-content: flex-end; gap: 15px; margin-top: 5px; margin-bottom: 15px;",
                          actionLink("select_all_soil_cat", "Select All Categorical",
                                     icon = icon("check-square"),
                                     style = "color: #5cb85c;"),
                          actionLink("clear_all_soil_cat", "Clear All Categorical",
                                     icon = icon("square-o"),
                                     style = "color: #d9534f;")
                      ),

                      # Continuous Soil Variables section
                      h5("Continuous Soil Variables:", style = "font-weight: bold; color: #5cb85c; margin-top: 15px;"),
                      checkboxGroupInput("selected_soil_cont_vars",
                                         label = NULL,
                                         choices = c(
                                           "AWC (Available water capacity - mm/m)" = "AWC",
                                           "ROOTS (Root depth - cm)" = "ROOTS",
                                           "TOPDEP (Top depth of soil layer - cm)" = "TOPDEP",
                                           "BOTDEP (Bottom depth of soil layer - cm)" = "BOTDEP",
                                           "COARSE (Coarse fragments content - % volume)" = "COARSE",
                                           "SAND (Sand content - % weight)" = "SAND",
                                           "SILT (Silt content - % weight)" = "SILT",
                                           "CLAY (Clay content - % weight)" = "CLAY",
                                           "BULK (Bulk density - g/cm³)" = "BULK",
                                           "ORG_CARBON (Organic carbon content - % weight)" = "ORG_CARBON",
                                           "PH_WATER (Soil pH in water)" = "PH_WATER",
                                           "TOTAL_N (Total nitrogen content - % weight)" = "TOTAL_N",
                                           "CN_RATIO (Carbon to nitrogen ratio)" = "CN_RATIO",
                                           "CEC_SOIL (Cation exchange capacity - cmol+/kg)" = "CEC_SOIL"
                                         ),
                                         selected = c("AWC", "ROOTS", "TOPDEP", "BOTDEP", "COARSE", "SAND", "SILT", "CLAY", "BULK", "ORG_CARBON", "PH_WATER", "TOTAL_N", "CN_RATIO", "CEC_SOIL"),
                                         inline = FALSE),

                      # Fixed alignment for continuous soil action links
                      div(style = "display: flex; justify-content: flex-end; gap: 15px; margin-top: 5px;",
                          actionLink("select_all_soil_cont", "Select All Continuous",
                                     icon = icon("check-square"),
                                     style = "color: #5cb85c;"),
                          actionLink("clear_all_soil_cont", "Clear All Continuous",
                                     icon = icon("square-o"),
                                     style = "color: #d9534f;")
                      )
                  )
                )
            ),

            # Solar Radiation category
            div(class = "well",
                style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px;",
                checkboxInput("select_srad",
                              HTML("<strong>Solar Radiation (6 indices)</strong>"),
                              value = FALSE),
                conditionalPanel(
                  condition = "input.select_srad == true",
                  div(style = "margin-left: 20px; padding: 10px; background-color: #ffffff; border-left: 3px solid #f0ad4e;",
                      h5("Select solar radiation indices:", style = "font-weight: bold; color: #f0ad4e;"),
                      checkboxGroupInput("selected_srad_vars",
                                         label = NULL,
                                         choices = c(
                                           "srad_mean (Mean monthly solar radiation)" = "srad_mean",
                                           "srad_sd (Standard deviation of solar radiation)" = "srad_sd",
                                           "srad_cv (Coefficient of variation of solar radiation)" = "srad_cv",
                                           "srad_max (Maximum monthly solar radiation)" = "srad_max",
                                           "srad_min (Minimum monthly solar radiation)" = "srad_min",
                                           "srad_range (Range of solar radiation: max - min)" = "srad_range"
                                         ),
                                         selected = c("srad_mean", "srad_sd", "srad_cv", "srad_max", "srad_min", "srad_range"),
                                         inline = FALSE),

                      # Fixed alignment for solar radiation action links
                      div(style = "display: flex; justify-content: flex-end; gap: 15px; margin-top: 10px;",
                          actionLink("select_all_srad", "Select All",
                                     icon = icon("check-square"),
                                     style = "color: #5bc0de;"),
                          actionLink("clear_all_srad", "Clear All",
                                     icon = icon("square-o"),
                                     style = "color: #d9534f;")
                      )
                  )
                )
            ),

            # Wind Speed category
            div(class = "well",
                style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px;",
                checkboxInput("select_wind",
                              HTML("<strong>Wind Speed (6 indices)</strong>"),
                              value = FALSE),
                conditionalPanel(
                  condition = "input.select_wind == true",
                  div(style = "margin-left: 20px; padding: 10px; background-color: #ffffff; border-left: 3px solid #d9534f;",
                      h5("Select wind speed indices:", style = "font-weight: bold; color: #d9534f;"),
                      checkboxGroupInput("selected_wind_vars",
                                         label = NULL,
                                         choices = c(
                                           "wind_mean (Mean monthly wind speed)" = "wind_mean",
                                           "wind_sd (Standard deviation of wind speed)" = "wind_sd",
                                           "wind_cv (Coefficient of variation of wind speed)" = "wind_cv",
                                           "wind_max (Maximum monthly wind speed)" = "wind_max",
                                           "wind_min (Minimum monthly wind speed)" = "wind_min",
                                           "wind_range (Range of wind speed: max - min)" = "wind_range"
                                         ),
                                         selected = c("wind_mean", "wind_sd", "wind_cv", "wind_max", "wind_min", "wind_range"),
                                         inline = FALSE),

                      # Fixed alignment for wind speed action links
                      div(style = "display: flex; justify-content: flex-end; gap: 15px; margin-top: 10px;",
                          actionLink("select_all_wind", "Select All",
                                     icon = icon("check-square"),
                                     style = "color: #5bc0de;"),
                          actionLink("clear_all_wind", "Clear All",
                                     icon = icon("square-o"),
                                     style = "color: #d9534f;")
                      )
                  )
                )
            ),

            # Vapor Pressure category
            div(class = "well",
                style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px;",
                checkboxInput("select_vapr",
                              HTML("<strong>Vapor Pressure (6 indices)</strong>"),
                              value = FALSE),
                conditionalPanel(
                  condition = "input.select_vapr == true",
                  div(style = "margin-left: 20px; padding: 10px; background-color: #ffffff; border-left: 3px solid #5bc0de;",
                      h5("Select vapor pressure indices:", style = "font-weight: bold; color: #5bc0de;"),
                      checkboxGroupInput("selected_vapr_vars",
                                         label = NULL,
                                         choices = c(
                                           "vapr_mean (Mean monthly vapor pressure)" = "vapr_mean",
                                           "vapr_sd (Standard deviation of vapor pressure)" = "vapr_sd",
                                           "vapr_cv (Coefficient of variation of vapor pressure)" = "vapr_cv",
                                           "vapr_max (Maximum monthly vapor pressure)" = "vapr_max",
                                           "vapr_min (Minimum monthly vapor pressure)" = "vapr_min",
                                           "vapr_range (Range of vapor pressure: max - min)" = "vapr_range"
                                         ),
                                         selected = c("vapr_mean", "vapr_sd", "vapr_cv", "vapr_max", "vapr_min", "vapr_range"),
                                         inline = FALSE),

                      # Fixed alignment for vapor pressure action links
                      div(style = "display: flex; justify-content: flex-end; gap: 15px; margin-top: 10px;",
                          actionLink("select_all_vapr", "Select All",
                                     icon = icon("check-square"),
                                     style = "color: #5bc0de;"),
                          actionLink("clear_all_vapr", "Clear All",
                                     icon = icon("square-o"),
                                     style = "color: #d9534f;")
                      )
                  )
                )
            ),

            fluidRow(
              column(6, actionButton("select_all_vars", "Select All",
                                     class = "btn-xs btn-info", width = "100%")),
              column(6, actionButton("clear_all_vars", "Clear All",
                                     class = "btn-xs btn-warning", width = "100%"))
            ),

            br(),
            actionButton("load_env", "Load Selected Environmental Data",
                         class = "btn-primary", icon = icon("cloud-download-alt")),
            tags$head(
              tags$style(HTML("
          .btn-primary:hover {
            background-color: white !important;
            color: black !important;
            border-color: #ccc !important;
          }
          "))),
          ),

          box(
            title = "Data Preview", width = 6, status = "info", solidHeader = TRUE,
            withSpinner(DT::dataTableOutput("data_preview"), type = 6),
            hr(),
            verbatimTextOutput("data_summary"),
            plotOutput("points_plot", height = "300px")
          )
        )
      ),

      # Tab 2: Species Distribution
      tabItem(
        tabName = "distribution",
        fluidRow(
          box(
            title = "Species Distribution Map", width = 12, status = "success", solidHeader = TRUE,
            withSpinner(leafletOutput("distribution_map", height = "600px"), type = 6)
          )
        ),
        fluidRow(
          box(
            title = "Distribution Statistics", width = 6, status = "info",
            verbatimTextOutput("dist_stats")
          ),
          box(
            title = "Map Controls", width = 6, status = "warning",
            sliderInput("point_size", "Point Size", min = 1, max = 10, value = 3),
            selectInput("basemap", "Base Map",
                        choices = c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"),
                        selected = "OpenStreetMap")
          )
        )
      ),

      # Tab 3: Environmental Data
      tabItem(
        tabName = "environment",
        fluidRow(
          box(
            title = "Environmental Variables", width = 12, status = "primary", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Climate Variables",
                       withSpinner(plotOutput("climate_plot", height = "500px"), type = 6),
                       selectInput("climate_var", "Select Climate Variable",
                                   choices = paste0("bio", sprintf("%02d", 1:19)),
                                   selected = "bio01")
              ),
              tabPanel("Soil Variables",
                       withSpinner(plotOutput("soil_plot", height = "500px"), type = 6),
                       selectInput("soil_var", "Select Soil Variable",
                                   choices = c(
                                     # Categorical variables
                                     "WRB4_CODE (WRB 4th level)" = "WRB4_CODE",
                                     "WRB2_CODE (WRB 2nd level)" = "WRB2_CODE",
                                     "FAO90_CODE (FAO-90 classification)" = "FAO90_CODE",
                                     "ROOT_DEPTH (Root depth class)" = "ROOT_DEPTH",
                                     "DRAINAGE (Drainage class)" = "DRAINAGE",
                                     # Continuous variables
                                     "AWC (Available water capacity)" = "AWC",
                                     "ROOTS (Root depth cm)" = "ROOTS",
                                     "TOPDEP (Top depth cm)" = "TOPDEP",
                                     "BOTDEP (Bottom depth cm)" = "BOTDEP",
                                     "COARSE (Coarse fragments %)" = "COARSE",
                                     "SAND (Sand %)" = "SAND",
                                     "SILT (Silt %)" = "SILT",
                                     "CLAY (Clay %)" = "CLAY",
                                     "BULK (Bulk density g/cm³)" = "BULK",
                                     "ORG_CARBON (Organic carbon %)" = "ORG_CARBON",
                                     "PH_WATER (pH in water)" = "PH_WATER",
                                     "TOTAL_N (Total nitrogen %)" = "TOTAL_N",
                                     "CN_RATIO (C:N ratio)" = "CN_RATIO",
                                     "CEC_SOIL (CEC cmol+/kg)" = "CEC_SOIL"
                                   ),
                                   selected = "FAO90_CODE")
              ),
              # In the Environmental Data tab, add a new tabPanel
              tabPanel("Soil Variables Info",
                       box(
                         title = "Loaded Soil Variables", width = 12, status = "info",
                         withSpinner(DT::dataTableOutput("soil_vars_table"), type = 6)
                       ),
                       box(
                         title = "Soil Variable Distributions", width = 12, status = "warning", solidHeader = TRUE,
                         selectInput("soil_dist_var", "Select Soil Variable for Distribution Plot",
                                     choices = NULL),
                         plotOutput("soil_dist_plot", height = "400px")
                       ),
                       box(
                         title = "Categorical Variable Mappings", width = 12, status = "warning",
                         uiOutput("soil_categorical_info")
                       ),
                       box(
                         title = "Soil Variables Summary", width = 12, status = "info", solidHeader = TRUE,
                         verbatimTextOutput("soil_summary_stats")
                       )
              ),
              tabPanel("Elevation",
                       withSpinner(plotOutput("elevation_plot", height = "500px"), type = 6)
              ),
              # CHANGE: Added three new tabPanels for the additional climate variables
              tabPanel("Solar Radiation",
                       withSpinner(plotOutput("srad_plot", height = "500px"), type = 6),
                       selectInput("srad_var", "Select Solar Radiation Variable",
                                   choices = c("srad_mean", "srad_sd", "srad_cv", "srad_max", "srad_min", "srad_range"),
                                   selected = "srad_mean")
              ),
              tabPanel("Wind Speed",
                       withSpinner(plotOutput("wind_plot", height = "500px"), type = 6),
                       selectInput("wind_var", "Select Wind Speed Variable",
                                   choices = c("wind_mean", "wind_sd", "wind_cv", "wind_max", "wind_min", "wind_range"),
                                   selected = "wind_mean")
              ),
              tabPanel("Vapor Pressure",
                       withSpinner(plotOutput("vapr_plot", height = "500px"), type = 6),
                       selectInput("vapr_var", "Select Vapor Pressure Variable",
                                   choices = c("vapr_mean", "vapr_sd", "vapr_cv", "vapr_max", "vapr_min", "vapr_range"),
                                   selected = "vapr_mean")
              )
            )
          )
        )
      ),

      # Tab 4: Model Training
      tabItem(
        tabName = "model",
        fluidRow(
          box(
            title = "Model Configuration", width = 4, status = "warning", solidHeader = TRUE,
            selectInput("model_type", "Select Model Algorithm",
                        choices = c("Random Forest", "Decision Tree", "MaxEnt",
                                    "Support Vector Machine (SVM)"),
                        selected = "Random Forest"),
            numericInput("ntree", "Number of Trees (RF only)", value = 500, min = 100, max = 2000),
            sliderInput("train_split", "Training Data Percentage", min = 60, max = 90, value = 70),
            actionButton("train_model", "Train Model", class = "btn-success", icon = icon("play"))
          ),

          box(
            title = "Variable Importance", width = 8, status = "info", solidHeader = TRUE,
            withSpinner(plotOutput("var_importance", height = "400px"), type = 6)
          )
        ),

        fluidRow(
          box(
            title = "Model Performance", width = 6, status = "success", solidHeader = TRUE,
            withSpinner(plotOutput("roc_curve", height = "300px"), type = 6),
            verbatimTextOutput("model_metrics")
          ),

          box(
            title = "Confusion Matrix", width = 6, status = "primary", solidHeader = TRUE,
            withSpinner(plotOutput("conf_matrix_plot", height = "300px"), type = 6),
            hr(),
            h4("Confusion Matrix Values"),
            verbatimTextOutput("conf_matrix_text"),
            hr(),
            h4("Model Summary"),
            verbatimTextOutput("model_summary")
          )
        )
      ),

      # Tab 5: Predictions
      tabItem(
        tabName = "predictions",
        fluidRow(
          box(
            title = "Habitat Suitability Map", width = 8, status = "success", solidHeader = TRUE,
            withSpinner(plotOutput("suitability_map", height = "500px"), type = 6)
          ),

          box(
            title = "Map Controls", width = 4, status = "warning", solidHeader = TRUE,
            selectInput("pred_type", "Map Type",
                        choices = c("Probability", "Binary Presence/Absence"),
                        selected = "Probability"),
            conditionalPanel(
              condition = "input.pred_type == 'Binary Presence/Absence'",
              sliderInput("threshold", "Presence Threshold",
                          min = 0.1, max = 0.9, value = 0.5, step = 0.05)
            ),
            # NEW: Add checkbox for land restriction
            #sliderInput("threshold", "Presence Threshold", min = 0.1, max = 0.9, value = 0.5, step = 0.05),
            #actionButton("update_pred", "Update Map", class = "btn-primary"),
            hr(),
            #actionButton("train_map", "Run Map Prediction", class = "btn-success", icon = icon("play"), style = "width: 150px;"),
            helpText("Downloads the map shown on the left based on selected Map Type"),
            #hr(),
            #br(),
            #downloadButton("download_map", "Download Map"),
            # Download buttons
            helpText(tags$span("PNG: Map image", style = "color: #3498db; font-weight: bold;")),
            downloadButton("download_map_png", "Download Map (PNG)",
                           class = "btn-primary",
                           style = "width: 200px"),
            #br(),
            #hr(),
            helpText(tags$span("TIF: Raster data for GIS", style = "color: #cc0000; font-weight: bold;")),
            downloadButton("download_map_tif", "Download Map (TIF)", class = "btn-secondary", style = "width: 200px;"),
            #helpText("PNG: Map image | TIF: Raster data for GIS"),
            tags$head(tags$style(HTML("
              .btn-primary {
                background-color: #337ab7;
                border-color: #2e6da4;
                color: white;
              }
              .btn-secondary {
                background-color: #cc0000;
                border-color: #b30000;
                color: white;
              }
            ")))
          )
        ),

        fluidRow(
          box(
            title = "Binary Presence/Absence Map with Optimal Threshold", width = 12, status = "info", solidHeader = TRUE,
            withSpinner(plotOutput("binary_map", height = "400px"), type = 6)
          )
        ),

        # CHANGE: Add area calculation box
        fluidRow(
          box(
            title = "Habitat Area Calculations", align = "center", width = 12, status = "primary", solidHeader = TRUE,
            h4("Area Statistics Based on Suitability Map", align = "center"),
            fluidRow(
              column(12, align = "center",
                     conditionalPanel(
                       condition = "input.pred_type == 'Probability'",
                       h5("Probability Map Analysis (Continuous)", align = "center"),
                       div(style = "text-align: center;",
                           tags$style(HTML("
                     #area_stats_probability {
                       margin-left: auto;
                       margin-right: auto;
                     }
                     #area_stats_probability table {
                       margin-left: auto;
                       margin-right: auto;
                     }
                   ")), tableOutput("area_stats_probability")),
                       #tableOutput("area_stats_probability"),
                       helpText(tags$span("Analysis based on probability thresholds (0-1 scale)",
                                          style = "color: #b30000; font-weight: bold;"),
                                align = "center")
                     ),
                     conditionalPanel(
                       condition = "input.pred_type == 'Binary Presence/Absence'",
                       h5("Binary Map Analysis (Presence/Absence)", align = "center"),
                       div(style = "text-align: center;",
                           tags$style(HTML("
                     #area_stats_binary {
                       margin-left: auto;
                       margin-right: auto;
                     }
                     #area_stats_binary table {
                       margin-left: auto;
                       margin-right: auto;
                     }
                   ")), tableOutput("area_stats_binary")),
                       #tableOutput("area_stats_binary"),
                       helpText(tags$span("Analysis based on binary classification with selected threshold",
                                          style = "color: #b30000; font-weight: bold;"),
                                align = "center")
                     )
              )
            ),

            hr(),
            h4("Detailed Area Calculations"),
            fluidRow(
              column(6,
                     h5("Environmental Suitability Areas"),
                     tableOutput("environmental_area_table"),
                     helpText("Areas suitable based on environmental conditions only")
              ),
              column(6,
                     h5("Species Occurrence Areas based on Optimal Threshold"),
                     tableOutput("species_occurrence_table"),
                     helpText("Areas based on actual species presence/absence records")
              )
            ),
            hr(),
            fluidRow(
              column(12,
                     h5("Area Calculation Methodology"),
                     p("Area calculations use latitude-corrected pixel areas:"),
                     verbatimTextOutput("area_calculation_details"),
                     helpText("Pixel area corrected for latitude using cosine(latitude) factor")
              )
            )
          )
        )
      ),

      # Tab 6: Future Projections
      tabItem(
        tabName = "future",
        fluidRow(
          box(
            title = "Future Climate Scenarios", width = 4, status = "warning", solidHeader = TRUE,
            selectInput("climate_model", "Climate Model",
                        choices = c("ACCESS-CM2", "MIROC6"),
                        selected = "ACCESS-CM2"),
            selectInput("ssp_scenario", "SSP Scenario",
                        choices = c("SSP126", "SSP245", "SSP370", "SSP585"),
                        selected = "SSP126"),
            selectInput("time_period", "Time Period",
                        choices = c("2021-2040", "2041-2060", "2061-2080", "2081-2100"),
                        selected = "2081-2100"),
            #actionButton("run_future", "Run Future Projection", class = "btn-danger", icon = icon("bolt"))
            #actionButton("run_future", "Run Future Projection", class = "btn-danger", icon = icon("bolt"), style = "width: 100%; margin-bottom: 10px;"),
            actionButton("run_future", "Run Future Projection", class = "btn-danger", icon = icon("bolt"), style = "width: 200px;"),
            tags$head(tags$style(HTML("
              .btn-danger {
                background-color: #e63900;
                border-color: #cc2900;
              }
            "))),
            hr(),
            h4("Map Future Controls"),
            selectInput("pred_type_future", "Map Type",  # Note: using different input ID
                        choices = c("Probability", "Binary Presence/Absence"),
                        selected = "Probability"),
            conditionalPanel(
              condition = "input.pred_type_future == 'Binary Presence/Absence'",
              sliderInput("threshold_future", "Presence Threshold",
                          min = 0.1, max = 0.9, value = 0.5, step = 0.05)
            ),
            helpText("Downloads the map shown on the right based on selected Map Type"),
            hr(),
            h4("Download Future Habiata Suitability Map", align = "center"),
            fluidRow(
              column(6, align = "center",
                     helpText(tags$span("PNG: Map image", style = "color: #3498db; font-weight: bold;")),
                     downloadButton("download_future_map_png", "Download Future Map (PNG)",
                                    class = "btn-primary",
                                    style = "width: 225px;")
              ),
              column(6, align = "center",
                     helpText(tags$span("TIF: Raster data for GIS", style = "color: #cc0000; font-weight: bold;")),
                     downloadButton("download_future_map_tif", "Download Future Map (TIF)",
                                    class = "btn-secondary",
                                    style = "width: 225px;")
              )
            ),
            br(),
            hr(),
            h4("Download Change Analysis Map", align = "center"),
            fluidRow(
              column(6, align = "center",
                     helpText(tags$span("PNG: Change map image", style = "color: #3498db; font-weight: bold;")),
                     downloadButton("download_change_map_png", "Download Change Map (PNG)",
                                    class = "btn-primary",
                                    style = "width: 225px;")
              ),
              column(6, align = "center",
                     helpText(tags$span("TIF: Raster data for GIS", style = "color: #cc0000; font-weight: bold;")),
                     downloadButton("download_change_map_tif", "Download Change Map (TIF)",
                                    class = "btn-secondary",
                                    style = "width: 225px;")
              )
            )
          ),
          column(width = 8,
                 box(title = "Future Habitat Suitability", width = 12, status = "success", solidHeader = TRUE,
                     withSpinner(plotOutput("future_map", height = "400px"), type = 6)
                 ),
                 box(title = "Change Analysis", width = 12, status = "info", solidHeader = TRUE,
                     withSpinner(plotOutput("change_map", height = "400px"), type = 6)
                 )
          )
        ),
        # New row for Future Area Calculations
        fluidRow(
          box(
            title = "Change Statistics", width = 6, status = "primary", solidHeader = TRUE,
            align = "center",
            verbatimTextOutput("change_stats"),
            hr(),
            h4("Area Calculations", align = "center"),
            tableOutput("area_table")
          ),
          box(
            title = "Future Area Calculations & Extent Comparison",
            width = 6,
            status = "primary",
            solidHeader = TRUE,

            # Area Statistics Section
            h4("Area Statistics Based on Future Suitability Map", align = "center"),
            fluidRow(
              column(12, align = "center",
                     conditionalPanel(
                       condition = "input.pred_type_future == 'Probability'",
                       h5("Probability Map Analysis (Continuous)", align = "center"),
                       div(style = "text-align: center;",
                           tags$style(HTML("
              #future_area_stats_probability {
                margin-left: auto;
                margin-right: auto;
              }
              #future_area_stats_probability table {
                margin-left: auto;
                margin-right: auto;
              }
            ")),
                           tableOutput("future_area_stats_probability")),
                       helpText(tags$span("Analysis based on probability thresholds (0-1 scale)",
                                          style = "color: #b30000; font-weight: bold;"),
                                lign = "center")
                     ),
                     conditionalPanel(
                       condition = "input.pred_type_future == 'Binary Presence/Absence'",
                       h5("Binary Map Analysis (Presence/Absence)", align = "center"),
                       div(style = "text-align: center;",
                           tags$style(HTML("
              #future_area_stats_binary {
                margin-left: auto;
                margin-right: auto;
              }
              #future_area_stats_binary table {
                margin-left: auto;
                margin-right: auto;
              }
            ")),
                           tableOutput("future_area_stats_binary")),
                       helpText(tags$span("Analysis based on binary classification with selected threshold",
                                          style = "color: #b30000; font-weight: bold;"),
                                align = "center")
                     )
              )
            ),

            hr(),  # Add a horizontal line to separate sections

            # Extent Comparison Section
            h4("Raster Extent Comparison", align = "center"),
            div(style = "text-align: center;",
                verbatimTextOutput("extent_comparison")
            )
          )
        ),
        # Bottom box - Future Binary Map with Optimal Threshold
        fluidRow(
          box(
            title = "Future Binary Map Suitability (Optimal Threshold Only)", align = "center", width = 12, status = "info", solidHeader = TRUE,
            withSpinner(plotOutput("future_map_binary", height = "500px"), type = 6),
            hr(),
            fluidRow(
              column(6,
                     h5("Optimal Threshold Information"),
                     verbatimTextOutput("future_optimal_threshold_info")
              ),
              column(6,
                     h5("Binary Map Statistics"),
                     tableOutput("future_binary_stats_table"), align = "center"
              )
            )
          )
        )
      )
    ),
    # White container box with rounded corners that extends to include Playmaker image
    tags$div(
      style = "
    margin-top: 30px;
    padding: 20px 20px 10px 20px;
    background-color: #ffffff;
    border: 2px solid rgba(144, 238, 144, 0.7);
    border-radius: 9px;
  ",

      # First line of text
      tags$h6(
        span(
          HTML("This web is developed & modified from many sources focusing on EDA-shiny by:"),
          style = 'color:blue; font-weight: bold;'
        )
      ),

      # Second line of text
      tags$h6(
        span(
          HTML("Ex-room member of Kitashirakawa Kotobuki Haitsu (北しらかわー寿 ハイツ) number:206, 熱帯ーりん 環境ー学 研究室"),
          style = 'color:green; font-weight: bold;'
        )
      ),

      # Container for the Playmaker image with adjusted positioning
      tags$div(
        style = "
      text-align: left;
      margin-top: 10px;
      margin-bottom: 5px;
      position: relative;
    ",
        img(src = "playmaker.png", height = "100px")))
  )
)
