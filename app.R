# Enhanced Exploratory Data Analysis Application

########## Setup and Dependencies ################

# List of packages required for the enhanced EDA app
#required_packages <- c(
# Core packages
# "shiny", "shinydashboard", "shinythemes", "shinyWidgets", 

# Data manipulation and visualization
#"tidyverse", "ggplot2", "plotly", "DT", "data.table",

# Statistical analysis
#"corrplot", "reshape2", "scales", "moments",

# Export functionalities
#"officer", "flextable", "openxlsx"
#)

# Function to install missing packages
#install_missing_packages <- function(packages) {
#new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
#if(length(new_packages)) {
# message("Installing missing packages: ", paste(new_packages, collapse = ", "))
#install.packages(new_packages, dependencies = TRUE)
#  }
#}

# Install missing packages (uncomment this when running for the first time)
#install_missing_packages(required_packages)

# Load all the required packages
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)
library(data.table)
library(corrplot)
library(reshape2)
library(scales)
library(moments)
library(officer)
library(flextable)
library(openxlsx)
library(igraph)
library(GGally)
library(forecast)
library(tidyverse)
library(ISLR)
library(data.table)

# Set some global options
options(shiny.maxRequestSize = 100 * 1024^2)  # Increase file upload size limit to 100MB
options(scipen = 999)  # Avoid scientific notation
options(DT.options = list(pageLength = 10, autoWidth = TRUE))  # Default DT options
theme_set(theme_minimal())  # Set default ggplot theme
options(shiny.port = 3838)
options(shiny.host = "0.0.0.0") 

# Enhanced Exploratory Data Analysis Application
# UI Structure

ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard Header
  dashboardHeader(
    title = "FNK ANALYTICS PILOT",
    # Add theme selector in the header
    tags$li(class = "dropdown",
            tags$style(".skin-blue .main-header .navbar .sidebar-toggle{float:left;}"),
            pickerInput(
              inputId = "theme",
              label = NULL,
              choices = c("Default" = "default",
                          "Cerulean" = "cerulean",
                          "Cosmo" = "cosmo",
                          "Flatly" = "flatly",
                          "Journal" = "journal",
                          "Lumen" = "lumen",
                          "Sandstone" = "sandstone",
                          "Simplex" = "simplex",
                          "Spacelab" = "spacelab",
                          "United" = "united",
                          "Yeti" = "yeti",
                          "Darkly" = "darkly"),
              selected = "default",
              width = "150px",
              options = list(
                style = "btn-primary"
              )
            )
    ),
    # Add About button
    tags$li(class = "dropdown",
            actionButton(
              inputId = "aboutBtn",
              label = "About",
              icon = icon("info-circle"),
              style = "margin-top: 7px; margin-right: 10px;"
            )
    )
  ),
  
  # Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Upload & Overview", tabName = "data_upload", icon = icon("upload")),
      menuItem("Data Preprocessing", tabName = "preprocessing", icon = icon("filter")),
      menuItem("Data Transformation", tabName = "transformation", icon = icon("cogs")),
      menuItem("Analysis", icon = icon("chart-line"), 
               menuSubItem("Univariate Analysis", tabName = "univariate", icon = icon("chart-bar")),
               menuSubItem("Bivariate Analysis", tabName = "bivariate", icon = icon("chart-line")),
               menuSubItem("Multivariate Analysis", tabName = "multivariate", icon = icon("cubes"))
      ),
      menuItem("Time Series Analysis", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Modeling", tabName = "modeling", icon = icon("brain")),
      
      # Add dataset selector (will be populated dynamically)
      conditionalPanel(
        condition = "input.sidebar != 'data_upload'",
        selectInput("dataset_selector", "Select Dataset:", choices = NULL)
      ),
      
      # Add variable selector (will be populated dynamically)
      conditionalPanel(
        condition = "input.sidebar != 'data_upload'",
        pickerInput(
          inputId = "selected_vars",
          label = "Select Variables:",
          choices = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `select-all-text` = "Select All",
            `deselect-all-text` = "Deselect All",
            `none-selected-text` = "No variables selected"
          )
        )
      )
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    # Apply selected theme
    uiOutput("selected_theme"),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 2px 5px 0 rgba(0,0,0,.16), 0 2px 10px 0 rgba(0,0,0,.12);
          border-radius: 5px;
        }
        .box-header {
          border-bottom: 1px solid #f4f4f4;
        }
        .small-box {
          border-radius: 5px;
        }
        .nav-tabs-custom {
          box-shadow: 0 1px 3px rgba(0,0,0,.12), 0 1px 2px rgba(0,0,0,.24);
        }
        .dataTables_wrapper .dataTables_filter input {
          border: 1px solid #ddd;
          border-radius: 4px;
          padding: 3px;
          background-color: #fff;
          margin-left: 3px;
        }
        #file_upload_ui .btn-file {
          color: #fff;
          background-color: #007bff;
          border-color: #007bff;
        }
        #file_upload_ui .progress {
          margin-top: 10px;
        }
        .shiny-output-error {
          color: #a94442;
          background-color: #f2dede;
          border-color: #ebccd1;
          padding: 15px;
          margin-bottom: 20px;
          border: 1px solid transparent;
          border-radius: 4px;
        }
        .model-equation {
          background-color: #f8f9fa;
          padding: 10px;
          border-left: 4px solid #007bff;
          font-family: monospace;
          margin: 10px 0;
        }
      "))
    ),
    
    # Tab content
    tabItems(
      # 1. Data Upload & Overview Tab
      tabItem(
        tabName = "data_upload",
        fluidRow(
          box(
            title = "Upload Dataset", status = "primary", solidHeader = TRUE, width = 12,
            id = "file_upload_ui",
            tabsetPanel(
              tabPanel("File Upload",
                       fluidRow(
                         column(6,
                                fileInput("file_upload", "Choose CSV or Excel File",
                                          accept = c(".csv", ".xlsx", ".xls"),
                                          buttonLabel = "Browse...",
                                          placeholder = "No file selected"
                                ),
                                checkboxInput("header", "First Row as Header", TRUE),
                                selectInput("sep", "Separator (for CSV):",
                                            choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "),
                                            selected = ",")
                         ),
                         column(6,
                                # Sample datasets for demo
                                selectInput("sample_dataset", "Or Choose Sample Dataset:",
                                            choices = c("None", "Iris", "Diamonds (subset)", "Titanic", "mtcars", "Default"),
                                            selected = "None"),
                                actionButton("load_data", "Load Data", icon = icon("database"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745"),
                                textOutput("upload_status")
                         )
                       )
              ),
              tabPanel("Data Preview",
                       fluidRow(
                         box(
                           width = 12,
                           title = "Data Preview", status = "primary", solidHeader = TRUE,
                           DTOutput("data_preview")
                         )
                       )
              ),
              tabPanel("Data Summary",
                       fluidRow(
                         box(
                           width = 12,
                           title = "Data Structure", status = "primary", solidHeader = TRUE,
                           verbatimTextOutput("data_structure")
                         )
                       ),
                       fluidRow(
                         box(
                           width = 12,
                           title = "Data Summary", status = "primary", solidHeader = TRUE,
                           verbatimTextOutput("data_summary")
                         )
                       )
              ),
              tabPanel("Data Dictionary",
                       fluidRow(
                         box(
                           width = 12,
                           title = "Data Dictionary", status = "primary", solidHeader = TRUE,
                           p("Edit the data dictionary to add descriptions and modify column types."),
                           DTOutput("data_dictionary"),
                           actionButton("save_dictionary", "Save Changes", icon = icon("save"), 
                                        style = "color: #fff; background-color: #007bff; border-color: #007bff")
                         )
                       )
              )
            )
          )
        )
      ),
      
      # 2. Data Preprocessing Tab
      tabItem(
        tabName = "preprocessing",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Data Preprocessing Tools",
                   id = "preprocessing_tabs",
                   tabPanel("Filter Data",
                            fluidRow(
                              box(
                                width = 12,
                                title = "Filter Criteria", status = "primary", solidHeader = TRUE,
                                uiOutput("filter_ui"),
                                actionButton("apply_filter", "Apply Filter", icon = icon("filter"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745"),
                                actionButton("reset_filter", "Reset", icon = icon("sync"), 
                                             style = "color: #fff; background-color: #6c757d; border-color: #6c757d")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Filtered Data Preview", status = "primary", solidHeader = TRUE,
                                DTOutput("filtered_data_preview")
                              )
                            )
                   ),
                   tabPanel("Missing Values",
                            fluidRow(
                              box(
                                width = 6,
                                title = "Missing Values Overview", status = "primary", solidHeader = TRUE,
                                plotOutput("missing_values_plot")
                              ),
                              box(
                                width = 6,
                                title = "Handle Missing Values", status = "primary", solidHeader = TRUE,
                                uiOutput("missing_values_ui"),
                                actionButton("apply_missing", "Apply Changes", icon = icon("check"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745")
                              )
                            )
                   ),
                   tabPanel("Feature Engineering",
                            fluidRow(
                              box(
                                width = 12,
                                title = "Create New Features", status = "primary", solidHeader = TRUE,
                                uiOutput("feature_engineering_ui"),
                                actionButton("add_feature", "Add Feature", icon = icon("plus"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745")
                              )
                            )
                   ),
                   tabPanel("Outlier Detection",
                            fluidRow(
                              box(
                                width = 6,
                                title = "Outlier Detection Method", status = "primary", solidHeader = TRUE,
                                selectInput("outlier_method", "Select Detection Method:",
                                            choices = c("Z-Score", "IQR Method", "Percentile")),
                                uiOutput("outlier_params_ui"),
                                actionButton("detect_outliers", "Detect Outliers", icon = icon("search"), 
                                             style = "color: #fff; background-color: #17a2b8; border-color: #17a2b8")
                              ),
                              box(
                                width = 6,
                                title = "Outlier Action", status = "primary", solidHeader = TRUE,
                                selectInput("outlier_action", "Action for Outliers:",
                                            choices = c("Highlight", "Remove", "Replace with NA", "Cap at threshold")),
                                actionButton("apply_outlier_action", "Apply Action", icon = icon("check"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Outlier Visualization", status = "primary", solidHeader = TRUE,
                                plotOutput("outlier_plot")
                              )
                            )
                   )
                 )
          )
        )
      ),
      
      #   3. Data Transformation Tab
      tabItem(
        tabName = "transformation",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Data Transformation & Encoding",
                   id = "transformation_tabs",
                   
                   # LABEL ENCODING TAB
                   tabPanel("Label Encoding",
                            fluidRow(
                              # Configuration Panel
                              column(4,
                                     box(
                                       width = 12,
                                       title = "Label Encoding Configuration", 
                                       status = "primary", 
                                       solidHeader = TRUE,
                                       
                                       h5("Select Variable for Label Encoding", style = "color: #337ab7; font-weight: bold;"),
                                       uiOutput("label_encode_variable_select"),
                                       
                                       conditionalPanel(
                                         condition = "output.show_label_encoding_options",
                                         div(style = "border: 1px solid #e3e3e3; padding: 15px; margin: 10px 0; border-radius: 8px; background-color: #f9f9f9;",
                                             h6("Current Categories:", style = "color: #333; font-weight: bold; margin-bottom: 10px;"),
                                             div(style = "max-height: 120px; overflow-y: auto; background: white; padding: 8px; border: 1px solid #ddd; border-radius: 4px;",
                                                 verbatimTextOutput("label_encode_current_categories", placeholder = TRUE)
                                             ),
                                             br(),
                                             
                                             # Encoding Method Selection
                                             radioButtons("label_encoding_method", 
                                                          "Encoding Method:",
                                                          choices = list(
                                                            "Alphabetical Order (A=1, B=2, ...)" = "alphabetical",
                                                            "Frequency Order (Most frequent=1)" = "frequency",
                                                            "Custom Order" = "custom",
                                                            "Reverse Alphabetical (Z=1, Y=2, ...)" = "reverse_alpha",
                                                            "Reverse Frequency (Least frequent=1)" = "reverse_freq"
                                                          ),
                                                          selected = "alphabetical"),
                                             
                                             # Custom order input (shown only when custom is selected)
                                             conditionalPanel(
                                               condition = "input.label_encoding_method == 'custom'",
                                               div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   h6("Custom Order Instructions:", style = "color: #856404; margin: 0 0 5px 0;"),
                                                   p("Enter categories in desired order (first = 1, second = 2, etc.)", 
                                                     style = "font-size: 12px; color: #856404; margin: 0;")
                                               ),
                                               textAreaInput("custom_label_order", 
                                                             "Categories in Order (one per line):",
                                                             height = "100px",
                                                             placeholder = "Category1\nCategory2\nCategory3")
                                             ),
                                             
                                             br(),
                                             div(style = "text-align: center;",
                                                 actionButton("apply_label_encoding", "Apply Label Encoding", 
                                                              icon = icon("tags"), 
                                                              class = "btn-success",
                                                              style = "padding: 8px 20px; font-weight: bold;")
                                             )
                                         )
                                       ),
                                       
                                       br(),
                                       h5("Preview Encoding", style = "color: #337ab7; font-weight: bold;"),
                                       div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
                                           verbatimTextOutput("label_encoding_preview")
                                       ),
                                       
                                       br(),
                                       checkboxInput("create_new_label_column", "Create New Column (keep original)", TRUE),
                                       conditionalPanel(
                                         condition = "input.create_new_label_column",
                                         textInput("new_label_column_name", "New Column Name:", 
                                                   placeholder = "variable_encoded")
                                       )
                                     )
                              ),
                              
                              # Results Panel
                              column(8,
                                     box(
                                       width = 12,
                                       title = "Encoding Results", 
                                       status = "primary", 
                                       solidHeader = TRUE,
                                       
                                       tabsetPanel(
                                         tabPanel("Encoding Mapping",
                                                  br(),
                                                  DTOutput("label_encoding_mapping_table")
                                         ),
                                         tabPanel("Before vs After",
                                                  br(),
                                                  fluidRow(
                                                    column(6,
                                                           h6("Original Data Sample:", style = "font-weight: bold;"),
                                                           DTOutput("label_before_sample")
                                                    ),
                                                    column(6,
                                                           h6("Encoded Data Sample:", style = "font-weight: bold;"),
                                                           DTOutput("label_after_sample")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Summary Statistics",
                                                  br(),
                                                  verbatimTextOutput("label_encoding_summary")
                                         )
                                       )
                                     )
                              )
                            )
                   ),
                   
                   # CATEGORY COMBINATION TAB
                   tabPanel("Category Combination",
                            fluidRow(
                              # Configuration Panel
                              column(4,
                                     box(
                                       width = 12,
                                       title = "Category Combination Configuration", 
                                       status = "info", 
                                       solidHeader = TRUE,
                                       
                                       h5("Select Variable for Category Combination", style = "color: #337ab7; font-weight: bold;"),
                                       uiOutput("combine_variable_select"),
                                       
                                       conditionalPanel(
                                         condition = "output.show_combination_options",
                                         div(style = "border: 1px solid #e3e3e3; padding: 15px; margin: 10px 0; border-radius: 8px; background-color: #f0f8ff;",
                                             h6("Current Categories & Frequencies:", style = "color: #333; font-weight: bold;"),
                                             div(style = "max-height: 150px; overflow-y: auto; background: white; padding: 8px; border: 1px solid #ddd; border-radius: 4px;",
                                                 DTOutput("combine_categories_frequency")
                                             ),
                                             br(),
                                             
                                             # Combination Methods
                                             radioButtons("combination_method",
                                                          "Combination Method:",
                                                          choices = list(
                                                            "Manual Selection" = "manual",
                                                            "Low Frequency Threshold" = "frequency_threshold",
                                                            "Top N Categories (combine rest)" = "top_n",
                                                            "Similar Categories (text similarity)" = "similarity"
                                                          ),
                                                          selected = "manual"),
                                             
                                             # Manual selection
                                             conditionalPanel(
                                               condition = "input.combination_method == 'manual'",
                                               div(style = "background: #f8f9fa; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   uiOutput("manual_categories_checklist"),
                                                   textInput("manual_combined_name", "New Combined Category Name:",
                                                             placeholder = "Combined_Category")
                                               )
                                             ),
                                             
                                             # Frequency threshold
                                             conditionalPanel(
                                               condition = "input.combination_method == 'frequency_threshold'",
                                               div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   numericInput("frequency_threshold", "Minimum Frequency:",
                                                                value = 10, min = 1, step = 1),
                                                   textInput("threshold_combined_name", "Name for Low-Frequency Categories:",
                                                             value = "Other", placeholder = "Other")
                                               )
                                             ),
                                             
                                             # Top N categories
                                             conditionalPanel(
                                               condition = "input.combination_method == 'top_n'",
                                               div(style = "background: #d1ecf1; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   numericInput("top_n_categories", "Keep Top N Categories:",
                                                                value = 5, min = 2, step = 1),
                                                   textInput("top_n_combined_name", "Name for Remaining Categories:",
                                                             value = "Others", placeholder = "Others")
                                               )
                                             ),
                                             
                                             # Similarity-based
                                             conditionalPanel(
                                               condition = "input.combination_method == 'similarity'",
                                               div(style = "background: #d4edda; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   sliderInput("similarity_threshold", "Similarity Threshold:",
                                                               min = 0.3, max = 1.0, value = 0.8, step = 0.05),
                                                   p("Higher values = more strict similarity", 
                                                     style = "font-size: 11px; color: #666; margin: 5px 0 0 0;")
                                               )
                                             ),
                                             
                                             br(),
                                             div(style = "text-align: center;",
                                                 actionButton("apply_category_combination", "Apply Combination", 
                                                              icon = icon("layer-group"), 
                                                              class = "btn-info",
                                                              style = "padding: 8px 20px; font-weight: bold;")
                                             )
                                         )
                                       ),
                                       
                                       br(),
                                       checkboxInput("create_new_combine_column", "Create New Column (keep original)", TRUE),
                                       conditionalPanel(
                                         condition = "input.create_new_combine_column",
                                         textInput("new_combine_column_name", "New Column Name:", 
                                                   placeholder = "variable_combined")
                                       )
                                     )
                              ),
                              
                              # Results Panel
                              column(8,
                                     box(
                                       width = 12,
                                       title = "Combination Results", 
                                       status = "info", 
                                       solidHeader = TRUE,
                                       
                                       tabsetPanel(
                                         tabPanel("Combination Preview",
                                                  br(),
                                                  DTOutput("combination_preview_table")
                                         ),
                                         tabPanel("Before vs After Comparison",
                                                  br(),
                                                  fluidRow(
                                                    column(6,
                                                           h6("Original Category Frequencies:", style = "font-weight: bold;"),
                                                           plotOutput("original_category_plot", height = "300px")
                                                    ),
                                                    column(6,
                                                           h6("Combined Category Frequencies:", style = "font-weight: bold;"),
                                                           plotOutput("combined_category_plot", height = "300px")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Combination Summary",
                                                  br(),
                                                  verbatimTextOutput("combination_summary_stats")
                                         )
                                       )
                                     )
                              )
                            )
                   ),
                   
                   # NUMERIC TO CATEGORICAL TAB
                   tabPanel("Numeric to Categorical",
                            fluidRow(
                              # Configuration Panel
                              column(4,
                                     box(
                                       width = 12,
                                       title = "Numeric to Categorical Conversion", 
                                       status = "warning", 
                                       solidHeader = TRUE,
                                       
                                       h5("Select Numeric Variable", style = "color: #337ab7; font-weight: bold;"),
                                       uiOutput("numeric_to_cat_variable_select"),
                                       
                                       conditionalPanel(
                                         condition = "output.show_numeric_conversion_options",
                                         div(style = "border: 1px solid #e3e3e3; padding: 15px; margin: 10px 0; border-radius: 8px; background-color: #fffbf0;",
                                             h6("Variable Summary:", style = "color: #333; font-weight: bold;"),
                                             verbatimTextOutput("numeric_variable_summary"),
                                             br(),
                                             
                                             # Conversion Methods
                                             radioButtons("numeric_conversion_method",
                                                          "Conversion Method:",
                                                          choices = list(
                                                            "Equal Width Bins" = "equal_width",
                                                            "Equal Frequency Bins (Quantiles)" = "quantiles",
                                                            "Custom Breakpoints" = "custom_breaks",
                                                            "Standard Deviations" = "std_dev",
                                                            "Natural Breaks (Jenks)" = "jenks"
                                                          ),
                                                          selected = "equal_width"),
                                             
                                             # Equal width bins
                                             conditionalPanel(
                                               condition = "input.numeric_conversion_method == 'equal_width'",
                                               div(style = "background: #f8f9fa; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   numericInput("equal_width_bins", "Number of Bins:",
                                                                value = 5, min = 2, max = 20, step = 1)
                                               )
                                             ),
                                             
                                             # Quantile bins
                                             conditionalPanel(
                                               condition = "input.numeric_conversion_method == 'quantiles'",
                                               div(style = "background: #e7f3ff; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   numericInput("quantile_bins", "Number of Quantile Bins:",
                                                                value = 4, min = 2, max = 10, step = 1),
                                                   checkboxInput("include_quantile_labels", "Use Quartile Labels (Q1, Q2, etc.)", FALSE)
                                               )
                                             ),
                                             
                                             # Custom breakpoints
                                             conditionalPanel(
                                               condition = "input.numeric_conversion_method == 'custom_breaks'",
                                               div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   textAreaInput("custom_breakpoints", 
                                                                 "Breakpoints (comma-separated):",
                                                                 placeholder = "0, 25, 50, 75, 100",
                                                                 height = "80px"),
                                                   p("Note: Values will be sorted automatically", 
                                                     style = "font-size: 11px; color: #856404; margin: 5px 0 0 0;")
                                               )
                                             ),
                                             
                                             # Standard deviations
                                             conditionalPanel(
                                               condition = "input.numeric_conversion_method == 'std_dev'",
                                               div(style = "background: #f0f8e7; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   checkboxInput("include_extreme_std", "Include extreme values (±2σ, ±3σ)", TRUE),
                                                   p("Categories: Very Low, Low, Normal, High, Very High", 
                                                     style = "font-size: 11px; color: #666; margin: 5px 0 0 0;")
                                               )
                                             ),
                                             
                                             # Jenks natural breaks
                                             conditionalPanel(
                                               condition = "input.numeric_conversion_method == 'jenks'",
                                               div(style = "background: #ffe6f0; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   numericInput("jenks_classes", "Number of Classes:",
                                                                value = 5, min = 3, max = 10, step = 1),
                                                   p("Optimizes within-class homogeneity", 
                                                     style = "font-size: 11px; color: #666; margin: 5px 0 0 0;")
                                               )
                                             ),
                                             
                                             br(),
                                             h6("Labeling Options:", style = "color: #333; font-weight: bold;"),
                                             div(style = "background: #f8f9fa; padding: 10px; border-radius: 4px;",
                                                 radioButtons("bin_labels_type",
                                                              "Bin Labels:",
                                                              choices = list(
                                                                "Range (e.g., '0-25')" = "range",
                                                                "Interval (e.g., '[0,25)')" = "interval", 
                                                                "Custom Names" = "custom_names",
                                                                "Ordinal (e.g., 'Low', 'Medium', 'High')" = "ordinal"
                                                              ),
                                                              selected = "range"),
                                                 
                                                 conditionalPanel(
                                                   condition = "input.bin_labels_type == 'custom_names'",
                                                   textAreaInput("custom_bin_names", 
                                                                 "Custom Names (one per line):",
                                                                 placeholder = "Very Low\nLow\nMedium\nHigh\nVery High",
                                                                 height = "100px")
                                                 )
                                             ),
                                             
                                             br(),
                                             div(style = "text-align: center;",
                                                 actionButton("apply_numeric_conversion", "Convert to Categorical", 
                                                              icon = icon("exchange-alt"), 
                                                              class = "btn-warning",
                                                              style = "padding: 8px 20px; font-weight: bold;")
                                             )
                                         )
                                       ),
                                       
                                       br(),
                                       checkboxInput("create_new_numeric_column", "Create New Column (keep original)", TRUE),
                                       conditionalPanel(
                                         condition = "input.create_new_numeric_column",
                                         textInput("new_numeric_column_name", "New Column Name:", 
                                                   placeholder = "variable_categorical")
                                       )
                                     )
                              ),
                              
                              # Results Panel
                              column(8,
                                     box(
                                       width = 12,
                                       title = "Conversion Results", 
                                       status = "warning", 
                                       solidHeader = TRUE,
                                       
                                       tabsetPanel(
                                         tabPanel("Conversion Preview",
                                                  br(),
                                                  fluidRow(
                                                    column(6,
                                                           h6("Breakpoints & Bins:", style = "font-weight: bold;"),
                                                           verbatimTextOutput("numeric_conversion_breaks")
                                                    ),
                                                    column(6,
                                                           h6("Category Frequencies:", style = "font-weight: bold;"),
                                                           DTOutput("numeric_conversion_frequencies")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Distribution Comparison",
                                                  br(),
                                                  fluidRow(
                                                    column(6,
                                                           h6("Original Distribution:", style = "font-weight: bold;"),
                                                           plotOutput("original_numeric_distribution", height = "300px")
                                                    ),
                                                    column(6,
                                                           h6("Categorical Distribution:", style = "font-weight: bold;"),
                                                           plotOutput("categorical_distribution", height = "300px")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Data Sample",
                                                  br(),
                                                  DTOutput("numeric_conversion_sample")
                                         )
                                       )
                                     )
                              )
                            )
                   ),
                   
                   # DATA TYPE CONVERSION TAB
                   tabPanel("Data Type Conversion",
                            fluidRow(
                              # Configuration Panel
                              column(4,
                                     box(
                                       width = 12,
                                       title = "Data Type Conversion", 
                                       status = "success", 
                                       solidHeader = TRUE,
                                       
                                       h5("Variable Selection", style = "color: #337ab7; font-weight: bold;"),
                                       uiOutput("datatype_variable_select"),
                                       
                                       conditionalPanel(
                                         condition = "output.show_datatype_options",
                                         div(style = "border: 1px solid #e3e3e3; padding: 15px; margin: 10px 0; border-radius: 8px; background-color: #f0fff4;",
                                             h6("Current Data Type Information:", style = "color: #333; font-weight: bold;"),
                                             verbatimTextOutput("current_datatype_info"),
                                             br(),
                                             
                                             h6("Convert To:", style = "color: #333; font-weight: bold;"),
                                             radioButtons("target_data_type",
                                                          NULL,
                                                          choices = list(
                                                            "Factor (Categorical)" = "factor",
                                                            "Character (Text)" = "character",
                                                            "Numeric (Continuous)" = "numeric",
                                                            "Integer" = "integer",
                                                            "Logical (TRUE/FALSE)" = "logical",
                                                            "Date" = "date",
                                                            "Date-Time" = "datetime"
                                                          )),
                                             
                                             # Factor-specific options
                                             conditionalPanel(
                                               condition = "input.target_data_type == 'factor'",
                                               div(style = "background: #f8f9fa; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   checkboxInput("factor_ordered", "Ordered Factor", FALSE),
                                                   conditionalPanel(
                                                     condition = "input.factor_ordered",
                                                     textAreaInput("factor_level_order", 
                                                                   "Level Order (one per line):",
                                                                   placeholder = "Low\nMedium\nHigh",
                                                                   height = "80px")
                                                   )
                                               )
                                             ),
                                             
                                             # Numeric conversion options
                                             conditionalPanel(
                                               condition = "input.target_data_type == 'numeric' || input.target_data_type == 'integer'",
                                               div(style = "background: #e7f3ff; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   checkboxInput("remove_non_numeric", "Remove non-numeric characters", TRUE),
                                                   checkboxInput("convert_na_to_zero", "Convert NA to 0", FALSE),
                                                   conditionalPanel(
                                                     condition = "input.remove_non_numeric",
                                                     textInput("decimal_separator", "Decimal Separator:", value = ".", placeholder = ".")
                                                   )
                                               )
                                             ),
                                             
                                             # Date conversion options
                                             conditionalPanel(
                                               condition = "input.target_data_type == 'date' || input.target_data_type == 'datetime'",
                                               div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   selectInput("date_format", "Date Format:",
                                                               choices = list(
                                                                 "YYYY-MM-DD" = "%Y-%m-%d",
                                                                 "MM/DD/YYYY" = "%m/%d/%Y",
                                                                 "DD/MM/YYYY" = "%d/%m/%Y",
                                                                 "YYYY/MM/DD" = "%Y/%m/%d",
                                                                 "DD-MM-YYYY" = "%d-%m-%Y",
                                                                 "Custom Format" = "custom"
                                                               ),
                                                               selected = "%Y-%m-%d"),
                                                   conditionalPanel(
                                                     condition = "input.date_format == 'custom'",
                                                     textInput("custom_date_format", "Custom Format (R strptime format):",
                                                               placeholder = "%d/%b/%Y")
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.target_data_type == 'datetime'",
                                                     textInput("time_format", "Time Format (if applicable):",
                                                               value = "%H:%M:%S", placeholder = "%H:%M:%S")
                                                   )
                                               )
                                             ),
                                             
                                             # Logical conversion options
                                             conditionalPanel(
                                               condition = "input.target_data_type == 'logical'",
                                               div(style = "background: #d4edda; padding: 10px; border-radius: 4px; margin: 10px 0;",
                                                   h6("TRUE Values (comma-separated):", style = "margin-bottom: 5px;"),
                                                   textInput("true_values", NULL, 
                                                             value = "TRUE, True, true, 1, yes, Yes, Y",
                                                             placeholder = "TRUE, 1, yes"),
                                                   h6("FALSE Values (comma-separated):", style = "margin-bottom: 5px;"),
                                                   textInput("false_values", NULL,
                                                             value = "FALSE, False, false, 0, no, No, N",
                                                             placeholder = "FALSE, 0, no")
                                               )
                                             ),
                                             
                                             br(),
                                             div(style = "text-align: center;",
                                                 actionButton("apply_datatype_conversion", "Apply Conversion", 
                                                              icon = icon("sync-alt"), 
                                                              class = "btn-success",
                                                              style = "padding: 8px 20px; font-weight: bold;")
                                             )
                                         )
                                       ),
                                       
                                       br(),
                                       checkboxInput("create_new_datatype_column", "Create New Column (keep original)", TRUE),
                                       conditionalPanel(
                                         condition = "input.create_new_datatype_column",
                                         textInput("new_datatype_column_name", "New Column Name:", 
                                                   placeholder = "variable_converted")
                                       )
                                     )
                              ),
                              
                              # Results Panel
                              column(8,
                                     box(
                                       width = 12,
                                       title = "Conversion Results", 
                                       status = "success", 
                                       solidHeader = TRUE,
                                       
                                       tabsetPanel(
                                         tabPanel("Conversion Summary",
                                                  br(),
                                                  verbatimTextOutput("datatype_conversion_summary")
                                         ),
                                         tabPanel("Before vs After",
                                                  br(),
                                                  fluidRow(
                                                    column(6,
                                                           h6("Original Data:", style = "font-weight: bold;"),
                                                           verbatimTextOutput("original_datatype_sample")
                                                    ),
                                                    column(6,
                                                           h6("Converted Data:", style = "font-weight: bold;"),
                                                           verbatimTextOutput("converted_datatype_sample")
                                                    )
                                                  )
                                         ),
                                         tabPanel("Conversion Issues",
                                                  br(),
                                                  div(id = "conversion_issues",
                                                      DTOutput("datatype_conversion_issues")
                                                  )
                                         )
                                       )
                                     )
                              )
                            )
                   ),
                   
                   # TRANSFORMATION HISTORY TAB
                   tabPanel("Transformation History",
                            fluidRow(
                              column(12,
                                     box(
                                       width = 12,
                                       title = "Applied Transformations History", 
                                       status = "primary", 
                                       solidHeader = TRUE,
                                       
                                       div(style = "margin-bottom: 15px;",
                                           actionButton("clear_transformation_history", "Clear History", 
                                                        icon = icon("trash"), class = "btn-danger btn-sm"),
                                           actionButton("export_transformation_log", "Export Log", 
                                                        icon = icon("download"), class = "btn-info btn-sm"),
                                           actionButton("undo_last_transformation", "Undo Last", 
                                                        icon = icon("undo"), class = "btn-warning btn-sm")
                                       ),
                                       
                                       DTOutput("transformation_history_table")
                                     )
                              )
                            ),
                            
                            fluidRow(
                              column(6,
                                     box(
                                       width = 12,
                                       title = "Current Dataset Overview", 
                                       status = "info", 
                                       solidHeader = TRUE,
                                       DTOutput("current_dataset_overview")
                                     )
                              ),
                              column(6,
                                     box(
                                       width = 12,
                                       title = "Transformation Statistics", 
                                       status = "info", 
                                       solidHeader = TRUE,
                                       plotOutput("transformation_stats_plot", height = "300px")
                                     )
                              )
                            )
                   )
                 )
          )
        )
      ),
      
      # 4. Univariate Analysis Tab
      tabItem(
        tabName = "univariate",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Univariate Analysis",
                   id = "univariate_tabs",
                   tabPanel("Categorical Variables",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Variable Selection", status = "primary", solidHeader = TRUE,
                                uiOutput("categorical_var_selector")
                              ),
                              box(
                                width = 8,
                                title = "Visualization Type", status = "primary", solidHeader = TRUE,
                                selectInput("cat_plot_type", "Select Plot Type:",
                                            choices = c("Bar Chart", "Pie Chart", "Donut Chart", "Treemap")),
                                uiOutput("cat_plot_options")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("cat_plot")
                              ),
                              box(
                                width = 4,
                                title = "Summary Statistics", status = "primary", solidHeader = TRUE,
                                DTOutput("cat_summary")
                              )
                            )
                   ),
                   tabPanel("Numerical Variables",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Variable Selection", status = "primary", solidHeader = TRUE,
                                uiOutput("numerical_var_selector")
                              ),
                              box(
                                width = 8,
                                title = "Visualization Type", status = "primary", solidHeader = TRUE,
                                selectInput("num_plot_type", "Select Plot Type:",
                                            choices = c("Histogram", "Density Plot", "Box Plot", "Violin Plot", "Q-Q Plot")),
                                uiOutput("num_plot_options")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("num_plot")
                              ),
                              box(
                                width = 4,
                                title = "Summary Statistics", status = "primary", solidHeader = TRUE,
                                verbatimTextOutput("num_summary")
                              )
                            )
                   )
                 )
          )
        )
      ),
      
      # 5. Bivariate Analysis Tab
      tabItem(
        tabName = "bivariate",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Bivariate Analysis",
                   id = "bivariate_tabs",
                   tabPanel("Numeric vs Numeric",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Variable Selection", status = "primary", solidHeader = TRUE,
                                uiOutput("num_num_var_selector")
                              ),
                              box(
                                width = 8,
                                title = "Visualization Type", status = "primary", solidHeader = TRUE,
                                selectInput("num_num_plot_type", "Select Plot Type:",
                                            choices = c("Scatter Plot", "Hexbin Plot", "Contour Plot", "Heatmap")),
                                uiOutput("num_num_plot_options")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("num_num_plot")
                              ),
                              box(
                                width = 4,
                                title = "Correlation", status = "primary", solidHeader = TRUE,
                                verbatimTextOutput("num_num_correlation"),
                                uiOutput("num_num_correlation_download_ui")
                              )
                            )
                   ),
                   tabPanel("Numeric vs Categorical",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Variable Selection", status = "primary", solidHeader = TRUE,
                                uiOutput("num_cat_var_selector")
                              ),
                              box(
                                width = 8,
                                title = "Visualization Type", status = "primary", solidHeader = TRUE,
                                selectInput("num_cat_plot_type", "Select Plot Type:",
                                            choices = c("Box Plot", "Violin Plot", "Bar Chart", "Density Plot")),
                                uiOutput("num_cat_plot_options")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("num_cat_plot")
                              ),
                              box(
                                width = 4,
                                title = "Group Statistics", status = "primary", solidHeader = TRUE,
                                DTOutput("num_cat_stats"),
                                uiOutput("num_cat_stats_download_ui")
                              )
                            )
                   ),
                   tabPanel("Categorical vs Categorical",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Variable Selection", status = "primary", solidHeader = TRUE,
                                uiOutput("cat_cat_var_selector")
                              ),
                              box(
                                width = 8,
                                title = "Visualization Type", status = "primary", solidHeader = TRUE,
                                selectInput("cat_cat_plot_type", "Select Plot Type:",
                                            choices = c("Stacked Bar Chart", "Grouped Bar Chart", "Mosaic Plot", "Heatmap")),
                                uiOutput("cat_cat_plot_options")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("cat_cat_plot")
                              ),
                              box(
                                width = 4,
                                title = "Contingency Table", status = "primary", solidHeader = TRUE,
                                DTOutput("cat_cat_table"),
                                uiOutput("cat_cat_table_download_ui")
                              )
                            ),
                            # Add this new row for the chi-square test results
                            fluidRow(
                              box(
                                width = 12,
                                title = "Chi-Square Test of Independence", status = "info", solidHeader = TRUE,
                                verbatimTextOutput("cat_cat_chisq"),
                                uiOutput("cat_cat_chisq_download_ui")
                              )
                            )
                   )
                 )
          )
        )
      ),
      
      # 6. Multivariate Analysis Tab
      tabItem(
        tabName = "multivariate",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Multivariate Analysis",
                   id = "multivariate_tabs",
                   tabPanel("Correlation Matrix",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Options", status = "primary", solidHeader = TRUE,
                                uiOutput("corr_matrix_options")
                              ),
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("corr_matrix_plot")
                              )
                            )
                   ),
                   tabPanel("Scatter Plot Matrix",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Options", status = "primary", solidHeader = TRUE,
                                uiOutput("scatter_matrix_options")
                              ),
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("scatter_matrix_plot")
                              )
                            )
                   ),
                   tabPanel("Dimensionality Reduction",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Method", status = "primary", solidHeader = TRUE,
                                selectInput("dim_red_method", "Select Method:",
                                            choices = c("PCA", "t-SNE")),
                                uiOutput("dim_red_options")
                              ),
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("dim_red_plot")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Component Analysis", status = "primary", solidHeader = TRUE,
                                conditionalPanel(
                                  condition = "input.dim_red_method == 'PCA'",
                                  plotOutput("pca_variance_plot"),
                                  DTOutput("pca_loadings"),
                                  downloadButton("download_pca_loadings_csv", "Download CSV", class = "btn btn-primary btn-sm"),
                                  downloadButton("download_pca_loadings_excel", "Download Excel", class = "btn btn-success btn-sm"),
                                  actionButton("copy_pca_loadings", "Copy to Clipboard", class = "btn btn-info btn-sm")
                                )
                              )
                            )
                   ),
                   tabPanel("Cluster Analysis",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Method", status = "primary", solidHeader = TRUE,
                                selectInput("cluster_method", "Select Method:",
                                            choices = c("K-Means", "Hierarchical")),
                                uiOutput("cluster_options")
                              ),
                              box(
                                width = 8,
                                title = "Visualization", status = "primary", solidHeader = TRUE,
                                plotlyOutput("cluster_plot")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Cluster Summary", status = "primary", solidHeader = TRUE,
                                DTOutput("cluster_summary"),
                                downloadButton("download_cluster_summary_csv", "Download CSV", class = "btn btn-primary btn-sm"),
                                downloadButton("download_cluster_summary_excel", "Download Excel", class = "btn btn-success btn-sm"),
                                actionButton("copy_cluster_summary", "Copy to Clipboard", class = "btn btn-info btn-sm")
                              )
                            )
                   )
                 )
          )
        )
      ),
      
      # 6. Modeling Tab
      tabItem(
        tabName = "modeling",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Statistical Modeling",
                   id = "modeling_tabs",
                   tabPanel("Linear Regression",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Model Configuration", status = "primary", solidHeader = TRUE,
                                h5("Simple Linear Regression"),
                                uiOutput("linear_target_var"),
                                uiOutput("linear_predictor_var"),
                                br(),
                                h5("Multiple Linear Regression"),
                                uiOutput("linear_multiple_predictors"),
                                br(),
                                actionButton("fit_linear_model", "Fit Model", icon = icon("play"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745"),
                                br(), br(),
                                checkboxInput("include_interactions", "Include Interactions", FALSE),
                                checkboxInput("standardize_vars", "Standardize Variables", FALSE)
                              ),
                              box(
                                width = 8,
                                title = "Model Results", status = "primary", solidHeader = TRUE,
                                div(class = "model-equation", 
                                    h5("Model Equation:"),
                                    verbatimTextOutput("linear_equation")
                                ),
                                verbatimTextOutput("linear_model_summary")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 6,
                                title = "Residual Plots", status = "primary", solidHeader = TRUE,
                                plotOutput("linear_residual_plots")
                              ),
                              box(
                                width = 6,
                                title = "Model Diagnostics", status = "primary", solidHeader = TRUE,
                                plotOutput("linear_diagnostic_plots")
                              )
                            )
                   ),
                   tabPanel("Logistic Regression",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Model Configuration", status = "primary", solidHeader = TRUE,
                                h5("Binary Logistic Regression"),
                                uiOutput("logistic_target_var"),
                                # Category combination section for target variable
                                conditionalPanel(
                                  condition = "output.show_logistic_category_combine",
                                  div(style = "border: 1px solid #ddd; padding: 10px; margin: 10px 0; border-radius: 5px;",
                                      h6("Combine Categories", style = "color: #337ab7; font-weight: bold;"),
                                      p("Current categories:", style = "font-size: 12px; color: #666;"),
                                      verbatimTextOutput("logistic_current_categories", placeholder = TRUE),
                                      textInput("logistic_combine_categories", 
                                                "Categories to combine (comma-separated):",
                                                placeholder = "e.g., Category1, Category2"),
                                      textInput("logistic_new_category_name", 
                                                "New combined category name:",
                                                placeholder = "e.g., Combined_Group"),
                                      actionButton("apply_logistic_combine", "Apply Combination", 
                                                   icon = icon("merge"), class = "btn-info btn-sm"),
                                      br(), br(),
                                      actionButton("reset_logistic_categories", "Reset Categories", 
                                                   icon = icon("undo"), class = "btn-warning btn-sm")
                                  )
                                ),
                                uiOutput("logistic_predictor_vars"),
                                br(),
                                actionButton("fit_logistic_model", "Fit Model", icon = icon("play"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745"),
                                br(), br(),
                                numericInput("classification_threshold", "Classification Threshold:", 
                                             value = 0.5, min = 0, max = 1, step = 0.01)
                              ),
                              box(
                                width = 8,
                                title = "Model Results", status = "primary", solidHeader = TRUE,
                                div(class = "model-equation", 
                                    h5("Logistic Model Equation:"),
                                    verbatimTextOutput("logistic_equation")
                                ),
                                verbatimTextOutput("logistic_model_summary")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 6,
                                title = "ROC Curve", status = "primary", solidHeader = TRUE,
                                plotOutput("logistic_roc_curve")
                              ),
                              box(
                                width = 6,
                                title = "Classification Metrics", status = "primary", solidHeader = TRUE,
                                DTOutput("logistic_metrics")
                              )
                            )
                   ),
                   tabPanel("Multinomial Logistic",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Model Configuration", status = "primary", solidHeader = TRUE,
                                h5("Multinomial Logistic Regression"),
                                uiOutput("multinomial_target_var"),
                                # Category combination section for target variable
                                conditionalPanel(
                                  condition = "output.show_multinomial_category_combine",
                                  div(style = "border: 1px solid #ddd; padding: 10px; margin: 10px 0; border-radius: 5px;",
                                      h6("Combine Categories", style = "color: #337ab7; font-weight: bold;"),
                                      p("Current categories:", style = "font-size: 12px; color: #666;"),
                                      verbatimTextOutput("multinomial_current_categories", placeholder = TRUE),
                                      textInput("multinomial_combine_categories", 
                                                "Categories to combine (comma-separated):",
                                                placeholder = "e.g., Category1, Category2"),
                                      textInput("multinomial_new_category_name", 
                                                "New combined category name:",
                                                placeholder = "e.g., Combined_Group"),
                                      actionButton("apply_multinomial_combine", "Apply Combination", 
                                                   icon = icon("merge"), class = "btn-info btn-sm"),
                                      br(), br(),
                                      actionButton("reset_multinomial_categories", "Reset Categories", 
                                                   icon = icon("undo"), class = "btn-warning btn-sm")
                                  )
                                ),
                                uiOutput("multinomial_predictor_vars"),
                                br(),
                                actionButton("fit_multinomial_model", "Fit Model", icon = icon("play"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745"),
                                br(), br(),
                                p("Reference Category:"),
                                uiOutput("multinomial_reference_level")
                              ),
                              box(
                                width = 8,
                                title = "Model Results", status = "primary", solidHeader = TRUE,
                                div(class = "model-equation", 
                                    h5("Multinomial Model:"),
                                    verbatimTextOutput("multinomial_equation")
                                ),
                                verbatimTextOutput("multinomial_model_summary")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 6,
                                title = "Confusion Matrix", status = "primary", solidHeader = TRUE,
                                plotOutput("multinomial_confusion_matrix")
                              ),
                              box(
                                width = 6,
                                title = "Classification Report", status = "primary", solidHeader = TRUE,
                                DTOutput("multinomial_classification_report")
                              )
                            )
                   ),
                   tabPanel("Model Comparison",
                            fluidRow(
                              box(
                                width = 12,
                                title = "Model Performance Comparison", status = "primary", solidHeader = TRUE,
                                DTOutput("model_comparison_table")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 6,
                                title = "AIC/BIC Comparison", status = "primary", solidHeader = TRUE,
                                plotOutput("model_ic_comparison")
                              ),
                              box(
                                width = 6,
                                title = "Prediction Accuracy", status = "primary", solidHeader = TRUE,
                                plotOutput("model_accuracy_comparison")
                              )
                            )
                   )
                 )
          )
        )
      ),
      
      # 8. Time Series Analysis Tab (NEW)
      tabItem(
        tabName = "timeseries",
        fluidRow(
          column(12,
                 tabBox(
                   width = 12,
                   title = "Time Series Analysis",
                   id = "timeseries_tabs",
                   tabPanel("Data Setup",
                            fluidRow(
                              box(
                                width = 6,
                                title = "Time Series Configuration", status = "primary", solidHeader = TRUE,
                                h5("Select Date/Time Column:"),
                                uiOutput("ts_date_column_selector"),
                                br(),
                                h5("Select Value Column(s):"),
                                uiOutput("ts_value_columns_selector"),
                                br(),
                                h5("Date Format (if not auto-detected):"),
                                selectInput("ts_date_format", "Date Format:",
                                            choices = c("Auto-detect" = "auto",
                                                        "YYYY-MM-DD" = "%Y-%m-%d",
                                                        "MM/DD/YYYY" = "%m/%d/%Y",
                                                        "DD/MM/YYYY" = "%d/%m/%Y",
                                                        "YYYY-MM-DD HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                                                        "Custom" = "custom"),
                                            selected = "auto"),
                                conditionalPanel(
                                  condition = "input.ts_date_format == 'custom'",
                                  textInput("ts_custom_format", "Custom Format:", placeholder = "e.g., %Y-%m-%d")
                                ),
                                br(),
                                actionButton("create_ts", "Create Time Series", icon = icon("clock"), 
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745")
                              ),
                              box(
                                width = 6,
                                title = "Time Series Summary", status = "primary", solidHeader = TRUE,
                                div(class = "ts-summary",
                                    verbatimTextOutput("ts_summary")
                                ),
                                h5("Data Frequency Detection:"),
                                verbatimTextOutput("ts_frequency"),
                                h5("Missing Values in Time Series:"),
                                verbatimTextOutput("ts_missing_summary")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Time Series Preview", status = "primary", solidHeader = TRUE,
                                plotlyOutput("ts_preview_plot")
                              )
                            )
                   ),
                   tabPanel("Visualization",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Plot Configuration", status = "primary", solidHeader = TRUE,
                                selectInput("ts_plot_type", "Plot Type:",
                                            choices = c("Line Plot" = "line",
                                                        "Area Plot" = "area",
                                                        "Candlestick" = "candlestick",
                                                        "Multiple Series" = "multi")),
                                conditionalPanel(
                                  condition = "input.ts_plot_type == 'multi'",
                                  uiOutput("ts_multi_series_selector")
                                ),
                                br(),
                                h5("Time Range Selection:"),
                                uiOutput("ts_date_range_selector"),
                                br(),
                                checkboxInput("ts_show_trend", "Show Trend Line", FALSE),
                                checkboxInput("ts_show_seasonality", "Highlight Seasonality", FALSE),
                                checkboxInput("ts_log_scale", "Log Scale", FALSE),
                                br(),
                                selectInput("ts_aggregation", "Data Aggregation:",
                                            choices = c("None" = "none",
                                                        "Daily" = "daily",
                                                        "Weekly" = "weekly",
                                                        "Monthly" = "monthly",
                                                        "Quarterly" = "quarterly",
                                                        "Yearly" = "yearly"))
                              ),
                              box(
                                width = 8,
                                title = "Time Series Plot", status = "primary", solidHeader = TRUE,
                                plotlyOutput("ts_main_plot", height = "500px")
                              )
                            )
                   ),
                   tabPanel("Decomposition",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Decomposition Settings", status = "primary", solidHeader = TRUE,
                                selectInput("ts_decomp_method", "Decomposition Method:",
                                            choices = c("STL (Seasonal and Trend)" = "stl",
                                                        "Classical Additive" = "additive",
                                                        "Classical Multiplicative" = "multiplicative")),
                                conditionalPanel(
                                  condition = "input.ts_decomp_method == 'stl'",
                                  numericInput("ts_stl_window", "Seasonal Window:", 
                                               value = 13, min = 3, step = 2)
                                ),
                                br(),
                                uiOutput("ts_decomp_variable_selector"),
                                br(),
                                actionButton("perform_decomposition", "Perform Decomposition", 
                                             icon = icon("chart-line"),
                                             style = "color: #fff; background-color: #007bff; border-color: #007bff")
                              ),
                              box(
                                width = 8,
                                title = "Decomposition Components", status = "primary", solidHeader = TRUE,
                                verbatimTextOutput("ts_decomp_summary")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Decomposition Plot", status = "primary", solidHeader = TRUE,
                                plotOutput("ts_decomposition_plot", height = "600px")
                              )
                            )
                   ),
                   tabPanel("Stationarity Tests",
                            fluidRow(
                              box(
                                width = 6,
                                title = "Stationarity Testing", status = "primary", solidHeader = TRUE,
                                uiOutput("ts_stationarity_variable_selector"),
                                br(),
                                h5("Select Tests to Perform:"),
                                checkboxGroupInput("ts_stationarity_tests", "Tests:",
                                                   choices = c("Augmented Dickey-Fuller (ADF)" = "adf",
                                                               "KPSS Test" = "kpss",
                                                               "Phillips-Perron" = "pp"),
                                                   selected = c("adf", "kpss")),
                                br(),
                                actionButton("perform_stationarity_tests", "Run Tests", 
                                             icon = icon("calculator"),
                                             style = "color: #fff; background-color: #17a2b8; border-color: #17a2b8")
                              ),
                              box(
                                width = 6,
                                title = "Test Results", status = "primary", solidHeader = TRUE,
                                verbatimTextOutput("ts_stationarity_results")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 6,
                                title = "Differencing", status = "primary", solidHeader = TRUE,
                                numericInput("ts_diff_order", "Differencing Order:", 
                                             value = 1, min = 0, max = 3, step = 1),
                                actionButton("apply_differencing", "Apply Differencing", 
                                             icon = icon("exchange-alt"),
                                             style = "color: #fff; background-color: #28a745; border-color: #28a745"),
                                br(), br(),
                                h5("Differenced Series Preview:"),
                                plotOutput("ts_differenced_plot", height = "250px")
                              ),
                              box(
                                width = 6,
                                title = "ACF/PACF Analysis", status = "primary", solidHeader = TRUE,
                                numericInput("ts_acf_lags", "Number of Lags:", 
                                             value = 20, min = 5, max = 100, step = 5),
                                actionButton("plot_acf_pacf", "Generate ACF/PACF", 
                                             icon = icon("wave-square"),
                                             style = "color: #fff; background-color: #6f42c1; border-color: #6f42c1"),
                                br(), br(),
                                plotOutput("ts_acf_pacf_plot", height = "300px")
                              )
                            )
                   ),
                   tabPanel("Forecasting",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Forecasting Setup", status = "primary", solidHeader = TRUE,
                                uiOutput("ts_forecast_variable_selector"),
                                br(),
                                selectInput("ts_forecast_method", "Forecasting Method:",
                                            choices = c("ARIMA (Auto)" = "auto_arima",
                                                        "Exponential Smoothing" = "ets",
                                                        "Linear Trend" = "linear",
                                                        "Seasonal Naive" = "snaive",
                                                        "Moving Average" = "ma")),
                                br(),
                                numericInput("ts_forecast_horizon", "Forecast Horizon (periods):",
                                             value = 12, min = 1, max = 100, step = 1),
                                br(),
                                numericInput("ts_train_split", "Training Data (%):",
                                             value = 80, min = 50, max = 95, step = 5),
                                br(),
                                conditionalPanel(
                                  condition = "input.ts_forecast_method == 'ma'",
                                  numericInput("ts_ma_order", "Moving Average Order:",
                                               value = 3, min = 2, max = 20, step = 1)
                                ),
                                br(),
                                actionButton("generate_forecast", "Generate Forecast", 
                                             icon = icon("crystal-ball"),
                                             style = "color: #fff; background-color: #e83e8c; border-color: #e83e8c")
                              ),
                              box(
                                width = 8,
                                title = "Forecast Results", status = "primary", solidHeader = TRUE,
                                plotlyOutput("ts_forecast_plot", height = "400px"),
                                br(),
                                h5("Forecast Accuracy Metrics:"),
                                DTOutput("ts_forecast_accuracy")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Model Diagnostics", status = "primary", solidHeader = TRUE,
                                plotOutput("ts_forecast_diagnostics", height = "400px")
                              )
                            )
                   ),
                   tabPanel("Anomaly Detection",
                            fluidRow(
                              box(
                                width = 4,
                                title = "Anomaly Detection Setup", status = "primary", solidHeader = TRUE,
                                uiOutput("ts_anomaly_variable_selector"),
                                br(),
                                selectInput("ts_anomaly_method", "Detection Method:",
                                            choices = c("Statistical (Z-Score)" = "zscore",
                                                        "IQR Method" = "iqr",
                                                        "Isolation Forest" = "isolation",
                                                        "Seasonal Decomposition" = "seasonal")),
                                br(),
                                conditionalPanel(
                                  condition = "input.ts_anomaly_method == 'zscore'",
                                  numericInput("ts_anomaly_threshold", "Z-Score Threshold:",
                                               value = 3, min = 1, max = 5, step = 0.5)
                                ),
                                conditionalPanel(
                                  condition = "input.ts_anomaly_method == 'iqr'",
                                  numericInput("ts_iqr_multiplier", "IQR Multiplier:",
                                               value = 1.5, min = 1, max = 3, step = 0.1)
                                ),
                                conditionalPanel(
                                  condition = "input.ts_anomaly_method == 'seasonal'",
                                  numericInput("ts_seasonal_window", "Seasonal Window:",
                                               value = 12, min = 3, max = 50, step = 1)
                                ),
                                br(),
                                actionButton("detect_anomalies", "Detect Anomalies", 
                                             icon = icon("exclamation-triangle"),
                                             style = "color: #fff; background-color: #fd7e14; border-color: #fd7e14")
                              ),
                              box(
                                width = 8,
                                title = "Anomaly Detection Results", status = "primary", solidHeader = TRUE,
                                plotlyOutput("ts_anomaly_plot", height = "400px"),
                                br(),
                                verbatimTextOutput("ts_anomaly_summary")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                title = "Detected Anomalies", status = "primary", solidHeader = TRUE,
                                DTOutput("ts_anomaly_table")
                              )
                            )
                   )
                 )
          )
        )
      )
    )
  )
)



# Theme selector - to be used in server.R
themeSelector <- function(input) {
  renderUI({
    if (input$theme != "default") {
      shinythemes::shinytheme(input$theme)
    }
  })
}



# Enhanced Exploratory Data Analysis Application
# Server Logic

server <- function(input, output, session) {
  
  # Create reactive values to store datasets and their metadata
  rv <- reactiveValues(
    datasets = list(),  # Will store all loaded datasets
    current_data = NULL,  # Currently active dataset
    filtered_data = NULL,  # Filtered version of current dataset
    data_dict = NULL,  # Data dictionary for current dataset
    outliers = NULL,  # Detected outliers
    column_types = NULL  # Column types for current dataset
  )
  
  # Theme selector implementation
  output$selected_theme <- themeSelector(input)
  
  # About modal dialog
  observeEvent(input$aboutBtn, {
    showModal(modalDialog(
      title = "About Enhanced EDA App",
      HTML("<p>This application provides comprehensive exploratory data analysis tools to help you understand your data quickly and thoroughly.</p>
           <p>Created using R Shiny with multiple statistical and visualization packages.</p>
           <p>Version 1.0</p>"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # DATA UPLOAD & VALIDATION SECTION ============================
  
  # Function to validate uploaded file
  validate_file <- function(filepath, filename, filetype) {
    # Check file size (already handled by Shiny's maxRequestSize option)
    
    # Check file extension and format
    if (tolower(filetype) %in% c("csv", "txt")) {
      # Try to read a few rows to validate format
      tryCatch({
        data <- read.csv(filepath, nrows = 5, header = input$header, sep = input$sep)
        if (ncol(data) < 2) {
          return(list(valid = FALSE, message = "File contains fewer than 2 columns. Check separator settings."))
        }
        return(list(valid = TRUE, message = "File validated successfully."))
      }, error = function(e) {
        return(list(valid = FALSE, message = paste("Error reading CSV file:", e$message)))
      })
    } else if (tolower(filetype) %in% c("xlsx", "xls")) {
      # Try to read Excel file
      tryCatch({
        data <- readxl::read_excel(filepath, n_max = 5)
        if (ncol(data) < 2) {
          return(list(valid = FALSE, message = "Excel file contains fewer than 2 columns."))
        }
        return(list(valid = TRUE, message = "File validated successfully."))
      }, error = function(e) {
        return(list(valid = FALSE, message = paste("Error reading Excel file:", e$message)))
      })
    } else {
      return(list(valid = FALSE, message = "Unsupported file format. Please upload CSV or Excel files."))
    }
  }
  
  # Function to load sample datasets
  load_sample_dataset <- function(dataset_name) {
    if (dataset_name == "Iris") {
      data <- iris
    } else if (dataset_name == "Diamonds (subset)") {
      # Load a subset of the diamonds dataset to keep it manageable
      data <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000), ]
    } else if (dataset_name == "mtcars") {
      data <- mtcars
      data$car_name <- rownames(mtcars)  # Add car names as a column
      rownames(data) <- NULL
    } else if (dataset_name == "Default") {
      data <- Default
    }
    return(data)
  }
  
  # Create automatic data dictionary from dataset
  create_data_dictionary <- function(data) {
    data_dict <- data.frame(
      Column = names(data),
      Type = sapply(data, function(x) class(x)[1]),
      Description = "",
      stringsAsFactors = FALSE
    )
    return(data_dict)
  }
  
  # Load data event observer
  observeEvent(input$load_data, {
    # Show loading message
    showNotification("Loading dataset...", type = "message", id = "loading_notif")
    
    dataset_loaded <- FALSE
    dataset_name <- ""
    
    # Check if using sample dataset or uploaded file
    if (input$sample_dataset != "None") {
      # Load sample dataset
      tryCatch({
        data <- load_sample_dataset(input$sample_dataset)
        dataset_name <- input$sample_dataset
        dataset_loaded <- TRUE
      }, error = function(e) {
        showNotification(paste("Error loading sample dataset:", e$message), 
                         type = "error", duration = 10)
      })
    } else if (!is.null(input$file_upload) && input$file_upload$size > 0) {
      # Process uploaded file
      file_ext <- tools::file_ext(input$file_upload$name)
      
      # Validate file
      validation <- validate_file(input$file_upload$datapath, 
                                  input$file_upload$name, 
                                  file_ext)
      
      if (validation$valid) {
        tryCatch({
          if (tolower(file_ext) %in% c("csv", "txt")) {
            data <- read.csv(input$file_upload$datapath, 
                             header = input$header, 
                             sep = input$sep,
                             stringsAsFactors = FALSE)
          } else if (tolower(file_ext) %in% c("xlsx", "xls")) {
            data <- readxl::read_excel(input$file_upload$datapath)
            data <- as.data.frame(data)  # Convert to data.frame for consistency
          }
          
          # If successful, set dataset name and loaded flag
          dataset_name <- input$file_upload$name
          dataset_loaded <- TRUE
        }, error = function(e) {
          showNotification(paste("Error loading file:", e$message), 
                           type = "error", duration = 10)
        })
      } else {
        showNotification(validation$message, type = "error", duration = 10)
      }
    } else {
      showNotification("Please select a file to upload or choose a sample dataset.", 
                       type = "warning", duration = 5)
    }
    
    # If dataset was successfully loaded
    if (dataset_loaded) {
      # Process data types - convert character columns with few unique values to factors
      for (col in names(data)) {
        if (is.character(data[[col]]) && length(unique(data[[col]])) < min(20, nrow(data) * 0.2)) {
          data[[col]] <- as.factor(data[[col]])
        }
      }
      
      # Create dataset ID
      dataset_id <- paste0("dataset_", length(rv$datasets) + 1)
      
      # Add to datasets list
      rv$datasets[[dataset_id]] <- list(
        name = dataset_name,
        data = data,
        filtered_data = data,
        data_dict = create_data_dictionary(data)
      )
      
      # Set as current dataset
      rv$current_data <- data
      rv$filtered_data <- data
      rv$data_dict <- create_data_dictionary(data)
      rv$column_types <- sapply(data, class)
      
      # Update dataset selector choices
      updateSelectInput(session, "dataset_selector", 
                        choices = sapply(rv$datasets, function(ds) ds$name),
                        selected = dataset_name)
      
      # Update variable selector
      updatePickerInput(session, "selected_vars",
                        choices = names(data),
                        selected = names(data)[1:min(5, ncol(data))])
      
      # Success message
      showNotification(paste("Dataset", dataset_name, "loaded successfully!"), 
                       type = "message", duration = 5)
      
      # Hide loading notification
      removeNotification("loading_notif")
    } else {
      removeNotification("loading_notif")
    }
  })
  
  # Dataset selector observer
  observeEvent(input$dataset_selector, {
    # Find the selected dataset
    selected_ds <- NULL
    for (ds in rv$datasets) {
      if (ds$name == input$dataset_selector) {
        selected_ds <- ds
        break
      }
    }
    
    # Update the current data if dataset was found
    if (!is.null(selected_ds)) {
      rv$current_data <- selected_ds$data
      rv$filtered_data <- selected_ds$filtered_data
      rv$data_dict <- selected_ds$data_dict
      rv$column_types <- sapply(selected_ds$data, class)
      
      # Update variable selector
      updatePickerInput(session, "selected_vars",
                        choices = names(rv$current_data),
                        selected = names(rv$current_data)[1:min(5, ncol(rv$current_data))])
    }
  })
  
  # DATA PREVIEW OUTPUTS =========================================
  
  # Data preview table
  output$data_preview <- renderDT({
    req(rv$current_data)
    datatable(rv$current_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              filter = 'top',
              class = 'cell-border stripe')
  })
  
  # Data structure output
  output$data_structure <- renderPrint({
    req(rv$current_data)
    str(rv$current_data)
  })
  
  # Data summary output
  output$data_summary <- renderPrint({
    req(rv$current_data)
    summary(rv$current_data)
  })
  
  # Data dictionary table (editable)
  output$data_dictionary <- renderDT({
    req(rv$data_dict)
    datatable(rv$data_dict,
              editable = list(target = 'cell', disable = list(columns = c(0, 1))),
              options = list(
                pageLength = nrow(rv$data_dict),
                dom = 't',
                ordering = FALSE
              ))
  })
  
  # Update data dictionary when edited
  observeEvent(input$data_dictionary_cell_edit, {
    info <- input$data_dictionary_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    
    # Only allow editing the Description column (column 3)
    if (j == 3) {
      rv$data_dict[i, j] <- v
    }
  })
  
  # Save data dictionary changes
  observeEvent(input$save_dictionary, {
    showNotification("Data dictionary saved successfully!", type = "message", duration = 3)
  })
  
  # Upload status text
  output$upload_status <- renderText({
    if (length(rv$datasets) > 0) {
      paste("Status: Dataset loaded with", nrow(rv$current_data), "rows and", 
            ncol(rv$current_data), "columns")
    } else {
      "Status: No dataset loaded"
    }
  })
  
  # DATA PREPROCESSING SECTION ============================
  
  # 1. FILTER DATA ============================
  
  # Generate dynamic filter UI based on selected variables
  output$filter_ui <- renderUI({
    req(rv$current_data, input$selected_vars)
    
    # Get selected variables or use all if none selected
    vars <- if (length(input$selected_vars) > 0) input$selected_vars else names(rv$current_data)
    
    # Create filter UI elements for each variable
    filter_elements <- lapply(vars, function(var) {
      var_class <- class(rv$current_data[[var]])[1]
      
      # Different UI based on variable type
      if (var_class %in% c("factor", "character")) {
        # For categorical variables
        unique_vals <- sort(unique(as.character(rv$current_data[[var]])))
        pickerInput(
          inputId = paste0("filter_", var),
          label = paste("Filter", var),
          choices = unique_vals,
          selected = unique_vals,
          multiple = TRUE,
          options = list(
            `actions-box`   = TRUE,   # Use backticks here
            `live-search`   = TRUE     # Use backticks here
          )
        )
      } else if (var_class %in% c("numeric", "integer", "double")) {
        # For numerical variables
        min_val <- min(rv$current_data[[var]], na.rm = TRUE)
        max_val <- max(rv$current_data[[var]], na.rm = TRUE)
        
        fluidRow(
          column(12, HTML(paste("<strong>Filter", var, "</strong>"))),
          column(6, numericInput(
            inputId = paste0("filter_min_", var),
            label = "Min",
            value = min_val,
            min = min_val,
            max = max_val
          )),
          column(6, numericInput(
            inputId = paste0("filter_max_", var),
            label = "Max",
            value = max_val,
            min = min_val,
            max = max_val
          ))
        )
      } else if (var_class %in% c("Date", "POSIXct")) {
        # For date variables
        min_date <- min(rv$current_data[[var]], na.rm = TRUE)
        max_date <- max(rv$current_data[[var]], na.rm = TRUE)
        
        fluidRow(
          column(12, HTML(paste("<strong>Filter", var, "</strong>"))),
          column(6, dateInput(
            inputId = paste0("filter_min_date_", var),
            label = "From",
            value = min_date
          )),
          column(6, dateInput(
            inputId = paste0("filter_max_date_", var),
            label = "To",
            value = max_date
          ))
        )
      } else {
        # For other variable types
        NULL
      }
    })
    
    # Return filter UI elements
    tagList(filter_elements)
  })
  
  # Apply filter button observer
  observeEvent(input$apply_filter, {
    req(rv$current_data)
    
    # Get selected variables or use all if none selected
    vars <- if (length(input$selected_vars) > 0) input$selected_vars else names(rv$current_data)
    
    # Start with a copy of the current data
    filtered_data <- rv$current_data
    
    # Apply filters for each variable
    for (var in vars) {
      var_class <- class(rv$current_data[[var]])[1]
      
      # Different filtering logic based on variable type
      if (var_class %in% c("factor", "character")) {
        # For categorical variables
        filter_input_id <- paste0("filter_", var)
        if (!is.null(input[[filter_input_id]])) {
          selected_values <- input[[filter_input_id]]
          filtered_data <- filtered_data[filtered_data[[var]] %in% selected_values, ]
        }
      } else if (var_class %in% c("numeric", "integer", "double")) {
        # For numerical variables
        min_input_id <- paste0("filter_min_", var)
        max_input_id <- paste0("filter_max_", var)
        
        if (!is.null(input[[min_input_id]]) && !is.null(input[[max_input_id]])) {
          min_val <- input[[min_input_id]]
          max_val <- input[[max_input_id]]
          filtered_data <- filtered_data[filtered_data[[var]] >= min_val & filtered_data[[var]] <= max_val, ]
        }
      } else if (var_class %in% c("Date", "POSIXct")) {
        # For date variables
        min_date_id <- paste0("filter_min_date_", var)
        max_date_id <- paste0("filter_max_date_", var)
        
        if (!is.null(input[[min_date_id]]) && !is.null(input[[max_date_id]])) {
          min_date <- input[[min_date_id]]
          max_date <- input[[max_date_id]]
          filtered_data <- filtered_data[filtered_data[[var]] >= min_date & filtered_data[[var]] <= max_date, ]
        }
      }
    }
    
    # Update filtered data
    rv$filtered_data <- filtered_data
    
    # Show notification
    showNotification(paste("Filter applied. Result:", nrow(filtered_data), "rows."), 
                     type = "message", duration = 3)
  })
  
  # Reset filter button observer
  observeEvent(input$reset_filter, {
    # Reset filtered data to original data
    rv$filtered_data <- rv$current_data
    
    # Show notification
    showNotification("Filters reset to original data.", type = "message", duration = 3)
  })
  
  # Filtered data preview
  output$filtered_data_preview <- renderDT({
    req(rv$filtered_data)
    datatable(rv$filtered_data,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              filter = 'top',
              class = 'cell-border stripe')
  })
  
  # 2. MISSING VALUES ============================
  
  # Generate UI for missing values
  output$missing_values_ui <- renderUI({
    req(rv$current_data, input$selected_vars)
    
    # Get selected variables or use all if none selected
    vars <- if (length(input$selected_vars) > 0) input$selected_vars else names(rv$current_data)
    
    # Calculate missing values for each variable
    missing_vars <- sapply(rv$current_data[vars], function(x) sum(is.na(x)))
    missing_vars <- missing_vars[missing_vars > 0]
    
    if (length(missing_vars) == 0) {
      return(HTML("<div class='alert alert-success'>No missing values found in the selected variables.</div>"))
    }
    
    # Create UI elements for variables with missing values
    missing_elements <- lapply(names(missing_vars), function(var) {
      var_class <- class(rv$current_data[[var]])[1]
      
      # Different UI based on variable type
      if (var_class %in% c("numeric", "integer", "double")) {
        # For numerical variables
        fluidRow(
          column(12, HTML(paste("<strong>", var, ":</strong>", missing_vars[var], "missing values"))),
          column(12, selectInput(
            inputId = paste0("missing_action_", var),
            label = "Action:",
            choices = c("Remove rows" = "remove", 
                        "Replace with mean" = "mean", 
                        "Replace with median" = "median",
                        "Replace with custom value" = "custom")
          )),
          column(12, conditionalPanel(
            condition = paste0("input.missing_action_", var, " == 'custom'"),
            numericInput(paste0("missing_custom_", var), "Custom value:", 0)
          ))
        )
      } else if (var_class %in% c("factor", "character")) {
        # For categorical variables
        levels <- c("NA" = "na_level", levels(rv$current_data[[var]]))
        if (var_class == "character") {
          unique_vals <- unique(rv$current_data[[var]])
          unique_vals <- unique_vals[!is.na(unique_vals)]
          levels <- c("NA" = "na_level", unique_vals)
        }
        
        fluidRow(
          column(12, HTML(paste("<strong>", var, ":</strong>", missing_vars[var], "missing values"))),
          column(12, selectInput(
            inputId = paste0("missing_action_", var),
            label = "Action:",
            choices = c("Remove rows" = "remove", 
                        "Replace with mode" = "mode", 
                        "Replace with new level" = "na_level",
                        "Replace with existing level" = "custom")
          )),
          column(12, conditionalPanel(
            condition = paste0("input.missing_action_", var, " == 'custom'"),
            selectInput(paste0("missing_custom_", var), "Select level:", choices = levels)
          ))
        )
      } else {
        # For other variable types
        fluidRow(
          column(12, HTML(paste("<strong>", var, ":</strong>", missing_vars[var], "missing values"))),
          column(12, selectInput(
            inputId = paste0("missing_action_", var),
            label = "Action:",
            choices = c("Remove rows" = "remove", 
                        "Remove column" = "remove_col")
          ))
        )
      }
    })
    
    # Return UI elements for missing values
    tagList(missing_elements)
  })
  
  # Function to get the mode of a vector
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Apply missing values handling
  observeEvent(input$apply_missing, {
    req(rv$current_data)
    
    # Get selected variables or use all if none selected
    vars <- if (length(input$selected_vars) > 0) input$selected_vars else names(rv$current_data)
    
    # Start with a copy of the current data
    processed_data <- rv$current_data
    rows_removed <- 0
    cols_removed <- c()
    
    # Process missing values for each variable
    for (var in vars) {
      if (sum(is.na(processed_data[[var]])) > 0) {
        action_id <- paste0("missing_action_", var)
        custom_id <- paste0("missing_custom_", var)
        
        if (!is.null(input[[action_id]])) {
          var_class <- class(processed_data[[var]])[1]
          
          if (input[[action_id]] == "remove") {
            # Remove rows with missing values
            rows_before <- nrow(processed_data)
            processed_data <- processed_data[!is.na(processed_data[[var]]), ]
            rows_removed <- rows_removed + (rows_before - nrow(processed_data))
          } else if (input[[action_id]] == "remove_col") {
            # Remove the column
            processed_data[[var]] <- NULL
            cols_removed <- c(cols_removed, var)
          } else if (input[[action_id]] == "mean" && var_class %in% c("numeric", "integer", "double")) {
            # Replace with mean
            processed_data[[var]][is.na(processed_data[[var]])] <- mean(processed_data[[var]], na.rm = TRUE)
          } else if (input[[action_id]] == "median" && var_class %in% c("numeric", "integer", "double")) {
            # Replace with median
            processed_data[[var]][is.na(processed_data[[var]])] <- median(processed_data[[var]], na.rm = TRUE)
          } else if (input[[action_id]] == "mode" && var_class %in% c("factor", "character")) {
            # Replace with mode
            processed_data[[var]][is.na(processed_data[[var]])] <- get_mode(processed_data[[var]])
          } else if (input[[action_id]] == "na_level" && var_class %in% c("factor", "character")) {
            # Replace with "NA" level
            if (var_class == "factor") {
              levels(processed_data[[var]]) <- c(levels(processed_data[[var]]), "NA")
              processed_data[[var]][is.na(processed_data[[var]])] <- "NA"
            } else {
              processed_data[[var]][is.na(processed_data[[var]])] <- "NA"
            }
          } else if (input[[action_id]] == "custom") {
            # Replace with custom value
            if (!is.null(input[[custom_id]])) {
              if (var_class %in% c("numeric", "integer", "double")) {
                processed_data[[var]][is.na(processed_data[[var]])] <- as.numeric(input[[custom_id]])
              } else if (var_class %in% c("factor", "character")) {
                if (input[[custom_id]] == "na_level") {
                  if (var_class == "factor") {
                    levels(processed_data[[var]]) <- c(levels(processed_data[[var]]), "NA")
                    processed_data[[var]][is.na(processed_data[[var]])] <- "NA"
                  } else {
                    processed_data[[var]][is.na(processed_data[[var]])] <- "NA"
                  }
                } else {
                  processed_data[[var]][is.na(processed_data[[var]])] <- input[[custom_id]]
                }
              }
            }
          }
        }
      }
    }
    
    # Update current data
    rv$current_data <- processed_data
    rv$filtered_data <- processed_data
    
    # Show notification
    message <- paste("Missing values handled.", 
                     if(rows_removed > 0) paste(rows_removed, "rows removed."), 
                     if(length(cols_removed) > 0) paste(length(cols_removed), "columns removed:", paste(cols_removed, collapse = ", ")))
    showNotification(message, type = "message", duration = 5)
  })
  
  # Missing values plot
  output$missing_values_plot <- renderPlot({
    req(rv$current_data)
    
    # Get selected variables or use all if none selected
    vars <- if (length(input$selected_vars) > 0) input$selected_vars else names(rv$current_data)
    
    # Calculate missing values for each variable
    missing_data <- sapply(rv$current_data[vars], function(x) sum(is.na(x)))
    missing_data <- missing_data[order(-missing_data)]
    
    if (sum(missing_data) == 0) {
      # Create an empty plot with message
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "No missing values found in the selected variables", cex = 1.5)
    } else {
      # Create a bar plot of missing values
      barplot(missing_data, 
              main = "Missing Values by Variable", 
              xlab = "Variables", 
              ylab = "Count", 
              col = "steelblue",
              las = 2,
              cex.names = 0.8)
    }
  })
  
  # 3. FEATURE ENGINEERING ============================
  
  # Feature engineering UI
  output$feature_engineering_ui <- renderUI({
    req(rv$current_data)
    
    tagList(
      fluidRow(
        column(4,
               selectInput("feature_type", "Feature Type:", 
                           choices = c("Mathematical Operation" = "math", 
                                       "Text Operation" = "text",
                                       "Date Operation" = "date",
                                       "Binning" = "bin",
                                       "Encoding" = "encode"))
        ),
        column(8,
               textInput("new_feature_name", "New Feature Name:", "")
        )
      ),
      
      # UI for mathematical operations
      conditionalPanel(
        condition = "input.feature_type == 'math'",
        fluidRow(
          column(4,
                 selectInput("math_var1", "Variable 1:", 
                             choices = names(rv$current_data)[sapply(rv$current_data, is.numeric)])
          ),
          column(4,
                 selectInput("math_operation", "Operation:", 
                             choices = c(
                               "Addition" = "+", 
                               "Subtraction" = "-", 
                               "Multiplication" = "*", 
                               "Division" = "/",
                               "Square" = "^2",
                               "Square Root" = "sqrt",
                               "Log" = "log",
                               "Absolute" = "abs"
                             ))
          ),
          column(4,
                 uiOutput("math_var2_ui")
          )
        )
      ),
      
      # UI for text operations
      conditionalPanel(
        condition = "input.feature_type == 'text'",
        fluidRow(
          column(6,
                 selectInput("text_var1", "Text Variable 1:", 
                             choices = names(rv$current_data)[sapply(rv$current_data, function(x) is.character(x) || is.factor(x))])
          ),
          column(6,
                 selectInput("text_operation", "Operation:", 
                             choices = c(
                               "Concatenate" = "concat", 
                               "Length" = "length", 
                               "To Uppercase" = "upper", 
                               "To Lowercase" = "lower",
                               "Extract First N Chars" = "substr",
                               "Count Pattern" = "count"
                             ))
          )
        ),
        fluidRow(
          column(6,
                 uiOutput("text_var2_ui")
          ),
          column(6,
                 uiOutput("text_param_ui")
          )
        )
      ),
      
      # UI for date operations
      conditionalPanel(
        condition = "input.feature_type == 'date'",
        fluidRow(
          column(6,
                 selectInput("date_var", "Date Variable:", 
                             choices = names(rv$current_data))
          ),
          column(6,
                 selectInput("date_operation", "Extract:", 
                             choices = c(
                               "Year" = "year", 
                               "Month" = "month", 
                               "Day" = "day", 
                               "Weekday" = "weekday",
                               "Quarter" = "quarter",
                               "Week of Year" = "week",
                               "Is Weekend" = "is_weekend",
                               "Age (Years)" = "age"
                             ))
          )
        )
      ),
      
      # UI for binning
      conditionalPanel(
        condition = "input.feature_type == 'bin'",
        fluidRow(
          column(6,
                 selectInput("bin_var", "Variable to Bin:", 
                             choices = names(rv$current_data)[sapply(rv$current_data, is.numeric)])
          ),
          column(6,
                 selectInput("bin_method", "Binning Method:", 
                             choices = c(
                               "Equal Width" = "equal_width", 
                               "Equal Frequency" = "equal_freq", 
                               "Custom Breaks" = "custom"
                             ))
          )
        ),
        fluidRow(
          column(6,
                 numericInput("bin_count", "Number of Bins:", 5, min = 2, max = 100)
          ),
          column(6,
                 conditionalPanel(
                   condition = "input.bin_method == 'custom'",
                   textInput("custom_breaks", "Custom Breaks (comma-separated):", "")
                 )
          )
        )
      ),
      
      # UI for encoding
      conditionalPanel(
        condition = "input.feature_type == 'encode'",
        fluidRow(
          column(6,
                 selectInput("encode_var", "Variable to Encode:", 
                             choices = names(rv$current_data)[sapply(rv$current_data, function(x) is.character(x) || is.factor(x))])
          ),
          column(6,
                 selectInput("encode_method", "Encoding Method:", 
                             choices = c(
                               "One-Hot Encoding" = "one_hot", 
                               "Label Encoding" = "label", 
                               "Frequency Encoding" = "frequency"
                             ))
          )
        )
      )
    )
  })
  
  # Dynamic UI for math operation variable 2
  output$math_var2_ui <- renderUI({
    req(input$math_operation, rv$current_data)
    
    # For unary operations, no second variable is needed
    if (input$math_operation %in% c("^2", "sqrt", "log", "abs")) {
      return(NULL)
    } else {
      selectInput("math_var2", "Variable 2:", 
                  choices = names(rv$current_data)[sapply(rv$current_data, is.numeric)])
    }
  })
  
  # Dynamic UI for text operation variable 2
  output$text_var2_ui <- renderUI({
    req(input$text_operation, rv$current_data)
    
    # Only concat operation needs a second variable
    if (input$text_operation == "concat") {
      selectInput("text_var2", "Text Variable 2:", 
                  choices = names(rv$current_data)[sapply(rv$current_data, function(x) is.character(x) || is.factor(x))])
    } else {
      return(NULL)
    }
  })
  
  # Dynamic UI for text operation parameters
  output$text_param_ui <- renderUI({
    req(input$text_operation, rv$current_data)
    
    if (input$text_operation == "substr") {
      numericInput("text_n_chars", "Number of Characters:", 3, min = 1)
    } else if (input$text_operation == "count") {
      textInput("text_pattern", "Pattern to Count:", "")
    } else {
      return(NULL)
    }
  })
  
  # Add feature button observer
  observeEvent(input$add_feature, {
    req(rv$current_data, input$new_feature_name, input$feature_type)
    
    # Validate feature name
    if (input$new_feature_name == "") {
      showNotification("Please enter a name for the new feature.", type = "error", duration = 5)
      return()
    }
    
    if (input$new_feature_name %in% names(rv$current_data)) {
      showNotification("Feature name already exists. Please choose a different name.", type = "error", duration = 5)
      return()
    }
    
    # Create a copy of the current data
    data <- rv$current_data
    
    # Create the new feature based on the selected operation
    if (input$feature_type == "math") {
      req(input$math_var1, input$math_operation)
      
      if (input$math_operation == "+") {
        req(input$math_var2)
        data[[input$new_feature_name]] <- data[[input$math_var1]] + data[[input$math_var2]]
      } else if (input$math_operation == "-") {
        req(input$math_var2)
        data[[input$new_feature_name]] <- data[[input$math_var1]] - data[[input$math_var2]]
      } else if (input$math_operation == "*") {
        req(input$math_var2)
        data[[input$new_feature_name]] <- data[[input$math_var1]] * data[[input$math_var2]]
      } else if (input$math_operation == "/") {
        req(input$math_var2)
        data[[input$new_feature_name]] <- data[[input$math_var1]] / data[[input$math_var2]]
      } else if (input$math_operation == "^2") {
        data[[input$new_feature_name]] <- data[[input$math_var1]]^2
      } else if (input$math_operation == "sqrt") {
        data[[input$new_feature_name]] <- sqrt(data[[input$math_var1]])
      } else if (input$math_operation == "log") {
        data[[input$new_feature_name]] <- log(data[[input$math_var1]])
      } else if (input$math_operation == "abs") {
        data[[input$new_feature_name]] <- abs(data[[input$math_var1]])
      }
    } else if (input$feature_type == "text") {
      req(input$text_var1, input$text_operation)
      
      if (input$text_operation == "concat") {
        req(input$text_var2)
        data[[input$new_feature_name]] <- paste(data[[input$text_var1]], data[[input$text_var2]], sep = "")
      } else if (input$text_operation == "length") {
        data[[input$new_feature_name]] <- nchar(as.character(data[[input$text_var1]]));
      } else if (input$text_operation == "upper") {
        data[[input$new_feature_name]] <- toupper(as.character(data[[input$text_var1]]));
      } else if (input$text_operation == "lower") {
        data[[input$new_feature_name]] <- tolower(as.character(data[[input$text_var1]]));
      } else if (input$text_operation == "substr") {
        req(input$text_n_chars);
        data[[input$new_feature_name]] <- substr(as.character(data[[input$text_var1]]), 1, input$text_n_chars);
      } else if (input$text_operation == "count") {
        req(input$text_pattern);
        data[[input$new_feature_name]] <- sapply(as.character(data[[input$text_var1]]), function(x) {
          length(gregexpr(input$text_pattern, x)[[1]]);
        });
      }
    } else if (input$feature_type == "date") {
      req(input$date_var, input$date_operation);
      
      # Try to convert to date if not already a date
      if (!inherits(data[[input$date_var]], c("Date", "POSIXct", "POSIXlt"))) {
        tryCatch({
          temp_date <- as.Date(data[[input$date_var]]);
          if (all(is.na(temp_date))) {
            showNotification("Failed to convert column to date format.", type = "error", duration = 5);
            return();
          }
          data[[input$date_var]] <- temp_date;
        }, error = function(e) {
          showNotification("Failed to convert column to date format.", type = "error", duration = 5);
          return();
        });
      }
      
      if (input$date_operation == "year") {
        data[[input$new_feature_name]] <- as.numeric(format(data[[input$date_var]], "%Y"));
      } else if (input$date_operation == "month") {
        data[[input$new_feature_name]] <- as.numeric(format(data[[input$date_var]], "%m"));
      } else if (input$date_operation == "day") {
        data[[input$new_feature_name]] <- as.numeric(format(data[[input$date_var]], "%d"));
      } else if (input$date_operation == "weekday") {
        data[[input$new_feature_name]] <- weekdays(data[[input$date_var]]);
      } else if (input$date_operation == "quarter") {
        data[[input$new_feature_name]] <- ceiling(as.numeric(format(data[[input$date_var]], "%m")) / 3);
      } else if (input$date_operation == "week") {
        data[[input$new_feature_name]] <- as.numeric(format(data[[input$date_var]], "%W"));
      } else if (input$date_operation == "is_weekend") {
        data[[input$new_feature_name]] <- weekdays(data[[input$date_var]]) %in% c("Saturday", "Sunday");
      } else if (input$date_operation == "age") {
        today <- Sys.Date();
        data[[input$new_feature_name]] <- as.numeric(difftime(today, data[[input$date_var]], units = "days") / 365.25);
      }
    } else if (input$feature_type == "bin") {
      req(input$bin_var, input$bin_method, input$bin_count);
      
      if (input$bin_method == "equal_width") {
        # Equal-width binning
        breaks <- seq(min(data[[input$bin_var]], na.rm = TRUE), 
                      max(data[[input$bin_var]], na.rm = TRUE), 
                      length.out = input$bin_count + 1);
        data[[input$new_feature_name]] <- cut(data[[input$bin_var]], breaks = breaks, include.lowest = TRUE);
      } else if (input$bin_method == "equal_freq") {
        # Equal-frequency binning (quantiles)
        breaks <- quantile(data[[input$bin_var]], probs = seq(0, 1, length.out = input$bin_count + 1), na.rm = TRUE);
        data[[input$new_feature_name]] <- cut(data[[input$bin_var]], breaks = breaks, include.lowest = TRUE);
      } else if (input$bin_method == "custom") {
        # Custom breaks
        req(input$custom_breaks);
        breaks_str <- gsub(" ", "", input$custom_breaks);
        breaks <- as.numeric(strsplit(breaks_str, ",")[[1]]);
        
        if (length(breaks) < 2) {
          showNotification("Please provide at least 2 break points.", type = "error", duration = 5);
          return();
        }
        
        data[[input$new_feature_name]] <- cut(data[[input$bin_var]], breaks = breaks, include.lowest = TRUE);
      }
    } else if (input$feature_type == "encode") {
      req(input$encode_var, input$encode_method);
      
      if (input$encode_method == "one_hot") {
        # One-hot encoding
        unique_vals <- unique(data[[input$encode_var]]);
        unique_vals <- unique_vals[!is.na(unique_vals)];
        
        for (val in unique_vals) {
          col_name <- paste0(input$new_feature_name, "_", val);
          data[[col_name]] <- ifelse(data[[input$encode_var]] == val, 1, 0);
        }
      } else if (input$encode_method == "label") {
        # Label encoding
        data[[input$new_feature_name]] <- as.numeric(factor(data[[input$encode_var]]));
      } else if (input$encode_method == "frequency") {
        # Frequency encoding
        freq_table <- table(data[[input$encode_var]]);
        data[[input$new_feature_name]] <- freq_table[as.character(data[[input$encode_var]])];
      }
    }
    
    # Update current data with the new feature
    rv$current_data <- data;
    
    # Show success notification
    showNotification(paste("New feature", input$new_feature_name, "added successfully!"), type = "message", duration = 5);
  });
  
  # 4. OUTLIER DETECTION ============================
  
  # Outlier detection parameters UI
  output$outlier_params_ui <- renderUI({
    req(rv$current_data);
    
    if (input$outlier_method == "Z-Score") {
      numericInput("z_threshold", "Z-Score Threshold:", value = 3, min = 1);
    } else if (input$outlier_method == "IQR Method") {
      numericInput("iqr_multiplier", "IQR Multiplier:", value = 1.5, min = 1);
    } else if (input$outlier_method == "Percentile") {
      fluidRow(
        column(6, numericInput("lower_percentile", "Lower Percentile:", value = 1, min = 0, max = 100)),
        column(6, numericInput("upper_percentile", "Upper Percentile:", value = 99, min = 0, max = 100))
      );
    }
  });
  
  # Detect outliers
  observeEvent(input$detect_outliers, {
    req(rv$current_data);
    
    outliers <- NULL;
    if (input$outlier_method == "Z-Score") {
      z_scores <- scale(rv$current_data[sapply(rv$current_data, is.numeric)], center = TRUE, scale = TRUE);
      outliers <- which(abs(z_scores) > input$z_threshold, arr.ind = TRUE);
    } else if (input$outlier_method == "IQR Method") {
      numeric_cols <- rv$current_data[sapply(rv$current_data, is.numeric)];
      outlier_indices <- lapply(numeric_cols, function(col) {
        q1 <- quantile(col, 0.25, na.rm = TRUE);
        q3 <- quantile(col, 0.75, na.rm = TRUE);
        iqr <- q3 - q1;
        which(col < (q1 - input$iqr_multiplier * iqr) | col > (q3 + input$iqr_multiplier * iqr));
      });
      outliers <- unique(unlist(outlier_indices));
    } else if (input$outlier_method == "Percentile") {
      numeric_cols <- rv$current_data[sapply(rv$current_data, is.numeric)];
      outlier_indices <- lapply(numeric_cols, function(col) {
        lower_limit <- quantile(col, input$lower_percentile / 100, na.rm = TRUE);
        upper_limit <- quantile(col, input$upper_percentile / 100, na.rm = TRUE);
        which(col < lower_limit | col > upper_limit);
      });
      outliers <- unique(unlist(outlier_indices));
    }
    
    # Store detected outliers
    rv$outliers <- outliers;
    
    # Show notification
    showNotification(paste("Detected", length(outliers), "outliers."), type = "message", duration = 5);
  });
  
  # Outlier visualization
  output$outlier_plot <- renderPlot({
    req(rv$current_data, rv$outliers);
    
    numeric_cols <- rv$current_data[sapply(rv$current_data, is.numeric)];
    if (ncol(numeric_cols) > 0) {
      boxplot(numeric_cols, main = "Outlier Detection", outline = TRUE, col = ifelse(1:nrow(numeric_cols) %in% rv$outliers, "red", "lightgray"));
    }
  });
  
  # Apply outlier action
  observeEvent(input$apply_outlier_action, {
    req(rv$current_data);
    
    if (length(rv$outliers) > 0) {
      if (input$outlier_action == "Remove") {
        rv$current_data <- rv$current_data[-rv$outliers, ];
        showNotification("Outliers removed.", type = "message", duration = 5);
      } else if (input$outlier_action == "Highlight") {
        showNotification("Outliers highlighted in the plot.", type = "message", duration = 5);
      } else if (input$outlier_action == "Replace with NA") {
        rv$current_data[rv$outliers, ] <- NA;
        showNotification("Outliers replaced with NA.", type = "message", duration = 5);
      } else if (input$outlier_action == "Cap at threshold") {
        # Cap at threshold based on Z-Score or IQR
        # Logic to implement this action will be added here
      }
    } else {
      showNotification("No outliers detected to apply action.", type = "warning", duration = 5);
    }
  });
  
  # 5. REPORTS & DOWNLOADS ============================
  
  # Generate report action
  observeEvent(input$generate_report, {
    req(rv$filtered_data);
    
    report_title <- input$report_title;
    report_author <- input$report_author;
    report_format <- input$report_format;
    report_sections <- input$report_sections;
    
    # Logic to generate the report based on selected sections will be added here
    showNotification("Report generated successfully!", type = "message", duration = 5);
  });
  
  # Download data
  output$download_data_btn <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".", switch(input$download_format, "CSV" = "csv", "Excel" = "xlsx", "RDS" = "rds"), sep = "");
    },
    content = function(file) {
      req(rv$filtered_data);
      if (input$download_format == "CSV") {
        write.csv(rv$filtered_data, file, row.names = FALSE);
      } else if (input$download_format == "Excel") {
        openxlsx::write.xlsx(rv$filtered_data, file);
      } else if (input$download_format == "RDS") {
        saveRDS(rv$filtered_data, file);
      }
    }
  );
  
  # Download charts
  output$download_chart_btn <- downloadHandler(
    filename = function() {
      paste("chart_", Sys.Date(), ".", switch(input$chart_format, "PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "SVG" = "svg"), sep = "");
    },
    content = function(file) {
    }
  );

  
  # DATA TRANSFORMATION SERVER LOGIC ============================
  
  # Initialize transformation history
  rv$transformation_history <- data.frame(
    Timestamp = character(),
    Type = character(),
    Variable = character(),
    Details = character(),
    stringsAsFactors = FALSE
  )
  
  # Helper function to add transformation to history
  add_transformation_history <- function(type, variable, details) {
    new_entry <- data.frame(
      Timestamp = Sys.time(),
      Type = type,
      Variable = variable,
      Details = details,
      stringsAsFactors = FALSE
    )
    rv$transformation_history <- rbind(rv$transformation_history, new_entry)
  }
  
  # LABEL ENCODING SECTION =====================================
  
  # Label encoding variable selector
  output$label_encode_variable_select <- renderUI({
    req(rv$filtered_data)
    
    # Get categorical variables
    cat_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) is.factor(x) || is.character(x))]
    
    if (length(cat_vars) == 0) {
      p("No categorical variables found in the dataset.", style = "color: #999; font-style: italic;")
    } else {
      selectInput("label_encode_variable", "Choose Variable:",
                  choices = c("Select a variable..." = "", cat_vars),
                  selected = "")
    }
  })
  
  # Show label encoding options conditionally
  output$show_label_encoding_options <- reactive({
    !is.null(input$label_encode_variable) && input$label_encode_variable != ""
  })
  outputOptions(output, "show_label_encoding_options", suspendWhenHidden = FALSE)
  
  # Display current categories
  output$label_encode_current_categories <- renderText({
    req(input$label_encode_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$label_encode_variable]]
    categories <- if (is.factor(var_data)) levels(var_data) else sort(unique(var_data))
    frequencies <- table(var_data)
    
    result <- paste(names(frequencies), "(n =", frequencies, ")", collapse = "\n")
    return(result)
  })
  
  # Label encoding preview
  output$label_encoding_preview <- renderText({
    req(input$label_encode_variable, input$label_encoding_method, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$label_encode_variable]]
    categories <- if (is.factor(var_data)) levels(var_data) else sort(unique(var_data))
    
    # Generate encoding based on method
    encoding_map <- switch(input$label_encoding_method,
                           "alphabetical" = {
                             sorted_cats <- sort(categories)
                             setNames(1:length(sorted_cats), sorted_cats)
                           },
                           "frequency" = {
                             freq_order <- names(sort(table(var_data), decreasing = TRUE))
                             setNames(1:length(freq_order), freq_order)
                           },
                           "reverse_alpha" = {
                             sorted_cats <- sort(categories, decreasing = TRUE)
                             setNames(1:length(sorted_cats), sorted_cats)
                           },
                           "reverse_freq" = {
                             freq_order <- names(sort(table(var_data), decreasing = FALSE))
                             setNames(1:length(freq_order), freq_order)
                           },
                           "custom" = {
                             if (!is.null(input$custom_label_order) && input$custom_label_order != "") {
                               custom_order <- trimws(strsplit(input$custom_label_order, "\n")[[1]])
                               custom_order <- custom_order[custom_order != ""]
                               if (length(custom_order) == length(categories) && all(custom_order %in% categories)) {
                                 setNames(1:length(custom_order), custom_order)
                               } else {
                                 return("Error: Custom order must include all categories exactly once.")
                               }
                             } else {
                               return("Enter custom category order...")
                             }
                           }
    )
    
    # Format preview
    preview_text <- paste(names(encoding_map), "→", encoding_map, collapse = "\n")
    return(preview_text)
  })
  
  # Apply label encoding
  observeEvent(input$apply_label_encoding, {
    req(input$label_encode_variable, input$label_encoding_method, rv$filtered_data)
    
    tryCatch({
      var_data <- rv$filtered_data[[input$label_encode_variable]]
      categories <- if (is.factor(var_data)) levels(var_data) else sort(unique(var_data))
      
      # Generate encoding map
      encoding_map <- switch(input$label_encoding_method,
                             "alphabetical" = {
                               sorted_cats <- sort(categories)
                               setNames(1:length(sorted_cats), sorted_cats)
                             },
                             "frequency" = {
                               freq_order <- names(sort(table(var_data), decreasing = TRUE))
                               setNames(1:length(freq_order), freq_order)
                             },
                             "reverse_alpha" = {
                               sorted_cats <- sort(categories, decreasing = TRUE)
                               setNames(1:length(sorted_cats), sorted_cats)
                             },
                             "reverse_freq" = {
                               freq_order <- names(sort(table(var_data), decreasing = FALSE))
                               setNames(1:length(freq_order), freq_order)
                             },
                             "custom" = {
                               custom_order <- trimws(strsplit(input$custom_label_order, "\n")[[1]])
                               custom_order <- custom_order[custom_order != ""]
                               setNames(1:length(custom_order), custom_order)
                             }
      )
      
      # Apply encoding
      encoded_values <- encoding_map[as.character(var_data)]
      
      # Store encoding map for display
      rv$current_encoding_map <- data.frame(
        Category = names(encoding_map),
        Code = as.numeric(encoding_map),
        stringsAsFactors = FALSE
      )
      
      # Add to dataset
      if (input$create_new_label_column) {
        new_col_name <- if (input$new_label_column_name != "") {
          input$new_label_column_name
        } else {
          paste0(input$label_encode_variable, "_encoded")
        }
        rv$filtered_data[[new_col_name]] <- encoded_values
        rv$filtered_data[[new_col_name]] <- encoded_values
      } else {
        rv$filtered_data[[input$label_encode_variable]] <- encoded_values
        rv$filtered_data[[input$label_encode_variable]] <- encoded_values
      }
      
      # Add to history
      add_transformation_history(
        "Label Encoding",
        input$label_encode_variable,
        paste("Method:", input$label_encoding_method, "| Categories:", length(encoding_map))
      )
      
      showNotification("Label encoding applied successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error applying label encoding:", e$message), type = "error", duration = 5)
    })
  })
  
  # Label encoding mapping table
  output$label_encoding_mapping_table <- renderDT({
    req(rv$current_encoding_map)
    
    datatable(rv$current_encoding_map,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE)
  })
  
  # Before/after sample tables for label encoding
  output$label_before_sample <- renderDT({
    req(input$label_encode_variable, rv$filtered_data)
    
    sample_data <- data.frame(
      Original = head(rv$filtered_data[[input$label_encode_variable]], 20)
    )
    
    datatable(sample_data, options = list(pageLength = 10, dom = 't'), rownames = TRUE)
  })
  
  output$label_after_sample <- renderDT({
    req(rv$current_encoding_map, input$label_encode_variable, rv$filtered_data)
    
    var_data <- head(rv$filtered_data[[input$label_encode_variable]], 20)
    encoding_map <- setNames(rv$current_encoding_map$Code, rv$current_encoding_map$Category)
    encoded_values <- encoding_map[as.character(var_data)]
    
    sample_data <- data.frame(
      Encoded = encoded_values
    )
    
    datatable(sample_data, options = list(pageLength = 10, dom = 't'), rownames = TRUE)
  })
  
  # Label encoding summary
  output$label_encoding_summary <- renderPrint({
    req(rv$current_encoding_map)
    
    cat("Encoding Summary:\n")
    cat("================\n")
    cat("Total categories:", nrow(rv$current_encoding_map), "\n")
    cat("Encoding range:", min(rv$current_encoding_map$Code), "to", max(rv$current_encoding_map$Code), "\n\n")
    
    cat("Mapping:\n")
    for (i in 1:nrow(rv$current_encoding_map)) {
      cat(sprintf("%-20s → %d\n", rv$current_encoding_map$Category[i], rv$current_encoding_map$Code[i]))
    }
  })
  
  # CATEGORY COMBINATION SECTION ===============================
  
  # Category combination variable selector
  output$combine_variable_select <- renderUI({
    req(rv$filtered_data)
    
    cat_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) is.factor(x) || is.character(x))]
    
    if (length(cat_vars) == 0) {
      p("No categorical variables found in the dataset.", style = "color: #999; font-style: italic;")
    } else {
      selectInput("combine_variable", "Choose Variable:",
                  choices = c("Select a variable..." = "", cat_vars),
                  selected = "")
    }
  })
  
  # Show combination options conditionally
  output$show_combination_options <- reactive({
    !is.null(input$combine_variable) && input$combine_variable != ""
  })
  outputOptions(output, "show_combination_options", suspendWhenHidden = FALSE)
  
  # Categories frequency table
  output$combine_categories_frequency <- renderDT({
    req(input$combine_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$combine_variable]]
    freq_table <- as.data.frame(table(var_data))
    names(freq_table) <- c("Category", "Frequency")
    freq_table$Percentage <- round((freq_table$Frequency / sum(freq_table$Frequency)) * 100, 2)
    freq_table <- freq_table[order(freq_table$Frequency, decreasing = TRUE), ]
    
    rv$current_category_frequencies <- freq_table
    
    datatable(freq_table,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE)
  })
  
  # Manual categories checklist
  output$manual_categories_checklist <- renderUI({
    req(rv$current_category_frequencies)
    
    categories <- rv$current_category_frequencies$Category
    checkboxGroupInput("manual_selected_categories",
                       "Select Categories to Combine:",
                       choices = setNames(categories, paste0(categories, " (n=", rv$current_category_frequencies$Frequency, ")")),
                       selected = NULL)
  })
  
  # Apply category combination
  observeEvent(input$apply_category_combination, {
    req(input$combine_variable, input$combination_method, rv$filtered_data)
    
    tryCatch({
      var_data <- rv$filtered_data[[input$combine_variable]]
      
      # Determine which categories to combine based on method
      combined_categories <- switch(input$combination_method,
                                    "manual" = {
                                      req(input$manual_selected_categories, input$manual_combined_name)
                                      list(categories = input$manual_selected_categories, 
                                           new_name = input$manual_combined_name)
                                    },
                                    "frequency_threshold" = {
                                      req(input$frequency_threshold, input$threshold_combined_name)
                                      freq_table <- table(var_data)
                                      low_freq_cats <- names(freq_table)[freq_table < input$frequency_threshold]
                                      list(categories = low_freq_cats, 
                                           new_name = input$threshold_combined_name)
                                    },
                                    "top_n" = {
                                      req(input$top_n_categories, input$top_n_combined_name)
                                      freq_table <- sort(table(var_data), decreasing = TRUE)
                                      remaining_cats <- names(freq_table)[(input$top_n_categories + 1):length(freq_table)]
                                      list(categories = remaining_cats, 
                                           new_name = input$top_n_combined_name)
                                    },
                                    "similarity" = {
                                      # Simplified similarity-based combination (would need more sophisticated text similarity in practice)
                                      categories <- unique(as.character(var_data))
                                      # For demo purposes, combine categories with similar starting letters
                                      similar_cats <- categories[grepl("^A", categories, ignore.case = TRUE)]
                                      list(categories = similar_cats, 
                                           new_name = "A_categories")
                                    }
      )
      
      # Apply combination
      new_var_data <- as.character(var_data)
      new_var_data[new_var_data %in% combined_categories$categories] <- combined_categories$new_name
      
      # Store combination info
      rv$current_combination_info <- list(
        original_categories = length(unique(var_data)),
        combined_categories = combined_categories$categories,
        new_name = combined_categories$new_name,
        final_categories = length(unique(new_var_data))
      )
      
      # Add to dataset
      if (input$create_new_combine_column) {
        new_col_name <- if (input$new_combine_column_name != "") {
          input$new_combine_column_name
        } else {
          paste0(input$combine_variable, "_combined")
        }
        rv$filtered_data[[new_col_name]] <- as.factor(new_var_data)
        rv$filtered_data[[new_col_name]] <- as.factor(new_var_data)
      } else {
        rv$filtered_data[[input$combine_variable]] <- as.factor(new_var_data)
        rv$filtered_data[[input$combine_variable]] <- as.factor(new_var_data)
      }
      
      # Add to history
      add_transformation_history(
        "Category Combination",
        input$combine_variable,
        paste("Method:", input$combination_method, "| Combined:", length(combined_categories$categories), "categories")
      )
      
      showNotification("Category combination applied successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error applying category combination:", e$message), type = "error", duration = 5)
    })
  })
  
  # Combination preview table
  output$combination_preview_table <- renderDT({
    req(rv$current_combination_info)
    
    preview_data <- data.frame(
      "Combined Categories" = rv$current_combination_info$combined_categories,
      "New Category Name" = rv$current_combination_info$new_name,
      stringsAsFactors = FALSE
    )
    
    datatable(preview_data,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE)
  })
  
  # Before/after category plots
  output$original_category_plot <- renderPlot({
    req(input$combine_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$combine_variable]]
    freq_data <- as.data.frame(table(var_data))
    names(freq_data) <- c("Category", "Frequency")
    
    ggplot(freq_data, aes(x = reorder(Category, Frequency), y = Frequency)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(title = "Original Categories", x = "Category", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
  })
  
  output$combined_category_plot <- renderPlot({
    req(rv$current_combination_info, input$combine_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$combine_variable]]
    new_var_data <- as.character(var_data)
    new_var_data[new_var_data %in% rv$current_combination_info$combined_categories] <- rv$current_combination_info$new_name
    
    freq_data <- as.data.frame(table(new_var_data))
    names(freq_data) <- c("Category", "Frequency")
    
    ggplot(freq_data, aes(x = reorder(Category, Frequency), y = Frequency)) +
      geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
      coord_flip() +
      labs(title = "After Combination", x = "Category", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
  })
  
  # Combination summary statistics
  output$combination_summary_stats <- renderPrint({
    req(rv$current_combination_info)
    
    cat("Category Combination Summary:\n")
    cat("============================\n")
    cat("Original categories:", rv$current_combination_info$original_categories, "\n")
    cat("Final categories:", rv$current_combination_info$final_categories, "\n")
    cat("Categories combined:", length(rv$current_combination_info$combined_categories), "\n")
    cat("Reduction:", rv$current_combination_info$original_categories - rv$current_combination_info$final_categories, "categories\n\n")
    
    cat("Combined categories:\n")
    for (cat in rv$current_combination_info$combined_categories) {
      cat("  -", cat, "\n")
    }
    cat("Into:", rv$current_combination_info$new_name, "\n")
  })
  
  # NUMERIC TO CATEGORICAL SECTION =============================
  
  # Numeric to categorical variable selector
  output$numeric_to_cat_variable_select <- renderUI({
    req(rv$filtered_data)
    
    numeric_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, is.numeric)]
    
    if (length(numeric_vars) == 0) {
      p("No numeric variables found in the dataset.", style = "color: #999; font-style: italic;")
    } else {
      selectInput("numeric_to_cat_variable", "Choose Numeric Variable:",
                  choices = c("Select a variable..." = "", numeric_vars),
                  selected = "")
    }
  })
  
  # Show numeric conversion options conditionally
  output$show_numeric_conversion_options <- reactive({
    !is.null(input$numeric_to_cat_variable) && input$numeric_to_cat_variable != ""
  })
  outputOptions(output, "show_numeric_conversion_options", suspendWhenHidden = FALSE)
  
  # Numeric variable summary
  output$numeric_variable_summary <- renderPrint({
    req(input$numeric_to_cat_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$numeric_to_cat_variable]]
    var_data <- var_data[!is.na(var_data)]
    
    cat("Variable Summary:\n")
    cat("================\n")
    cat("Count:", length(var_data), "\n")
    cat("Min:", min(var_data), "\n")
    cat("Max:", max(var_data), "\n")
    cat("Mean:", round(mean(var_data), 3), "\n")
    cat("Median:", round(median(var_data), 3), "\n")
    cat("Std Dev:", round(sd(var_data), 3), "\n")
    cat("Missing:", sum(is.na(rv$filtered_data[[input$numeric_to_cat_variable]])), "\n")
  })
  
  # Apply numeric to categorical conversion
  observeEvent(input$apply_numeric_conversion, {
    req(input$numeric_to_cat_variable, input$numeric_conversion_method, rv$filtered_data)
    
    tryCatch({
      var_data <- rv$filtered_data[[input$numeric_to_cat_variable]]
      clean_data <- var_data[!is.na(var_data)]
      
      # Generate breaks based on method
      breaks <- switch(input$numeric_conversion_method,
                       "equal_width" = {
                         seq(min(clean_data), max(clean_data), length.out = input$equal_width_bins + 1)
                       },
                       "quantiles" = {
                         quantile(clean_data, probs = seq(0, 1, length.out = input$quantile_bins + 1))
                       },
                       "custom_breaks" = {
                         custom_breaks <- as.numeric(strsplit(input$custom_breakpoints, ",")[[1]])
                         sort(unique(c(min(clean_data), custom_breaks, max(clean_data))))
                       },
                       "std_dev" = {
                         mean_val <- mean(clean_data)
                         sd_val <- sd(clean_data)
                         if (input$include_extreme_std) {
                           c(min(clean_data), mean_val - 2*sd_val, mean_val - sd_val, 
                             mean_val + sd_val, mean_val + 2*sd_val, max(clean_data))
                         } else {
                           c(min(clean_data), mean_val - sd_val, mean_val + sd_val, max(clean_data))
                         }
                       },
                       "jenks" = {
                         # Simplified Jenks breaks (would need classInt package for true Jenks)
                         quantile(clean_data, probs = seq(0, 1, length.out = input$jenks_classes + 1))
                       }
      )
      
      # Remove duplicates and sort
      breaks <- sort(unique(breaks))
      
      # Create categorical variable
      categorical_var <- cut(var_data, breaks = breaks, include.lowest = TRUE, right = FALSE)
      
      # Generate labels based on type
      if (input$bin_labels_type == "custom_names" && !is.null(input$custom_bin_names) && input$custom_bin_names != "") {
        custom_names <- trimws(strsplit(input$custom_bin_names, "\n")[[1]])
        if (length(custom_names) == (length(breaks) - 1)) {
          levels(categorical_var) <- custom_names
        }
      } else if (input$bin_labels_type == "ordinal") {
        n_bins <- length(breaks) - 1
        if (n_bins <= 3) {
          ordinal_names <- c("Low", "Medium", "High")[1:n_bins]
        } else if (n_bins <= 5) {
          ordinal_names <- c("Very Low", "Low", "Medium", "High", "Very High")[1:n_bins]
        } else {
          ordinal_names <- paste("Level", 1:n_bins)
        }
        levels(categorical_var) <- ordinal_names
      } else if (input$bin_labels_type == "range") {
        range_labels <- paste(round(breaks[-length(breaks)], 2), "-", round(breaks[-1], 2))
        levels(categorical_var) <- range_labels
      }
      
      # Store conversion info
      rv$current_numeric_conversion <- list(
        breaks = breaks,
        method = input$numeric_conversion_method,
        n_categories = length(breaks) - 1,
        categorical_var = categorical_var
      )
      
      # Add to dataset
      if (input$create_new_numeric_column) {
        new_col_name <- if (input$new_numeric_column_name != "") {
          input$new_numeric_column_name
        } else {
          paste0(input$numeric_to_cat_variable, "_categorical")
        }
        rv$filtered_data[[new_col_name]] <- categorical_var
        rv$filtered_data[[new_col_name]] <- categorical_var
      } else {
        rv$filtered_data[[input$numeric_to_cat_variable]] <- categorical_var
        rv$filtered_data[[input$numeric_to_cat_variable]] <- categorical_var
      }
      
      # Add to history
      add_transformation_history(
        "Numeric to Categorical",
        input$numeric_to_cat_variable,
        paste("Method:", input$numeric_conversion_method, "| Bins:", length(breaks) - 1)
      )
      
      showNotification("Numeric to categorical conversion applied successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error in numeric conversion:", e$message), type = "error", duration = 5)
    })
  })
  
  # Numeric conversion breaks display
  output$numeric_conversion_breaks <- renderPrint({
    req(rv$current_numeric_conversion)
    
    cat("Breakpoints:\n")
    cat("============\n")
    for (i in 1:(length(rv$current_numeric_conversion$breaks) - 1)) {
      cat(sprintf("Bin %d: [%.3f, %.3f)\n", i, 
                  rv$current_numeric_conversion$breaks[i], 
                  rv$current_numeric_conversion$breaks[i + 1]))
    }
  })
  
  # Numeric conversion frequencies
  output$numeric_conversion_frequencies <- renderDT({
    req(rv$current_numeric_conversion)
    
    freq_table <- as.data.frame(table(rv$current_numeric_conversion$categorical_var))
    names(freq_table) <- c("Category", "Frequency")
    freq_table$Percentage <- round((freq_table$Frequency / sum(freq_table$Frequency)) * 100, 2)
    
    datatable(freq_table,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE)
  })
  
  # Original numeric distribution plot
  output$original_numeric_distribution <- renderPlot({
    req(input$numeric_to_cat_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$numeric_to_cat_variable]]
    
    ggplot(data.frame(x = var_data), aes(x = x)) +
      geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
      labs(title = "Original Distribution", x = input$numeric_to_cat_variable, y = "Frequency") +
      theme_minimal()
  })
  
  # Categorical distribution plot
  output$categorical_distribution <- renderPlot({
    req(rv$current_numeric_conversion)
    
    cat_data <- rv$current_numeric_conversion$categorical_var
    freq_data <- as.data.frame(table(cat_data))
    names(freq_data) <- c("Category", "Frequency")
    
    ggplot(freq_data, aes(x = Category, y = Frequency)) +
      geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
      labs(title = "Categorical Distribution", x = "Category", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Numeric conversion sample
  output$numeric_conversion_sample <- renderDT({
    req(rv$current_numeric_conversion, input$numeric_to_cat_variable, rv$filtered_data)
    
    sample_data <- data.frame(
      Original = head(rv$filtered_data[[input$numeric_to_cat_variable]], 20),
      Categorical = head(rv$current_numeric_conversion$categorical_var, 20)
    )
    
    datatable(sample_data,
              options = list(pageLength = 15, dom = 't'),
              rownames = TRUE)
  })
  
  # DATA TYPE CONVERSION SECTION ===============================
  
  # Data type variable selector
  output$datatype_variable_select <- renderUI({
    req(rv$filtered_data)
    
    all_vars <- names(rv$filtered_data)
    
    if (length(all_vars) == 0) {
      p("No variables found in the dataset.", style = "color: #999; font-style: italic;")
    } else {
      selectInput("datatype_variable", "Choose Variable:",
                  choices = c("Select a variable..." = "", all_vars),
                  selected = "")
    }
  })
  
  # Show datatype options conditionally
  output$show_datatype_options <- reactive({
    !is.null(input$datatype_variable) && input$datatype_variable != ""
  })
  outputOptions(output, "show_datatype_options", suspendWhenHidden = FALSE)
  
  # Current data type information
  output$current_datatype_info <- renderPrint({
    req(input$datatype_variable, rv$filtered_data)
    
    var_data <- rv$filtered_data[[input$datatype_variable]]
    
    cat("Current Data Type:", class(var_data)[1], "\n")
    cat("Length:", length(var_data), "\n")
    cat("Missing Values:", sum(is.na(var_data)), "\n")
    
    if (is.factor(var_data)) {
      cat("Factor Levels:", nlevels(var_data), "\n")
      cat("Ordered:", is.ordered(var_data), "\n")
    } else if (is.numeric(var_data)) {
      cat("Range:", min(var_data, na.rm = TRUE), "to", max(var_data, na.rm = TRUE), "\n")
    } else if (is.character(var_data)) {
      cat("Unique Values:", length(unique(var_data)), "\n")
    }
    
    cat("\nSample Values:\n")
    print(head(var_data, 10))
  })
  
  # Apply data type conversion
  observeEvent(input$apply_datatype_conversion, {
    req(input$datatype_variable, input$target_data_type, rv$filtered_data)
    
    tryCatch({
      var_data <- rv$filtered_data[[input$datatype_variable]]
      converted_data <- var_data
      conversion_issues <- character(0)
      
      # Apply conversion based on target type
      converted_data <- switch(input$target_data_type,
                               "factor" = {
                                 if (input$factor_ordered && !is.null(input$factor_level_order) && input$factor_level_order != "") {
                                   level_order <- trimws(strsplit(input$factor_level_order, "\n")[[1]])
                                   factor(var_data, levels = level_order, ordered = TRUE)
                                 } else {
                                   as.factor(var_data)
                                 }
                               },
                               "character" = {
                                 as.character(var_data)
                               },
                               "numeric" = {
                                 if (input$remove_non_numeric) {
                                   # Remove non-numeric characters
                                   clean_data <- gsub("[^0-9.-]", "", as.character(var_data))
                                   if (input$decimal_separator != ".") {
                                     clean_data <- gsub(input$decimal_separator, ".", clean_data)
                                   }
                                   numeric_data <- suppressWarnings(as.numeric(clean_data))
                                   if (input$convert_na_to_zero) {
                                     numeric_data[is.na(numeric_data)] <- 0
                                   }
                                   conversion_issues <<- which(is.na(numeric_data) & !is.na(var_data))
                                   numeric_data
                                 } else {
                                   suppressWarnings(as.numeric(var_data))
                                 }
                               },
                               "integer" = {
                                 if (input$remove_non_numeric) {
                                   clean_data <- gsub("[^0-9.-]", "", as.character(var_data))
                                   integer_data <- suppressWarnings(as.integer(as.numeric(clean_data)))
                                   if (input$convert_na_to_zero) {
                                     integer_data[is.na(integer_data)] <- 0L
                                   }
                                   conversion_issues <<- which(is.na(integer_data) & !is.na(var_data))
                                   integer_data
                                 } else {
                                   suppressWarnings(as.integer(var_data))
                                 }
                               },
                               "logical" = {
                                 true_vals <- trimws(strsplit(input$true_values, ",")[[1]])
                                 false_vals <- trimws(strsplit(input$false_values, ",")[[1]])
                                 
                                 logical_data <- rep(NA, length(var_data))
                                 logical_data[as.character(var_data) %in% true_vals] <- TRUE
                                 logical_data[as.character(var_data) %in% false_vals] <- FALSE
                                 
                                 conversion_issues <<- which(is.na(logical_data) & !is.na(var_data))
                                 logical_data
                               },
                               "date" = {
                                 date_format <- if (input$date_format == "custom") input$custom_date_format else input$date_format
                                 date_data <- suppressWarnings(as.Date(as.character(var_data), format = date_format))
                                 conversion_issues <<- which(is.na(date_data) & !is.na(var_data))
                                 date_data
                               },
                               "datetime" = {
                                 date_format <- if (input$date_format == "custom") input$custom_date_format else input$date_format
                                 datetime_format <- paste(date_format, input$time_format)
                                 datetime_data <- suppressWarnings(as.POSIXct(as.character(var_data), format = datetime_format))
                                 conversion_issues <<- which(is.na(datetime_data) & !is.na(var_data))
                                 datetime_data
                               }
      )
      
      # Store conversion info
      rv$current_datatype_conversion <- list(
        original_type = class(var_data)[1],
        target_type = input$target_data_type,
        conversion_issues = conversion_issues,
        converted_data = converted_data,
        original_data = var_data
      )
      
      # Add to dataset
      if (input$create_new_datatype_column) {
        new_col_name <- if (input$new_datatype_column_name != "") {
          input$new_datatype_column_name
        } else {
          paste0(input$datatype_variable, "_converted")
        }
        rv$filtered_data[[new_col_name]] <- converted_data
        rv$filtered_data[[new_col_name]] <- converted_data
      } else {
        rv$filtered_data[[input$datatype_variable]] <- converted_data
        rv$filtered_data[[input$datatype_variable]] <- converted_data
      }
      
      # Add to history
      add_transformation_history(
        "Data Type Conversion",
        input$datatype_variable,
        paste("From:", class(var_data)[1], "to", input$target_data_type, "| Issues:", length(conversion_issues))
      )
      
      showNotification("Data type conversion applied successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error in data type conversion:", e$message), type = "error", duration = 5)
    })
  })
  
  # Data type conversion summary
  output$datatype_conversion_summary <- renderPrint({
    req(rv$current_datatype_conversion)
    
    cat("Data Type Conversion Summary:\n")
    cat("=============================\n")
    cat("Original Type:", rv$current_datatype_conversion$original_type, "\n")
    cat("Target Type:", rv$current_datatype_conversion$target_type, "\n")
    cat("Total Records:", length(rv$current_datatype_conversion$original_data), "\n")
    cat("Successful Conversions:", sum(!is.na(rv$current_datatype_conversion$converted_data)), "\n")
    cat("Conversion Issues:", length(rv$current_datatype_conversion$conversion_issues), "\n")
    
    if (length(rv$current_datatype_conversion$conversion_issues) > 0) {
      cat("\nFirst few problematic values:\n")
      issues_sample <- head(rv$current_datatype_conversion$conversion_issues, 5)
      for (idx in issues_sample) {
        cat("Row", idx, ":", as.character(rv$current_datatype_conversion$original_data[idx]), "\n")
      }
    }
  })
  
  # Original vs converted data type samples
  output$original_datatype_sample <- renderPrint({
    req(rv$current_datatype_conversion)
    
    cat("Original Data (first 10 values):\n")
    cat("================================\n")
    print(head(rv$current_datatype_conversion$original_data, 10))
    cat("\nClass:", class(rv$current_datatype_conversion$original_data)[1])
  })
  
  output$converted_datatype_sample <- renderPrint({
    req(rv$current_datatype_conversion)
    
    cat("Converted Data (first 10 values):\n")
    cat("=================================\n")
    print(head(rv$current_datatype_conversion$converted_data, 10))
    cat("\nClass:", class(rv$current_datatype_conversion$converted_data)[1])
  })
  
  # Data type conversion issues
  output$datatype_conversion_issues <- renderDT({
    req(rv$current_datatype_conversion)
    
    if (length(rv$current_datatype_conversion$conversion_issues) > 0) {
      issues_data <- data.frame(
        Row = rv$current_datatype_conversion$conversion_issues,
        "Original Value" = as.character(rv$current_datatype_conversion$original_data[rv$current_datatype_conversion$conversion_issues]),
        "Issue" = "Could not convert to target type",
        stringsAsFactors = FALSE
      )
      
      datatable(issues_data,
                options = list(pageLength = 15, dom = 'tp'),
                rownames = FALSE)
    } else {
      data.frame("Message" = "No conversion issues found!")
    }
  })
  
  # TRANSFORMATION HISTORY SECTION =============================
  
  # Transformation history table
  output$transformation_history_table <- renderDT({
    req(rv$transformation_history)
    
    if (nrow(rv$transformation_history) > 0) {
      history_display <- rv$transformation_history
      history_display$Timestamp <- format(history_display$Timestamp, "%Y-%m-%d %H:%M:%S")
      
      datatable(history_display,
                options = list(pageLength = 15, dom = 'tp', order = list(list(0, 'desc'))),
                rownames = FALSE)
    } else {
      data.frame("Message" = "No transformations applied yet.")
    }
  })
  
  # Clear transformation history
  observeEvent(input$clear_transformation_history, {
    rv$transformation_history <- data.frame(
      Timestamp = character(),
      Type = character(),
      Variable = character(),
      Details = character(),
      stringsAsFactors = FALSE
    )
    showNotification("Transformation history cleared.", type = "message", duration = 2)
  })
  
  # Export transformation log
  observeEvent(input$export_transformation_log, {
    req(rv$transformation_history)
    
    if (nrow(rv$transformation_history) > 0) {
      write.csv(rv$transformation_history, "transformation_log.csv", row.names = FALSE)
      showNotification("Transformation log exported as 'transformation_log.csv'", type = "message", duration = 3)
    } else {
      showNotification("No transformations to export.", type = "warning", duration = 2)
    }
  })
  
  # Current dataset overview
  output$current_dataset_overview <- renderDT({
    req(rv$filtered_data)
    
    overview_data <- data.frame(
      Variable = names(rv$filtered_data),
      Type = sapply(rv$filtered_data, function(x) class(x)[1]),
      "Missing %" = round(sapply(rv$filtered_data, function(x) sum(is.na(x))/length(x) * 100), 2),
      "Unique Values" = sapply(rv$filtered_data, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    datatable(overview_data,
              options = list(pageLength = 15, dom = 't'),
              rownames = FALSE)
  })
  
  # Transformation statistics plot
  output$transformation_stats_plot <- renderPlot({
    req(rv$transformation_history)
    
    if (nrow(rv$transformation_history) > 0) {
      type_counts <- as.data.frame(table(rv$transformation_history$Type))
      names(type_counts) <- c("Transformation", "Count")
      
      ggplot(type_counts, aes(x = reorder(Transformation, Count), y = Count)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        coord_flip() +
        labs(title = "Transformation Types Applied", x = "Transformation Type", y = "Count") +
        theme_minimal() +
        theme(axis.text.y = element_text(size = 10))
    } else {
      # Empty plot with message
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No transformations applied yet", size = 6) +
        theme_void()
    }
  })

# UNIVARIATE ANALYSIS - SERVER SIDE IMPLEMENTATION ====================
  
  # Configuration for large dataset handling
  MAX_PLOT_POINTS <- 100000  # Maximum points to plot (sample if exceeded)
  MAX_CATEGORIES <- 50       # Maximum categories to display
  CHUNK_SIZE <- 10000        # Chunk size for processing
  
  # Enhanced function to check if variable is categorical with performance optimization
  is_categorical_var <- function(data, var = NULL, max_sample = 10000) {
    if(is.null(var) || missing(var)) {
      return(FALSE)
    }
    
    if(!var %in% names(data)) {
      return(FALSE)
    }
    
    # For large datasets, sample for type detection
    if(nrow(data) > max_sample) {
      sample_data <- data[sample(nrow(data), max_sample), ][[var]]
    } else {
      sample_data <- data[[var]]
    }
    
    is.factor(sample_data) || 
      (is.character(sample_data) && 
         length(unique(na.omit(sample_data))) <= min(20, length(sample_data) * 0.2))
  }
  
  # Enhanced function to check if variable is numeric with performance optimization
  is_numeric_var <- function(data, var = NULL, max_sample = 10000) {
    if(is.null(var) || missing(var)) {
      return(FALSE)
    }
    
    if(!var %in% names(data)) {
      return(FALSE)
    }
    
    # For large datasets, sample for type detection
    if(nrow(data) > max_sample) {
      sample_data <- data[sample(nrow(data), max_sample), ][[var]]
    } else {
      sample_data <- data[[var]]
    }
    
    is.numeric(sample_data) || is.integer(sample_data) || 
      (is.character(sample_data) && all(grepl("^[0-9.]+$", na.omit(sample_data))))
  }
  
  # Optimized categorical variable selector with caching
  output$categorical_var_selector <- renderUI({
    if(is.null(rv$filtered_data) || length(rv$filtered_data) == 0) {
      return(HTML("<div class='alert alert-warning'>Please load a dataset first.</div>"))
    }
    
    # Show progress for large datasets
    if(nrow(rv$filtered_data) > 50000) {
      showNotification("Analyzing large dataset. This may take a moment...", 
                       type = "message", duration = 3)
    }
    
    # Use caching to avoid recomputation
    if(is.null(rv$categorical_vars_cache)) {
      withProgress(message = 'Identifying categorical variables...', value = 0, {
        categorical_vars <- c()
        total_vars <- length(names(rv$filtered_data))
        
        for(i in seq_along(names(rv$filtered_data))) {
          colname <- names(rv$filtered_data)[i]
          incProgress(1/total_vars, detail = paste("Checking", colname))
          
          if(tryCatch(is_categorical_var(rv$filtered_data, colname), error = function(e) FALSE)) {
            categorical_vars <- c(categorical_vars, colname)
          }
        }
        
        rv$categorical_vars_cache <- categorical_vars
      })
    }
    
    cat_vars <- rv$categorical_vars_cache
    
    if (length(cat_vars) == 0) {
      return(HTML("<div class='alert alert-warning'>No categorical variables found in the dataset.</div>"))
    }
    
    pickerInput(
      inputId = "cat_var",
      label = "Select Categorical Variable:",
      choices = cat_vars,
      selected = if(length(cat_vars) > 0) cat_vars[1] else NULL,
      options = list(
        `live-search` = TRUE,
        size = 10
      )
    )
  })
  
  # Optimized numerical variable selector with caching
  output$numerical_var_selector <- renderUI({
    if(is.null(rv$filtered_data) || length(rv$filtered_data) == 0) {
      return(HTML("<div class='alert alert-warning'>Please load a dataset first.</div>"))
    }
    
    # Show progress for large datasets
    if(nrow(rv$filtered_data) > 50000) {
      showNotification("Analyzing large dataset. This may take a moment...", 
                       type = "message", duration = 3)
    }
    
    # Use caching to avoid recomputation
    if(is.null(rv$numerical_vars_cache)) {
      withProgress(message = 'Identifying numerical variables...', value = 0, {
        numerical_vars <- c()
        total_vars <- length(names(rv$filtered_data))
        
        for(i in seq_along(names(rv$filtered_data))) {
          colname <- names(rv$filtered_data)[i]
          incProgress(1/total_vars, detail = paste("Checking", colname))
          
          if(tryCatch(is_numeric_var(rv$filtered_data, colname), error = function(e) FALSE)) {
            numerical_vars <- c(numerical_vars, colname)
          }
        }
        
        rv$numerical_vars_cache <- numerical_vars
      })
    }
    
    num_vars <- rv$numerical_vars_cache
    
    if (length(num_vars) == 0) {
      return(HTML("<div class='alert alert-warning'>No numerical variables found in the dataset.</div>"))
    }
    
    pickerInput(
      inputId = "num_var",
      label = "Select Numerical Variable:",
      choices = num_vars,
      selected = if(length(num_vars) > 0) num_vars[1] else NULL,
      options = list(
        `live-search` = TRUE,
        size = 10
      )
    )
  })
  
  # Enhanced categorical plot options with dataset size awareness
  output$cat_plot_options <- renderUI({
    req(input$cat_var, input$cat_plot_type, rv$filtered_data)
    
    # Get number of unique categories efficiently
    if(nrow(rv$filtered_data) > 50000) {
      # For large datasets, use data.table for faster unique count
      dt <- as.data.table(rv$filtered_data)
      n_categories <- dt[, uniqueN(get(input$cat_var)), keyby = NULL]
    } else {
      n_categories <- length(unique(rv$filtered_data[[input$cat_var]]))
    }
    
    # Warning for too many categories
    warning_ui <- NULL
    if(n_categories > MAX_CATEGORIES) {
      warning_ui <- div(
        class = "alert alert-warning",
        paste("Warning: This variable has", n_categories, "categories. Only the top", 
              MAX_CATEGORIES, "will be displayed for performance.")
      )
    }
    
    # Dataset size info
    size_info <- div(
      class = "alert alert-info",
      paste("Dataset size:", format(nrow(rv$filtered_data), big.mark = ","), "rows |",
            "Categories:", format(n_categories, big.mark = ","))
    )
    
    # Common options with performance considerations
    common_options <- tagList(
      size_info,
      warning_ui,
      fluidRow(
        column(6,
               selectInput(
                 inputId = "cat_color_scheme",
                 label = "Color Scheme:",
                 choices = c("Default", "Viridis", "Plasma", "Blues", "Greens", "Reds", "Spectral"),
                 selected = "Default"
               )
        ),
        column(6,
               textInput(
                 inputId = "cat_plot_title",
                 label = "Plot Title:",
                 value = paste("Distribution of", input$cat_var)
               )
        )
      ),
      # Add sampling option for very large datasets
      if(nrow(rv$filtered_data) > MAX_PLOT_POINTS) {
        fluidRow(
          column(12,
                 checkboxInput(
                   inputId = "use_sampling",
                   label = paste("Use sampling for performance (dataset has", 
                                 format(nrow(rv$filtered_data), big.mark = ","), "rows)"),
                   value = TRUE
                 )
          )
        )
      }
    )
    
    # Plot-specific options
    specific_options <- switch(
      input$cat_plot_type,
      "Bar Chart" = tagList(
        fluidRow(
          column(6,
                 selectInput(
                   inputId = "bar_orientation",
                   label = "Bar Orientation:",
                   choices = c("Vertical", "Horizontal"),
                   selected = "Vertical"
                 )
          ),
          column(6,
                 selectInput(
                   inputId = "bar_sort",
                   label = "Sort Bars:",
                   choices = c("Descending" = -1, "Original" = 0, "Ascending" = 1),
                   selected = -1
                 )
          )
        )
      ),
      "Pie Chart" = tagList(
        fluidRow(
          column(6,
                 checkboxInput(
                   inputId = "pie_show_percentages",
                   label = "Show Percentages",
                   value = TRUE
                 )
          ),
          column(6,
                 sliderInput(
                   inputId = "pie_hole",
                   label = "Inner Hole Size:",
                   min = 0, max = 0.8, value = 0, step = 0.1
                 )
          )
        )
      ),
      "Donut Chart" = tagList(
        fluidRow(
          column(6,
                 checkboxInput(
                   inputId = "donut_show_percentages",
                   label = "Show Percentages",
                   value = TRUE
                 )
          ),
          column(6,
                 sliderInput(
                   inputId = "donut_hole",
                   label = "Inner Hole Size:",
                   min = 0.3, max = 0.8, value = 0.5, step = 0.1
                 )
          )
        )
      ),
      "Treemap" = tagList(
        fluidRow(
          column(6,
                 checkboxInput(
                   inputId = "treemap_show_count",
                   label = "Show Count",
                   value = TRUE
                 )
          ),
          column(6,
                 checkboxInput(
                   inputId = "treemap_show_percentages",
                   label = "Show Percentages",
                   value = TRUE
                 )
          )
        )
      )
    )
    
    tagList(common_options, specific_options)
  })
  
  # Enhanced numerical plot options with dataset size awareness
  output$num_plot_options <- renderUI({
    req(input$num_plot_type, input$num_var, rv$filtered_data)
    
    # Dataset size info
    size_info <- div(
      class = "alert alert-info",
      paste("Dataset size:", format(nrow(rv$filtered_data), big.mark = ","), "rows")
    )
    
    # Warning for large datasets
    warning_ui <- NULL
    if(nrow(rv$filtered_data) > MAX_PLOT_POINTS) {
      warning_ui <- div(
        class = "alert alert-warning",
        paste("Large dataset detected. Sampling will be used for performance.")
      )
    }
    
    # Common options
    common_options <- tagList(
      size_info,
      warning_ui,
      fluidRow(
        column(4,
               selectInput(
                 inputId = "num_color_scheme",
                 label = "Color Scheme:",
                 choices = c("Default", "Blues", "Greens", "Reds", "Purples", "Oranges"),
                 selected = "Default"
               )
        ),
        column(4,
               textInput(
                 inputId = "num_plot_title",
                 label = "Plot Title:",
                 value = paste("Distribution of", input$num_var)
               )
        ),
        column(4,
               checkboxInput(
                 inputId = "num_log_scale",
                 label = "Log Scale Y-Axis",
                 value = FALSE
               )
        )
      ),
      # Add sampling control for large datasets
      if(nrow(rv$filtered_data) > MAX_PLOT_POINTS) {
        fluidRow(
          column(6,
                 numericInput(
                   inputId = "sample_size",
                   label = "Sample Size for Plotting:",
                   value = MAX_PLOT_POINTS,
                   min = 1000,
                   max = min(nrow(rv$filtered_data), 500000),
                   step = 10000
                 )
          ),
          column(6,
                 checkboxInput(
                   inputId = "use_sampling_num",
                   label = "Enable Sampling",
                   value = TRUE
                 )
          )
        )
      }
    )
    
    # Plot-specific options
    specific_options <- switch(
      input$num_plot_type,
      "Histogram" = tagList(
        fluidRow(
          column(4,
                 sliderInput(
                   inputId = "hist_bins",
                   label = "Number of Bins:",
                   min = 5, max = min(100, sqrt(nrow(rv$filtered_data))), 
                   value = min(30, max(10, sqrt(nrow(rv$filtered_data))/10)), 
                   step = 5
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "hist_density",
                   label = "Show Density Curve",
                   value = nrow(rv$filtered_data) < 100000
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "hist_rug",
                   label = "Show Rug Plot",
                   value = FALSE
                 )
          )
        )
      ),
      "Density Plot" = tagList(
        fluidRow(
          column(4,
                 sliderInput(
                   inputId = "density_adjust",
                   label = "Bandwidth Adjustment:",
                   min = 0.2, max = 2, value = 1, step = 0.1
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "density_fill",
                   label = "Fill Density Plot",
                   value = TRUE
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "density_rug",
                   label = "Show Rug Plot",
                   value = nrow(rv$filtered_data) < 10000
                 )
          )
        )
      ),
      "Box Plot" = tagList(
        fluidRow(
          column(4,
                 checkboxInput(
                   inputId = "box_notch",
                   label = "Show Notches",
                   value = FALSE
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "box_points",
                   label = "Show Data Points",
                   value = nrow(rv$filtered_data) < 5000
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "box_stats",
                   label = "Show Statistics",
                   value = TRUE
                 )
          )
        )
      ),
      "Violin Plot" = tagList(
        fluidRow(
          column(4,
                 checkboxInput(
                   inputId = "violin_box",
                   label = "Show Box Plot",
                   value = TRUE
                 )
          ),
          column(4,
                 checkboxInput(
                   inputId = "violin_points",
                   label = "Show Data Points",
                   value = FALSE
                 )
          ), 
          column(4,
                 checkboxInput(
                   inputId = "violin_stats",
                   label = "Show Statistics",
                   value = TRUE
                 )
          )
        )
      ),
      "Q-Q Plot" = tagList(
        fluidRow(
          column(6,
                 checkboxInput(
                   inputId = "qq_line",
                   label = "Show Reference Line",
                   value = TRUE
                 )
          ),
          column(6,
                 checkboxInput(
                   inputId = "qq_conf",
                   label = "Show Confidence Bands",
                   value = nrow(rv$filtered_data) < 50000
                 )
          )
        )
      )
    )
    
    tagList(common_options, specific_options)
  })
  
  # OPTIMIZED CATEGORICAL PLOT GENERATION
  output$cat_plot <- renderPlotly({
    req(input$cat_var, input$cat_plot_type, rv$filtered_data)
    
    withProgress(message = 'Creating categorical plot...', value = 0, {
      
      # Use data.table for faster operations on large datasets
      if(nrow(rv$filtered_data) > 10000) {
        dt <- as.data.table(rv$filtered_data)
        incProgress(0.2, detail = "Processing data...")
        
        # Get frequency table using data.table (much faster)
        freq_table <- dt[, .N, by = get(input$cat_var)]
        setnames(freq_table, c("Category", "Count"))
        
        # Handle too many categories
        if(nrow(freq_table) > MAX_CATEGORIES) {
          freq_table <- freq_table[order(-Count)][1:MAX_CATEGORIES]
          showNotification(paste("Showing top", MAX_CATEGORIES, "categories only"), 
                           type = "warning", duration = 5)
        }
      } else {
        # Use regular method for smaller datasets
        var_data <- rv$filtered_data[[input$cat_var]]
        freq_table <- as.data.frame(table(var_data))
        names(freq_table) <- c("Category", "Count")
      }
      
      incProgress(0.4, detail = "Calculating percentages...")
      
      # Calculate percentages
      freq_table$Percentage <- freq_table$Count / sum(freq_table$Count) * 100
      
      # Sorting logic
      if (exists("input$bar_sort") && !is.null(input$bar_sort)) {
        if (input$bar_sort == -1) {
          freq_table <- freq_table[order(freq_table$Count, decreasing = TRUE), ]
        } else if (input$bar_sort == 1) {
          freq_table <- freq_table[order(freq_table$Count), ]
        }
      }
      
      incProgress(0.6, detail = "Applying colors...")
      
      # Get color palette
      color_palette <- switch(
        input$cat_color_scheme %||% "Default",
        "Default" = NULL,
        "Viridis" = viridis::viridis(nrow(freq_table)),
        "Plasma" = viridis::plasma(nrow(freq_table)),
        "Blues" = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(nrow(freq_table)),
        "Greens" = colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"))(nrow(freq_table)),
        "Reds" = colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))(nrow(freq_table)),
        "Spectral" = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(nrow(freq_table))
      )
      
      incProgress(0.8, detail = "Creating plot...")
      
      # Plot title
      plot_title <- input$cat_plot_title %||% paste("Distribution of", input$cat_var)
      
      # Create plots
      plot <- switch(
        input$cat_plot_type,
        "Bar Chart" = {
          if ((exists("input$bar_orientation") && input$bar_orientation == "Horizontal")) {
            p <- plot_ly(freq_table, x = ~Count, y = ~Category, type = "bar", 
                         marker = list(color = color_palette),
                         text = ~paste0(Category, "<br>Count: ", format(Count, big.mark = ","), 
                                        "<br>Percentage: ", round(Percentage, 1), "%"),
                         hoverinfo = "text")
            p <- p %>% layout(title = plot_title,
                              xaxis = list(title = "Count"),
                              yaxis = list(title = ""),
                              margin = list(l = 100))
          } else {
            p <- plot_ly(freq_table, x = ~Category, y = ~Count, type = "bar", 
                         marker = list(color = color_palette),
                         text = ~paste0(Category, "<br>Count: ", format(Count, big.mark = ","), 
                                        "<br>Percentage: ", round(Percentage, 1), "%"),
                         hoverinfo = "text")
            p <- p %>% layout(title = plot_title,
                              xaxis = list(title = ""),
                              yaxis = list(title = "Count"))
          }
          p
        },
        "Pie Chart" = {
          show_pct <- exists("input$pie_show_percentages") && input$pie_show_percentages
          hole_size <- input$pie_hole %||% 0
          
          plot_ly(freq_table, labels = ~Category, values = ~Count, type = "pie",
                  textinfo = ifelse(show_pct, "label+percent", "label"),
                  hoverinfo = "text",
                  text = ~paste0(Category, "<br>Count: ", format(Count, big.mark = ","), 
                                 "<br>Percentage: ", round(Percentage, 1), "%"),
                  marker = list(colors = color_palette),
                  hole = hole_size) %>%
            layout(title = plot_title)
        },
        "Donut Chart" = {
          show_pct <- exists("input$donut_show_percentages") && input$donut_show_percentages
          hole_size <- input$donut_hole %||% 0.5
          
          plot_ly(freq_table, labels = ~Category, values = ~Count, type = "pie",
                  textinfo = ifelse(show_pct, "label+percent", "label"),
                  hoverinfo = "text",
                  text = ~paste0(Category, "<br>Count: ", format(Count, big.mark = ","), 
                                 "<br>Percentage: ", round(Percentage, 1), "%"),
                  marker = list(colors = color_palette),
                  hole = hole_size) %>%
            layout(title = plot_title)
        },
        "Treemap" = {
          show_count <- exists("input$treemap_show_count") && input$treemap_show_count
          show_pct <- exists("input$treemap_show_percentages") && input$treemap_show_percentages
          
          text_template <- freq_table$Category
          if (show_count) {
            text_template <- paste0(text_template, "<br>Count: ", format(freq_table$Count, big.mark = ","))
          }
          if (show_pct) {
            text_template <- paste0(text_template, "<br>", round(freq_table$Percentage, 1), "%")
          }
          
          plot_ly(freq_table, labels = ~Category, parents = rep("", nrow(freq_table)), 
                  values = ~Count, type = "treemap",
                  textinfo = "label+value+percent",
                  hoverinfo = "text",
                  text = text_template,
                  marker = list(colors = color_palette)) %>%
            layout(title = plot_title)
        }
      )
      
      incProgress(1, detail = "Complete!")
      return(plot)
    })
  })
  
  # OPTIMIZED NUMERICAL PLOT GENERATION
  output$num_plot <- renderPlotly({
    req(input$num_var, input$num_plot_type, rv$filtered_data)
    
    withProgress(message = 'Creating numerical plot...', value = 0, {
      
      # Extract and clean data
      var_data <- rv$filtered_data[[input$num_var]]
      var_data <- na.omit(var_data)
      
      incProgress(0.2, detail = "Processing data...")
      
      # Apply sampling for large datasets
      use_sampling <- (exists("input$use_sampling_num") && input$use_sampling_num) || 
        nrow(rv$filtered_data) > MAX_PLOT_POINTS
      
      if(use_sampling && length(var_data) > MAX_PLOT_POINTS) {
        sample_size <- ifelse(exists("input$sample_size"), 
                              input$sample_size, 
                              MAX_PLOT_POINTS)
        var_data <- sample(var_data, min(sample_size, length(var_data)))
        showNotification(paste("Using sample of", length(var_data), "points for performance"), 
                         type = "message", duration = 3)
      }
      
      incProgress(0.4, detail = "Setting plot parameters...")
      
      # Get plot parameters
      plot_title <- input$num_plot_title %||% paste("Distribution of", input$num_var)
      main_color <- switch(
        input$num_color_scheme %||% "Default",
        "Default" = "#1f77b4",
        "Blues" = "#3182bd",
        "Greens" = "#31a354",
        "Reds" = "#e6550d",
        "Purples" = "#756bb1",
        "Oranges" = "#fd8d3c"
      )
      
      use_log <- exists("input$num_log_scale") && input$num_log_scale
      
      incProgress(0.6, detail = "Creating plot...")
      
      # Create plots with optimizations
      plot <- switch(
        input$num_plot_type,
        "Histogram" = {
          nbins <- input$hist_bins %||% min(30, max(10, sqrt(length(var_data))/5))
          show_density <- exists("input$hist_density") && input$hist_density && length(var_data) < 100000
          show_rug <- exists("input$hist_rug") && input$hist_rug && length(var_data) < 10000
          
          p <- plot_ly(x = var_data, type = "histogram", 
                       nbinsx = nbins,
                       marker = list(color = main_color, 
                                     line = list(color = "white", width = 0.5)),
                       hoverinfo = "y+x",
                       name = "Histogram") %>%
            layout(title = plot_title,
                   xaxis = list(title = input$num_var),
                   yaxis = list(title = "Count"))
          
          # Add density curve only for smaller datasets
          if (show_density) {
            dens <- density(var_data)
            hist_data <- hist(var_data, breaks = nbins, plot = FALSE)
            scaling_factor <- max(hist_data$counts) / max(dens$y)
            dens_df <- data.frame(x = dens$x, y = dens$y * scaling_factor)
            
            p <- p %>% add_lines(data = dens_df, x = ~x, y = ~y,
                                 line = list(color = 'rgba(255, 0, 0, 0.8)', width = 2),
                                 name = "Density")
          }
          
          # Add rug plot only for very small datasets
          if (show_rug && length(var_data) < 5000) {
            rug_df <- data.frame(x = var_data)
            p <- p %>% add_markers(data = rug_df, x = ~x, y = 0,
                                   marker = list(symbol = "line-ns", 
                                                 color = "rgba(0, 0, 0, 0.3)",
                                                 line = list(width = 0.5),
                                                 size = 8),
                                   name = "Data Points",
                                   hoverinfo = "x")
          }
          
          p
        },
        "Density Plot" = {
          adjust <- input$density_adjust %||% 1
          fill_density <- exists("input$density_fill") && input$density_fill
          show_rug <- exists("input$density_rug") && input$density_rug && length(var_data) < 10000
          
          dens <- density(var_data, adjust = adjust)
          dens_df <- data.frame(x = dens$x, y = dens$y)
          
          p <- plot_ly()
          
          if (fill_density) {
            p <- p %>% add_trace(data = dens_df, x = ~x, y = ~y,
                                 type = "scatter", mode = "none",
                                 fill = "tozeroy",
                                 fillcolor = paste0(substr(main_color, 1, 7), "70"),
                                 name = "Density")
          }
          
          p <- p %>% add_trace(data = dens_df, x = ~x, y = ~y,
                               type = "scatter", mode = "lines",
                               line = list(color = main_color, width = 2),
                               name = "Density")
          
          # Add rug plot if requested
          if (show_rug) {
            rug_sample <- if(length(var_data) > 1000) sample(var_data, 1000) else var_data
            rug_df <- data.frame(x = rug_sample)
            p <- p %>% add_markers(data = rug_df, x = ~x, y = 0,
                                   marker = list(symbol = "line-ns", 
                                                 color = "rgba(0, 0, 0, 0.3)",
                                                 line = list(width = 0.5),
                                                 size = 8),
                                   name = "Data Points",
                                   hoverinfo = "x")
          }
          
          p %>% layout(title = plot_title,
                       xaxis = list(title = input$num_var),
                       yaxis = list(title = "Density"))
        },
        "Box Plot" = {
          show_notch <- exists("input$box_notch") && input$box_notch
          show_points <- exists("input$box_points") && input$box_points && length(var_data) < 5000
          show_stats <- exists("input$box_stats") && input$box_stats
          
          # Base box plot
          p <- plot_ly(y = var_data, type = "box", 
                       name = input$num_var,
                       marker = list(color = main_color),
                       boxmean = show_stats,  # Show mean if stats are requested
                       notched = show_notch,
                       line = list(width = 2),
                       fillcolor = paste0(substr(main_color, 1, 7), "70"))
          
          # Add jittered points if requested and dataset is small enough
          if (show_points) {
            set.seed(123)  # For reproducible jitter
            jitter_amount <- (max(var_data) - min(var_data)) * 0.05
            jittered_x <- rnorm(length(var_data), mean = 0, sd = 0.08)
            
            p <- p %>% add_markers(x = jittered_x, y = var_data,
                                   marker = list(color = "rgba(0, 0, 0, 0.3)",
                                                 size = 4),
                                   name = "Data Points",
                                   hoverinfo = "y")
          }
          
          p %>% layout(title = plot_title,
                       xaxis = list(title = ""),
                       yaxis = list(title = input$num_var,
                                    type = ifelse(use_log, "log", "linear")))
        },
        "Violin Plot" = {
          show_box <- exists("input$violin_box") && input$violin_box
          show_points <- exists("input$violin_points") && input$violin_points && length(var_data) < 5000
          show_stats <- exists("input$violin_stats") && input$violin_stats
          
          # Create violin plot
          p <- plot_ly(y = var_data, type = "violin",
                       name = input$num_var,
                       box = list(visible = show_box),
                       meanline = list(visible = show_stats),
                       fillcolor = paste0(substr(main_color, 1, 7), "70"),
                       line = list(color = main_color),
                       points = ifelse(show_points, "all", FALSE),
                       jitter = 0.3,
                       pointpos = -0.5,
                       marker = list(size = 3, opacity = 0.6))
          
          p %>% layout(title = plot_title,
                       xaxis = list(title = ""),
                       yaxis = list(title = input$num_var,
                                    type = ifelse(use_log, "log", "linear")))
        },
        "Q-Q Plot" = {
          # Compute theoretical quantiles efficiently
          n <- length(var_data)
          var_data_sorted <- sort(var_data)
          
          # For large datasets, use approximation for efficiency
          if(n > 10000) {
            # Sample quantiles for performance
            sample_size <- min(5000, n)
            indices <- round(seq(1, n, length.out = sample_size))
            y <- var_data_sorted[indices]
            x <- qnorm(ppoints(sample_size))
            showNotification("Using sample for Q-Q plot performance", 
                             type = "message", duration = 3)
          } else {
            y <- var_data_sorted
            x <- qnorm(ppoints(n))
          }
          
          qq_df <- data.frame(x = x, y = y)
          
          # Create Q-Q plot
          p <- plot_ly(data = qq_df, x = ~x, y = ~y, type = "scatter", mode = "markers",
                       marker = list(color = main_color, size = 5),
                       name = "Q-Q Plot",
                       hovertemplate = "Theoretical: %{x:.3f}<br>Sample: %{y:.3f}<extra></extra>")
          
          # Add reference line if requested
          if (exists("input$qq_line") && input$qq_line) {
            # Calculate slope and intercept for reference line
            slope <- sd(var_data)
            intercept <- mean(var_data)
            
            # Calculate line endpoints
            x_range <- range(x)
            y_line <- intercept + slope * x_range
            
            # Add reference line
            p <- p %>% add_trace(x = x_range, 
                                 y = y_line,
                                 type = "scatter", mode = "lines",
                                 line = list(color = "red", width = 2, dash = "dash"),
                                 name = "Reference Line",
                                 hoverinfo = "skip")
          }
          
          # Add confidence bands if requested (only for smaller datasets)
          if (exists("input$qq_conf") && input$qq_conf && length(var_data) < 50000) {
            # Calculate confidence bands (95%)
            n_bands <- length(x)
            se <- (slope / dnorm(x)) * sqrt(ppoints(n_bands) * (1 - ppoints(n_bands)) / n_bands)
            z_crit <- qnorm(0.975)
            
            # Upper and lower bands
            y_fitted <- intercept + slope * x
            upper <- y_fitted + z_crit * se
            lower <- y_fitted - z_crit * se
            
            # Add confidence bands
            p <- p %>% add_ribbons(x = x, ymin = lower, ymax = upper,
                                   fillcolor = "rgba(128, 128, 128, 0.2)",
                                   line = list(color = "transparent"),
                                   name = "95% Confidence Band",
                                   hoverinfo = "skip")
          }
          
          p %>% layout(title = plot_title,
                       xaxis = list(title = "Theoretical Quantiles"),
                       yaxis = list(title = paste("Sample Quantiles -", input$num_var)))
        }
      )
      
      # Apply log scale to y-axis if requested
      if(use_log && input$num_plot_type %in% c("Histogram", "Density Plot")) {
        plot <- plot %>% layout(yaxis = list(type = "log"))
      }
      
      incProgress(1, detail = "Complete!")
      return(plot)
    })
  })
  
  # ENHANCED SUMMARY STATISTICS WITH PERFORMANCE OPTIMIZATIONS
  
  # Optimized categorical summary statistics with chunking for large datasets
  output$cat_summary <- renderDT({
    req(input$cat_var, rv$filtered_data)
    
    withProgress(message = 'Calculating categorical summary...', value = 0, {
      
      incProgress(0.3, detail = "Processing frequencies...")
      
      # Use data.table for large datasets
      if(nrow(rv$filtered_data) > 50000) {
        dt <- as.data.table(rv$filtered_data)
        freq_table <- dt[, .N, by = get(input$cat_var)]
        setnames(freq_table, c("Value", "Frequency"))
        
        # Handle missing values
        if(any(is.na(freq_table$Value))) {
          freq_table$Value <- as.character(freq_table$Value)
          freq_table$Value[is.na(freq_table$Value)] <- "Missing"
        }
      } else {
        # Standard method for smaller datasets
        var_data <- rv$filtered_data[[input$cat_var]]
        freq_table <- as.data.frame(table(var_data, useNA = "ifany"))
        names(freq_table) <- c("Value", "Frequency")
        freq_table$Value <- as.character(freq_table$Value)
        freq_table$Value[is.na(freq_table$Value)] <- "Missing"
      }
      
      incProgress(0.6, detail = "Calculating percentages...")
      
      # Calculate percentages and cumulative statistics
      total_freq <- sum(freq_table$Frequency)
      freq_table$Percentage <- freq_table$Frequency / total_freq * 100
      
      # Sort by frequency for better display
      freq_table <- freq_table[order(freq_table$Frequency, decreasing = TRUE), ]
      
      # Calculate cumulative statistics
      freq_table$Cumulative <- cumsum(freq_table$Frequency)
      freq_table$CumulativePct <- cumsum(freq_table$Percentage)
      
      # Format percentages for display
      freq_table$Percentage <- sprintf("%.2f%%", freq_table$Percentage)
      freq_table$CumulativePct <- sprintf("%.2f%%", freq_table$CumulativePct)
      
      # Add summary row
      summary_row <- data.frame(
        Value = "TOTAL",
        Frequency = total_freq,
        Percentage = "100.00%",
        Cumulative = total_freq,
        CumulativePct = "100.00%",
        stringsAsFactors = FALSE
      )
      
      freq_table <- rbind(freq_table, summary_row)
      
      incProgress(1, detail = "Complete!")
      
      # Render the summary table with performance optimizations
      datatable(
        freq_table, 
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'All')),
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-right', targets = c(1, 3)), # Right-align numeric columns
            list(className = 'dt-center', targets = c(2, 4)) # Center-align percentage columns
          ),
          # Optimize for large datasets
          deferRender = TRUE,
          scroller = nrow(freq_table) > 100
        ),
        extensions = c('Buttons', 'Scroller')
      ) %>%
        formatStyle(
          'Value',
          backgroundColor = styleEqual('TOTAL', '#f0f0f0'),
          fontWeight = styleEqual('TOTAL', 'bold')
        )
    })
  })
  
  # Enhanced numerical summary statistics with robust calculations
  output$num_summary <- renderPrint({
    req(input$num_var, rv$filtered_data)
    
    withProgress(message = 'Calculating numerical summary...', value = 0, {
      
      var_data <- rv$filtered_data[[input$num_var]]
      
      incProgress(0.2, detail = "Processing data...")
      
      # Handle missing values
      n_total <- length(var_data)
      n_missing <- sum(is.na(var_data))
      var_data_clean <- na.omit(var_data)
      n_valid <- length(var_data_clean)
      
      if(n_valid == 0) {
        cat("Error: No valid (non-missing) values found in the selected variable.\n")
        return()
      }
      
      incProgress(0.4, detail = "Computing basic statistics...")
      
      # Basic statistics
      basic_stats <- summary(var_data_clean)
      
      # Additional statistics
      std_dev <- sd(var_data_clean)
      variance <- var(var_data_clean)
      range_val <- range(var_data_clean)
      iqr_val <- IQR(var_data_clean)
      
      incProgress(0.6, detail = "Computing distribution statistics...")
      
      # Distribution statistics (with error handling for edge cases)
      tryCatch({
        skewness_val <- moments::skewness(var_data_clean)
        kurtosis_val <- moments::kurtosis(var_data_clean)
      }, error = function(e) {
        skewness_val <- NA
        kurtosis_val <- NA
      })
      
      incProgress(0.8, detail = "Performing normality test...")
      
      # Normality test (handle large datasets)
      normality_test <- NULL
      if(n_valid <= 5000) {
        tryCatch({
          normality_test <- shapiro.test(var_data_clean)
        }, error = function(e) {
          normality_test <- NULL
        })
      } else {
        # Use Kolmogorov-Smirnov test for large datasets
        tryCatch({
          normality_test <- ks.test(var_data_clean, "pnorm", 
                                    mean = mean(var_data_clean), 
                                    sd = sd(var_data_clean))
        }, error = function(e) {
          normality_test <- NULL
        })
      }
      
      incProgress(1, detail = "Complete!")
      
      # Display results with better formatting
      cat("=== NUMERICAL VARIABLE SUMMARY ===\n")
      cat("Variable:", input$num_var, "\n\n")
      
      cat("--- Data Overview ---\n")
      cat("Total observations:", format(n_total, big.mark = ","), "\n")
      cat("Valid observations:", format(n_valid, big.mark = ","), "\n")
      cat("Missing values:", format(n_missing, big.mark = ","), 
          paste0("(", round(n_missing/n_total*100, 2), "%)"), "\n\n")
      
      cat("--- Basic Statistics ---\n")
      print(basic_stats)
      cat("\nStandard Deviation:", format(std_dev, digits = 4), "\n")
      cat("Variance:", format(variance, digits = 4), "\n")
      cat("Range:", format(range_val[1], digits = 4), "to", format(range_val[2], digits = 4), "\n")
      cat("Interquartile Range (IQR):", format(iqr_val, digits = 4), "\n\n")
      
      cat("--- Distribution Statistics ---\n")
      if(!is.na(skewness_val)) {
        cat("Skewness:", format(skewness_val, digits = 4))
        if(abs(skewness_val) < 0.5) {
          cat(" (approximately symmetric)")
        } else if(skewness_val > 0.5) {
          cat(" (right-skewed)")
        } else {
          cat(" (left-skewed)")
        }
        cat("\n")
      } else {
        cat("Skewness: Unable to calculate\n")
      }
      
      if(!is.na(kurtosis_val)) {
        cat("Kurtosis:", format(kurtosis_val, digits = 4))
        if(kurtosis_val > 3) {
          cat(" (heavy-tailed)")
        } else if(kurtosis_val < 3) {
          cat(" (light-tailed)")
        } else {
          cat(" (normal-tailed)")
        }
        cat("\n")
      } else {
        cat("Kurtosis: Unable to calculate\n")
      }
      
      # Normality test results
      cat("\n--- Normality Test ---\n")
      if(!is.null(normality_test)) {
        if(n_valid <= 5000) {
          cat("Shapiro-Wilk Test:\n")
        } else {
          cat("Kolmogorov-Smirnov Test (used for large datasets):\n")
        }
        cat("Test statistic:", format(normality_test$statistic, digits = 4), "\n")
        cat("p-value:", format(normality_test$p.value, scientific = TRUE), "\n")
        
        if(normality_test$p.value < 0.05) {
          cat("Interpretation: Data significantly deviates from normal distribution (p < 0.05)\n")
        } else {
          cat("Interpretation: Data does not significantly deviate from normal distribution (p >= 0.05)\n")
        }
      } else {
        cat("Normality test could not be performed.\n")
        if(n_valid > 5000) {
          cat("Note: For very large datasets, visual inspection of Q-Q plots may be more informative.\n")
        }
      }
      
      # Additional percentiles for comprehensive summary
      cat("\n--- Percentiles ---\n")
      percentiles <- quantile(var_data_clean, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
      for(i in 1:length(percentiles)) {
        cat(sprintf("%3s%%: %s\n", 
                    names(percentiles)[i], 
                    format(percentiles[i], digits = 4)))
      }
    })
  })
  
  # ADDITIONAL UTILITY FUNCTIONS FOR PERFORMANCE OPTIMIZATION
  
  # Function to clear variable caches when new data is loaded
  observeEvent(rv$filtered_data, {
    rv$categorical_vars_cache <- NULL
    rv$numerical_vars_cache <- NULL
    
    # Show dataset info for large datasets
    if(!is.null(rv$filtered_data) && nrow(rv$filtered_data) > 100000) {
      showNotification(
        paste("Large dataset detected:", format(nrow(rv$filtered_data), big.mark = ","), 
              "rows. Performance optimizations are active."),
        type = "message",
        duration = 5
      )
    }
  })
  
  # Memory usage monitoring for very large datasets
  observe({
    if(!is.null(rv$filtered_data) && nrow(rv$filtered_data) > 500000) {
      # Check memory usage periodically
      invalidateLater(30000)  # Check every 30 seconds
      
      mem_info <- pryr::object_size(rv$filtered_data)
      if(as.numeric(mem_info) > 1e9) {  # > 1GB
        showNotification(
          paste("High memory usage detected:", format(mem_info, units = "auto"), 
                "Consider using data filters to reduce dataset size."),
          type = "warning",
          duration = 10
        )
      }
    }
  })
  
  # Performance tips UI element
  output$performance_tips <- renderUI({
    if(is.null(rv$filtered_data)) return(NULL)
    
    n_rows <- nrow(rv$filtered_data)
    n_cols <- ncol(rv$filtered_data)
    
    tips <- c()
    
    if(n_rows > 100000) {
      tips <- c(tips, "• Large dataset: Sampling is automatically enabled for plotting performance")
    }
    
    if(n_cols > 50) {
      tips <- c(tips, "• Many variables: Use search functionality in variable selectors")
    }
    
    if(n_rows > 1000000) {
      tips <- c(tips, "• Very large dataset: Consider applying filters to reduce data size")
      tips <- c(tips, "• Some statistical tests are optimized or disabled for performance")
    }
    
    if(length(tips) > 0) {
      div(
        class = "alert alert-info",
        h5("Performance Tips:"),
        HTML(paste(tips, collapse = "<br>"))
      )
    }
  })
  
  # Export functionality for summaries
  output$export_cat_summary <- downloadHandler(
    filename = function() {
      paste0("categorical_summary_", input$cat_var, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Recreate the summary data
      if(nrow(rv$filtered_data) > 50000) {
        dt <- as.data.table(rv$filtered_data)
        freq_table <- dt[, .N, by = get(input$cat_var)]
        setnames(freq_table, c("Value", "Frequency"))
      } else {
        var_data <- rv$filtered_data[[input$cat_var]]
        freq_table <- as.data.frame(table(var_data, useNA = "ifany"))
        names(freq_table) <- c("Value", "Frequency")
      }
      
      freq_table$Percentage <- freq_table$Frequency / sum(freq_table$Frequency) * 100
      freq_table <- freq_table[order(freq_table$Frequency, decreasing = TRUE), ]
      freq_table$Cumulative <- cumsum(freq_table$Frequency)
      freq_table$CumulativePct <- cumsum(freq_table$Percentage)
      
      write.csv(freq_table, file, row.names = FALSE)
    }
  )
  
  output$export_num_summary <- downloadHandler(
    filename = function() {
      paste0("numerical_summary_", input$num_var, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      # Capture the printed summary
      output_text <- capture.output({
        var_data <- rv$filtered_data[[input$num_var]]
        var_data_clean <- na.omit(var_data)
        
        cat("NUMERICAL VARIABLE SUMMARY\n")
        cat("Variable:", input$num_var, "\n")
        cat("Generated on:", as.character(Sys.time()), "\n\n")
        
        print(summary(var_data_clean))
        cat("\nStandard Deviation:", sd(var_data_clean), "\n")
        
        if(length(var_data_clean) <= 5000) {
          normality_test <- shapiro.test(var_data_clean)
          cat("\nNormality Test (Shapiro-Wilk):\n")
          print(normality_test)
        }
      })
      
      writeLines(output_text, file)
    }
  )


  # Bivariate Analysis Module for Enhanced EDA App =============================
  
  # ========== ROBUST NUMERIC VS NUMERIC ANALYSIS ==========
  
  # UI for numeric-numeric variable selection with data size handling
  output$num_num_var_selector <- renderUI({
    req(rv$filtered_data)
    
    # Get numeric columns with safe type checking
    numeric_cols <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) {
      tryCatch({
        is.numeric(x) || is.integer(x)
      }, error = function(e) FALSE)
    })]
    
    if (length(numeric_cols) < 2) {
      return(HTML("<div class='alert alert-warning'>Need at least 2 numeric variables for this analysis.</div>"))
    }
    
    # Add data size warning if dataset is very large
    n_rows <- nrow(rv$filtered_data)
    size_warning <- if (n_rows > 100000) {
      HTML("<div class='alert alert-info'>Large dataset detected. Plots may take longer to render and might be sampled for performance.</div>")
    } else NULL
    
    tagList(
      size_warning,
      selectInput("num_x_var", "X Variable:", choices = numeric_cols, selected = numeric_cols[1]),
      selectInput("num_y_var", "Y Variable:", choices = numeric_cols, selected = numeric_cols[2])
    )
  })
  
  # UI for numeric-numeric plot options
  output$num_num_plot_options <- renderUI({
    req(input$num_num_plot_type)
    
    options_list <- tagList()
    
    # Add sampling option for large datasets
    if (!is.null(rv$filtered_data) && nrow(rv$filtered_data) > 50000) {
      options_list <- tagList(
        options_list,
        checkboxInput("enable_sampling", "Enable sampling for performance", value = TRUE),
        conditionalPanel(
          condition = "input.enable_sampling == true",
          numericInput("sample_size", "Sample size:", value = 10000, min = 1000, max = 50000, step = 1000)
        )
      )
    }
    
    # Specific options for plot types
    if (input$num_num_plot_type == "Scatter Plot") {
      options_list <- tagList(
        options_list,
        selectInput("scatter_point_style", "Point Style:", 
                    choices = c("Circle" = "circle", "Square" = "square", "Cross" = "cross", 
                                "Diamond" = "diamond", "Triangle" = "triangle"), 
                    selected = "circle"),
        sliderInput("scatter_point_opacity", "Point Opacity:", min = 0.1, max = 1, value = 0.7, step = 0.1),
        numericInput("scatter_point_size", "Point Size:", value = 8, min = 3, max = 20)
      )
      
      # Check if there's a potential third variable to use for coloring
      tryCatch({
        if (!is.null(rv$filtered_data)) {
          cat_cols <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) {
            tryCatch({
              unique_vals <- length(unique(x[!is.na(x)]))
              (is.factor(x) || is.character(x) || (is.numeric(x) && unique_vals <= 10 && unique_vals > 1))
            }, error = function(e) FALSE)
          })]
          
          if (length(cat_cols) > 0) {
            options_list <- tagList(
              options_list,
              selectInput("scatter_color_var", "Color By:", 
                          choices = c("None" = "none", cat_cols), 
                          selected = "none")
            )
          }
        }
      }, error = function(e) {
        # Silently continue if there's an error detecting categorical columns
      })
      
    } else if (input$num_num_plot_type == "Hexbin Plot") {
      options_list <- tagList(
        options_list,
        sliderInput("hexbin_bins", "Number of Bins:", min = 10, max = 100, value = 30, step = 5),
        selectInput("hexbin_colorscale", "Color Scale:", 
                    choices = c("Viridis" = "Viridis", "Plasma" = "Plasma", "Blues" = "Blues", 
                                "YlOrRd" = "YlOrRd", "YlGnBu" = "YlGnBu"),
                    selected = "Viridis")
      )
    } else if (input$num_num_plot_type == "Contour Plot") {
      options_list <- tagList(
        options_list,
        sliderInput("contour_bins", "Number of Contours:", min = 5, max = 50, value = 15, step = 5),
        checkboxInput("contour_fill", "Fill Contours", value = TRUE)
      )
    } else if (input$num_num_plot_type == "Heatmap") {
      options_list <- tagList(
        options_list,
        sliderInput("heatmap_x_bins", "X-axis Bins:", min = 5, max = 50, value = 20, step = 5),
        sliderInput("heatmap_y_bins", "Y-axis Bins:", min = 5, max = 50, value = 20, step = 5),
        selectInput("heatmap_colorscale", "Color Scale:", 
                    choices = c("Viridis" = "Viridis", "Plasma" = "Plasma", "Blues" = "Blues", 
                                "YlOrRd" = "YlOrRd", "YlGnBu" = "YlGnBu"),
                    selected = "Viridis")
      )
    }
    
    return(options_list)
  })
  
  # Safe data sampling function
  safe_sample_data <- function(data, x_var, y_var, sample_size = NULL, color_var = NULL) {
    # Validate inputs
    if (is.null(data) || nrow(data) == 0) {
      return(list(data = data.frame(), sampled = FALSE, original_size = 0))
    }
    
    # Remove rows with missing values in required variables
    required_vars <- c(x_var, y_var)
    if (!is.null(color_var) && color_var != "none" && color_var %in% names(data)) {
      required_vars <- c(required_vars, color_var)
    }
    
    valid_rows <- complete.cases(data[, required_vars, drop = FALSE])
    clean_data <- data[valid_rows, , drop = FALSE]
    
    # Check if we have enough data
    if (nrow(clean_data) < 2) {
      return(list(data = clean_data, sampled = FALSE, original_size = nrow(data)))
    }
    
    # Sample if requested and dataset is large enough
    sampled <- FALSE
    if (!is.null(sample_size) && sample_size > 0 && nrow(clean_data) > sample_size) {
      set.seed(123)  # For reproducible sampling
      sample_indices <- sample(nrow(clean_data), min(sample_size, nrow(clean_data)))
      clean_data <- clean_data[sample_indices, , drop = FALSE]
      sampled <- TRUE
    }
    
    return(list(data = clean_data, sampled = sampled, original_size = nrow(data)))
  }
  
  # Generate numeric vs numeric plot with robust error handling
  output$num_num_plot <- renderPlotly({
    # Validate required inputs
    req(rv$filtered_data)
    req(input$num_x_var)
    req(input$num_y_var)
    req(input$num_num_plot_type)
    
    tryCatch({
      # Get variables
      x_var <- input$num_x_var
      y_var <- input$num_y_var
      
      # Validate variables exist in data
      if (!x_var %in% names(rv$filtered_data) || !y_var %in% names(rv$filtered_data)) {
        p <- plot_ly(type = "scatter", mode = "markers") %>%
          layout(title = "Selected variables not found in data",
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        return(p)
      }
      
      # Determine if sampling should be used
      use_sampling <- !is.null(input$enable_sampling) && input$enable_sampling
      sample_size <- if (use_sampling && !is.null(input$sample_size)) {
        max(1000, min(50000, input$sample_size))
      } else NULL
      
      color_var <- NULL
      if (!is.null(input$scatter_color_var) && input$scatter_color_var != "none") {
        color_var <- input$scatter_color_var
      }
      
      # Get and clean data
      data_result <- safe_sample_data(rv$filtered_data, x_var, y_var, sample_size, color_var)
      plot_data <- data_result$data
      
      # Check if we have enough data
      if (nrow(plot_data) < 2) {
        p <- plot_ly(type = "scatter", mode = "markers") %>%
          layout(title = "Not enough valid data points (need at least 2)",
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        return(p)
      }
      
      # Create subtitle with sampling info
      subtitle <- if (data_result$sampled) {
        sprintf("Showing %d sampled points from %d total", nrow(plot_data), data_result$original_size)
      } else {
        sprintf("%d data points", nrow(plot_data))
      }
      
      # Extract data vectors for plotting - FIX: Ensure proper conversion to numeric
      x_data <- tryCatch({
        as.numeric(as.character(plot_data[[x_var]]))
      }, error = function(e) {
        as.numeric(plot_data[[x_var]])
      })
      
      y_data <- tryCatch({
        as.numeric(as.character(plot_data[[y_var]]))
      }, error = function(e) {
        as.numeric(plot_data[[y_var]])
      })
      
      # Remove any remaining NAs
      valid_indices <- !is.na(x_data) & !is.na(y_data) & is.finite(x_data) & is.finite(y_data)
      x_data <- x_data[valid_indices]
      y_data <- y_data[valid_indices]
      
      if (length(x_data) < 2) {
        p <- plot_ly(type = "scatter", mode = "markers") %>%
          layout(title = "Not enough valid numeric data points",
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        return(p)
      }
      
      # Update plot_data to only include valid rows
      plot_data <- plot_data[valid_indices, , drop = FALSE]
      
      # Create base plot based on plot type
      if (input$num_num_plot_type == "Scatter Plot") {
        # Set point properties with safe defaults
        marker_size <- if (!is.null(input$scatter_point_size)) {
          max(3, min(20, input$scatter_point_size))
        } else 8
        
        marker_alpha <- if (!is.null(input$scatter_point_opacity)) {
          max(0.1, min(1, input$scatter_point_opacity))
        } else 0.7
        
        marker_symbol <- if (!is.null(input$scatter_point_style)) {
          input$scatter_point_style
        } else "circle"
        
        # Create base plot - FIX: Use direct vectors instead of get() function
        if (!is.null(color_var) && color_var %in% names(plot_data)) {
          # Plot with color variable
          color_data <- plot_data[[color_var]]
          
          p <- plot_ly(x = x_data, 
                       y = y_data, 
                       color = color_data,
                       type = "scatter", 
                       mode = "markers",
                       marker = list(size = marker_size, opacity = marker_alpha),
                       hovertemplate = paste0("<b>", x_var, "</b>: %{x}<br>", 
                                              "<b>", y_var, "</b>: %{y}<br>", 
                                              "<b>", color_var, "</b>: %{marker.color}<extra></extra>"))
        } else {
          # Plot without color variable
          p <- plot_ly(x = x_data, 
                       y = y_data, 
                       type = "scatter", 
                       mode = "markers",
                       marker = list(size = marker_size, opacity = marker_alpha, color = "steelblue"),
                       hovertemplate = paste0("<b>", x_var, "</b>: %{x}<br>", 
                                              "<b>", y_var, "</b>: %{y}<extra></extra>"))
        }
        
        # Add layout
        p <- p %>% layout(
          xaxis = list(title = x_var),
          yaxis = list(title = y_var),
          title = list(text = sprintf("Scatter Plot: %s vs %s<br><sub>%s</sub>", y_var, x_var, subtitle))
        )
        
        
      } else if (input$num_num_plot_type == "Hexbin Plot") {
        # Create hexbin plot with safe parameters
        n_bins <- if (!is.null(input$hexbin_bins)) {
          max(10, min(100, input$hexbin_bins))
        } else 30
        
        colorscale <- if (!is.null(input$hexbin_colorscale)) {
          tolower(input$hexbin_colorscale)
        } else "viridis"
        
        p <- plot_ly(x = x_data, 
                     y = y_data, 
                     type = "histogram2d", 
                     nbinsx = n_bins, 
                     nbinsy = n_bins,
                     colorscale = colorscale,
                     hovertemplate = "Count: %{z}<extra></extra>") %>%
          layout(xaxis = list(title = x_var), 
                 yaxis = list(title = y_var),
                 title = list(text = sprintf("Hexbin Plot: %s vs %s<br><sub>%s</sub>", y_var, x_var, subtitle)))
        
      } else if (input$num_num_plot_type == "Contour Plot") {
        # Create contour plot with safe parameters
        n_bins <- if (!is.null(input$contour_bins)) {
          max(5, min(50, input$contour_bins))
        } else 15
        
        fill_contours <- !is.null(input$contour_fill) && input$contour_fill
        
        p <- plot_ly(x = x_data, 
                     y = y_data,
                     type = "histogram2dcontour",
                     nbinsx = n_bins, 
                     nbinsy = n_bins,
                     contours = list(coloring = ifelse(fill_contours, "fill", "lines")),
                     colorscale = "Blues",
                     hovertemplate = "Density: %{z}<extra></extra>") %>%
          layout(xaxis = list(title = x_var), 
                 yaxis = list(title = y_var),
                 title = list(text = sprintf("Contour Plot: %s vs %s<br><sub>%s</sub>", y_var, x_var, subtitle)))
        
      } else if (input$num_num_plot_type == "Heatmap") {
        # Create heatmap with safe parameters
        x_bins <- if (!is.null(input$heatmap_x_bins)) {
          max(5, min(50, input$heatmap_x_bins))
        } else 20
        
        y_bins <- if (!is.null(input$heatmap_y_bins)) {
          max(5, min(50, input$heatmap_y_bins))
        } else 20
        
        colorscale <- if (!is.null(input$heatmap_colorscale)) {
          tolower(input$heatmap_colorscale)
        } else "viridis"
        
        p <- plot_ly(x = x_data, 
                     y = y_data,
                     type = "histogram2d",
                     nbinsx = x_bins, 
                     nbinsy = y_bins,
                     colorscale = colorscale,
                     hovertemplate = "Count: %{z}<extra></extra>") %>%
          layout(xaxis = list(title = x_var), 
                 yaxis = list(title = y_var),
                 title = list(text = sprintf("Heatmap: %s vs %s<br><sub>%s</sub>", y_var, x_var, subtitle)))
      } else {
        # Default fallback to scatter plot
        p <- plot_ly(x = x_data, 
                     y = y_data, 
                     type = "scatter", 
                     mode = "markers",
                     marker = list(size = 8, opacity = 0.7, color = "steelblue"),
                     hovertemplate = paste0("<b>", x_var, "</b>: %{x}<br>", 
                                            "<b>", y_var, "</b>: %{y}<extra></extra>")) %>%
          layout(xaxis = list(title = x_var), 
                 yaxis = list(title = y_var),
                 title = list(text = sprintf("Scatter Plot: %s vs %s<br><sub>%s</sub>", y_var, x_var, subtitle)))
      }
      
      return(p)
      
    }, error = function(e) {
      # Create error plot with detailed message
      cat("Plot Error:", e$message, "\n")
      cat("Traceback:", "\n")
      traceback()
      
      p <- plot_ly(type = "scatter", mode = "markers") %>%
        layout(title = paste("Plot Error - Check Console for Details"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               annotations = list(
                 text = paste("Unable to create plot.\nCheck R console for error details."),
                 x = 0.5, y = 0.5,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE,
                 font = list(size = 16, color = "red")
               ))
      return(p)
    })
  })
  
  # Robust correlation analysis
  output$num_num_correlation <- renderPrint({
    req(rv$filtered_data, input$num_x_var, input$num_y_var)
    
    tryCatch({
      data <- rv$filtered_data
      x_var <- input$num_x_var
      y_var <- input$num_y_var
      
      # Remove rows with NA values
      valid_idx <- complete.cases(data[, c(x_var, y_var)])
      x_values <- as.numeric(data[[x_var]][valid_idx])
      y_values <- as.numeric(data[[y_var]][valid_idx])
      
      # Remove any additional NAs that might have been introduced
      valid_numeric <- !is.na(x_values) & !is.na(y_values)
      x_values <- x_values[valid_numeric]
      y_values <- y_values[valid_numeric]
      
      # Check if we have enough data
      if (length(x_values) < 3) {
        cat("Error: Not enough valid data points for correlation analysis.\n")
        cat("Need at least 3 complete pairs of observations.\n")
        return()
      }
      
      # Check for constant variables
      if (var(x_values, na.rm = TRUE) == 0 || var(y_values, na.rm = TRUE) == 0) {
        cat("Error: One or both variables have zero variance.\n")
        cat("Correlation cannot be calculated for constant variables.\n")
        return()
      }
      
      # Sample data if too large for normality tests
      sample_for_tests <- if (length(x_values) > 5000) {
        set.seed(123)
        sample_idx <- sample(length(x_values), 5000)
        list(x = x_values[sample_idx], y = y_values[sample_idx])
      } else {
        list(x = x_values, y = y_values)
      }
      
      # Test for normality with error handling
      shapiro_x <- tryCatch({
        if (length(sample_for_tests$x) > 3 && length(sample_for_tests$x) <= 5000) {
          shapiro.test(sample_for_tests$x)$p.value
        } else {
          NA
        }
      }, error = function(e) NA)
      
      shapiro_y <- tryCatch({
        if (length(sample_for_tests$y) > 3 && length(sample_for_tests$y) <= 5000) {
          shapiro.test(sample_for_tests$y)$p.value
        } else {
          NA
        }
      }, error = function(e) NA)
      
      # Print correlation results
      cat("Correlation Analysis\n")
      cat("==================\n\n")
      cat("Sample size:", length(x_values), "observations\n\n")
      
      # Pearson correlation (parametric)
      pearson_test <- tryCatch({
        cor.test(x_values, y_values, method = "pearson")
      }, error = function(e) NULL)
      
      if (!is.null(pearson_test)) {
        cat("Pearson correlation coefficient (r):", round(pearson_test$estimate, 4), "\n")
        cat("95% Confidence Interval:", paste(round(pearson_test$conf.int, 4), collapse = " to "), "\n")
        cat("p-value:", format.pval(pearson_test$p.value, digits = 4), "\n\n")
      } else {
        cat("Pearson correlation: Could not be calculated\n\n")
      }
      
      # Spearman correlation (non-parametric) - more robust
      spearman_test <- tryCatch({
        cor.test(x_values, y_values, method = "spearman", exact = FALSE)
      }, error = function(e) NULL)
      
      if (!is.null(spearman_test)) {
        cat("Spearman rank correlation (rho):", round(spearman_test$estimate, 4), "\n")
        cat("p-value:", format.pval(spearman_test$p.value, digits = 4), "\n\n")
      } else {
        cat("Spearman correlation: Could not be calculated\n\n")
      }
      
      # Provide interpretation if we have valid results
      if (!is.null(pearson_test)) {
        cat("Interpretation:\n")
        cat("===============\n")
        
        # Strength based on absolute value
        corr_strength <- abs(pearson_test$estimate)
        if (corr_strength < 0.3) {
          strength_desc <- "weak"
        } else if (corr_strength < 0.7) {
          strength_desc <- "moderate"
        } else {
          strength_desc <- "strong"
        }
        
        # Direction
        direction <- ifelse(pearson_test$estimate > 0, "positive", "negative")
        
        # Significance
        significance <- ifelse(pearson_test$p.value < 0.05, 
                               "statistically significant", 
                               "not statistically significant")
        
        cat("- There is a", strength_desc, direction, "correlation between", x_var, "and", y_var, "\n")
        cat("- This correlation is", significance, "at the 0.05 level\n")
        
        # Normality advice
        if (!is.na(shapiro_x) && !is.na(shapiro_y) && (shapiro_x < 0.05 || shapiro_y < 0.05)) {
          cat("- Note: At least one variable appears to be non-normally distributed\n")
          cat("  Therefore, the Spearman correlation may be more appropriate\n")
        } else if (is.na(shapiro_x) || is.na(shapiro_y)) {
          cat("- Note: Normality could not be assessed (dataset too large or small)\n")
          cat("  Consider both Pearson and Spearman results\n")
        }
        
        # R-squared (coefficient of determination)
        r_squared <- pearson_test$estimate^2
        cat("- R² value:", round(r_squared, 4), "\n")
        cat("  This means approximately", round(r_squared * 100, 1), 
            "% of the variation in", y_var, "can be explained by", x_var, "\n")
      }
      
    }, error = function(e) {
      cat("Error in correlation analysis:", e$message, "\n")
      cat("Please check that both variables are numeric and contain valid data.\n")
    })
  })
  
  output$num_num_correlation_download_ui <- renderUI({
    req(rv$filtered_data, input$num_x_var, input$num_y_var)
    
    tagList(
      br(),
      h5("Download Correlation Results:"),
      div(style = "margin-bottom: 10px;",
          downloadButton("download_correlation_txt", "Download as Text", 
                         class = "btn btn-primary btn-sm", style = "margin-right: 5px;"),
          downloadButton("download_correlation_csv", "Download as CSV", 
                         class = "btn btn-success btn-sm")
      )
    )
  })
  
  # ========== ROBUST NUMERIC VS CATEGORICAL ANALYSIS ==========
  
  # UI for numeric-categorical variable selection with robustness
  output$num_cat_var_selector <- renderUI({
    req(rv$filtered_data)
    
    tryCatch({
      # Get numeric columns with safe type checking
      numeric_cols <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) {
        tryCatch(is.numeric(x) || is.integer(x), error = function(e) FALSE)
      })]
      
      # Get categorical columns with safe type checking
      categorical_cols <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) {
        tryCatch({
          is.factor(x) || is.character(x) || is.logical(x)
        }, error = function(e) FALSE)
      })]
      
      # Also include numeric columns with few unique values as potential categorical variables
      for (col in names(rv$filtered_data)) {
        tryCatch({
          col_data <- rv$filtered_data[[col]]
          if ((is.numeric(col_data) || is.integer(col_data)) && 
              length(unique(col_data[!is.na(col_data)])) <= 15 && 
              length(unique(col_data[!is.na(col_data)])) > 1 &&
              !(col %in% categorical_cols)) {
            categorical_cols <- c(categorical_cols, col)
          }
        }, error = function(e) {
          # Skip this column if there's an error
        })
      }
      
      if (length(numeric_cols) == 0 || length(categorical_cols) == 0) {
        return(HTML("<div class='alert alert-warning'>Need at least one numeric and one categorical variable for this analysis.</div>"))
      }
      
      # Add data size warning
      n_rows <- nrow(rv$filtered_data)
      size_warning <- if (n_rows > 50000) {
        HTML("<div class='alert alert-info'>Large dataset detected. Analysis may be sampled for performance.</div>")
      } else NULL
      
      tagList(
        size_warning,
        selectInput("num_cat_num_var", "Numeric Variable:", choices = numeric_cols, selected = numeric_cols[1]),
        selectInput("num_cat_cat_var", "Categorical Variable:", choices = categorical_cols, selected = categorical_cols[1])
      )
      
    }, error = function(e) {
      return(HTML(paste("<div class='alert alert-danger'>Error detecting variables:", e$message, "</div>")))
    })
  })
  
  # Generate robust numeric vs categorical plot
  output$num_cat_plot <- renderPlotly({
    req(rv$filtered_data, input$num_cat_num_var, input$num_cat_cat_var, input$num_cat_plot_type)
    
    tryCatch({
      # Get data and variables
      data <- rv$filtered_data
      num_var <- input$num_cat_num_var
      cat_var <- input$num_cat_cat_var
      
      # Ensure categorical variable is treated as factor with safe conversion
      tryCatch({
        if (!is.factor(data[[cat_var]])) {
          data[[cat_var]] <- as.factor(as.character(data[[cat_var]]))
        }
      }, error = function(e) {
        data[[cat_var]] <- factor(data[[cat_var]], exclude = NULL)
      })
      
      # Remove rows with missing values in either variable
      valid_rows <- complete.cases(data[, c(num_var, cat_var)])
      data <- data[valid_rows, ]
      
      # Check if we have enough data after filtering
      if (nrow(data) < 2) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "Not enough data points after removing missing values"))
      }
      
      # Sample data if too large (for performance)
      sampled <- FALSE
      original_size <- nrow(data)
      if (nrow(data) > 20000) {
        set.seed(123)
        sample_size <- min(20000, nrow(data))
        data <- data[sample(nrow(data), sample_size), ]
        sampled <- TRUE
      }
      
      # Check number of categories and handle if too many
      n_categories <- length(levels(data[[cat_var]]))
      if (n_categories > 20) {
        # Keep only top 20 most frequent categories
        top_categories <- names(sort(table(data[[cat_var]]), decreasing = TRUE)[1:20])
        data <- data[data[[cat_var]] %in% top_categories, ]
        data[[cat_var]] <- droplevels(data[[cat_var]])
        n_categories <- length(levels(data[[cat_var]]))
      }
      
      # Create subtitle with sampling info
      subtitle <- if (sampled) {
        paste("(Showing", nrow(data), "sampled points from", original_size, "total)")
      } else {
        paste("(", nrow(data), "data points)")
      }
      
      # Create plot based on plot type
      if (input$num_cat_plot_type == "Box Plot") {
        # Set box plot options with safe defaults
        box_notch <- !is.null(input$box_notch) && input$box_notch
        box_points <- is.null(input$box_points) || input$box_points  # Default to TRUE
        
        p <- plot_ly(data = data, 
                     y = ~get(num_var), 
                     x = ~get(cat_var), 
                     type = "box", 
                     notched = box_notch,
                     boxpoints = ifelse(box_points, "all", "false"),
                     jitter = ifelse(box_points && !is.null(input$box_jitter), input$box_jitter, 0.2),
                     marker = list(size = 3, opacity = 0.7)) %>%
          layout(yaxis = list(title = num_var),
                 xaxis = list(title = cat_var, categoryorder = "category ascending"),
                 title = list(text = paste("Box Plot of", num_var, "by", cat_var, "<br><sub>", subtitle, "</sub>")))
        
      } else if (input$num_cat_plot_type == "Violin Plot") {
        # For violin plots, we need to use a different approach since plotly doesn't directly support violin plots
        # We'll create density-based violin-like plots
        
        # Calculate density for each category
        density_data <- data %>%
          group_by(!!sym(cat_var)) %>%
          do({
            if (nrow(.) > 2 && var(.[[num_var]], na.rm = TRUE) > 0) {
              tryCatch({
                d <- density(.[[num_var]], na.rm = TRUE)
                data.frame(
                  y = d$x,
                  density = d$y,
                  category = unique(.[[cat_var]])
                )
              }, error = function(e) {
                data.frame(
                  y = .[[num_var]],
                  density = 0,
                  category = unique(.[[cat_var]])
                )
              })
            } else {
              data.frame(
                y = .[[num_var]],
                density = 0,
                category = unique(.[[cat_var]])
              )
            }
          }) %>%
          ungroup()
        
        # Create a box plot as fallback for violin plot functionality
        p <- plot_ly(data = data, 
                     y = ~get(num_var), 
                     x = ~get(cat_var), 
                     type = "box",
                     boxpoints = "all",
                     jitter = 0.3,
                     pointpos = 0,
                     marker = list(size = 3, opacity = 0.5)) %>%
          layout(yaxis = list(title = num_var),
                 xaxis = list(title = cat_var, categoryorder = "category ascending"),
                 title = list(text = paste("Distribution of", num_var, "by", cat_var, "<br><sub>", subtitle, "</sub>")))
        
      } else if (input$num_cat_plot_type == "Bar Chart") {
        # Calculate statistic by group with error handling
        stat_func <- switch(ifelse(is.null(input$bar_stat), "Mean", input$bar_stat),
                            "Mean" = function(x) mean(x, na.rm = TRUE),
                            "Median" = function(x) median(x, na.rm = TRUE),
                            "Sum" = function(x) sum(x, na.rm = TRUE),
                            "Count" = function(x) length(x[!is.na(x)]),
                            function(x) mean(x, na.rm = TRUE)  # Default to mean
        )
        
        # Calculate statistics by group
        agg_data <- tryCatch({
          data %>%
            group_by(!!sym(cat_var)) %>%
            summarise(
              value = stat_func(!!sym(num_var)),
              .groups = 'drop'
            ) %>%
            filter(!is.na(value) & !is.infinite(value))
        }, error = function(e) {
          # Fallback: simple aggregation
          aggregate(data[[num_var]], by = list(data[[cat_var]]), FUN = stat_func, na.rm = TRUE) %>%
            setNames(c(cat_var, "value")) %>%
            filter(!is.na(value) & !is.infinite(value))
        })
        
        if (nrow(agg_data) == 0) {
          return(plotly_empty(type = "bar") %>%
                   layout(title = "No valid data for bar chart"))
        }
        
        p <- plot_ly(data = agg_data, 
                     x = ~get(cat_var), 
                     y = ~value, 
                     type = "bar",
                     marker = list(color = "steelblue")) %>%
          layout(yaxis = list(title = paste(ifelse(is.null(input$bar_stat), "Mean", input$bar_stat), "of", num_var)),
                 xaxis = list(title = cat_var, categoryorder = "category ascending"),
                 title = list(text = paste("Bar Chart of", num_var, "by", cat_var, "<br><sub>", subtitle, "</sub>")))
        
      } else if (input$num_cat_plot_type == "Density Plot") {
        # Create density plot for each category
        categories <- levels(data[[cat_var]])
        
        # Create base plot
        p <- plot_ly(type = "scatter", mode = "lines")
        
        # Add density curve for each category
        for (i in seq_along(categories)) {
          cat_data <- data[data[[cat_var]] == categories[i], num_var]
          cat_data <- cat_data[!is.na(cat_data)]
          
          if (length(cat_data) > 2 && var(cat_data) > 0) {
            tryCatch({
              d <- density(cat_data)
              p <- p %>% add_trace(x = d$x, y = d$y, 
                                   name = categories[i],
                                   type = "scatter", mode = "lines",
                                   fill = "tozeroy", alpha = 0.7)
            }, error = function(e) {
              # Skip categories that can't be plotted
            })
          }
        }
        
        p <- p %>%
          layout(xaxis = list(title = num_var),
                 yaxis = list(title = "Density"),
                 title = list(text = paste("Density Plot of", num_var, "by", cat_var, "<br><sub>", subtitle, "</sub>")))
      }
      
      return(p)
      
    }, error = function(e) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("Error creating plot:", e$message)))
    })
  })
  
  # UI for numeric-categorical plot options
  output$num_cat_plot_options <- renderUI({
    req(input$num_cat_plot_type)
    
    options_list <- tagList()
    
    if (input$num_cat_plot_type == "Box Plot") {
      options_list <- tagList(
        checkboxInput("box_notch", "Show Notches", value = FALSE),
        checkboxInput("box_points", "Show Data Points", value = TRUE),
        conditionalPanel(
          condition = "input.box_points == true",
          sliderInput("box_jitter", "Point Jitter:", min = 0, max = 0.5, value = 0.2, step = 0.05)
        )
      )
    } else if (input$num_cat_plot_type == "Bar Chart") {
      options_list <- tagList(
        selectInput("bar_stat", "Statistic:", 
                    choices = c("Mean", "Median", "Sum", "Count"), 
                    selected = "Mean")
      )
    }
    
    return(options_list)
  })
  
  # Robust group statistics table
  output$num_cat_stats <- renderDT({
    req(rv$filtered_data, input$num_cat_num_var, input$num_cat_cat_var)
    
    tryCatch({
      data <- rv$filtered_data
      num_var <- input$num_cat_num_var
      cat_var <- input$num_cat_cat_var
      
      # Convert categorical variable to factor
      if (!is.factor(data[[cat_var]])) {
        data[[cat_var]] <- as.factor(as.character(data[[cat_var]]))
      }
      
      # Remove rows with missing values
      valid_rows <- complete.cases(data[, c(num_var, cat_var)])
      data <- data[valid_rows, ]
      
      if (nrow(data) == 0) {
        return(data.frame(Message = "No valid data available"))
      }
      
      # Calculate comprehensive statistics by group
      stats_table <- data %>%
        group_by(!!sym(cat_var)) %>%
        summarise(
          Count = n(),
          Mean = round(mean(!!sym(num_var), na.rm = TRUE), 3),
          Median = round(median(!!sym(num_var), na.rm = TRUE), 3),
          SD = round(sd(!!sym(num_var), na.rm = TRUE), 3),
          Min = round(min(!!sym(num_var), na.rm = TRUE), 3),
          Max = round(max(!!sym(num_var), na.rm = TRUE), 3),
          Q1 = round(quantile(!!sym(num_var), 0.25, na.rm = TRUE), 3),
          Q3 = round(quantile(!!sym(num_var), 0.75, na.rm = TRUE), 3),
          .groups = 'drop'
        )
      
      # Rename first column to the actual category variable name
      names(stats_table)[1] <- cat_var
      
      datatable(stats_table, 
                options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
                rownames = FALSE) %>%
        formatRound(columns = c("Mean", "Median", "SD", "Min", "Max", "Q1", "Q3"), digits = 3)
      
    }, error = function(e) {
      return(data.frame(Error = paste("Error calculating statistics:", e$message)))
    })
  })
  
  output$num_cat_stats_download_ui <- renderUI({
    req(rv$filtered_data, input$num_cat_num_var, input$num_cat_cat_var)
    
    tagList(
      br(),
      h5("Download Group Statistics:"),
      div(style = "margin-bottom: 10px;",
          downloadButton("download_group_stats_csv", "Download as CSV", 
                         class = "btn btn-primary btn-sm", style = "margin-right: 5px;"),
          downloadButton("download_group_stats_xlsx", "Download as Excel", 
                         class = "btn btn-success btn-sm")
      )
    )
  })
  
  # ========== ROBUST CATEGORICAL VS CATEGORICAL ANALYSIS ==========
  
  # UI for categorical-categorical variable selection
  output$cat_cat_var_selector <- renderUI({
    req(rv$filtered_data)
    
    tryCatch({
      # Get categorical columns with safe type checking
      categorical_cols <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) {
        tryCatch({
          is.factor(x) || is.character(x) || is.logical(x)
        }, error = function(e) FALSE)
      })]
      
      # Also include numeric columns with few unique values as potential categorical variables
      for (col in names(rv$filtered_data)) {
        tryCatch({
          col_data <- rv$filtered_data[[col]]
          if ((is.numeric(col_data) || is.integer(col_data)) && 
              length(unique(col_data[!is.na(col_data)])) <= 15 && 
              length(unique(col_data[!is.na(col_data)])) > 1 &&
              !(col %in% categorical_cols)) {
            categorical_cols <- c(categorical_cols, col)
          }
        }, error = function(e) {
          # Skip this column if there's an error
        })
      }
      
      if (length(categorical_cols) < 2) {
        return(HTML("<div class='alert alert-warning'>Need at least 2 categorical variables for this analysis.</div>"))
      }
      
      # Add data size warning
      n_rows <- nrow(rv$filtered_data)
      size_warning <- if (n_rows > 50000) {
        HTML("<div class='alert alert-info'>Large dataset detected. Analysis may be sampled for performance.</div>")
      } else NULL
      
      tagList(
        size_warning,
        selectInput("cat_cat_var1", "First Categorical Variable:", 
                    choices = categorical_cols, selected = categorical_cols[1]),
        selectInput("cat_cat_var2", "Second Categorical Variable:", 
                    choices = categorical_cols, selected = categorical_cols[2])
      )
      
    }, error = function(e) {
      return(HTML(paste("<div class='alert alert-danger'>Error detecting variables:", e$message, "</div>")))
    })
  })
  
  # Generate robust categorical vs categorical plot
  output$cat_cat_plot <- renderPlotly({
    req(rv$filtered_data, input$cat_cat_var1, input$cat_cat_var2, input$cat_cat_plot_type)
    
    tryCatch({
      # Get data and variables
      data <- rv$filtered_data
      var1 <- input$cat_cat_var1
      var2 <- input$cat_cat_var2
      
      # Convert to factors with safe conversion
      tryCatch({
        if (!is.factor(data[[var1]])) {
          data[[var1]] <- as.factor(as.character(data[[var1]]))
        }
        if (!is.factor(data[[var2]])) {
          data[[var2]] <- as.factor(as.character(data[[var2]]))
        }
      }, error = function(e) {
        data[[var1]] <- factor(data[[var1]], exclude = NULL)
        data[[var2]] <- factor(data[[var2]], exclude = NULL)
      })
      
      # Remove rows with missing values in either variable
      valid_rows <- complete.cases(data[, c(var1, var2)])
      data <- data[valid_rows, ]
      
      if (nrow(data) < 1) {
        return(plotly_empty(type = "bar") %>%
                 layout(title = "No valid data available"))
      }
      
      # Handle too many categories by keeping only the most frequent ones
      if (length(levels(data[[var1]])) > 15) {
        top_cats1 <- names(sort(table(data[[var1]]), decreasing = TRUE)[1:15])
        data <- data[data[[var1]] %in% top_cats1, ]
        data[[var1]] <- droplevels(data[[var1]])
      }
      
      if (length(levels(data[[var2]])) > 15) {
        top_cats2 <- names(sort(table(data[[var2]]), decreasing = TRUE)[1:15])
        data <- data[data[[var2]] %in% top_cats2, ]
        data[[var2]] <- droplevels(data[[var2]])
      }
      
      # Create contingency table
      cont_table <- table(data[[var1]], data[[var2]])
      
      if (sum(cont_table) == 0) {
        return(plotly_empty(type = "bar") %>%
                 layout(title = "No data available for selected categories"))
      }
      
      # Create plot based on plot type
      if (input$cat_cat_plot_type == "Stacked Bar Chart") {
        # Convert table to data frame for plotting
        plot_data <- as.data.frame(cont_table)
        names(plot_data) <- c(var1, var2, "Freq")
        
        p <- plot_ly(data = plot_data, 
                     x = ~get(var1), 
                     y = ~Freq, 
                     color = ~get(var2),
                     type = "bar") %>%
          layout(barmode = "stack",
                 xaxis = list(title = var1),
                 yaxis = list(title = "Count"),
                 title = list(text = paste("Stacked Bar Chart:", var1, "vs", var2)))
        
      } else if (input$cat_cat_plot_type == "Grouped Bar Chart") {
        # Convert table to data frame for plotting
        plot_data <- as.data.frame(cont_table)
        names(plot_data) <- c(var1, var2, "Freq")
        
        p <- plot_ly(data = plot_data, 
                     x = ~get(var1), 
                     y = ~Freq, 
                     color = ~get(var2),
                     type = "bar") %>%
          layout(barmode = "group",
                 xaxis = list(title = var1),
                 yaxis = list(title = "Count"),
                 title = list(text = paste("Grouped Bar Chart:", var1, "vs", var2)))
        
      } else if (input$cat_cat_plot_type == "Heatmap") {
        # Create heatmap from contingency table
        p <- plot_ly(z = ~as.matrix(cont_table), 
                     type = "heatmap",
                     x = colnames(cont_table),
                     y = rownames(cont_table),
                     colorscale = "Blues",
                     hovertemplate = paste("Row:", "%{y}<br>",
                                           "Column:", "%{x}<br>",
                                           "Count:", "%{z}<extra></extra>")) %>%
          layout(xaxis = list(title = var2),
                 yaxis = list(title = var1),
                 title = list(text = paste("Heatmap:", var1, "vs", var2)))
        
      } else if (input$cat_cat_plot_type == "Mosaic Plot") {
        # For mosaic plot, we'll create a proportion-based stacked bar chart
        # Calculate proportions
        prop_table <- prop.table(cont_table, margin = 1)
        plot_data <- as.data.frame(prop_table)
        names(plot_data) <- c(var1, var2, "Proportion")
        
        p <- plot_ly(data = plot_data, 
                     x = ~get(var1), 
                     y = ~Proportion, 
                     color = ~get(var2),
                     type = "bar") %>%
          layout(barmode = "stack",
                 xaxis = list(title = var1),
                 yaxis = list(title = "Proportion"),
                 title = list(text = paste("Mosaic Plot (Proportions):", var1, "vs", var2)))
      }
      
      return(p)
      
    }, error = function(e) {
      return(plotly_empty(type = "bar") %>%
               layout(title = paste("Error creating plot:", e$message)))
    })
  })
  
  # UI options for categorical vs categorical plots
  output$cat_cat_plot_options <- renderUI({
    req(input$cat_cat_plot_type)
    
    # Most categorical vs categorical plots don't need many options
    # We can add specific options here if needed
    
    if (input$cat_cat_plot_type == "Heatmap") {
      return(tagList(
        selectInput("cat_heatmap_colorscale", "Color Scale:", 
                    choices = c("Blues", "Viridis" = "viridis", "Plasma" = "plasma", 
                                "YlOrRd" = "YlOrRd", "YlGnBu" = "YlGnBu"),
                    selected = "Blues")
      ))
    }
    
    return(NULL)
  })
  
  # Generate contingency table
  output$cat_cat_table <- renderDT({
  req(rv$filtered_data, input$cat_cat_var1, input$cat_cat_var2)
  
  tryCatch({
    data <- rv$filtered_data
    var1 <- input$cat_cat_var1
    var2 <- input$cat_cat_var2
    
    # Convert to factors
    if (!is.factor(data[[var1]])) {
      data[[var1]] <- as.factor(as.character(data[[var1]]))
    }
    if (!is.factor(data[[var2]])) {
      data[[var2]] <- as.factor(as.character(data[[var2]]))
    }
    
    # Remove rows with missing values
    valid_rows <- complete.cases(data[, c(var1, var2)])
    data <- data[valid_rows, ]
    
    if (nrow(data) == 0) {
      return(data.frame(Message = "No valid data available"))
    }
    
    # Create contingency table
    cont_table <- table(data[[var1]], data[[var2]])
    
    # Convert to data frame for display
    table_df <- as.data.frame.matrix(cont_table)
    
    # Add row totals
    table_df$Total <- rowSums(table_df)
    
    # Add column totals
    col_totals <- c(colSums(table_df[, -ncol(table_df)]), sum(table_df$Total))
    names(col_totals)[length(col_totals)] <- "Total"
    
    # Combine
    table_df <- rbind(table_df, col_totals)
    rownames(table_df)[nrow(table_df)] <- "Total"
    
    # Add row names as first column
    table_df <- cbind(Category = rownames(table_df), table_df)
    rownames(table_df) <- NULL
    
    # Set proper column name
    names(table_df)[1] <- paste(var1, "\\", var2)
    
    # FIXED: Use column names instead of column numbers
    datatable(table_df, 
              options = list(pageLength = 15, scrollX = TRUE, dom = 't'),
              rownames = FALSE) %>%
      formatStyle("Total", fontWeight = "bold") %>%              # Use column name
      formatStyle(0, target = "row", fontWeight = "bold", 
                  backgroundColor = styleEqual(nrow(table_df), "lightgray"))  # Style last row
    
  }, error = function(e) {
    return(data.frame(Error = paste("Error creating table:", e$message)))
  })
})
  
  output$cat_cat_table_download_ui <- renderUI({
    req(rv$filtered_data, input$cat_cat_var1, input$cat_cat_var2)
    
    tagList(
      br(),
      h5("Download Contingency Table:"),
      div(style = "margin-bottom: 10px;",
          downloadButton("download_contingency_csv", "Download Table as CSV", 
                         class = "btn btn-primary btn-sm", style = "margin-right: 5px;"),
          downloadButton("download_contingency_xlsx", "Download Table as Excel", 
                         class = "btn btn-success btn-sm")
      )
    )
  })
  
  
  # Add chi-square test results for categorical vs categorical
  output$cat_cat_chisq <- renderPrint({
    req(rv$filtered_data, input$cat_cat_var1, input$cat_cat_var2)
    
    tryCatch({
      data <- rv$filtered_data
      var1 <- input$cat_cat_var1
      var2 <- input$cat_cat_var2
      
      # Convert to factors and remove missing values
      if (!is.factor(data[[var1]])) {
        data[[var1]] <- as.factor(as.character(data[[var1]]))
      }
      if (!is.factor(data[[var2]])) {
        data[[var2]] <- as.factor(as.character(data[[var2]]))
      }
      
      valid_rows <- complete.cases(data[, c(var1, var2)])
      data <- data[valid_rows, ]
      
      if (nrow(data) < 5) {
        cat("Error: Not enough data for chi-square test (need at least 5 observations)\n")
        return()
      }
      
      # Create contingency table
      cont_table <- table(data[[var1]], data[[var2]])
      
      # Check if chi-square test assumptions are met
      expected_freq <- chisq.test(cont_table)$expected
      cells_less_than_5 <- sum(expected_freq < 5)
      total_cells <- length(expected_freq)
      
      # Perform chi-square test
      chisq_result <- chisq.test(cont_table)
      
      cat("Chi-Square Test of Independence\n")
      cat("==============================\n\n")
      cat("Variables:", var1, "and", var2, "\n")
      cat("Sample size:", sum(cont_table), "observations\n\n")
      
      cat("Chi-square statistic:", round(chisq_result$statistic, 4), "\n")
      cat("Degrees of freedom:", chisq_result$parameter, "\n")
      cat("p-value:", format.pval(chisq_result$p.value, digits = 4), "\n\n")
      
      # Interpretation
      cat("Interpretation:\n")
      cat("===============\n")
      
      if (chisq_result$p.value < 0.05) {
        cat("- There IS a statistically significant association between", var1, "and", var2, "\n")
        cat("- We reject the null hypothesis of independence (p < 0.05)\n")
      } else {
        cat("- There is NO statistically significant association between", var1, "and", var2, "\n")
        cat("- We fail to reject the null hypothesis of independence (p >= 0.05)\n")
      }
      
      # Check test assumptions
      cat("\nTest Assumptions:\n")
      cat("=================\n")
      if (cells_less_than_5 == 0) {
        cat("- All expected frequencies are >= 5: ASSUMPTION MET\n")
      } else if (cells_less_than_5 / total_cells <= 0.2) {
        cat("- Less than 20% of cells have expected frequency < 5: ASSUMPTION MET\n")
      } else {
        cat("- WARNING: More than 20% of cells have expected frequency < 5\n")
        cat("- Consider using Fisher's exact test or combining categories\n")
      }
      
      # Effect size (Cramér's V)
      cramers_v <- sqrt(chisq_result$statistic / (sum(cont_table) * (min(dim(cont_table)) - 1)))
      cat("- Cramér's V (effect size):", round(cramers_v, 4), "\n")
      
      if (cramers_v < 0.1) {
        effect_size <- "negligible"
      } else if (cramers_v < 0.3) {
        effect_size <- "small"
      } else if (cramers_v < 0.5) {
        effect_size <- "medium"
      } else {
        effect_size <- "large"
      }
      
      cat("- Effect size interpretation:", effect_size, "\n")
      
    }, error = function(e) {
      cat("Error in chi-square analysis:", e$message, "\n")
      cat("Please check that both variables are categorical and contain valid data.\n")
    })
  })
  
  output$cat_cat_chisq_download_ui <- renderUI({
    req(rv$filtered_data, input$cat_cat_var1, input$cat_cat_var2)
    
    tagList(
      br(),
      h5("Download Chi-Square Results:"),
      div(style = "margin-bottom: 10px;",
          downloadButton("download_chisq_txt", "Download Results as Text", 
                         class = "btn btn-primary btn-sm", style = "margin-right: 5px;"),
          downloadButton("download_chisq_csv", "Download Summary as CSV", 
                         class = "btn btn-success btn-sm")
      )
    )
  })

  # DOWNLOAD HANDLERS FOR BIVARIATE ANALYSIS ==================
  
  # Download handlers for Correlation Analysis
  output$download_correlation_txt <- downloadHandler(
    filename = function() {
      paste0("correlation_analysis_", Sys.Date(), ".txt")
    },
    content = function(file) {
      tryCatch({
        # Capture the correlation analysis output
        data <- rv$filtered_data
        x_var <- input$num_x_var
        y_var <- input$num_y_var
        
        # Get clean data
        valid_idx <- complete.cases(data[, c(x_var, y_var)])
        x_values <- as.numeric(data[[x_var]][valid_idx])
        y_values <- as.numeric(data[[y_var]][valid_idx])
        
        valid_numeric <- !is.na(x_values) & !is.na(y_values)
        x_values <- x_values[valid_numeric]
        y_values <- y_values[valid_numeric]
        
        # Generate text output
        sink(file)
        cat("Correlation Analysis Results\n")
        cat("============================\n\n")
        cat("Dataset:", deparse(substitute(rv$filtered_data)), "\n")
        cat("Variables:", x_var, "vs", y_var, "\n")
        cat("Date:", Sys.Date(), "\n")
        cat("Sample size:", length(x_values), "observations\n\n")
        
        # Pearson correlation
        pearson_test <- cor.test(x_values, y_values, method = "pearson")
        cat("Pearson correlation coefficient (r):", round(pearson_test$estimate, 4), "\n")
        cat("95% Confidence Interval:", paste(round(pearson_test$conf.int, 4), collapse = " to "), "\n")
        cat("p-value:", format.pval(pearson_test$p.value, digits = 4), "\n\n")
        
        # Spearman correlation
        spearman_test <- cor.test(x_values, y_values, method = "spearman", exact = FALSE)
        cat("Spearman rank correlation (rho):", round(spearman_test$estimate, 4), "\n")
        cat("p-value:", format.pval(spearman_test$p.value, digits = 4), "\n\n")
        
        # Interpretation
        corr_strength <- abs(pearson_test$estimate)
        strength_desc <- ifelse(corr_strength < 0.3, "weak", 
                                ifelse(corr_strength < 0.7, "moderate", "strong"))
        direction <- ifelse(pearson_test$estimate > 0, "positive", "negative")
        significance <- ifelse(pearson_test$p.value < 0.05, "statistically significant", "not statistically significant")
        
        cat("Interpretation:\n")
        cat("There is a", strength_desc, direction, "correlation that is", significance, "\n")
        cat("R-squared:", round(pearson_test$estimate^2, 4), "\n")
        
        sink()
      }, error = function(e) {
        writeLines("Error generating correlation report", file)
      })
    }
  )
  
  output$download_correlation_csv <- downloadHandler(
    filename = function() {
      paste0("correlation_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        data <- rv$filtered_data
        x_var <- input$num_x_var
        y_var <- input$num_y_var
        
        valid_idx <- complete.cases(data[, c(x_var, y_var)])
        x_values <- as.numeric(data[[x_var]][valid_idx])
        y_values <- as.numeric(data[[y_var]][valid_idx])
        
        valid_numeric <- !is.na(x_values) & !is.na(y_values)
        x_values <- x_values[valid_numeric]
        y_values <- y_values[valid_numeric]
        
        pearson_test <- cor.test(x_values, y_values, method = "pearson")
        spearman_test <- cor.test(x_values, y_values, method = "spearman", exact = FALSE)
        
        summary_df <- data.frame(
          Variable_X = x_var,
          Variable_Y = y_var,
          Sample_Size = length(x_values),
          Pearson_r = round(pearson_test$estimate, 4),
          Pearson_p_value = pearson_test$p.value,
          Pearson_CI_Lower = round(pearson_test$conf.int[1], 4),
          Pearson_CI_Upper = round(pearson_test$conf.int[2], 4),
          Spearman_rho = round(spearman_test$estimate, 4),
          Spearman_p_value = spearman_test$p.value,
          R_squared = round(pearson_test$estimate^2, 4)
        )
        
        write.csv(summary_df, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = "Could not generate correlation summary"), file, row.names = FALSE)
      })
    }
  )
  
  # Download handlers for Group Statistics
  output$download_group_stats_csv <- downloadHandler(
    filename = function() {
      paste0("group_statistics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        data <- rv$filtered_data
        num_var <- input$num_cat_num_var
        cat_var <- input$num_cat_cat_var
        
        if (!is.factor(data[[cat_var]])) {
          data[[cat_var]] <- as.factor(as.character(data[[cat_var]]))
        }
        
        valid_rows <- complete.cases(data[, c(num_var, cat_var)])
        data <- data[valid_rows, ]
        
        stats_table <- data %>%
          group_by(!!sym(cat_var)) %>%
          summarise(
            Count = n(),
            Mean = round(mean(!!sym(num_var), na.rm = TRUE), 3),
            Median = round(median(!!sym(num_var), na.rm = TRUE), 3),
            SD = round(sd(!!sym(num_var), na.rm = TRUE), 3),
            Min = round(min(!!sym(num_var), na.rm = TRUE), 3),
            Max = round(max(!!sym(num_var), na.rm = TRUE), 3),
            Q1 = round(quantile(!!sym(num_var), 0.25, na.rm = TRUE), 3),
            Q3 = round(quantile(!!sym(num_var), 0.75, na.rm = TRUE), 3),
            .groups = 'drop'
          )
        
        names(stats_table)[1] <- cat_var
        write.csv(stats_table, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = "Could not generate group statistics"), file, row.names = FALSE)
      })
    }
  )
  
  output$download_group_stats_xlsx <- downloadHandler(
    filename = function() {
      paste0("group_statistics_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Note: You'll need to install and load openxlsx package
      # install.packages("openxlsx")
      # library(openxlsx)
      
      tryCatch({
        data <- rv$filtered_data
        num_var <- input$num_cat_num_var
        cat_var <- input$num_cat_cat_var
        
        if (!is.factor(data[[cat_var]])) {
          data[[cat_var]] <- as.factor(as.character(data[[cat_var]]))
        }
        
        valid_rows <- complete.cases(data[, c(num_var, cat_var)])
        data <- data[valid_rows, ]
        
        stats_table <- data %>%
          group_by(!!sym(cat_var)) %>%
          summarise(
            Count = n(),
            Mean = round(mean(!!sym(num_var), na.rm = TRUE), 3),
            Median = round(median(!!sym(num_var), na.rm = TRUE), 3),
            SD = round(sd(!!sym(num_var), na.rm = TRUE), 3),
            Min = round(min(!!sym(num_var), na.rm = TRUE), 3),
            Max = round(max(!!sym(num_var), na.rm = TRUE), 3),
            Q1 = round(quantile(!!sym(num_var), 0.25, na.rm = TRUE), 3),
            Q3 = round(quantile(!!sym(num_var), 0.75, na.rm = TRUE), 3),
            .groups = 'drop'
          )
        
        names(stats_table)[1] <- cat_var
        
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(stats_table, file)
        } else {
          # Fallback to CSV if openxlsx not available
          write.csv(stats_table, file, row.names = FALSE)
        }
      }, error = function(e) {
        write.csv(data.frame(Error = "Could not generate group statistics"), file, row.names = FALSE)
      })
    }
  )
  
  # Download handlers for Contingency Table
  output$download_contingency_csv <- downloadHandler(
    filename = function() {
      paste0("contingency_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        data <- rv$filtered_data
        var1 <- input$cat_cat_var1
        var2 <- input$cat_cat_var2
        
        if (!is.factor(data[[var1]])) {
          data[[var1]] <- as.factor(as.character(data[[var1]]))
        }
        if (!is.factor(data[[var2]])) {
          data[[var2]] <- as.factor(as.character(data[[var2]]))
        }
        
        valid_rows <- complete.cases(data[, c(var1, var2)])
        data <- data[valid_rows, ]
        
        cont_table <- table(data[[var1]], data[[var2]])
        table_df <- as.data.frame.matrix(cont_table)
        table_df$Total <- rowSums(table_df)
        
        col_totals <- c(colSums(table_df[, -ncol(table_df)]), sum(table_df$Total))
        names(col_totals)[length(col_totals)] <- "Total"
        
        table_df <- rbind(table_df, col_totals)
        rownames(table_df)[nrow(table_df)] <- "Total"
        
        table_df <- cbind(Category = rownames(table_df), table_df)
        rownames(table_df) <- NULL
        names(table_df)[1] <- paste(var1, "vs", var2)
        
        write.csv(table_df, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = "Could not generate contingency table"), file, row.names = FALSE)
      })
    }
  )
  
  output$download_contingency_xlsx <- downloadHandler(
    filename = function() {
      paste0("contingency_table_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tryCatch({
        data <- rv$filtered_data
        var1 <- input$cat_cat_var1
        var2 <- input$cat_cat_var2
        
        if (!is.factor(data[[var1]])) {
          data[[var1]] <- as.factor(as.character(data[[var1]]))
        }
        if (!is.factor(data[[var2]])) {
          data[[var2]] <- as.factor(as.character(data[[var2]]))
        }
        
        valid_rows <- complete.cases(data[, c(var1, var2)])
        data <- data[valid_rows, ]
        
        cont_table <- table(data[[var1]], data[[var2]])
        table_df <- as.data.frame.matrix(cont_table)
        table_df$Total <- rowSums(table_df)
        
        col_totals <- c(colSums(table_df[, -ncol(table_df)]), sum(table_df$Total))
        names(col_totals)[length(col_totals)] <- "Total"
        
        table_df <- rbind(table_df, col_totals)
        rownames(table_df)[nrow(table_df)] <- "Total"
        
        table_df <- cbind(Category = rownames(table_df), table_df)
        rownames(table_df) <- NULL
        names(table_df)[1] <- paste(var1, "vs", var2)
        
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(table_df, file)
        } else {
          write.csv(table_df, file, row.names = FALSE)
        }
      }, error = function(e) {
        write.csv(data.frame(Error = "Could not generate contingency table"), file, row.names = FALSE)
      })
    }
  )
  
  # Download handlers for Chi-Square Test
  output$download_chisq_txt <- downloadHandler(
    filename = function() {
      paste0("chisquare_test_", Sys.Date(), ".txt")
    },
    content = function(file) {
      tryCatch({
        data <- rv$filtered_data
        var1 <- input$cat_cat_var1
        var2 <- input$cat_cat_var2
        
        if (!is.factor(data[[var1]])) {
          data[[var1]] <- as.factor(as.character(data[[var1]]))
        }
        if (!is.factor(data[[var2]])) {
          data[[var2]] <- as.factor(as.character(data[[var2]]))
        }
        
        valid_rows <- complete.cases(data[, c(var1, var2)])
        data <- data[valid_rows, ]
        
        cont_table <- table(data[[var1]], data[[var2]])
        chisq_result <- chisq.test(cont_table)
        expected_freq <- chisq_result$expected
        cells_less_than_5 <- sum(expected_freq < 5)
        total_cells <- length(expected_freq)
        
        sink(file)
        cat("Chi-Square Test of Independence\n")
        cat("==============================\n\n")
        cat("Dataset:", deparse(substitute(rv$filtered_data)), "\n")
        cat("Variables:", var1, "and", var2, "\n")
        cat("Date:", Sys.Date(), "\n")
        cat("Sample size:", sum(cont_table), "observations\n\n")
        
        cat("Chi-square statistic:", round(chisq_result$statistic, 4), "\n")
        cat("Degrees of freedom:", chisq_result$parameter, "\n")
        cat("p-value:", format.pval(chisq_result$p.value, digits = 4), "\n\n")
        
        cat("Interpretation:\n")
        if (chisq_result$p.value < 0.05) {
          cat("There IS a statistically significant association (p < 0.05)\n")
        } else {
          cat("There is NO statistically significant association (p >= 0.05)\n")
        }
        
        cramers_v <- sqrt(chisq_result$statistic / (sum(cont_table) * (min(dim(cont_table)) - 1)))
        cat("Cramér's V (effect size):", round(cramers_v, 4), "\n")
        
        sink()
      }, error = function(e) {
        writeLines("Error generating chi-square report", file)
      })
    }
  )
  
  output$download_chisq_csv <- downloadHandler(
    filename = function() {
      paste0("chisquare_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        data <- rv$filtered_data
        var1 <- input$cat_cat_var1
        var2 <- input$cat_cat_var2
        
        if (!is.factor(data[[var1]])) {
          data[[var1]] <- as.factor(as.character(data[[var1]]))
        }
        if (!is.factor(data[[var2]])) {
          data[[var2]] <- as.factor(as.character(data[[var2]]))
        }
        
        valid_rows <- complete.cases(data[, c(var1, var2)])
        data <- data[valid_rows, ]
        
        cont_table <- table(data[[var1]], data[[var2]])
        chisq_result <- chisq.test(cont_table)
        cramers_v <- sqrt(chisq_result$statistic / (sum(cont_table) * (min(dim(cont_table)) - 1)))
        
        summary_df <- data.frame(
          Variable_1 = var1,
          Variable_2 = var2,
          Sample_Size = sum(cont_table),
          Chi_Square_Statistic = round(chisq_result$statistic, 4),
          Degrees_of_Freedom = chisq_result$parameter,
          P_Value = chisq_result$p.value,
          Cramers_V = round(cramers_v, 4),
          Significant = ifelse(chisq_result$p.value < 0.05, "Yes", "No")
        )
        
        write.csv(summary_df, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = "Could not generate chi-square summary"), file, row.names = FALSE)
      })
    }
  )

# MULTIVARIATE ANALYSIS MODULE ================================

# Helper function to get numeric columns
get_numeric_columns <- function(data) {
  numeric_cols <- sapply(data, function(x) is.numeric(x) || is.integer(x))
  names(data)[numeric_cols]
}

# Helper function to get categorical columns
get_categorical_columns <- function(data) {
  cat_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
  names(data)[cat_cols]
}

# CORRELATION MATRIX OPTIONS ===================================

output$corr_matrix_options <- renderUI({
  req(rv$filtered_data)
  numeric_vars <- get_numeric_columns(rv$filtered_data)
  
  if (length(numeric_vars) < 2) {
    return(div("Not enough numeric variables for correlation analysis."))
  }
  
  tagList(
    pickerInput(
      "corr_vars", "Select Variables:",
      choices = numeric_vars,
      selected = numeric_vars[1:min(length(numeric_vars), 6)],
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    selectInput("corr_method", "Correlation Method:",
                choices = c("Pearson" = "pearson", 
                            "Spearman" = "spearman", 
                            "Kendall" = "kendall"),
                selected = "pearson"),
    selectInput("corr_vis_type", "Visualization Type:",
                choices = c("Heatmap", "Circle Plot", "Network Plot"),
                selected = "Heatmap"),
    conditionalPanel(
      condition = "input.corr_vis_type == 'Network Plot'",
      sliderInput("corr_threshold", "Correlation Threshold:",
                  min = 0, max = 1, value = 0.3, step = 0.1)
    ),
    checkboxInput("show_corr_values", "Show Values", TRUE),
    actionButton("update_corr", "Update Analysis", 
                 style = "color: #fff; background-color: #007bff; border-color: #007bff")
  )
})

# CORRELATION MATRIX PLOT ======================================

output$corr_matrix_plot <- renderPlotly({
  req(rv$filtered_data, input$corr_vars, input$update_corr)
  
  isolate({
    if (length(input$corr_vars) < 2) {
      return(plotly_empty())
    }
    
    # Prepare data
    data_subset <- rv$filtered_data[, input$corr_vars, drop = FALSE]
    data_subset <- data_subset[complete.cases(data_subset), ]
    
    if (nrow(data_subset) == 0) {
      return(plotly_empty())
    }
    
    # Calculate correlation matrix
    corr_matrix <- cor(data_subset, method = input$corr_method, use = "complete.obs")
    
    if (input$corr_vis_type == "Heatmap") {
      # Create heatmap
      corr_melted <- reshape2::melt(corr_matrix)
      
      p <- ggplot(corr_melted, aes(Var1, Var2, fill = value, text = paste(
        "Variables:", Var1, "vs", Var2, "<br>",
        "Correlation:", round(value, 3)
      ))) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                             midpoint = 0, limit = c(-1, 1), space = "Lab",
                             name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              axis.title = element_blank()) +
        coord_fixed()
      
      if (input$show_corr_values) {
        p <- p + geom_text(aes(label = round(value, 2)), color = "black", size = 3)
      }
      
      ggplotly(p, tooltip = "text") %>%
        layout(title = paste("Correlation Matrix -", stringr::str_to_title(input$corr_method)))
      
    } else if (input$corr_vis_type == "Circle Plot") {
      # Create circle plot
      corr_melted <- reshape2::melt(corr_matrix)
      corr_melted$abs_value <- abs(corr_melted$value)
      
      p <- ggplot(corr_melted, aes(Var1, Var2, text = paste(
        "Variables:", Var1, "vs", Var2, "<br>",
        "Correlation:", round(value, 3)
      ))) +
        geom_point(aes(size = abs_value, color = value)) +
        scale_size_continuous(range = c(1, 15), name = "Abs. Correlation") +
        scale_color_gradient2(low = "blue", high = "red", mid = "white",
                              midpoint = 0, limit = c(-1, 1), space = "Lab",
                              name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              axis.title = element_blank()) +
        coord_fixed()
      
      ggplotly(p, tooltip = "text") %>%
        layout(title = paste("Correlation Circle Plot -", stringr::str_to_title(input$corr_method)))
      
    } else if (input$corr_vis_type == "Network Plot") {
      # Create network plot
      corr_df <- expand.grid(Var1 = rownames(corr_matrix), Var2 = colnames(corr_matrix))
      corr_df$correlation <- as.vector(corr_matrix)
      corr_df <- corr_df[corr_df$Var1 != corr_df$Var2, ]
      corr_df <- corr_df[abs(corr_df$correlation) >= input$corr_threshold, ]
      
      if (nrow(corr_df) == 0) {
        return(plotly_empty())
      }
      
      # Create edge list for network
      nodes <- data.frame(id = unique(c(as.character(corr_df$Var1), as.character(corr_df$Var2))))
      edges <- corr_df[, c("Var1", "Var2", "correlation")]
      names(edges) <- c("from", "to", "weight")
      
      # Simple network layout (circular)
      n_nodes <- nrow(nodes)
      angles <- seq(0, 2*pi, length.out = n_nodes + 1)[1:n_nodes]
      nodes$x <- cos(angles)
      nodes$y <- sin(angles)
      
      # Create network plot
      p <- plot_ly() %>%
        # Add edges
        add_segments(data = edges, 
                     x = ~nodes$x[match(from, nodes$id)], 
                     y = ~nodes$y[match(from, nodes$id)],
                     xend = ~nodes$x[match(to, nodes$id)], 
                     yend = ~nodes$y[match(to, nodes$id)],
                     line = list(width = ~abs(weight) * 5, 
                                 color = ~ifelse(weight > 0, "red", "blue")),
                     hoverinfo = "text",
                     text = ~paste("Correlation:", round(weight, 3))) %>%
        # Add nodes
        add_markers(data = nodes, x = ~x, y = ~y, 
                    text = ~id, textposition = "middle center",
                    marker = list(size = 20, color = "lightblue", 
                                  line = list(width = 2, color = "darkblue")),
                    hoverinfo = "text") %>%
        layout(title = "Correlation Network", 
               showlegend = FALSE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      p
    }
  })
})

# SCATTER PLOT MATRIX OPTIONS ==================================

output$scatter_matrix_options <- renderUI({
  req(rv$filtered_data)
  numeric_vars <- get_numeric_columns(rv$filtered_data)
  cat_vars <- get_categorical_columns(rv$filtered_data)
  
  if (length(numeric_vars) < 2) {
    return(div("Not enough numeric variables for scatter plot matrix."))
  }
  
  tagList(
    pickerInput(
      "scatter_matrix_vars", "Select Variables:",
      choices = numeric_vars,
      selected = numeric_vars[1:min(length(numeric_vars), 4)],
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    conditionalPanel(
      condition = "input.scatter_matrix_vars != null && input.scatter_matrix_vars.length > 0",
      selectInput("scatter_color_var", "Color by Variable (optional):",
                  choices = c("None" = "", cat_vars),
                  selected = "")
    ),
    numericInput("scatter_alpha", "Point Transparency:", 
                 value = 0.6, min = 0.1, max = 1, step = 0.1),
    actionButton("update_scatter_matrix", "Update Plot", 
                 style = "color: #fff; background-color: #007bff; border-color: #007bff")
  )
})

# SCATTER PLOT MATRIX ==========================================

output$scatter_matrix_plot <- renderPlotly({
  req(rv$filtered_data, input$scatter_matrix_vars, input$update_scatter_matrix)
  
  isolate({
    if (length(input$scatter_matrix_vars) < 2) {
      return(plotly_empty())
    }
    
    # Prepare data
    plot_vars <- input$scatter_matrix_vars
    color_var <- if (input$scatter_color_var != "") input$scatter_color_var else NULL
    
    data_subset <- rv$filtered_data[, c(plot_vars, color_var), drop = FALSE]
    data_subset <- data_subset[complete.cases(data_subset), ]
    
    if (nrow(data_subset) == 0) {
      return(plotly_empty())
    }
    
    # Create scatter plot matrix using plotly
    if (is.null(color_var)) {
      fig <- plot_ly(data_subset, type = "splom",
                     dimensions = lapply(plot_vars, function(var) {
                       list(label = var, values = data_subset[[var]])
                     }),
                     marker = list(color = "blue", size = 4, opacity = input$scatter_alpha),
                     showupperhalf = FALSE,
                     diagonal = list(visible = FALSE))
    } else {
      color_values <- data_subset[[color_var]]
      fig <- plot_ly(data_subset, type = "splom",
                     dimensions = lapply(plot_vars, function(var) {
                       list(label = var, values = data_subset[[var]])
                     }),
                     marker = list(color = color_values, size = 4, opacity = input$scatter_alpha,
                                   colorscale = "Viridis", showscale = TRUE),
                     showupperhalf = FALSE,
                     diagonal = list(visible = FALSE))
    }
    
    fig %>% layout(title = "Scatter Plot Matrix")
  })
})

# DIMENSIONALITY REDUCTION OPTIONS =============================

output$dim_red_options <- renderUI({
  req(rv$filtered_data)
  numeric_vars <- get_numeric_columns(rv$filtered_data)
  cat_vars <- get_categorical_columns(rv$filtered_data)
  
  if (length(numeric_vars) < 2) {
    return(div("Not enough numeric variables for dimensionality reduction."))
  }
  
  common_ui <- tagList(
    pickerInput(
      "dim_red_vars", "Select Variables:",
      choices = numeric_vars,
      selected = numeric_vars,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    selectInput("dim_red_color", "Color by Variable (optional):",
                choices = c("None" = "", cat_vars),
                selected = ""),
    checkboxInput("scale_data", "Scale Variables", TRUE)
  )
  
  method_specific <- if (input$dim_red_method == "PCA") {
    tagList(
      numericInput("pca_components", "Number of Components to Display:", 
                   value = 2, min = 2, max = 3, step = 1)
    )
  } else if (input$dim_red_method == "t-SNE") {
    tagList(
      numericInput("tsne_perplexity", "Perplexity:", 
                   value = 30, min = 5, max = 50, step = 5),
      numericInput("tsne_iterations", "Iterations:", 
                   value = 1000, min = 250, max = 2000, step = 250)
    )
  }
  
  tagList(
    common_ui,
    method_specific,
    actionButton("run_dim_reduction", "Run Analysis", 
                 style = "color: #fff; background-color: #28a745; border-color: #28a745")
  )
})

# DIMENSIONALITY REDUCTION PLOT ================================

output$dim_red_plot <- renderPlotly({
  req(rv$filtered_data, input$dim_red_vars, input$run_dim_reduction)
  
  isolate({
    if (length(input$dim_red_vars) < 2) {
      return(plotly_empty())
    }
    
    # Prepare data
    data_subset <- rv$filtered_data[, input$dim_red_vars, drop = FALSE]
    data_subset <- data_subset[complete.cases(data_subset), ]
    
    if (nrow(data_subset) == 0) {
      return(plotly_empty())
    }
    
    # Scale data if requested
    if (input$scale_data) {
      data_subset <- scale(data_subset)
    }
    
    if (input$dim_red_method == "PCA") {
      # Perform PCA
      pca_result <- prcomp(data_subset, center = !input$scale_data, scale. = FALSE)
      
      # Store PCA result for other outputs
      rv$pca_result <- pca_result
      
      # Create plot data
      plot_data <- data.frame(pca_result$x[, 1:min(input$pca_components, ncol(pca_result$x))])
      
      # Add color variable if specified
      color_var <- NULL
      if (input$dim_red_color != "") {
        complete_rows <- complete.cases(rv$filtered_data[, input$dim_red_vars, drop = FALSE])
        color_var <- rv$filtered_data[complete_rows, input$dim_red_color]
        plot_data$color_var <- color_var
      }
      
      # Create plot
      if (input$pca_components == 2) {
        p <- plot_ly(plot_data, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers")
        
        if (!is.null(color_var)) {
          p <- p %>% add_markers(color = ~color_var, colors = "Viridis")
        } else {
          p <- p %>% add_markers(marker = list(color = "blue"))
        }
        
        # Add explained variance to axis labels
        var_exp <- summary(pca_result)$importance[2, 1:2] * 100
        p <- p %>% layout(
          title = "PCA Analysis",
          xaxis = list(title = paste0("PC1 (", round(var_exp[1], 1), "% variance)")),
          yaxis = list(title = paste0("PC2 (", round(var_exp[2], 1), "% variance)"))
        )
        
      } else if (input$pca_components == 3) {
        p <- plot_ly(plot_data, x = ~PC1, y = ~PC2, z = ~PC3, type = "scatter3d", mode = "markers")
        
        if (!is.null(color_var)) {
          p <- p %>% add_markers(color = ~color_var, colors = "Viridis")
        } else {
          p <- p %>% add_markers(marker = list(color = "blue"))
        }
        
        # Add explained variance to axis labels
        var_exp <- summary(pca_result)$importance[2, 1:3] * 100
        p <- p %>% layout(
          title = "PCA Analysis (3D)",
          scene = list(
            xaxis = list(title = paste0("PC1 (", round(var_exp[1], 1), "% variance)")),
            yaxis = list(title = paste0("PC2 (", round(var_exp[2], 1), "% variance)")),
            zaxis = list(title = paste0("PC3 (", round(var_exp[3], 1), "% variance)"))
          )
        )
      }
      
      p
      
    } else if (input$dim_red_method == "t-SNE") {
      # Perform t-SNE
      library(Rtsne)
      set.seed(42)  # For reproducibility
      
      tsne_result <- Rtsne(data_subset, 
                           dims = 2, 
                           perplexity = input$tsne_perplexity,
                           max_iter = input$tsne_iterations,
                           check_duplicates = FALSE)
      
      # Create plot data
      plot_data <- data.frame(
        tSNE1 = tsne_result$Y[, 1],
        tSNE2 = tsne_result$Y[, 2]
      )
      
      # Add color variable if specified
      if (input$dim_red_color != "") {
        complete_rows <- complete.cases(rv$filtered_data[, input$dim_red_vars, drop = FALSE])
        color_var <- rv$filtered_data[complete_rows, input$dim_red_color]
        plot_data$color_var <- color_var
      }
      
      # Create plot
      p <- plot_ly(plot_data, x = ~tSNE1, y = ~tSNE2, type = "scatter", mode = "markers")
      
      if (input$dim_red_color != "") {
        p <- p %>% add_markers(color = ~color_var, colors = "Viridis")
      } else {
        p <- p %>% add_markers(marker = list(color = "blue"))
      }
      
      p %>% layout(
        title = paste0("t-SNE Analysis (Perplexity: ", input$tsne_perplexity, ")"),
        xaxis = list(title = "t-SNE Dimension 1"),
        yaxis = list(title = "t-SNE Dimension 2")
      )
    }
  })
})

# PCA VARIANCE PLOT ============================================

output$pca_variance_plot <- renderPlot({
  req(rv$pca_result)
  
  # Calculate variance explained
  var_exp <- summary(rv$pca_result)$importance
  var_df <- data.frame(
    Component = 1:ncol(var_exp),
    Variance = var_exp[2, ],
    Cumulative = var_exp[3, ]
  )
  
  # Create variance plot
  p1 <- ggplot(var_df[1:min(10, nrow(var_df)), ], aes(x = Component, y = Variance)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_line(aes(y = Cumulative), color = "red", size = 1) +
    geom_point(aes(y = Cumulative), color = "red", size = 2) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Cumulative Variance")) +
    labs(title = "PCA Variance Explained", 
         x = "Principal Component", 
         y = "Proportion of Variance") +
    theme_minimal()
  
  print(p1)
})

# PCA LOADINGS TABLE ===========================================

output$pca_loadings <- renderDT({
  req(rv$pca_result)
  
  # Get loadings for first few components
  n_comp <- min(5, ncol(rv$pca_result$rotation))
  loadings_df <- data.frame(
    Variable = rownames(rv$pca_result$rotation),
    rv$pca_result$rotation[, 1:n_comp, drop = FALSE]
  )
  
  datatable(loadings_df,
            options = list(pageLength = 15, scrollX = TRUE),
            caption = "PCA Loadings (Variable Contributions to Components)") %>%
    formatRound(columns = 2:(n_comp + 1), digits = 3)
})

# CSV Download for PCA Loadings
output$download_pca_loadings_csv <- downloadHandler(
  filename = function() {
    paste("pca_loadings_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    req(rv$pca_result)
    
    # Get loadings for first few components
    n_comp <- min(5, ncol(rv$pca_result$rotation))
    loadings_df <- data.frame(
      Variable = rownames(rv$pca_result$rotation),
      rv$pca_result$rotation[, 1:n_comp, drop = FALSE]
    )
    
    write.csv(loadings_df, file, row.names = FALSE)
  }
)

# Excel Download for PCA Loadings
output$download_pca_loadings_excel <- downloadHandler(
  filename = function() {
    paste("pca_loadings_", Sys.Date(), ".xlsx", sep = "")
  },
  content = function(file) {
    req(rv$pca_result)
    
    # Get loadings for first few components
    n_comp <- min(5, ncol(rv$pca_result$rotation))
    loadings_df <- data.frame(
      Variable = rownames(rv$pca_result$rotation),
      rv$pca_result$rotation[, 1:n_comp, drop = FALSE]
    )
    
    # Add variance explained as a separate sheet
    var_exp <- summary(rv$pca_result)$importance
    var_df <- data.frame(
      Component = paste0("PC", 1:ncol(var_exp)),
      Standard_Deviation = var_exp[1, ],
      Proportion_of_Variance = var_exp[2, ],
      Cumulative_Proportion = var_exp[3, ]
    )
    
    # Create workbook with multiple sheets
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "PCA_Loadings")
    openxlsx::addWorksheet(wb, "Variance_Explained")
    
    openxlsx::writeData(wb, "PCA_Loadings", loadings_df)
    openxlsx::writeData(wb, "Variance_Explained", var_df)
    
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
)

# Copy to Clipboard for PCA Loadings
observeEvent(input$copy_pca_loadings, {
  req(rv$pca_result)
  
  # Get loadings for first few components
  n_comp <- min(5, ncol(rv$pca_result$rotation))
  loadings_df <- data.frame(
    Variable = rownames(rv$pca_result$rotation),
    rv$pca_result$rotation[, 1:n_comp, drop = FALSE]
  )
  
  # Convert to tab-separated format for clipboard
  clipboard_text <- paste(capture.output(write.table(loadings_df, sep = "\t", row.names = FALSE)), collapse = "\n")
  
  # Use JavaScript to copy to clipboard
  runjs(paste0('
    navigator.clipboard.writeText(`', gsub("`", "\\`", clipboard_text), '`).then(function() {
      alert("PCA loadings copied to clipboard!");
    });
  '))
})

# CLUSTER ANALYSIS OPTIONS =====================================

output$cluster_options <- renderUI({
  req(rv$filtered_data)
  
  # Get numeric columns more safely
  numeric_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, is.numeric)]
  
  if (length(numeric_vars) < 2) {
    return(div("Not enough numeric variables for cluster analysis."))
  }
  
  # Common UI elements
  common_ui <- tagList(
    pickerInput(
      "cluster_vars", "Select Variables:",
      choices = numeric_vars,
      selected = numeric_vars[1:min(length(numeric_vars), 5)],
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    ),
    checkboxInput("scale_cluster_data", "Scale Variables", TRUE)
  )
  
  # Method-specific UI
  method_specific <- switch(input$cluster_method,
                            "K-Means" = tagList(
                              numericInput("n_clusters", "Number of Clusters:", 
                                           value = 3, min = 2, max = 10, step = 1),
                              numericInput("kmeans_iter", "Max Iterations:", 
                                           value = 100, min = 50, max = 500, step = 50),
                              checkboxInput("show_elbow", "Show Elbow Plot", TRUE)
                            ),
                            "Hierarchical" = tagList(
                              selectInput("hclust_method", "Linkage Method:",
                                          choices = c("Complete" = "complete", "Average" = "average", 
                                                      "Single" = "single", "Ward" = "ward.D2"),
                                          selected = "complete"),
                              numericInput("hclust_clusters", "Number of Clusters:", 
                                           value = 3, min = 2, max = 10, step = 1),
                              checkboxInput("show_dendrogram", "Show Dendrogram", TRUE)
                            ),
                            NULL
  )
  
  tagList(
    common_ui,
    method_specific,
    br(),
    actionButton("run_clustering", "Run Clustering", 
                 class = "btn btn-success")
  )
})

# CLUSTER ANALYSIS PLOT =======================================

output$cluster_plot <- renderPlotly({
  # Require all necessary inputs
  req(rv$filtered_data, input$cluster_vars, input$run_clustering)
  req(length(input$cluster_vars) >= 2)
  
  isolate({
    tryCatch({
      # Validate cluster method
      if (is.null(input$cluster_method) || !input$cluster_method %in% c("K-Means", "Hierarchical")) {
        return(plot_ly() %>% 
                 add_text(x = 0.5, y = 0.5, text = "Please select a clustering method", 
                          textfont = list(size = 16)) %>%
                 layout(showlegend = FALSE,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")))
      }
      
      # Prepare data - ensure we have the selected variables
      selected_vars <- intersect(input$cluster_vars, names(rv$filtered_data))
      if (length(selected_vars) < 2) {
        return(plot_ly() %>% 
                 add_text(x = 0.5, y = 0.5, text = "At least 2 variables required", 
                          textfont = list(size = 16)) %>%
                 layout(showlegend = FALSE,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")))
      }
      
      # Extract data subset
      data_subset <- rv$filtered_data[, selected_vars, drop = FALSE]
      
      # Remove rows with missing values
      complete_rows <- complete.cases(data_subset)
      data_subset <- data_subset[complete_rows, , drop = FALSE]
      
      if (nrow(data_subset) < 3) {
        return(plot_ly() %>% 
                 add_text(x = 0.5, y = 0.5, text = "Not enough complete observations", 
                          textfont = list(size = 16)) %>%
                 layout(showlegend = FALSE,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")))
      }
      
      # Scale data if requested
      if (input$scale_cluster_data) {
        data_for_clustering <- scale(data_subset)
        data_for_clustering <- as.data.frame(data_for_clustering)
        colnames(data_for_clustering) <- colnames(data_subset)
      } else {
        data_for_clustering <- data_subset
      }
      
      # Initialize variables
      clusters <- NULL
      plot_title <- ""
      kmeans_result <- NULL
      
      # Perform clustering based on method
      if (input$cluster_method == "K-Means") {
        # Validate number of clusters
        max_clusters <- min(nrow(data_subset) - 1, input$n_clusters)
        if (max_clusters < 2) {
          return(plot_ly() %>% 
                   add_text(x = 0.5, y = 0.5, text = "Not enough data points for clustering", 
                            textfont = list(size = 16)) %>%
                   layout(showlegend = FALSE,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")))
        }
        
        # Perform K-means clustering
        set.seed(42)
        kmeans_result <- kmeans(data_for_clustering, centers = max_clusters, 
                                iter.max = input$kmeans_iter, nstart = 10)
        
        # Extract clusters
        clusters <- kmeans_result$cluster
        
        # Store results
        rv$cluster_result <- list(
          method = "K-Means",
          clusters = clusters,
          centers = kmeans_result$centers,
          withinss = kmeans_result$withinss,
          totss = kmeans_result$totss,
          betweenss = kmeans_result$betweenss,
          n_clusters = max_clusters
        )
        
        plot_title <- paste("K-Means Clustering (k =", max_clusters, ")")
        
      } else if (input$cluster_method == "Hierarchical") {
        # Perform hierarchical clustering
        dist_matrix <- dist(data_for_clustering)
        hclust_result <- hclust(dist_matrix, method = input$hclust_method)
        
        # Validate number of clusters
        max_clusters <- min(nrow(data_subset), input$hclust_clusters)
        clusters <- cutree(hclust_result, k = max_clusters)
        
        # Store results
        rv$cluster_result <- list(
          method = "Hierarchical",
          clusters = clusters,
          hclust = hclust_result,
          n_clusters = max_clusters
        )
        
        plot_title <- paste("Hierarchical Clustering (k =", max_clusters, ")")
      }
      
      # Check if clustering was successful
      if (is.null(clusters) || length(clusters) == 0) {
        return(plot_ly() %>% 
                 add_text(x = 0.5, y = 0.5, text = "Clustering failed", 
                          textfont = list(size = 16)) %>%
                 layout(showlegend = FALSE,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")))
      }
      
      # Create plot data using first two variables
      var1 <- colnames(data_subset)[1]
      var2 <- colnames(data_subset)[2]
      
      # Ensure we have valid cluster assignments
      cluster_factor <- factor(clusters, levels = sort(unique(clusters)))
      
      plot_data <- data.frame(
        x = as.numeric(data_subset[[var1]]),
        y = as.numeric(data_subset[[var2]]),
        cluster_group = cluster_factor,
        stringsAsFactors = FALSE
      )
      
      # Create the plot
      p <- plot_ly() %>%
        add_markers(data = plot_data,
                    x = ~x, y = ~y, 
                    color = ~cluster_group, 
                    colors = "Set1",
                    marker = list(size = 8),
                    name = ~paste("Cluster", cluster_group),
                    hovertemplate = paste("Cluster: %{text}<br>",
                                          var1, ": %{x}<br>",
                                          var2, ": %{y}<extra></extra>"),
                    text = ~cluster_group) %>%
        layout(title = plot_title,
               xaxis = list(title = var1),
               yaxis = list(title = var2),
               showlegend = TRUE)
      
      # Add cluster centers for K-means
      if (input$cluster_method == "K-Means" && exists("kmeans_result")) {
        centers <- kmeans_result$centers
        if (ncol(centers) >= 2) {
          # Get the indices of the variables we're plotting
          var1_idx <- which(colnames(data_for_clustering) == var1)
          var2_idx <- which(colnames(data_for_clustering) == var2)
          
          centers_df <- data.frame(
            x = as.numeric(centers[, var1_idx]),
            y = as.numeric(centers[, var2_idx])
          )
          
          p <- p %>% add_markers(data = centers_df,
                                 x = ~x, y = ~y,
                                 marker = list(symbol = "x", size = 15, color = "black", line = list(width = 2)),
                                 name = "Centroids",
                                 hovertemplate = paste("Centroid<br>",
                                                       var1, ": %{x}<br>",
                                                       var2, ": %{y}<extra></extra>"))
        }
      }
      
      return(p)
      
    }, error = function(e) {
      # Error handling
      error_msg <- paste("Clustering Error:", e$message)
      return(plot_ly() %>% 
               add_text(x = 0.5, y = 0.5, text = error_msg, 
                        textfont = list(size = 14, color = "red")) %>%
               layout(showlegend = FALSE,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = "")))
    })
  })
})

# CLUSTER SUMMARY TABLE =======================================

output$cluster_summary <- renderDT({
  req(rv$cluster_result)
  
  tryCatch({
    if (rv$cluster_result$method == "K-Means") {
      # K-means summary
      cluster_counts <- table(rv$cluster_result$clusters)
      n_clusters <- length(cluster_counts)
      
      summary_df <- data.frame(
        Cluster = 1:n_clusters,
        Size = as.numeric(cluster_counts),
        Within_SS = round(rv$cluster_result$withinss, 3),
        Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
      )
      
      # Calculate R-squared
      r_squared <- round(rv$cluster_result$betweenss / rv$cluster_result$totss, 3)
      
      caption_text <- HTML(paste0(
        "<b>K-Means Clustering Summary</b><br>",
        "Total Sum of Squares: ", round(rv$cluster_result$totss, 2), "<br>",
        "Between Sum of Squares: ", round(rv$cluster_result$betweenss, 2), "<br>",
        "Within Sum of Squares: ", round(sum(rv$cluster_result$withinss), 2), "<br>",
        "R-squared: ", r_squared
      ))
      
      datatable(summary_df,
                options = list(pageLength = 10, dom = 't', ordering = FALSE),
                caption = caption_text,
                rownames = FALSE) %>%
        formatPercentage("Percentage", digits = 1)
      
    } else if (rv$cluster_result$method == "Hierarchical") {
      # Hierarchical clustering summary
      cluster_counts <- table(rv$cluster_result$clusters)
      
      summary_df <- data.frame(
        Cluster = 1:length(cluster_counts),
        Size = as.numeric(cluster_counts),
        Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
      )
      
      datatable(summary_df,
                options = list(pageLength = 10, dom = 't', ordering = FALSE),
                caption = "<b>Hierarchical Clustering Summary</b>",
                rownames = FALSE) %>%
        formatPercentage("Percentage", digits = 1)
    }
    
  }, error = function(e) {
    # Return error message
    error_df <- data.frame(
      Message = paste("Error generating summary:", e$message)
    )
    datatable(error_df, options = list(dom = 't'), rownames = FALSE)
  })
})

# CSV Download for Cluster Summary
output$download_cluster_summary_csv <- downloadHandler(
  filename = function() {
    paste("cluster_summary_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    req(rv$cluster_result)
    
    if (rv$cluster_result$method == "K-Means") {
      cluster_counts <- table(rv$cluster_result$clusters)
      n_clusters <- length(cluster_counts)
      
      summary_df <- data.frame(
        Cluster = 1:n_clusters,
        Size = as.numeric(cluster_counts),
        Within_SS = round(rv$cluster_result$withinss, 3),
        Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
      )
      
      # Add summary statistics
      r_squared <- round(rv$cluster_result$betweenss / rv$cluster_result$totss, 3)
      
      # Create summary info
      summary_info <- data.frame(
        Metric = c("Total Sum of Squares", "Between Sum of Squares", "Within Sum of Squares", "R-squared"),
        Value = c(
          round(rv$cluster_result$totss, 2),
          round(rv$cluster_result$betweenss, 2),
          round(sum(rv$cluster_result$withinss), 2),
          r_squared
        )
      )
      
      # Combine data
      final_df <- rbind(
        data.frame(Section = "Cluster Details", summary_df),
        data.frame(Section = "Summary Statistics", 
                   Cluster = summary_info$Metric,
                   Size = summary_info$Value,
                   Within_SS = NA,
                   Percentage = NA)
      )
      
    } else if (rv$cluster_result$method == "Hierarchical") {
      cluster_counts <- table(rv$cluster_result$clusters)
      
      final_df <- data.frame(
        Section = "Cluster Details",
        Cluster = 1:length(cluster_counts),
        Size = as.numeric(cluster_counts),
        Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
      )
    }
    
    write.csv(final_df, file, row.names = FALSE)
  }
)

# Excel Download for Cluster Summary
output$download_cluster_summary_excel <- downloadHandler(
  filename = function() {
    paste("cluster_summary_", Sys.Date(), ".xlsx", sep = "")
  },
  content = function(file) {
    req(rv$cluster_result)
    
    wb <- openxlsx::createWorkbook()
    
    if (rv$cluster_result$method == "K-Means") {
      cluster_counts <- table(rv$cluster_result$clusters)
      n_clusters <- length(cluster_counts)
      
      # Cluster details
      cluster_df <- data.frame(
        Cluster = 1:n_clusters,
        Size = as.numeric(cluster_counts),
        Within_SS = round(rv$cluster_result$withinss, 3),
        Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
      )
      
      # Summary statistics
      r_squared <- round(rv$cluster_result$betweenss / rv$cluster_result$totss, 3)
      summary_df <- data.frame(
        Metric = c("Total Sum of Squares", "Between Sum of Squares", "Within Sum of Squares", "R-squared"),
        Value = c(
          round(rv$cluster_result$totss, 2),
          round(rv$cluster_result$betweenss, 2),
          round(sum(rv$cluster_result$withinss), 2),
          r_squared
        )
      )
      
      # Create sheets
      openxlsx::addWorksheet(wb, "Cluster_Details")
      openxlsx::addWorksheet(wb, "Summary_Statistics")
      
      openxlsx::writeData(wb, "Cluster_Details", cluster_df)
      openxlsx::writeData(wb, "Summary_Statistics", summary_df)
      
    } else if (rv$cluster_result$method == "Hierarchical") {
      cluster_counts <- table(rv$cluster_result$clusters)
      
      cluster_df <- data.frame(
        Cluster = 1:length(cluster_counts),
        Size = as.numeric(cluster_counts),
        Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
      )
      
      openxlsx::addWorksheet(wb, "Cluster_Summary")
      openxlsx::writeData(wb, "Cluster_Summary", cluster_df)
    }
    
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
)

# Copy to Clipboard for Cluster Summary
observeEvent(input$copy_cluster_summary, {
  req(rv$cluster_result)
  
  if (rv$cluster_result$method == "K-Means") {
    cluster_counts <- table(rv$cluster_result$clusters)
    n_clusters <- length(cluster_counts)
    
    summary_df <- data.frame(
      Cluster = 1:n_clusters,
      Size = as.numeric(cluster_counts),
      Within_SS = round(rv$cluster_result$withinss, 3),
      Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
    )
    
  } else if (rv$cluster_result$method == "Hierarchical") {
    cluster_counts <- table(rv$cluster_result$clusters)
    
    summary_df <- data.frame(
      Cluster = 1:length(cluster_counts),
      Size = as.numeric(cluster_counts),
      Percentage = round(as.numeric(cluster_counts) / sum(cluster_counts) * 100, 1)
    )
  }
  
  # Convert to tab-separated format for clipboard
  clipboard_text <- paste(capture.output(write.table(summary_df, sep = "\t", row.names = FALSE)), collapse = "\n")
  
  # Use JavaScript to copy to clipboard
  runjs(paste0('
    navigator.clipboard.writeText(`', gsub("`", "\\`", clipboard_text), '`).then(function() {
      alert("Cluster summary copied to clipboard!");
    });
  '))
})



# TIME SERIES ANALYSIS SERVER LOGIC ============================

# Initialize time series reactive values
ts_rv <- reactiveValues(
  ts_data = NULL,
  ts_object = NULL,
  decomp_result = NULL,
  forecast_result = NULL,
  anomalies = NULL,
  stationarity_results = NULL
)

# 1. DATA SETUP TAB ============================

# Dynamic UI for date column selection
output$ts_date_column_selector <- renderUI({
  req(rv$filtered_data)
  
  # Get all columns that could potentially be dates
  all_cols <- names(rv$filtered_data)
  date_candidates <- c()
  
  # Check for date-like column names or actual date columns
  for (col in all_cols) {
    if (inherits(rv$filtered_data[[col]], c("Date", "POSIXct", "POSIXlt")) ||
        grepl("date|time|year|month|day", col, ignore.case = TRUE)) {
      date_candidates <- c(date_candidates, col)
    }
  }
  
  # If no obvious candidates, include all columns
  if (length(date_candidates) == 0) {
    date_candidates <- all_cols
  }
  
  selectInput("ts_date_column", "Date/Time Column:",
              choices = date_candidates,
              selected = date_candidates[1])
})

# Dynamic UI for value columns selection
output$ts_value_columns_selector <- renderUI({
  req(rv$filtered_data)
  
  # Get numeric columns
  numeric_cols <- names(rv$filtered_data)[sapply(rv$filtered_data, is.numeric)]
  
  pickerInput("ts_value_columns", "Value Column(s):",
              choices = numeric_cols,
              selected = numeric_cols[1],
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `max-options` = 5
              ))
})

# Create time series observer
observeEvent(input$create_ts, {
  req(rv$filtered_data, input$ts_date_column, input$ts_value_columns)
  
  tryCatch({
    # Get the data
    data <- rv$filtered_data
    
    # Parse date column
    date_col <- data[[input$ts_date_column]]
    
    # Convert to date if needed
    if (!inherits(date_col, c("Date", "POSIXct", "POSIXlt"))) {
      if (input$ts_date_format == "auto") {
        # Try multiple date formats
        date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y-%m-%d %H:%M:%S")
        date_col <- NULL
        
        for (fmt in date_formats) {
          temp_date <- as.Date(data[[input$ts_date_column]], format = fmt)
          if (!all(is.na(temp_date))) {
            date_col <- temp_date
            break
          }
        }
        
        if (is.null(date_col)) {
          stop("Could not automatically detect date format. Please specify custom format.")
        }
      } else if (input$ts_date_format == "custom") {
        req(input$ts_custom_format)
        date_col <- as.Date(data[[input$ts_date_column]], format = input$ts_custom_format)
      } else {
        date_col <- as.Date(data[[input$ts_date_column]], format = input$ts_date_format)
      }
    }
    
    # Check for successful conversion
    if (all(is.na(date_col))) {
      stop("Failed to parse dates. Please check the date format.")
    }
    
    # Create time series data frame
    ts_data <- data.frame(
      date = date_col,
      data[input$ts_value_columns],
      stringsAsFactors = FALSE
    )
    
    # Remove rows with NA dates
    ts_data <- ts_data[!is.na(ts_data$date), ]
    
    # Sort by date
    ts_data <- ts_data[order(ts_data$date), ]
    
    # Store in reactive values
    ts_rv$ts_data <- ts_data
    
    # Create time series objects for each value column
    ts_objects <- list()
    for (col in input$ts_value_columns) {
      if (sum(!is.na(ts_data[[col]])) > 0) {
        # Detect frequency
        dates <- ts_data$date[!is.na(ts_data[[col]])]
        values <- ts_data[[col]][!is.na(ts_data[[col]])]
        
        # Calculate time differences to infer frequency
        if (length(dates) > 1) {
          diffs <- as.numeric(diff(dates))
          median_diff <- median(diffs, na.rm = TRUE)
          
          # Determine frequency based on median difference
          if (median_diff <= 1.5) {
            frequency <- 365  # Daily
          } else if (median_diff <= 8) {
            frequency <- 52   # Weekly
          } else if (median_diff <= 32) {
            frequency <- 12   # Monthly
          } else if (median_diff <= 95) {
            frequency <- 4    # Quarterly
          } else {
            frequency <- 1    # Yearly
          }
          
          # Create time series object
          start_year <- as.numeric(format(min(dates), "%Y"))
          ts_objects[[col]] <- ts(values, 
                                  start = c(start_year, 1), 
                                  frequency = frequency)
        }
      }
    }
    
    ts_rv$ts_object <- ts_objects
    
    showNotification("Time series created successfully!", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error creating time series:", e$message), 
                     type = "error", duration = 10)
  })
})

# Time series summary output
output$ts_summary <- renderPrint({
  req(ts_rv$ts_data)
  
  cat("Time Series Summary:\n")
  cat("===================\n")
  cat("Date Range:", as.character(min(ts_rv$ts_data$date)), "to", 
      as.character(max(ts_rv$ts_data$date)), "\n")
  cat("Number of Observations:", nrow(ts_rv$ts_data), "\n")
  cat("Value Columns:", paste(names(ts_rv$ts_data)[-1], collapse = ", "), "\n")
  
  # Basic statistics for each value column
  for (col in names(ts_rv$ts_data)[-1]) {
    if (is.numeric(ts_rv$ts_data[[col]])) {
      cat("\n", col, "Statistics:\n")
      print(summary(ts_rv$ts_data[[col]]))
    }
  }
})

# Frequency detection output
output$ts_frequency <- renderPrint({
  req(ts_rv$ts_object)
  
  cat("Detected Frequencies:\n")
  cat("====================\n")
  
  for (col in names(ts_rv$ts_object)) {
    freq <- frequency(ts_rv$ts_object[[col]])
    freq_desc <- switch(as.character(freq),
                        "1" = "Yearly",
                        "4" = "Quarterly", 
                        "12" = "Monthly",
                        "52" = "Weekly",
                        "365" = "Daily",
                        paste("Custom (", freq, ")"))
    cat(col, ":", freq_desc, "\n")
  }
})

# Missing values summary for time series
output$ts_missing_summary <- renderPrint({
  req(ts_rv$ts_data)
  
  missing_summary <- sapply(ts_rv$ts_data[-1], function(x) sum(is.na(x)))
  missing_summary <- missing_summary[missing_summary > 0]
  
  if (length(missing_summary) == 0) {
    cat("No missing values found in time series data.\n")
  } else {
    cat("Missing Values:\n")
    cat("===============\n")
    for (col in names(missing_summary)) {
      pct <- round(missing_summary[col] / nrow(ts_rv$ts_data) * 100, 2)
      cat(col, ":", missing_summary[col], "(", pct, "%)\n")
    }
  }
})

# Time series preview plot
output$ts_preview_plot <- renderPlotly({
  req(ts_rv$ts_data)
  
  # Create plotly chart
  p <- plot_ly()
  
  # Add traces for each value column
  for (col in names(ts_rv$ts_data)[-1]) {
    if (is.numeric(ts_rv$ts_data[[col]])) {
      p <- p %>% add_trace(
        x = ts_rv$ts_data$date,
        y = ts_rv$ts_data[[col]],
        type = 'scatter',
        mode = 'lines',
        name = col,
        line = list(width = 2)
      )
    }
  }
  
  p <- p %>% layout(
    title = "Time Series Preview",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Value"),
    hovermode = 'x unified'
  )
  
  return(p)
})

# 2. VISUALIZATION TAB ============================

# Multi-series selector UI
output$ts_multi_series_selector <- renderUI({
  req(ts_rv$ts_data)
  
  value_cols <- names(ts_rv$ts_data)[-1]
  
  pickerInput("ts_selected_series", "Select Series:",
              choices = value_cols,
              selected = value_cols,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE
              ))
})

# Date range selector UI
output$ts_date_range_selector <- renderUI({
  req(ts_rv$ts_data)
  
  dateRangeInput("ts_date_range", "Date Range:",
                 start = min(ts_rv$ts_data$date),
                 end = max(ts_rv$ts_data$date),
                 min = min(ts_rv$ts_data$date),
                 max = max(ts_rv$ts_data$date))
})

# Main time series plot
output$ts_main_plot <- renderPlotly({
  req(ts_rv$ts_data, input$ts_plot_type)
  
  # Filter data by date range if specified
  plot_data <- ts_rv$ts_data
  if (!is.null(input$ts_date_range)) {
    plot_data <- plot_data[plot_data$date >= input$ts_date_range[1] & 
                             plot_data$date <= input$ts_date_range[2], ]
  }
  
  # Aggregate data if specified
  if (!is.null(input$ts_aggregation) && input$ts_aggregation != "none") {
    plot_data <- aggregate_ts_data(plot_data, input$ts_aggregation)
  }
  
  # Select series for multi-plot
  if (input$ts_plot_type == "multi" && !is.null(input$ts_selected_series)) {
    value_cols <- input$ts_selected_series
  } else {
    value_cols <- names(plot_data)[-1]
  }
  
  # Create plot based on type
  p <- plot_ly()
  
  if (input$ts_plot_type == "line") {
    for (col in value_cols) {
      y_vals <- plot_data[[col]]
      if (input$ts_log_scale && is.numeric(y_vals)) {
        y_vals <- log(y_vals + 1)  # Add 1 to handle zeros
      }
      
      p <- p %>% add_trace(
        x = plot_data$date,
        y = y_vals,
        type = 'scatter',
        mode = 'lines',
        name = col,
        line = list(width = 2)
      )
    }
  } else if (input$ts_plot_type == "area") {
    for (col in value_cols) {
      y_vals <- plot_data[[col]]
      if (input$ts_log_scale && is.numeric(y_vals)) {
        y_vals <- log(y_vals + 1)
      }
      
      p <- p %>% add_trace(
        x = plot_data$date,
        y = y_vals,
        type = 'scatter',
        mode = 'lines',
        fill = 'tonexty',
        name = col
      )
    }
  }
  
  # Add trend line if requested
  if (input$ts_show_trend && length(value_cols) >= 1) {
    col <- value_cols[1]
    if (is.numeric(plot_data[[col]])) {
      trend_data <- na.omit(data.frame(x = as.numeric(plot_data$date), 
                                       y = plot_data[[col]]))
      if (nrow(trend_data) > 1) {
        lm_model <- lm(y ~ x, data = trend_data)
        p <- p %>% add_trace(
          x = plot_data$date,
          y = predict(lm_model, newdata = data.frame(x = as.numeric(plot_data$date))),
          type = 'scatter',
          mode = 'lines',
          name = 'Trend',
          line = list(dash = 'dash', color = 'red')
        )
      }
    }
  }
  
  # Layout settings
  y_title <- if (input$ts_log_scale) "Log Value" else "Value"
  
  p <- p %>% layout(
    title = "Time Series Visualization",
    xaxis = list(title = "Date"),
    yaxis = list(title = y_title),
    hovermode = 'x unified'
  )
  
  return(p)
})

# Helper function to aggregate time series data
aggregate_ts_data <- function(data, aggregation) {
  if (aggregation == "daily") {
    return(data)  # Already daily
  }
  
  # Create grouping variable based on aggregation
  if (aggregation == "weekly") {
    data$group <- format(data$date, "%Y-W%W")
  } else if (aggregation == "monthly") {
    data$group <- format(data$date, "%Y-%m")
  } else if (aggregation == "quarterly") {
    data$quarter <- ceiling(as.numeric(format(data$date, "%m")) / 3)
    data$group <- paste0(format(data$date, "%Y"), "-Q", data$quarter)
  } else if (aggregation == "yearly") {
    data$group <- format(data$date, "%Y")
  }
  
  # Aggregate numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- numeric_cols[numeric_cols != "group"]
  
  agg_data <- aggregate(data[numeric_cols], by = list(group = data$group), 
                        FUN = mean, na.rm = TRUE)
  
  # Convert group back to date
  if (aggregation == "weekly") {
    # Simple approximation for weekly dates
    agg_data$date <- as.Date(paste0(agg_data$group, "-1"), format = "%Y-W%W-%w")
  } else if (aggregation == "monthly") {
    agg_data$date <- as.Date(paste0(agg_data$group, "-01"))
  } else if (aggregation == "quarterly") {
    # Extract year and quarter
    year_quarter <- strsplit(agg_data$group, "-Q")
    years <- sapply(year_quarter, function(x) x[1])
    quarters <- sapply(year_quarter, function(x) x[2])
    months <- as.numeric(quarters) * 3 - 2  # First month of quarter
    agg_data$date <- as.Date(paste0(years, "-", sprintf("%02d", months), "-01"))
  } else if (aggregation == "yearly") {
    agg_data$date <- as.Date(paste0(agg_data$group, "-01-01"))
  }
  
  # Remove group column and reorder
  agg_data$group <- NULL
  agg_data <- agg_data[, c("date", setdiff(names(agg_data), "date"))]
  
  return(agg_data)
}

# 3. DECOMPOSITION TAB ============================

# Variable selector for decomposition
output$ts_decomp_variable_selector <- renderUI({
  req(ts_rv$ts_object)
  
  selectInput("ts_decomp_variable", "Select Variable:",
              choices = names(ts_rv$ts_object),
              selected = names(ts_rv$ts_object)[1])
})

# Perform decomposition
observeEvent(input$perform_decomposition, {
  req(ts_rv$ts_object, input$ts_decomp_variable, input$ts_decomp_method)
  
  tryCatch({
    ts_obj <- ts_rv$ts_object[[input$ts_decomp_variable]]
    
    if (input$ts_decomp_method == "stl") {
      # STL decomposition
      decomp <- stl(ts_obj, s.window = input$ts_stl_window)
    } else if (input$ts_decomp_method == "additive") {
      # Classical additive decomposition
      decomp <- decompose(ts_obj, type = "additive")
    } else if (input$ts_decomp_method == "multiplicative") {
      # Classical multiplicative decomposition
      decomp <- decompose(ts_obj, type = "multiplicative")
    }
    
    ts_rv$decomp_result <- decomp
    
    showNotification("Decomposition completed successfully!", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error in decomposition:", e$message), 
                     type = "error", duration = 10)
  })
})

# Decomposition summary
output$ts_decomp_summary <- renderPrint({
  req(ts_rv$decomp_result)
  
  cat("Time Series Decomposition Summary:\n")
  cat("==================================\n")
  
  if (class(ts_rv$decomp_result)[1] == "stl") {
    cat("Method: STL (Seasonal and Trend decomposition using Loess)\n")
    
    # Calculate variance explained by each component
    original <- ts_rv$decomp_result$time.series[, "seasonal"] + 
      ts_rv$decomp_result$time.series[, "trend"] + 
      ts_rv$decomp_result$time.series[, "remainder"]
    
    seasonal_var <- var(ts_rv$decomp_result$time.series[, "seasonal"], na.rm = TRUE)
    trend_var <- var(ts_rv$decomp_result$time.series[, "trend"], na.rm = TRUE)
    remainder_var <- var(ts_rv$decomp_result$time.series[, "remainder"], na.rm = TRUE)
    total_var <- var(original, na.rm = TRUE)
    
    cat("\nVariance Explained:\n")
    cat("Seasonal: ", round(seasonal_var / total_var * 100, 2), "%\n")
    cat("Trend: ", round(trend_var / total_var * 100, 2), "%\n")
    cat("Remainder: ", round(remainder_var / total_var * 100, 2), "%\n")
    
  } else {
    cat("Method: Classical Decomposition\n")
    cat("Type:", attr(ts_rv$decomp_result, "type"), "\n")
    
    # Basic summary statistics for each component
    cat("\nComponent Summaries:\n")
    cat("Seasonal:\n")
    print(summary(as.numeric(ts_rv$decomp_result$seasonal)))
    cat("\nTrend:\n")
    print(summary(as.numeric(ts_rv$decomp_result$trend)))
    cat("\nRandom:\n")
    print(summary(as.numeric(ts_rv$decomp_result$random)))
  }
})

# Decomposition plot
output$ts_decomposition_plot <- renderPlot({
  req(ts_rv$decomp_result)
  
  plot(ts_rv$decomp_result, main = "Time Series Decomposition")
})

# 4. STATIONARITY TESTS TAB ============================

# Variable selector for stationarity tests
output$ts_stationarity_variable_selector <- renderUI({
  req(ts_rv$ts_object)
  
  selectInput("ts_stationarity_variable", "Select Variable:",
              choices = names(ts_rv$ts_object),
              selected = names(ts_rv$ts_object)[1])
})

# Perform stationarity tests
observeEvent(input$perform_stationarity_tests, {
  req(ts_rv$ts_object, input$ts_stationarity_variable, input$ts_stationarity_tests)
  
  tryCatch({
    ts_obj <- ts_rv$ts_object[[input$ts_stationarity_variable]]
    results <- list()
    
    # ADF Test
    if ("adf" %in% input$ts_stationarity_tests) {
      library(tseries)
      adf_result <- adf.test(ts_obj)
      results$adf <- adf_result
    }
    
    # KPSS Test
    if ("kpss" %in% input$ts_stationarity_tests) {
      library(tseries)
      kpss_result <- kpss.test(ts_obj)
      results$kpss <- kpss_result
    }
    
    # Phillips-Perron Test
    if ("pp" %in% input$ts_stationarity_tests) {
      library(tseries)
      pp_result <- pp.test(ts_obj)
      results$pp <- pp_result
    }
    
    ts_rv$stationarity_results <- results
    
    showNotification("Stationarity tests completed!", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error in stationarity tests:", e$message), 
                     type = "error", duration = 10)
  })
})

# Stationarity test results
output$ts_stationarity_results <- renderPrint({
  req(ts_rv$stationarity_results)
  
  cat("Stationarity Test Results:\n")
  cat("==========================\n\n")
  
  if (!is.null(ts_rv$stationarity_results$adf)) {
    cat("Augmented Dickey-Fuller Test:\n")
    cat("-----------------------------\n")
    result <- ts_rv$stationarity_results$adf
    cat("Test Statistic:", round(result$statistic, 4), "\n")
    cat("P-value:", format.pval(result$p.value), "\n")
    cat("Interpretation:", ifelse(result$p.value < 0.05, 
                                  "Series is stationary (reject null hypothesis)",
                                  "Series is non-stationary (fail to reject null hypothesis)"), "\n\n")
  }
  
  if (!is.null(ts_rv$stationarity_results$kpss)) {
    cat("KPSS Test:\n")
    cat("-----------\n")
    result <- ts_rv$stationarity_results$kpss
    cat("Test Statistic:", round(result$statistic, 4), "\n")
    cat("P-value:", format.pval(result$p.value), "\n")
    cat("Interpretation:", ifelse(result$p.value < 0.05, 
                                  "Series is non-stationary (reject null hypothesis)",
                                  "Series is stationary (fail to reject null hypothesis)"), "\n\n")
  }
  
  if (!is.null(ts_rv$stationarity_results$pp)) {
    cat("Phillips-Perron Test:\n")
    cat("---------------------\n")
    result <- ts_rv$stationarity_results$pp
    cat("Test Statistic:", round(result$statistic, 4), "\n")
    cat("P-value:", format.pval(result$p.value), "\n")
    cat("Interpretation:", ifelse(result$p.value < 0.05, 
                                  "Series is stationary (reject null hypothesis)",
                                  "Series is non-stationary (fail to reject null hypothesis)"), "\n\n")
  }
})

# Apply differencing
observeEvent(input$apply_differencing, {
  req(ts_rv$ts_object, input$ts_stationarity_variable, input$ts_diff_order)
  
  tryCatch({
    ts_obj <- ts_rv$ts_object[[input$ts_stationarity_variable]]
    
    # Apply differencing
    diff_ts <- diff(ts_obj, differences = input$ts_diff_order)
    
    # Store differenced series
    ts_rv$differenced_series <- diff_ts
    
    showNotification(paste("Applied", input$ts_diff_order, "order differencing."), 
                     type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error in differencing:", e$message), 
                     type = "error", duration = 10)
  })
})

# Differenced series plot
output$ts_differenced_plot <- renderPlot({
  req(ts_rv$differenced_series)
  
  plot(ts_rv$differenced_series, 
       main = paste("Differenced Series (Order", input$ts_diff_order, ")"),
       ylab = "Differenced Values",
       col = "steelblue",
       lwd = 1.5)
})

# Generate ACF/PACF plots
observeEvent(input$plot_acf_pacf, {
  req(ts_rv$ts_object, input$ts_stationarity_variable, input$ts_acf_lags)
  
  ts_rv$acf_pacf_ready <- TRUE
})

# ACF/PACF plot
output$ts_acf_pacf_plot <- renderPlot({
  req(ts_rv$acf_pacf_ready, ts_rv$ts_object, input$ts_stationarity_variable)
  
  # Use differenced series if available, otherwise original
  if (!is.null(ts_rv$differenced_series)) {
    ts_data <- ts_rv$differenced_series
    title_suffix <- " (Differenced)"
  } else {
    ts_data <- ts_rv$ts_object[[input$ts_stationarity_variable]]
    title_suffix <- ""
  }
  
  # Create ACF and PACF plots
  par(mfrow = c(2, 1))
  
  acf(ts_data, lag.max = input$ts_acf_lags, 
      main = paste("ACF", title_suffix),
      col = "steelblue")
  
  pacf(ts_data, lag.max = input$ts_acf_lags, 
       main = paste("PACF", title_suffix),
       col = "darkred")
})

# 5. FORECASTING TAB ============================

# Variable selector for forecasting
output$ts_forecast_variable_selector <- renderUI({
  req(ts_rv$ts_object)
  
  selectInput("ts_forecast_variable", "Select Variable:",
              choices = names(ts_rv$ts_object),
              selected = names(ts_rv$ts_object)[1])
})

# Generate forecast
observeEvent(input$generate_forecast, {
  req(ts_rv$ts_object, input$ts_forecast_variable, input$ts_forecast_method,
      input$ts_forecast_horizon, input$ts_train_split)
  
  tryCatch({
    ts_obj <- ts_rv$ts_object[[input$ts_forecast_variable]]
    
    # Split data into training and testing
    n <- length(ts_obj)
    train_size <- floor(n * input$ts_train_split / 100)
    train_data <- window(ts_obj, end = time(ts_obj)[train_size])
    test_data <- window(ts_obj, start = time(ts_obj)[train_size + 1])
    
    # Generate forecast based on method
    if (input$ts_forecast_method == "auto_arima") {
      library(forecast)
      model <- auto.arima(train_data)
      forecast_result <- forecast(model, h = input$ts_forecast_horizon)
    } else if (input$ts_forecast_method == "ets") {
      library(forecast)
      model <- ets(train_data)
      forecast_result <- forecast(model, h = input$ts_forecast_horizon)
    } else if (input$ts_forecast_method == "linear") {
      library(forecast)
      model <- tslm(train_data ~ trend)
      forecast_result <- forecast(model, h = input$ts_forecast_horizon)
    } else if (input$ts_forecast_method == "snaive") {
      library(forecast)
      forecast_result <- snaive(train_data, h = input$ts_forecast_horizon)
    } else if (input$ts_forecast_method == "ma") {
      # Simple moving average forecast
      ma_order <- input$ts_ma_order
      if (length(train_data) >= ma_order) {
        last_values <- tail(train_data, ma_order)
        forecast_value <- mean(last_values, na.rm = TRUE)
        
        # Create a simple forecast object
        forecast_result <- list(
          mean = rep(forecast_value, input$ts_forecast_horizon),
          method = paste("Moving Average (", ma_order, ")"),
          x = train_data
        )
        class(forecast_result) <- "forecast"
      }
    }
    
    # Store results
    ts_rv$forecast_result <- forecast_result
    ts_rv$test_data <- test_data
    ts_rv$train_data <- train_data
    
    showNotification("Forecast generated successfully!", type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error in forecasting:", e$message), 
                     type = "error", duration = 10)
  })
})

# Forecast plot
output$ts_forecast_plot <- renderPlotly({
  req(ts_rv$forecast_result, ts_rv$train_data)
  
  # Create plotly chart
  p <- plot_ly()
  
  # Add training data
  p <- p %>% add_trace(
    x = time(ts_rv$train_data),
    y = as.numeric(ts_rv$train_data),
    type = 'scatter',
    mode = 'lines',
    name = 'Training Data',
    line = list(color = 'blue')
  )
  
  # Add test data if available
  if (!is.null(ts_rv$test_data) && length(ts_rv$test_data) > 0) {
    p <- p %>% add_trace(
      x = time(ts_rv$test_data),
      y = as.numeric(ts_rv$test_data),
      type = 'scatter',
      mode = 'lines',
      name = 'Test Data',
      line = list(color = 'green')
    )
  }
  
  # Add forecast
  forecast_times <- seq(from = end(ts_rv$train_data)[1] + 1/frequency(ts_rv$train_data),
                        length.out = length(ts_rv$forecast_result$mean),
                        by = 1/frequency(ts_rv$train_data))
  
  p <- p %>% add_trace(
    x = forecast_times,
    y = as.numeric(ts_rv$forecast_result$mean),
    type = 'scatter',
    mode = 'lines',
    name = 'Forecast',
    line = list(color = 'red', dash = 'dash')
  )
  
  # Add confidence intervals if available
  if (!is.null(ts_rv$forecast_result$upper) && !is.null(ts_rv$forecast_result$lower)) {
    # 95% confidence interval
    if (ncol(ts_rv$forecast_result$upper) >= 2) {
      p <- p %>% add_ribbons(
        x = forecast_times,
        ymin = as.numeric(ts_rv$forecast_result$lower[,2]),
        ymax = as.numeric(ts_rv$forecast_result$upper[,2]),
        name = '95% CI',
        fillcolor = 'rgba(255,0,0,0.2)',
        line = list(color = 'transparent')
      )
    }
    
    # 80% confidence interval
    if (ncol(ts_rv$forecast_result$upper) >= 1) {
      p <- p %>% add_ribbons(
        x = forecast_times,
        ymin = as.numeric(ts_rv$forecast_result$lower[,1]),
        ymax = as.numeric(ts_rv$forecast_result$upper[,1]),
        name = '80% CI',
        fillcolor = 'rgba(255,0,0,0.3)',
        line = list(color = 'transparent')
      )
    }
  }
  
  p <- p %>% layout(
    title = "Time Series Forecast",
    xaxis = list(title = "Time"),
    yaxis = list(title = "Value"),
    hovermode = 'x unified'
  )
  
  return(p)
})

# Forecast accuracy metrics
output$ts_forecast_accuracy <- renderDT({
  req(ts_rv$forecast_result, ts_rv$test_data)
  
  if (length(ts_rv$test_data) > 0) {
    # Calculate accuracy metrics
    library(forecast)
    
    # Get overlapping period for comparison
    forecast_values <- ts_rv$forecast_result$mean
    test_values <- as.numeric(ts_rv$test_data)
    
    # Truncate to minimum length
    min_length <- min(length(forecast_values), length(test_values))
    forecast_values <- forecast_values[1:min_length]
    test_values <- test_values[1:min_length]
    
    if (min_length > 0) {
      accuracy_metrics <- accuracy(forecast_values, test_values)
      
      # Create data frame for display
      metrics_df <- data.frame(
        Metric = rownames(accuracy_metrics),
        Value = round(accuracy_metrics[,1], 4),
        stringsAsFactors = FALSE
      )
      
      datatable(metrics_df, 
                options = list(dom = 't', pageLength = 10),
                rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No test data available for accuracy calculation"),
                options = list(dom = 't'), rownames = FALSE)
    }
  } else {
    datatable(data.frame(Message = "No test data available for accuracy calculation"),
              options = list(dom = 't'), rownames = FALSE)
  }
})

# Forecast diagnostics plot
output$ts_forecast_diagnostics <- renderPlot({
  req(ts_rv$forecast_result)
  
  # Get residuals if available
  if (!is.null(ts_rv$forecast_result$residuals)) {
    residuals <- ts_rv$forecast_result$residuals
  } else if (!is.null(ts_rv$forecast_result$model) && !is.null(residuals(ts_rv$forecast_result$model))) {
    residuals <- residuals(ts_rv$forecast_result$model)
  } else {
    # Calculate residuals manually
    fitted_values <- ts_rv$train_data - ts_rv$forecast_result$x
    residuals <- fitted_values
  }
  
  if (!is.null(residuals) && length(residuals) > 1) {
    par(mfrow = c(2, 2))
    
    # Residuals vs Fitted
    plot(residuals, main = "Residuals vs Time", 
         ylab = "Residuals", type = "l", col = "steelblue")
    abline(h = 0, col = "red", lty = 2)
    
    # Q-Q plot
    qqnorm(residuals, main = "Normal Q-Q Plot")
    qqline(residuals, col = "red")
    
    # ACF of residuals
    acf(residuals, main = "ACF of Residuals", na.action = na.pass)
    
    # Histogram of residuals
    hist(residuals, main = "Histogram of Residuals", 
         xlab = "Residuals", col = "lightblue", border = "black")
  } else {
    plot(1, type = "n", main = "Diagnostics not available", 
         xlab = "", ylab = "", axes = FALSE)
    text(1, 1, "Residuals not available for this forecast method", cex = 1.2)
  }
})

# 6. ANOMALY DETECTION TAB ============================

# Variable selector for anomaly detection
output$ts_anomaly_variable_selector <- renderUI({
  req(ts_rv$ts_data)
  
  value_cols <- names(ts_rv$ts_data)[-1]
  
  selectInput("ts_anomaly_variable", "Select Variable:",
              choices = value_cols,
              selected = value_cols[1])
})

# Detect anomalies
observeEvent(input$detect_anomalies, {
  req(ts_rv$ts_data, input$ts_anomaly_variable, input$ts_anomaly_method)
  
  tryCatch({
    data <- ts_rv$ts_data
    values <- data[[input$ts_anomaly_variable]]
    dates <- data$date
    
    # Remove NA values
    valid_idx <- !is.na(values)
    values <- values[valid_idx]
    dates <- dates[valid_idx]
    
    anomaly_idx <- c()
    
    if (input$ts_anomaly_method == "zscore") {
      # Z-score method
      z_scores <- abs(scale(values))
      anomaly_idx <- which(z_scores > input$ts_anomaly_threshold)
      
    } else if (input$ts_anomaly_method == "iqr") {
      # IQR method
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - input$ts_iqr_multiplier * IQR
      upper_bound <- Q3 + input$ts_iqr_multiplier * IQR
      
      anomaly_idx <- which(values < lower_bound | values > upper_bound)
      
    } else if (input$ts_anomaly_method == "isolation") {
      # Isolation Forest (simplified version using statistical approach)
      # Since we don't have isotree package, use a statistical approximation
      median_val <- median(values, na.rm = TRUE)
      mad_val <- mad(values, na.rm = TRUE)
      
      # Points that are more than 3 MADs from median
      anomaly_idx <- which(abs(values - median_val) > 3 * mad_val)
      
    } else if (input$ts_anomaly_method == "seasonal") {
      # Seasonal decomposition method
      if (input$ts_anomaly_variable %in% names(ts_rv$ts_object)) {
        ts_obj <- ts_rv$ts_object[[input$ts_anomaly_variable]]
        
        # Perform seasonal decomposition
        if (frequency(ts_obj) > 1) {
          decomp <- stl(ts_obj, s.window = input$ts_seasonal_window)
          residuals <- decomp$time.series[, "remainder"]
          
          # Find anomalies in residuals
          residual_threshold <- 2 * sd(residuals, na.rm = TRUE)
          anomaly_idx <- which(abs(residuals) > residual_threshold)
        } else {
          # No seasonality, use simple statistical method
          threshold <- 2 * sd(values, na.rm = TRUE)
          mean_val <- mean(values, na.rm = TRUE)
          anomaly_idx <- which(abs(values - mean_val) > threshold)
        }
      }
    }
    
    # Create anomaly results
    anomalies_data <- data.frame(
      Date = dates[anomaly_idx],
      Value = values[anomaly_idx],
      Index = anomaly_idx,
      stringsAsFactors = FALSE
    )
    
    ts_rv$anomalies <- list(
      data = anomalies_data,
      all_dates = dates,
      all_values = values,
      anomaly_indices = anomaly_idx
    )
    
    showNotification(paste("Detected", length(anomaly_idx), "anomalies"), 
                     type = "message", duration = 3)
    
  }, error = function(e) {
    showNotification(paste("Error in anomaly detection:", e$message), 
                     type = "error", duration = 10)
  })
})

# Anomaly detection plot
output$ts_anomaly_plot <- renderPlotly({
  req(ts_rv$anomalies)
  
  p <- plot_ly()
  
  # Add normal data points
  p <- p %>% add_trace(
    x = ts_rv$anomalies$all_dates,
    y = ts_rv$anomalies$all_values,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Normal Data',
    line = list(color = 'blue'),
    marker = list(size = 4, color = 'blue')
  )
  
  # Add anomalies
  if (nrow(ts_rv$anomalies$data) > 0) {
    p <- p %>% add_trace(
      x = ts_rv$anomalies$data$Date,
      y = ts_rv$anomalies$data$Value,
      type = 'scatter',
      mode = 'markers',
      name = 'Anomalies',
      marker = list(size = 8, color = 'red', symbol = 'x')
    )
  }
  
  p <- p %>% layout(
    title = "Anomaly Detection Results",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Value"),
    hovermode = 'x unified'
  )
  
  return(p)
})

# Anomaly summary
output$ts_anomaly_summary <- renderPrint({
  req(ts_rv$anomalies)
  
  cat("Anomaly Detection Summary:\n")
  cat("==========================\n")
  cat("Total observations:", length(ts_rv$anomalies$all_values), "\n")
  cat("Anomalies detected:", nrow(ts_rv$anomalies$data), "\n")
  cat("Anomaly rate:", round(nrow(ts_rv$anomalies$data) / length(ts_rv$anomalies$all_values) * 100, 2), "%\n\n")
  
  if (nrow(ts_rv$anomalies$data) > 0) {
    cat("Anomaly Statistics:\n")
    cat("Min anomaly value:", round(min(ts_rv$anomalies$data$Value), 4), "\n")
    cat("Max anomaly value:", round(max(ts_rv$anomalies$data$Value), 4), "\n")
    cat("Mean anomaly value:", round(mean(ts_rv$anomalies$data$Value), 4), "\n")
  }
})

# Anomaly table
output$ts_anomaly_table <- renderDT({
  req(ts_rv$anomalies)
  
  if (nrow(ts_rv$anomalies$data) > 0) {
    display_data <- ts_rv$anomalies$data
    display_data$Value <- round(display_data$Value, 4)
    
    datatable(display_data,
              options = list(
                pageLength = 15,
                scrollX = TRUE,
                order = list(list(0, 'desc'))  # Sort by date descending
              ),
              rownames = FALSE) %>%
      formatStyle('Value', 
                  backgroundColor = 'rgba(255, 0, 0, 0.1)',
                  fontWeight = 'bold')
  } else {
    datatable(data.frame(Message = "No anomalies detected"),
              options = list(dom = 't'), 
              rownames = FALSE)
  }
})

# Helper function to format p-values
format.pval <- function(pv, digits = 4) {
  if (is.na(pv)) return("NA")
  if (pv < 0.001) return("< 0.001")
  if (pv < 0.01) return(paste("=", format(round(pv, 3), nsmall = 3)))
  return(paste("=", format(round(pv, digits), nsmall = digits)))
}

# MODELING TAB SERVER LOGIC ============================

# Initialize reactive values for models
rv$models <- list()

# DYNAMIC UI ELEMENTS FOR MODEL INPUTS ============================

# Linear Regression UI Elements
output$linear_target_var <- renderUI({
  req(rv$filtered_data)
  numeric_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, is.numeric)]
  selectInput("linear_target", "Target Variable (Y):", 
              choices = numeric_vars, selected = numeric_vars[1])
})

output$linear_predictor_var <- renderUI({
  req(rv$filtered_data, input$linear_target)
  numeric_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, is.numeric)]
  predictor_vars <- setdiff(numeric_vars, input$linear_target)
  selectInput("linear_predictor", "Predictor Variable (X):", 
              choices = predictor_vars, selected = predictor_vars[1])
})

output$linear_multiple_predictors <- renderUI({
  req(rv$filtered_data, input$linear_target)
  all_vars <- names(rv$filtered_data)
  predictor_vars <- setdiff(all_vars, input$linear_target)
  pickerInput("linear_multiple_pred", "Multiple Predictors:", 
              choices = predictor_vars, multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

# Logistic Regression UI Elements
output$logistic_target_var <- renderUI({
  req(rv$filtered_data)
  factor_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) is.factor(x) || is.character(x))]
  selectInput("logistic_target", "Target Variable (Binary):", 
              choices = factor_vars, selected = factor_vars[1])
})

output$logistic_predictor_vars <- renderUI({
  req(rv$filtered_data, input$logistic_target)
  all_vars <- names(rv$filtered_data)
  predictor_vars <- setdiff(all_vars, input$logistic_target)
  pickerInput("logistic_predictors", "Predictor Variables:", 
              choices = predictor_vars, multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

# Category combination logic for logistic regression
output$show_logistic_category_combine <- reactive({
  req(input$logistic_target, rv$filtered_data)
  target_var <- rv$filtered_data[[input$logistic_target]]
  length(unique(target_var)) > 2
})
outputOptions(output, "show_logistic_category_combine", suspendWhenHidden = FALSE)

output$logistic_current_categories <- renderText({
  req(input$logistic_target, rv$filtered_data)
  target_var <- rv$filtered_data[[input$logistic_target]]
  paste(unique(as.character(target_var)), collapse = ", ")
})

# Multinomial Logistic Regression UI Elements
output$multinomial_target_var <- renderUI({
  req(rv$filtered_data)
  factor_vars <- names(rv$filtered_data)[sapply(rv$filtered_data, function(x) is.factor(x) || is.character(x))]
  selectInput("multinomial_target", "Target Variable (Multi-class):", 
              choices = factor_vars, selected = factor_vars[1])
})

output$multinomial_predictor_vars <- renderUI({
  req(rv$filtered_data, input$multinomial_target)
  all_vars <- names(rv$filtered_data)
  predictor_vars <- setdiff(all_vars, input$multinomial_target)
  pickerInput("multinomial_predictors", "Predictor Variables:", 
              choices = predictor_vars, multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$multinomial_reference_level <- renderUI({
  req(input$multinomial_target, rv$filtered_data)
  target_var <- rv$filtered_data[[input$multinomial_target]]
  levels_list <- unique(as.character(target_var))
  selectInput("multinomial_ref_level", "Reference Level:", 
              choices = levels_list, selected = levels_list[1])
})

# Category combination logic for multinomial regression
output$show_multinomial_category_combine <- reactive({
  req(input$multinomial_target, rv$filtered_data)
  target_var <- rv$filtered_data[[input$multinomial_target]]
  length(unique(target_var)) > 3
})
outputOptions(output, "show_multinomial_category_combine", suspendWhenHidden = FALSE)

output$multinomial_current_categories <- renderText({
  req(input$multinomial_target, rv$filtered_data)
  target_var <- rv$filtered_data[[input$multinomial_target]]
  paste(unique(as.character(target_var)), collapse = ", ")
})

# LINEAR REGRESSION MODEL FITTING ============================

observeEvent(input$fit_linear_model, {
  req(rv$filtered_data, input$linear_target)
  
  tryCatch({
    # Prepare data
    model_data <- rv$filtered_data
    
    # Create formula
    if (length(input$linear_multiple_pred) > 0) {
      # Multiple regression
      predictors <- input$linear_multiple_pred
      if (input$include_interactions && length(predictors) > 1) {
        formula_str <- paste(input$linear_target, "~", paste(predictors, collapse = " * "))
      } else {
        formula_str <- paste(input$linear_target, "~", paste(predictors, collapse = " + "))
      }
    } else if (!is.null(input$linear_predictor)) {
      # Simple regression
      formula_str <- paste(input$linear_target, "~", input$linear_predictor)
    } else {
      showNotification("Please select predictor variables.", type = "warning")
      return()
    }
    
    # Standardize variables if requested
    if (input$standardize_vars) {
      numeric_cols <- sapply(model_data, is.numeric)
      model_data[numeric_cols] <- scale(model_data[numeric_cols])
    }
    
    # Fit model
    formula_obj <- as.formula(formula_str)
    linear_model <- lm(formula_obj, data = model_data)
    
    # Store model
    rv$models$linear <- list(
      model = linear_model,
      formula = formula_str,
      type = "linear",
      data = model_data,
      metrics = list(
        r_squared = summary(linear_model)$r.squared,
        adj_r_squared = summary(linear_model)$adj.r.squared,
        rmse = sqrt(mean(residuals(linear_model)^2)),
        aic = AIC(linear_model),
        bic = BIC(linear_model)
      )
    )
    
    showNotification("Linear regression model fitted successfully!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error fitting linear model:", e$message), type = "error")
  })
})

# Linear model outputs
output$linear_equation <- renderText({
  req(rv$models$linear)
  rv$models$linear$formula
})

output$linear_model_summary <- renderPrint({
  req(rv$models$linear)
  summary(rv$models$linear$model)
})

output$linear_residual_plots <- renderPlot({
  req(rv$models$linear)
  model <- rv$models$linear$model
  
  par(mfrow = c(2, 2))
  plot(model)
})

output$linear_diagnostic_plots <- renderPlot({
  req(rv$models$linear)
  model <- rv$models$linear$model
  
  par(mfrow = c(2, 2))
  
  # Residuals vs Fitted
  plot(fitted(model), residuals(model), main = "Residuals vs Fitted",
       xlab = "Fitted Values", ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)
  
  # Q-Q Plot
  qqnorm(residuals(model))
  qqline(residuals(model), col = "red")
  
  # Scale-Location
  plot(fitted(model), sqrt(abs(residuals(model))), main = "Scale-Location",
       xlab = "Fitted Values", ylab = "√|Residuals|")
  
  # Cook's Distance
  plot(cooks.distance(model), main = "Cook's Distance",
       xlab = "Observation", ylab = "Cook's Distance")
  abline(h = 4/length(residuals(model)), col = "red", lty = 2)
})

# LOGISTIC REGRESSION MODEL FITTING ============================

# Category combination for logistic regression
observeEvent(input$apply_logistic_combine, {
  req(input$logistic_target, input$logistic_combine_categories, input$logistic_new_category_name)
  
  categories_to_combine <- trimws(strsplit(input$logistic_combine_categories, ",")[[1]])
  new_name <- input$logistic_new_category_name
  
  # Apply combination
  target_var <- as.character(rv$filtered_data[[input$logistic_target]])
  target_var[target_var %in% categories_to_combine] <- new_name
  rv$filtered_data[[input$logistic_target]] <- as.factor(target_var)
  
  showNotification("Categories combined successfully!", type = "message")
})

observeEvent(input$reset_logistic_categories, {
  req(input$logistic_target)
  # Reset to original data
  original_var <- rv$filtered_data[[input$logistic_target]]
  rv$filtered_data[[input$logistic_target]] <- original_var
  showNotification("Categories reset to original.", type = "message")
})

observeEvent(input$fit_logistic_model, {
  req(rv$filtered_data, input$logistic_target, input$logistic_predictors)
  
  tryCatch({
    # Prepare data
    model_data <- rv$filtered_data
    target_var <- as.factor(model_data[[input$logistic_target]])
    
    # Check if binary
    if (length(levels(target_var)) != 2) {
      showNotification("Target variable must be binary for logistic regression. Use category combination or multinomial regression.", type = "warning")
      return()
    }
    
    # Create formula
    formula_str <- paste(input$logistic_target, "~", paste(input$logistic_predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model
    logistic_model <- glm(formula_obj, data = model_data, family = binomial())
    
    # Calculate predictions and metrics
    predicted_probs <- predict(logistic_model, type = "response")
    predicted_classes <- ifelse(predicted_probs > input$classification_threshold, 
                                levels(target_var)[2], levels(target_var)[1])
    
    # Confusion matrix
    conf_matrix <- table(Actual = target_var, Predicted = predicted_classes)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    # Store model
    rv$models$logistic <- list(
      model = logistic_model,
      formula = formula_str,
      type = "logistic",
      data = model_data,
      predictions = predicted_probs,
      predicted_classes = predicted_classes,
      confusion_matrix = conf_matrix,
      metrics = list(
        accuracy = accuracy,
        aic = AIC(logistic_model),
        bic = BIC(logistic_model),
        deviance = deviance(logistic_model),
        null_deviance = logistic_model$null.deviance
      )
    )
    
    showNotification("Logistic regression model fitted successfully!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error fitting logistic model:", e$message), type = "error")
  })
})

# Logistic model outputs
output$logistic_equation <- renderText({
  req(rv$models$logistic)
  paste("log(p/(1-p)) =", rv$models$logistic$formula)
})

output$logistic_model_summary <- renderPrint({
  req(rv$models$logistic)
  summary(rv$models$logistic$model)
})

output$logistic_roc_curve <- renderPlot({
  req(rv$models$logistic)
  
  # ROC curve calculation
  predictions <- rv$models$logistic$predictions
  actual <- as.numeric(rv$filtered_data[[input$logistic_target]]) - 1
  
  # Calculate ROC points
  thresholds <- seq(0, 1, by = 0.01)
  roc_data <- data.frame(
    threshold = thresholds,
    tpr = numeric(length(thresholds)),
    fpr = numeric(length(thresholds))
  )
  
  for (i in seq_along(thresholds)) {
    predicted_binary <- as.numeric(predictions > thresholds[i])
    tp <- sum(predicted_binary == 1 & actual == 1)
    fp <- sum(predicted_binary == 1 & actual == 0)
    tn <- sum(predicted_binary == 0 & actual == 0)
    fn <- sum(predicted_binary == 0 & actual == 1)
    
    roc_data$tpr[i] <- tp / (tp + fn)
    roc_data$fpr[i] <- fp / (fp + tn)
  }
  
  # Calculate AUC
  auc <- -sum(diff(roc_data$fpr) * (roc_data$tpr[-1] + roc_data$tpr[-length(roc_data$tpr)])) / 2
  
  plot(roc_data$fpr, roc_data$tpr, type = "l", col = "blue", lwd = 2,
       main = paste("ROC Curve (AUC =", round(auc, 3), ")"),
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  abline(0, 1, col = "red", lty = 2)
  grid()
})

output$logistic_metrics <- renderDT({
  req(rv$models$logistic)
  
  conf_matrix <- rv$models$logistic$confusion_matrix
  
  # Calculate additional metrics
  tp <- conf_matrix[2, 2]
  fp <- conf_matrix[1, 2]
  tn <- conf_matrix[1, 1]
  fn <- conf_matrix[2, 1]
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  specificity <- tn / (tn + fp)
  
  metrics_df <- data.frame(
    Metric = c("Accuracy", "Precision", "Recall", "F1-Score", "Specificity"),
    Value = round(c(rv$models$logistic$metrics$accuracy, precision, recall, f1_score, specificity), 4)
  )
  
  datatable(metrics_df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
})

# MULTINOMIAL LOGISTIC REGRESSION ============================

# Category combination for multinomial regression
observeEvent(input$apply_multinomial_combine, {
  req(input$multinomial_target, input$multinomial_combine_categories, input$multinomial_new_category_name)
  
  categories_to_combine <- trimws(strsplit(input$multinomial_combine_categories, ",")[[1]])
  new_name <- input$multinomial_new_category_name
  
  # Apply combination
  target_var <- as.character(rv$filtered_data[[input$multinomial_target]])
  target_var[target_var %in% categories_to_combine] <- new_name
  rv$filtered_data[[input$multinomial_target]] <- as.factor(target_var)
  
  showNotification("Categories combined successfully!", type = "message")
})

observeEvent(input$reset_multinomial_categories, {
  req(input$multinomial_target)
  # Reset to original data
  original_var <- rv$fitered_data[[input$multinomial_target]]
  rv$filtered_data[[input$multinomial_target]] <- original_var
  showNotification("Categories reset to original.", type = "message")
})

observeEvent(input$fit_multinomial_model, {
  req(rv$filtered_data, input$multinomial_target, input$multinomial_predictors)
  
  tryCatch({
    # Load required library
    if (!require(nnet, quietly = TRUE)) {
      showNotification("nnet package required for multinomial regression. Please install it.", type = "error")
      return()
    }
    
    # Prepare data
    model_data <- rv$filtered_data
    target_var <- as.factor(model_data[[input$multinomial_target]])
    
    # Set reference level
    if (!is.null(input$multinomial_ref_level)) {
      target_var <- relevel(target_var, ref = input$multinomial_ref_level)
      model_data[[input$multinomial_target]] <- target_var
    }
    
    # Create formula
    formula_str <- paste(input$multinomial_target, "~", paste(input$multinomial_predictors, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Fit model
    multinomial_model <- multinom(formula_obj, data = model_data, trace = FALSE)
    
    # Predictions
    predicted_classes <- predict(multinomial_model, type = "class")
    predicted_probs <- predict(multinomial_model, type = "probs")
    
    # Confusion matrix
    conf_matrix <- table(Actual = target_var, Predicted = predicted_classes)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    # Store model
    rv$models$multinomial <- list(
      model = multinomial_model,
      formula = formula_str,
      type = "multinomial",
      data = model_data,
      predictions = predicted_probs,
      predicted_classes = predicted_classes,
      confusion_matrix = conf_matrix,
      metrics = list(
        accuracy = accuracy,
        aic = AIC(multinomial_model),
        bic = BIC(multinomial_model),
        deviance = deviance(multinomial_model)
      )
    )
    
    showNotification("Multinomial logistic regression model fitted successfully!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error fitting multinomial model:", e$message), type = "error")
  })
})

# Multinomial model outputs
output$multinomial_equation <- renderText({
  req(rv$models$multinomial)
  paste("Multinomial Model:", rv$models$multinomial$formula)
})

output$multinomial_model_summary <- renderPrint({
  req(rv$models$multinomial)
  summary(rv$models$multinomial$model)
})

output$multinomial_confusion_matrix <- renderPlot({
  req(rv$models$multinomial)
  
  conf_matrix <- rv$models$multinomial$confusion_matrix
  
  # Convert to data frame for plotting
  conf_df <- as.data.frame(conf_matrix)
  
  ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 4) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$multinomial_classification_report <- renderDT({
  req(rv$models$multinomial)
  
  conf_matrix <- rv$models$multinomial$confusion_matrix
  classes <- rownames(conf_matrix)
  
  # Calculate metrics for each class
  report_data <- data.frame(
    Class = classes,
    Precision = numeric(length(classes)),
    Recall = numeric(length(classes)),
    F1_Score = numeric(length(classes)),
    Support = numeric(length(classes))
  )
  
  for (i in seq_along(classes)) {
    tp <- conf_matrix[i, i]
    fp <- sum(conf_matrix[-i, i])
    fn <- sum(conf_matrix[i, -i])
    
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1_score <- ifelse(precision + recall == 0, 0, 2 * (precision * recall) / (precision + recall))
    support <- sum(conf_matrix[i, ])
    
    report_data$Precision[i] <- round(precision, 4)
    report_data$Recall[i] <- round(recall, 4)
    report_data$F1_Score[i] <- round(f1_score, 4)
    report_data$Support[i] <- support
  }
  
  datatable(report_data, options = list(dom = 't', pageLength = 10), rownames = FALSE)
})

# MODEL COMPARISON ============================

output$model_comparison_table <- renderDT({
  # Check if any models exist
  if (length(rv$models) == 0) {
    return(datatable(data.frame(Message = "No models fitted yet."), options = list(dom = 't')))
  }
  
  # Create comparison table
  comparison_data <- data.frame(
    Model = character(),
    Type = character(),
    AIC = numeric(),
    BIC = numeric(),
    Accuracy = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (model_name in names(rv$models)) {
    model_info <- rv$models[[model_name]]
    
    accuracy_val <- if (model_info$type == "linear") {
      model_info$metrics$r_squared
    } else {
      model_info$metrics$accuracy
    }
    
    comparison_data <- rbind(comparison_data, data.frame(
      Model = model_name,
      Type = model_info$type,
      AIC = round(model_info$metrics$aic, 2),
      BIC = round(model_info$metrics$bic, 2),
      Accuracy = round(accuracy_val, 4)
    ))
  }
  
  datatable(comparison_data, options = list(dom = 't', pageLength = 10), rownames = FALSE)
})

output$model_ic_comparison <- renderPlot({
  if (length(rv$models) == 0) {
    plot.new()
    text(0.5, 0.5, "No models to compare", cex = 1.5)
    return()
  }
  
  # Extract AIC and BIC values
  aic_values <- sapply(rv$models, function(x) x$metrics$aic)
  bic_values <- sapply(rv$models, function(x) x$metrics$bic)
  
  # Create comparison plot
  model_names <- names(rv$models)
  
  barplot_data <- rbind(aic_values, bic_values)
  colnames(barplot_data) <- model_names
  rownames(barplot_data) <- c("AIC", "BIC")
  
  barplot(barplot_data, beside = TRUE, 
          main = "Model Information Criteria Comparison",
          ylab = "IC Value", xlab = "Models",
          col = c("lightblue", "lightcoral"),
          legend.text = TRUE)
})

output$model_accuracy_comparison <- renderPlot({
  if (length(rv$models) == 0) {
    plot.new()
    text(0.5, 0.5, "No models to compare", cex = 1.5)
    return()
  }
  
  # Extract accuracy values
  accuracy_values <- sapply(rv$models, function(x) {
    if (x$type == "linear") {
      x$metrics$r_squared
    } else {
      x$metrics$accuracy
    }
  })
  
  model_names <- names(rv$models)
  
  barplot(accuracy_values, names.arg = model_names,
          main = "Model Accuracy Comparison",
          ylab = "Accuracy/R-squared", xlab = "Models",
          col = "lightgreen", ylim = c(0, 1))
  
  # Add value labels on bars
  text(x = seq_along(accuracy_values), y = accuracy_values + 0.02, 
       labels = round(accuracy_values, 3), pos = 3)
})



}

# Run the application
shinyApp(ui = ui, server = server)