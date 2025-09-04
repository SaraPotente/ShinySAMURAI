# ui.R
source("global.R")
ui <- function(x) {
  navbarPage(
    # Custom header with logo and title
    header = tags$div(
      class = "app-header",
      tags$div(
        class = "header-content",
        tags$div(
          class = "logo-container",
          img(src = "samurai_logo_mqc.png", height = "100px", class = "logo", align="center")
        ),
        tags$div(
          class = "title-container",
          tags$h2("ShinySAMURAI", class = "app-title"),
          tags$span("A user-friendly interface for your copy number analysis with SAMURAI", class = "app-subtitle")
        )
      )
    ),
    
    # Main navbar with title button that triggers reset
    title = tags$button("",
                        id = "pipelineButton",
                        class = "action-button navbar-button",
                        title = "If you want to start over, just reload the page.",
                        onMouseOver = "this.style.color='black'",
                        onMouseOut = "this.style.color='darkviolet'",
                        style = "color: darkviolet; font-weight: bold; border: none; background-color: inherit;"),
    windowTitle = "ShinySAMURAI", 
    collapsible = TRUE,
    
    tabPanel(title = span(icon("house"), "Home"),
             # Include your CSS 
             includeCSS("css/custom.css"),
             useShinyjs(),
             #use_notiflix_notify(position = "left-bottom", width = "800px"),
             #
             #shiny::uiOutput("report_button", inline = TRUE),
             #shiny::uiOutput("nxf_report_button", inline = TRUE),
             #shiny::uiOutput("outputFilesLocation", inline = TRUE),
             
             shiny::div(id = "commands_panel",
                        fluidRow(
                          column(6,
                                 shinyFilesButton(id = "input_file",
                                                  label = "Select input file",
                                                  multiple = FALSE,
                                                  buttonType = "default",
                                                  class = NULL,
                                                  style = "
                                       color: #ff0087; 
                                       background-color: white; 
                                       font-weight: bold;
                                       border: 2px solid #e1e5e9;
                                       border-radius: 12px;
                                       padding: 12px 20px;
                                       box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
                                       transition: all 0.3s ease;
                                       font-size: 16px;
                                       cursor: pointer;
                                       display: inline-flex;
                                       align-items: center;
                                       gap: 10px;
                                       margin-bottom: 10px;
                                     ",
                                                  onMouseOver = "
                                       this.style.color = 'white'; 
                                       this.style.backgroundColor = '#ff0087';
                                       this.style.borderColor = '#ff0087';
                                       this.style.boxShadow = '0 4px 16px rgba(255, 0, 135, 0.3)';
                                       this.style.transform = 'translateY(-2px)';
                                     ",
                                                  onMouseOut = "
                                       this.style.color = '#ff0087'; 
                                       this.style.backgroundColor = 'white';
                                       this.style.borderColor = '#e1e5e9';
                                       this.style.boxShadow = '0 2px 8px rgba(0, 0, 0, 0.1)';
                                       this.style.transform = 'translateY(0px)';
                                     ",
                                                  title = "Please select an input file for dincalcilab/samurai.", 
                                                  icon = icon("file")),
                                 
                                 shinyDirButton(id = "outdir", 
                                                label = "Select output directory", 
                                                title = "Please select an output directory for results",
                                                buttonType = "default",
                                                class = NULL,
                                                icon = icon("folder-open"),
                                                style = "
                 color: #0695fd; 
                 background-color: white; 
                 font-weight: bold; 
                 margin-left: 10px;
                 border: 2px solid #e1e5e9;
                 border-radius: 12px;
                 padding: 12px 20px;
                 box-shadow: 0 2px 8px rgba(10, 20, 10, 0.1);
                 transition: all 0.3s ease;
                 font-size: 16px;
                 cursor: pointer;
                 display: inline-flex;
                 align-items: center;
                 gap: 10px;
                 margin-bottom: 10px;
               ",
                                                onMouseOver = "
                 this.style.color = 'white'; 
                 this.style.backgroundColor = '#0695fd';
                 this.style.borderColor = '#0695fd';
                 this.style.boxShadow = '0 4px 16px rgba(6, 149, 253, 0.3)';
                 this.style.transform = 'translateY(-2px)';
               ",
                                                onMouseOut = "
                 this.style.color = '#0695fd'; 
                 this.style.backgroundColor = 'white';
                 this.style.borderColor = '#e1e5e9';
                 this.style.boxShadow = '0 2px 8px rgba(0, 0, 0, 0.1)';
                 this.style.transform = 'translateY(0px)';
               "),
              shinyFilesButton(id = "params_yaml", 
                                                  label = "Select params YAML", 
                                                  multiple = FALSE,
                                                  buttonType = "default", 
                                                  class = NULL,
                                                  style = "
                                       color: #ff0087; 
                                       background-color: white; 
                                       font-weight: bold; 
                                       margin-left: 10px;
                                       border: 2px solid #e1e5e9;
                                       border-radius: 12px;
                                       padding: 12px 20px;
                                       box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
                                       transition: all 0.3s ease;
                                       font-size: 16px;
                                       cursor: pointer;
                                       display: inline-flex;
                                       align-items: center;
                                       gap: 10px;
                                       margin-bottom: 10px;
                                     ", 
                                                  onMouseOver = "
                                       this.style.color = 'white'; 
                                       this.style.backgroundColor = '#ff0087';
                                       this.style.borderColor = '#ff0087';
                                       this.style.boxShadow = '0 4px 16px rgba(255, 0, 135, 0.3)';
                                       this.style.transform = 'translateY(-2px)';
                                     ", 
                                                  onMouseOut = "
                                       this.style.color = '#ff0087'; 
                                       this.style.backgroundColor = 'white';
                                       this.style.borderColor = '#e1e5e9';
                                       this.style.boxShadow = '0 2px 8px rgba(0, 0, 0, 0.1)';
                                       this.style.transform = 'translateY(0px)';
                                     ", 
                                                  title = "Select a YAML file with parameters", 
                                                  icon = icon("file-code"))
                          ),
                          column(6, 
                                 actionButton(
                                   "run", "Run SAMURAI",
                                   class = "primary-button",
                                   style = "color: white; font-weight: bold;",
                                   onMouseOver = "
    this.style.backgroundColor = '#ff0087';
    this.style.borderColor = '#ff0087';
    this.style.boxShadow = '0 4px 16px rgba(255, 0, 135, 0.3)';
    this.style.transform = 'translateY(-2px)';
  ",
                                   onMouseOut = "
    this.style.backgroundColor = '#0695fd';
    this.style.borderColor = '#0695fd';
    this.style.boxShadow = '0 2px 8px rgba(0, 0, 0, 0.1)';
    this.style.transform = 'translateY(0px)';
  ",
                                   icon = icon("play")
                                 )
                                 ,
                                 
                                 actionButton("reset", "Reset", 
                                              icon = icon("redo"))
                          )
                        ),
                        
                        # Panel to display selected paths
                        tags$div(
                          style = "margin-top: 20px; font-size: 1em;",
                          tags$div(id = "input_path_display", style = "margin-bottom: 10px;"),
                          tags$div(id = "output_path_display", style = "margin-bottom: 10px;"),
                          tags$div(id = "config_path_display", style = "margin-bottom: 10px;"),
                          tags$div(id = "yaml_path_display", style = "margin-bottom: 10px;")
                        ),
                        
                        # Pipeline parameters 
                        tags$div(id = "pipeline_params_section",
                                 style = "margin-top: 20px; display: none;",
                                 h4("Pipeline Parameters"),
                                 tags$div(id = "params_container", style = "max-height: 200px; overflow-y: auto;")
                        ),
                        
                        tags$div(id = "optional_inputs",
                                 style = "display: none; margin-top: 15px; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
                                 h4("Advanced Options"),
                                 
                                 fluidRow(
                                   column(6,
                                          selectizeInput("nxf_profile",
                                                         label = "Select nextflow profile",
                                                         choices = c("docker", "conda", "singularity"),
                                                         selected = "singularity",
                                                         multiple = FALSE)
                                   ),
                                   column(6,
                                          
                                          textInput("pipeline_version",
                                                    label = "Pipeline version",
                                                    value = pipeline_info$version)
                                   )
                                 ),
                                 
                                 hr(),
                                 
                                 fluidRow(
                                   column(12,
                                          checkboxInput("tower", "Use Nextflow Tower to monitor run", value = FALSE),
                                          checkboxInput("resume", "Resume previously failed run", value = TRUE)
                                   )
                                 )
                        )
             ),
             
             verbatimTextOutput("stdout")
    ),
    
    tabPanel(title = span(icon("question-circle"), "Help"), 
             includeMarkdown("help.md")),
    
    tabPanel(title = span(icon("chart-bar"), "Results"),
             fluidRow(
               column(12,
                      h3("Pipeline Results Explorer"),
                      p("This tab will be populated with visualizations once a pipeline run is complete."),
                      uiOutput("results_content")
               )
             )
    ),
    
    tabPanel(title = span(icon("cogs"), "Settings"),
             fluidRow(
               column(6,
                      h3("Global Settings"),
                      wellPanel(
                        selectInput("default_profile", "Default Nextflow Profile:", 
                                    choices = c("docker", "conda", "singularity"),
                                    selected = "singularity"),
                        numericInput("default_cpus", "Default CPU Cores:", value = 8),
                        textInput("default_memory", "Default Memory:", value = "16.GB")
                      )
               ),
               column(6,
                      h3("Pipeline Information"),
                      wellPanel(
                        p("Pipeline Version: ", tags$code(pipeline_info$version)),
                        p("Pipeline Updated: ", tags$code(pipeline_info$published_at)),
                        p("Nextflow Version: ", tags$code(nextflow_version)),
                        p("Last App Update: ", tags$code(format(Sys.Date(), "%B %d, %Y"))),
                        tags$hr(),
                        actionButton("check_updates", "Check for Updates", 
                                     icon = icon("sync"), 
                                     style = "color: blue; font-weight: bold;")
                      )
               )
             )
    )
  )
}