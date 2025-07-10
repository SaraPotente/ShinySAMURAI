#### dincalcilab/samurai Shiny App ####

# A shiny frontend for your Nextflow pipeline
libs <- c("shiny", "shinyFiles", "shinyjs", "shinyalert", "shinypop",
          "processx", "stringr", "digest", "dplyr", "fs", "yaml",
          "jsonlite", "httr")
lapply(libs, library, character.only = TRUE)

# Define reactive to track user counts
users <- reactiveValues(count = 0)

# Function to detect Nextflow version
get_nextflow_version <- function() {
  tryCatch({
    nxf_ver <- system2("nextflow", args = "-version", stdout = TRUE, stderr = FALSE)
    if(length(nxf_ver) > 0) {
      ver_line <- nxf_ver[grep("version", nxf_ver)[1]]
      version <- str_extract(ver_line, "\\d+\\.\\d+\\.\\d+")
      return(version)
    } else {
      return("Not detected")
    }
  }, error = function(e) {
    return("Not installed")
  })
}

# Function to get latest pipeline version from GitHub
get_pipeline_version <- function(repo = "dincalcilab/samurai") {
  tryCatch({
    api_url <- paste0("https://api.github.com/repos/", repo, "/releases/latest")
    response <- httr::GET(api_url)
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      return(list(version = data$tag_name, 
                  published_at = substr(data$published_at, 1, 10)))
    } else {
      return(list(version = "1.0.0", published_at = format(Sys.Date())))
    }
  }, error = function(e) {
    return(list(version = "1.0.0", published_at = format(Sys.Date())))
  })
}

# Function to load nextflow.config parameters
parse_nextflow_config <- function(config_file = "nextflow.config") {
  if (!file.exists(config_file)) {
    return(list())
  }
  
  config_text <- readLines(config_file)
  params <- list()
  
  # Extract params section
  in_params <- FALSE
  params_lines <- character()
  
  for (line in config_text) {
    if (grepl("^params\\s*\\{", line)) {
      in_params <- TRUE
      next
    }
    if (in_params && grepl("^\\s*\\}", line)) {
      in_params <- FALSE
      next
    }
    if (in_params) {
      params_lines <- c(params_lines, line)
    }
  }
  
  # Parse parameter values
  for (line in params_lines) {
    # Skip comments and empty lines
    if (grepl("^\\s*//", line) || grepl("^\\s*$", line)) {
      next
    }
    
    # Extract parameter name and value
    param_match <- str_match(line, "\\s*(\\w+)\\s*=\\s*(.+)")
    if (!is.na(param_match[1])) {
      param_name <- param_match[2]
      param_value <- trimws(param_match[3])
      
      # Remove trailing commas
      param_value <- gsub(",\\s*$", "", param_value)
      
      # Handle quoted strings
      if (grepl("^['\"].*['\"]$", param_value)) {
        param_value <- gsub("^['\"]|['\"]$", "", param_value)
      }
      
      params[[param_name]] <- param_value
    }
  }
  
  return(params)
}


# Get pipeline info
pipeline_info <- get_pipeline_version()
nextflow_version <- get_nextflow_version()

#### UI ####
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
    title = tags$button("dincalcilab/samurai",
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
                                              icon = icon("redo")),
                                 
                                 actionButton("docs", "Documentation", 
                                              icon = icon("book"),
                                              class = "rightAlign", 
                                              onclick ="window.open('https://github.com/dincalcilab/samurai', '_blank')"),
                                 
                                 actionButton("more", "Advanced Options", 
                                              icon = icon("cogs"),
                                              style = "float: right;")
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
                                                         selected = "docker",
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
    
    tabPanel(title = span(icon("chart-bar"), "Results Viewer"),
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
                                    selected = "docker"),
                        numericInput("default_cpus", "Default CPU Cores:", value = 8),
                        textInput("default_memory", "Default Memory:", value = "16.GB"),
                        checkboxInput("save_defaults", "Save as defaults", value = FALSE)
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
                      ),
                      h3("Default Parameters"),
                      wellPanel(
                        uiOutput("default_params_display")
                      )
               )
             )
    )
  )
}

#### SERVER ####
server <- function(input, output, session) {
  options(shiny.error = function() { 
    recover()
  })
  ncores <- parallel::detectCores()
  
  nx_notify_success(paste("Hello ", Sys.getenv("LOGNAME"), 
                          "! There are ", ncores, " cores available.", sep = ""))
  
  # Initialization of reactive values
  optional_params <- reactiveValues(
    tower = "", 
    profile = "", 
    resume = "",
    input_file = NULL,
    outdir = NULL,
    config_file = NULL,
    params_yaml = NULL,
    params_list = list(),
    custom_params = NULL
  )
  
  # Update user counts
  isolate({
    users$count <- users$count + 1
  })
  
  # Log user count
  observe({
    writeLines(as.character(users$count), con = "userlog")
  })
  
  # Toggle optional inputs
  observeEvent(input$more, {
    shinyjs::toggle("optional_inputs")
  })
  
  # Generate random hash for report filenames
  report_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)))
  nxf_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)))
  
  # Directory and file selection
  # Enhanced volumes setup with system paths and error handling
  volumes <- tryCatch({
    base_volumes <- c(
      "Home" = fs::path_home(),
      "Desktop" = file.path(fs::path_home(), "Desktop"),
      "Documents" = file.path(fs::path_home(), "Documents"),
      "Downloads" = file.path(fs::path_home(), "Downloads")
    )
    
    # Add system directories if they exist and are accessible
    system_paths <- c(
      "Root" = "/",
      "Mnt" = "/mnt",
      "Media" = "/media",
      "Opt" = "/opt",
      "Tmp" = "/tmp",
      "Var" = "/var",
      "Usr" = "/usr"
    )
    
    # Check which system paths exist and are readable
    accessible_paths <- c()
    for (path_name in names(system_paths)) {
      path_value <- system_paths[[path_name]]
      if (dir.exists(path_value) && file.access(path_value, mode = 4) == 0) {
        accessible_paths[path_name] <- path_value
      }
    }
    
    # Combine base volumes with accessible system paths
    c(base_volumes, accessible_paths)
    
  }, error = function(e) {
    # Fallback to just home directory if there are issues
    warning("Could not set up extended volumes, falling back to home directory")
    c("Home" = fs::path_home())
  })
  
  # Alternative: More comprehensive approach with getVolumes
  volumes_comprehensive <- tryCatch({
    # Start with user directories
    user_volumes <- c(
      "Home" = fs::path_home(),
      "Desktop" = file.path(fs::path_home(), "Desktop"),
      "Documents" = file.path(fs::path_home(), "Documents"),
      "Downloads" = file.path(fs::path_home(), "Downloads")
    )
    
    # Add system volumes from getVolumes if available
    system_volumes <- tryCatch({
      getVolumes()()
    }, error = function(e) {
      c()
    })
    
    # Add common Linux mount points
    linux_mounts <- c(
      "Root" = "/",
      "Mnt" = "/mnt",
      "Media" = "/media"
    )
    
    # Filter to only include existing and accessible paths
    all_volumes <- c(user_volumes, system_volumes, linux_mounts)
    accessible_volumes <- c()
    
    for (vol_name in names(all_volumes)) {
      vol_path <- all_volumes[[vol_name]]
      if (dir.exists(vol_path) && file.access(vol_path, mode = 4) == 0) {
        accessible_volumes[vol_name] <- vol_path
      }
    }
    
    accessible_volumes
    
  }, error = function(e) {
    # Ultimate fallback
    c("Home" = fs::path_home())
  })
  
  # Use the comprehensive version
  volumes <- volumes_comprehensive
  
  # Input file selection
  shinyFileChoose(input, 
                  'input_file', 
                  session = session,
                  roots = volumes, 
                  filetypes = c('', 'csv'))
  
  # Output directory selection  
  shinyDirChoose(input, 
                 'outdir', 
                 session = session,
                 roots = volumes,
                 allowDirCreate = TRUE)
  
  # Config file selection
  shinyFileChoose(input, 
                  'config_file', 
                  session = session,
                  roots = volumes, 
                  filetypes = c('', 'config', 'conf'))  
  
  # YAML parameters file selection
  shinyFileChoose(input, 
                  'params_yaml', 
                  session = session,
                  roots = volumes, 
                  filetypes = c('', 'yaml', 'yml'))
  
  # Handle input file selection
  observeEvent(input$input_file, {
    if(!is.integer(input$input_file)) {
      optional_params$input_file <- parseFilePaths(volumes, input$input_file)$datapath
      shinyjs::html("input_path_display", 
                    paste("<strong>Input file:</strong> ", optional_params$input_file))
    }
  })
  
  # Handle output directory selection
  observeEvent(input$outdir, {
    if(!is.integer(input$outdir)) {
      selected_path <- parseDirPath(volumes, input$outdir)
      
      # Check if the directory exists, if not create it
      if(!dir.exists(selected_path)) {
        tryCatch({
          dir.create(selected_path, recursive = TRUE)
          nx_notify_success(paste("Directory created:", selected_path))
        }, error = function(e) {
          nx_notify_error(paste("Failed to create directory:", e$message))
          return()
        })
      }
      
      optional_params$outdir <- selected_path
      shinyjs::html("output_path_display", 
                    paste("<strong>Output directory:</strong> ", selected_path))
    }
  })
  
  # Handle config file selection
  observeEvent(input$config_file, {
    if(!is.integer(input$config_file)) {
      optional_params$config_file <- parseFilePaths(volumes, input$config_file)$datapath
      shinyjs::html("config_path_display", 
                    paste("<strong>Config file:</strong> ", optional_params$config_file))
    }
  })
  
  # Handle YAML parameters file selection
  observeEvent(input$params_yaml, {
    if(!is.integer(input$params_yaml)) {
      optional_params$params_yaml <- parseFilePaths(volumes, input$params_yaml)$datapath
      shinyjs::html("yaml_path_display", 
                    paste("<strong>Parameters YAML:</strong> ", optional_params$params_yaml))
      
      }
    }  )
  
  # Load default parameters from nextflow.config if it exists
  default_params <- reactive({
    # Try to find nextflow.config in current directory or parent directories
    config_paths <- c(
      "nextflow.config",
      "../nextflow.config",
      "../../nextflow.config"
    )
    
    for(config_path in config_paths) {
      if(file.exists(config_path)) {
        return(parse_nextflow_config(config_path))
      }
    }
    
    return(list())
  })
  
  # Display default parameters in Settings tab
  output$default_params_display <- renderUI({
    params <- default_params()
    
    if(length(params) == 0) {
      return(p("No default parameters found in nextflow.config"))
    }
    
    param_items <- tagList()
    for(name in names(params)) {
      param_items[[name]] <- tags$li(
        tags$strong(name), ": ", tags$code(params[[name]])
      )
    }
    
    tags$ul(param_items)
  })
  
  # Display selected file and construct command
  output$stdout <- renderPrint({
    # Resume parameter
    optional_params$resume <- if(input$resume) {
      "-resume"
    } else {
      ""
    }
    
    # Profile parameter
    optional_params$profile <- input$nxf_profile
    
    # Tower parameter
    optional_params$tower <- if(input$tower) {
      "-with-tower"
    } else {
      ""
    }
    
    # Check if required files are selected
    if(is.null(optional_params$input_file) || is.null(optional_params$outdir)) {
      cat("Please select an input file and output directory to start the pipeline\n")
    } else {
      # Input and output are selected, proceed with command construction
      # Get working directory from input file
      wd <- fs::path_dir(optional_params$input_file)
      resultsdir <- optional_params$outdir
      
      # Base Nextflow command
      nxf_args <- c("run", "dincalcilab/samurai",
                    "-r", input$pipeline_version,
                    "--input", optional_params$input_file,
                    "--outdir", optional_params$outdir,
                    "-profile", optional_params$profile)
      
      # Add custom config if selected
      if(!is.null(optional_params$config_file)) {
        nxf_args <- c(nxf_args, "-c", optional_params$config_file)
      }
      
   if(!is.null(optional_params$params_yaml)) {
        nxf_args <- c(nxf_args, "-params-file", optional_params$params_yaml)
      }
      
      # Add optional parameters
      nxf_args <- c(nxf_args, optional_params$tower, optional_params$resume)
      
      # Store the final command for execution
      nxf_args <<- nxf_args
      
      cat(" Nextflow command to be executed:\n\n",
          "nextflow", paste(nxf_args, collapse = " "))
    }
  })
  
  # Set up progress tracking
  progress <- shiny::Progress$new(min = 0, max = 1, style = "old")
  
  # Run the pipeline when the run button is clicked
  observeEvent(input$run, {
    if(is.null(optional_params$input_file) || is.null(optional_params$outdir)) {
      shinyjs::html(id = "stdout", "\nPlease select an input file and output directory first...", add = TRUE)
      nx_notify_warning("Missing required selections!")
    } else {
      # Disable UI during run
      shinyjs::disable(id = "commands_panel")
      nx_notify_success("Starting pipeline...")
      
      # Change button label during run
      shinyjs::html(id = "run", html = "Running... please wait")
      progress$set(message = "Pipeline running... ", value = 0)
      on.exit(progress$close())
      
      # Run Nextflow and capture output
      withCallingHandlers({
        shinyjs::html(id = "stdout", "")
        
        # Set working directory to the output directory for the run
        p <- processx::run("nextflow", 
                           args = nxf_args,
                           wd = optional_params$outdir,
                           stdout_line_callback = function(line, proc) {message(line)},
                           stderr_to_stdout = TRUE, 
                           error_on_status = FALSE
        )
      }, 
      message = function(m) {
        shinyjs::html(id = "stdout", html = m$message, add = TRUE)
        runjs("document.getElementById('stdout').scrollTo(0,1e9);") # Scroll down
      })
      
      # Handle run completion
      if(p$status == 0) {
        # Hide command panel
        shinyjs::hide("commands_panel")
        
        # Clean work directory
        work_dir <- file.path(optional_params$outdir, "work")
        rmwork <- system2("rm", args = c("-rf", work_dir))
        
        if(rmwork == 0) {
          nx_notify_success(paste("Temp work directory deleted -", work_dir))
        } else {
          nx_notify_warning("Could not delete temp work directory!")
        }
        
        # Copy reports to www/ directory
        # Adjust report paths to match your pipeline's output structure
        main_report <- file.path(optional_params$outdir, "multiqc", "multiqc_report.html")
        nxf_report <- file.path(optional_params$outdir, "pipeline_info", "execution_report.html")
        
        # Copy reports if they exist
        if(file.exists(main_report)) {
          system2("cp", args = c(main_report, paste("www/", report_hash, sep = "")))
        }
        
        if(file.exists(nxf_report)) {
          system2("cp", args = c(nxf_report, paste("www/", nxf_hash, sep = "")))
        }
        
        # Add report buttons
        if(file.exists(main_report)) {
          output$report_button <- renderUI({
            actionButton("report", label = "View MultiQC Report", 
                         icon = icon("file-alt"), 
                         onclick = sprintf("window.open('%s', '_blank')", report_hash)
            )
          })
        }
        
        if(file.exists(nxf_report)) {
          output$nxf_report_button <- renderUI({
            actionButton("nxf", label = "Nextflow Execution Report", 
                         icon = icon("chart-line"), 
                         onclick = sprintf("window.open('%s', '_blank')", nxf_hash)
            )
          })
        }
        
        # Show results location
        output$outputFilesLocation <- renderUI({
          actionButton("outLoc", label = "Results Location", 
                       icon = icon("folder-open"), 
                       onclick = sprintf("window.alert('%s')", optional_params$outdir)
          )
        })
        
        # Show success message
        shinyalert("Run finished!", type = "success", 
                   animation = "slide-from-bottom",
                   text = "Pipeline finished successfully!", 
                   showCancelButton = FALSE, 
                   confirmButtonText = "OK"
        )
      } else {
        # Handle errors
        shinyjs::html(id = "run", html = "Finished with errors")
        shinyjs::enable(id = "commands_panel")
        shinyjs::disable(id = "run")
        shinyalert("Error!", type = "error", 
                   animation = "slide-from-bottom", 
                   text = "Pipeline finished with errors. Press OK to reload the app and try again.", 
                   showCancelButton = TRUE, 
                   callbackJS = "function(x) { if (x == true) {history.go(0);} }"
        )
      }
    }
  })
  
  # Check for updates
  observeEvent(input$check_updates, {
    withProgress(message = 'Checking for updates...', value = 0.5, {
      current_info <- get_pipeline_version()
      
      if(current_info$version != pipeline_info$version) {
        shinyalert(
          title = "Update Available!",
          text = paste0("New version available: ", current_info$version, 
                        "<br>Current version: ", pipeline_info$version),
          type = "info",
          html = TRUE
        )
      } else {
        shinyalert(
          title = "Up to date",
          text = paste0("You are using the latest version: ", pipeline_info$version),
          type = "success"
        )
      }
    })
  })
  
  # Cleanup when session ends
  session$onSessionEnded(function() {
    # Delete temporary reports
    if(file.exists(paste("www/", report_hash, sep = ""))) {
      system2("rm", args = c("-rf", paste("www/", report_hash, sep = "")))
    }
    if(file.exists(paste("www/", nxf_hash, sep = ""))) {
      system2("rm", args = c("-rf", paste("www/", nxf_hash, sep = "")))
    }
    
    # Update user count
    isolate({
      users$count <- users$count - 1
      writeLines(as.character(users$count), con = "userlog")
    })
  })
  
  # Reset buttons functionality
  observeEvent(input$pipelineButton, {
    shinyalert(title = "",
               type = "warning",
               text = "Start again or stay on page?", 
               html = TRUE, 
               confirmButtonText = "Start again", 
               showCancelButton = TRUE, 
               callbackJS = "function(x) { if (x == true) {history.go(0);} }"
    )
  })
  
  observeEvent(input$reset, {
    shinyalert(title = "",
               type = "warning",
               text = "Start again or stay on page?", 
               html = TRUE, 
               confirmButtonText = "Start again", 
               showCancelButton = TRUE, 
               callbackJS = "function(x) { if (x == true) {history.go(0);} }"
    )
  })
}

shinyApp(ui, server)