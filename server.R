# server.R

server <- function(input, output, session) {
  options(shiny.error = function() { 
    recover()
  })
  ncores <- parallel::detectCores()
  
  # nx_notify_success comes from global.R
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
  
  # Update user counts (users reactive value comes from global.R)
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
  # volumes comes from global.R
  
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
    # Path to the specific repository for clarity
    repo_owner <- "dincalciLab"
    repo_name <- "samurai"
    
    # 1. First, try to find nextflow.config in common local development paths
    config_paths <- c(
      "nextflow.config",         # Current working directory
      "../nextflow.config",      # One level up
      "../../nextflow.config"    # Two levels up
    )

    for(config_path in config_paths) {
      if(file.exists(config_path)) {
        message(paste("Found nextflow.config in local path:", config_path))
        # parse_nextflow_config comes from global.R
        return(parse_nextflow_config(config_path))
      }
    }

    # 2. If not found, try to locate it within Nextflow's default asset cache
    # This path is generally $HOME/.nextflow/assets/<owner>/<repo>/
    nxf_cache_dir <- file.path(Sys.getenv("HOME"), ".nextflow", "assets", repo_owner, repo_name)
    nxf_cached_config_path <- file.path(nxf_cache_dir, "nextflow.config")

    if (file.exists(nxf_cached_config_path)) {
      message(paste("Found nextflow.config in Nextflow's cache:", nxf_cached_config_path))
      # parse_nextflow_config comes from global.R
      return(parse_nextflow_config(nxf_cached_config_path))
    } else {
      message(paste("Nextflow config not found in Nextflow's cache:", nxf_cached_config_path))
    }
    
    # 3. If still not found, as a fallback, attempt to download the raw config from GitHub
    # This ensures the app can always get the config if internet is available,
    # but it won't reflect a specific cached version Nextflow might be using.
    raw_config_url <- paste0("https://raw.githubusercontent.com/", repo_owner, "/", repo_name, "/master/nextflow.config")
    temp_config_file <- tempfile(pattern = "nextflow_config_download", fileext = ".config")

    message(paste("Attempting to download nextflow.config from:", raw_config_url))
    download_success <- tryCatch({
      # Use a robust download method
      download.file(raw_config_url, destfile = temp_config_file, method = "curl", quiet = TRUE)
      TRUE
    }, error = function(e) {
      warning("Failed to download nextflow.config from GitHub: ", e$message)
      FALSE
    })

    if (download_success && file.exists(temp_config_file)) {
      message(paste("Successfully downloaded nextflow.config to:", temp_config_file))
      # parse_nextflow_config comes from global.R
      params <- parse_nextflow_config(temp_config_file)
      # Clean up the temporary file after parsing
      file.remove(temp_config_file) 
      return(params)
    } else {
      message("Could not find or download nextflow.config. Returning empty list.")
      return(list())
    }
  })
  
  # Display default parameters in Settings tab
  #output$default_params_display <- renderUI({
  #  params <- default_params()
  #  
  #  if(length(params) == 0) {
  #    return(p("No default parameters found in nextflow.config"))
  #  }
  #  
  #  param_items <- tagList()
  #  for(name in names(params)) {
  #    param_items[[name]] <- tags$li(
  #      tags$strong(name), ": ", tags$code(params[[name]])
  #    )
  #  }
  #  
  #  tags$ul(param_items)
  #})
  
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
      nx_notify_warning("Missing required selections!") # nx_notify_warning comes from global.R
    } else {
      # Disable UI during run
      shinyjs::disable(id = "commands_panel")
      nx_notify_success("Starting pipeline...") # nx_notify_success comes from global.R
      
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
                    
        # Adjust report paths to match your pipeline's output structure
        main_report <- file.path(optional_params$outdir, "multiqc", "multiqc_report.html")
        nxf_report <- file.path(optional_params$outdir, "pipeline_info", "execution_report.html")
        
     
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
                   showCancelButton = TRUE
                   
        )
      }
    }
  })
}
