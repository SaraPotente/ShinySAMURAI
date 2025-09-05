source("global.R")

server <- function(input, output, session) {
  options(shiny.error = function() { recover() })
  ncores <- parallel::detectCores()
  nx_notify_success(paste("Hello ", Sys.getenv("LOGNAME"), "! There are ", ncores, " cores available.", sep = ""))
  
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
  
  # Track user count
  isolate({ users$count <- users$count + 1 })
  observe({ writeLines(as.character(users$count), con = "userlog") })
  
  observeEvent(input$more, { shinyjs::toggle("optional_inputs") })
  
  report_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)))
  nxf_hash <- sprintf("%s_%s.html", as.integer(Sys.time()), digest::digest(runif(1)))
  
  # File system setup
  volumes <- tryCatch({
    user_volumes <- c(
      "Home" = fs::path_home(),
      "Desktop" = file.path(fs::path_home(), "Desktop"),
      "Documents" = file.path(fs::path_home(), "Documents"),
      "Downloads" = file.path(fs::path_home(), "Downloads")
    )
    system_volumes <- tryCatch({ getVolumes()() }, error = function(e) c())
    linux_mounts <- c("Root" = "/", "Mnt" = "/mnt", "Media" = "/media")
    all_volumes <- c(user_volumes, system_volumes, linux_mounts)
    accessible_volumes <- c()
    for (vol_name in names(all_volumes)) {
      vol_path <- all_volumes[[vol_name]]
      if (dir.exists(vol_path) && file.access(vol_path, mode = 4) == 0) {
        accessible_volumes[vol_name] <- vol_path
      }
    }
    accessible_volumes
  }, error = function(e) c("Home" = fs::path_home()))
  
  shinyFileChoose(input, 'input_file', session = session, roots = volumes, filetypes = c('', 'csv'))
  shinyDirChoose(input, 'outdir', session = session, roots = volumes, allowDirCreate = TRUE)
  shinyFileChoose(input, 'config_file', session = session, roots = volumes, filetypes = c('', 'config', 'conf'))
  shinyFileChoose(input, 'params_yaml', session = session, roots = volumes, filetypes = c('', 'yaml', 'yml'))
  
  observeEvent(input$input_file, {
    if (!is.integer(input$input_file)) {
      optional_params$input_file <- parseFilePaths(volumes, input$input_file)$datapath
      shinyjs::html("input_path_display", paste("<strong>Input file:</strong> ", optional_params$input_file))
    }
  })
  
  observeEvent(input$outdir, {
    if (!is.integer(input$outdir)) {
      selected_path <- parseDirPath(volumes, input$outdir)
      if (!dir.exists(selected_path)) {
        tryCatch({
          dir.create(selected_path, recursive = TRUE)
          nx_notify_success(paste("Directory created:", selected_path))
        }, error = function(e) {
          nx_notify_error(paste("Failed to create directory:", e$message))
          return()
        })
      }
      optional_params$outdir <- selected_path
      shinyjs::html("output_path_display", paste("<strong>Output directory:</strong> ", selected_path))
    }
  })
  
  observeEvent(input$config_file, {
    if (!is.integer(input$config_file)) {
      optional_params$config_file <- parseFilePaths(volumes, input$config_file)$datapath
      shinyjs::html("config_path_display", paste("<strong>Config file:</strong> ", optional_params$config_file))
    }
  })
  
  observeEvent(input$params_yaml, {
    if (!is.integer(input$params_yaml)) {
      optional_params$params_yaml <- parseFilePaths(volumes, input$params_yaml)$datapath
      shinyjs::html("yaml_path_display", paste("<strong>Parameters YAML:</strong> ", optional_params$params_yaml))
    }
  })
  
  output$stdout <- renderPrint({
    optional_params$resume <- if (input$resume) "-resume" else ""
    optional_params$profile <- input$nxf_profile
    optional_params$tower <- if (input$tower) "-with-tower" else ""
    
    if (is.null(optional_params$input_file) || is.null(optional_params$outdir)) {
      cat("Please select an input file and output directory to start the pipeline\n")
    } else {
      nxf_args <- c("run", "dincalcilab/samurai", "-r", input$pipeline_version,
                    "--input", optional_params$input_file,
                    "--outdir", optional_params$outdir,
                    "-profile", optional_params$profile)
      if (!is.null(optional_params$config_file)) nxf_args <- c(nxf_args, "-c", optional_params$config_file)
      if (!is.null(optional_params$params_yaml)) nxf_args <- c(nxf_args, "-params-file", optional_params$params_yaml)
      nxf_args <- c(nxf_args, optional_params$tower, optional_params$resume)
      nxf_args <<- nxf_args
      cat(" Nextflow command to be executed:\n\n", "nextflow", paste(nxf_args, collapse = " "))
    }
  })
  
  # Reactive values for MultiQC report
  #main_report <- reactiveVal(NULL)
  #multiqc_path <- reactiveVal(NULL)
  
  # Display results
  output$results_content <- renderUI({
    dir <- shinyFiles::parseDirPath(volumes, input$outdir)
    print(paste("Parsed output directory:", dir))
    
    if (is.null(dir) || dir == "") {
      return(tags$p("No output directory selected"))
    }
    
    # Look for MultiQC reports
    html_files <- list.files(
      file.path(dir, "multiqc"), 
      pattern = "\\.html$", 
      full.names = TRUE, 
      recursive = TRUE
    )
    
    if (length(html_files) > 0) {
      # Pick the most recent report
      latest_report <- html_files[which.max(file.info(html_files)$mtime)]
      print(paste("Found MultiQC report:", latest_report))
      
      # Register path so assets are served by Shiny
      shiny::addResourcePath("multiqc_report", dirname(latest_report))
      
      tags$iframe(
        src = paste0("multiqc_report/", basename(latest_report)),
        width = "100%",
        height = "800px",
        frameborder = 0
      )
    } else {
      tags$p("MultiQC Report Not Available")
    }
  })
  
  # Placeholder text
  output$results_placeholder <- renderUI({
    dir <- shinyFiles::parseDirPath(volumes, input$outdir)
    
    # If no directory selected
    if (is.null(dir) || dir == "") {
      return(tags$p("No output directory selected"))
    }
    
    # Check for MultiQC report
    html_files <- list.files(
      file.path(dir, "multiqc"), 
      pattern = "\\.html$", 
      full.names = TRUE, 
      recursive = TRUE
    )
    
    if (length(html_files) == 0) {
      tags$p("This tab will be populated with visualizations once a pipeline run is complete.")
    } else {
      NULL  # hide placeholder if report exists
    }
  })
  
  
  
  observeEvent(input$refresh_report, {
    path <- main_report()
    if (!is.null(path) && file.exists(path)) {
      main_report(NULL)
      Sys.sleep(0.1)
      main_report(path)
      nx_notify_success("Report refreshed")
    } else {
      nx_notify_warning("No report available to refresh")
    }
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("multiqc_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      path <- main_report()
      if (!is.null(path) && file.exists(path)) {
        file.copy(path, file)
      }
    }
  )
  
  observeEvent(input$run, {
    if (is.null(optional_params$input_file) || is.null(optional_params$outdir)) {
      shinyjs::html(id = "stdout", "\nPlease select an input file and output directory first...", add = TRUE)
      nx_notify_warning("Missing required selections!")
    } else {
      shinyjs::disable(id = "commands_panel")
      nx_notify_success("Starting pipeline...")
      shinyjs::html(id = "run", html = "Running... please wait")
      progress <- shiny::Progress$new(min = 0, max = 1, style = "old")
      progress$set(message = "Pipeline running...", value = 0)
      on.exit(progress$close())
      
      withCallingHandlers({
        shinyjs::html(id = "stdout", "")
        p <- processx::run("nextflow", args = nxf_args, wd = optional_params$outdir,
                           stdout_line_callback = function(line, proc) { message(line) },
                           stderr_to_stdout = TRUE, error_on_status = FALSE)
      }, message = function(m) {
        shinyjs::html(id = "stdout", html = m$message, add = TRUE)
        runjs("document.getElementById('stdout').scrollTo(0,1e9);")
      })
      
      if (p$status == 0) {
        shinyjs::hide("commands_panel")
        rmwork <- system2("rm", args = c("-rf", file.path(optional_params$outdir, "work")))
        if (rmwork == 0) nx_notify_success("Temp work directory deleted")
        
        # Search dynamically for any .html file inside multiqc directory
        report_dir <- file.path(optional_params$outdir, "multiqc")
        report_path <- NULL
        if (dir.exists(report_dir)) {
          html_reports <- list.files(report_dir, pattern = "\\.html$", full.names = TRUE)
          if (length(html_reports) > 0) {
            # Use most recent
            report_path <- html_reports[which.max(file.info(html_reports)$mtime)]
          }
        }
        
        if (!is.null(report_path)) {
          main_report(report_path)
          multiqc_path(report_path)
          nx_notify_success("MultiQC report generated successfully!")
        } else {
          nx_notify_warning("MultiQC report not found in expected location")
        }
        
        shinyalert("Run finished!", type = "success", animation = "slide-from-bottom",
                   text = "Pipeline finished successfully!", confirmButtonText = "OK")
      } else {
        shinyjs::html(id = "run", html = "Finished with errors")
        shinyjs::enable(id = "commands_panel")
        shinyjs::disable(id = "run")
        shinyalert("Error!", type = "error", animation = "slide-from-bottom",
                   text = "Pipeline finished with errors. Please review the output.", confirmButtonText = "OK")
      }
    }
  })
  
  session$onSessionEnded(function() {
    report_files <- list.files("www", pattern = "multiqc_report_.*\\.html", full.names = TRUE)
    for (file in report_files) {
      if (file.exists(file)) file.remove(file)
    }
    if (file.exists(paste0("www/", report_hash))) system2("rm", args = c("-rf", paste0("www/", report_hash)))
    if (file.exists(paste0("www/", nxf_hash))) system2("rm", args = c("-rf", paste0("www/", nxf_hash)))
    isolate({
      users$count <- users$count - 1
      writeLines(as.character(users$count), con = "userlog")
    })
  })
}
