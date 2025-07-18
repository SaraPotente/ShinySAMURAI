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

  default_params <- reactive({
    config_paths <- c("nextflow.config", "../nextflow.config", "../../nextflow.config")
    for (config_path in config_paths) {
      if (file.exists(config_path)) {
        return(parse_nextflow_config(config_path))
      }
    }
    return(list())
  })

  output$default_params_display <- renderUI({
    params <- default_params()
    if (length(params) == 0) return(p("No default parameters found in nextflow.config"))
    tagList(lapply(names(params), function(name) tags$li(tags$strong(name), ": ", tags$code(params[[name]]))))
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

  # Reactive value for MultiQC report
  main_report <- reactiveVal(NULL)
  multiqc_path <- reactiveVal(NULL)

  # Simple approach - open in new tab only (no iframe)
  output$results_content <- renderUI({
    path <- main_report()
    if (!is.null(path) && file.exists(path)) {
      # Create a unique filename for the www directory
      report_filename <- paste0("multiqc_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      www_path <- file.path("www", report_filename)
      
      # Copy the report to www directory
      file.copy(path, www_path, overwrite = TRUE)
      
      # Only provide a link to open in new tab - no iframe
      div(
        style = "text-align: center; padding: 50px;",
        icon("chart-bar", style = "font-size: 48px; color: #28a745; margin-bottom: 20px;"),
        h3("MultiQC Report Ready!", style = "color: #333; margin-bottom: 30px;"),
        p("Your MultiQC report has been generated successfully.", style = "color: #666; margin-bottom: 30px;"),
        
        # Big button to open report in new tab
        div(
          style = "margin-bottom: 20px;",
          tags$a(
            href = report_filename,
            target = "_blank",
            class = "btn btn-success btn-lg",
            style = "padding: 15px 30px; font-size: 16px; text-decoration: none;",
            icon("external-link-alt"),
            " View MultiQC Report"
          )
        ),
        
        # Additional info
        div(
          style = "margin-top: 30px;",
          p("The report will open in a new tab/window.", style = "color: #666; font-size: 14px;"),
          p("If the report doesn't open automatically, please check your browser's popup blocker settings.", style = "color: #999; font-size: 12px;")
        )
      )
    } else {
      div(
        style = "text-align: center; padding: 50px;",
        icon("chart-bar", style = "font-size: 48px; color: #ccc; margin-bottom: 20px;"),
        h4("MultiQC Report Not Available", style = "color: #666;"),
        p("The MultiQC report will appear here once the pipeline run is complete.", style = "color: #999;")
      )
    }
  })

  # REMOVED: All problematic observeEvent handlers for buttons that don't exist or cause resets
  # - observeEvent(input$view_report, ...)
  # - observeEvent(input$open_report_tab, ...)
  # - observeEvent(input$pipelineButton, ...)
  # - observeEvent(input$reset, ...)

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
        
        # Check for both possible report names
        possible_reports <- c(
          file.path(optional_params$outdir, "multiqc", "multiqc_report.html"),
          file.path(optional_params$outdir, "multiqc", "SAMURAI-Results_multiqc_report.html")
        )
        
        report_path <- NULL
        for (path in possible_reports) {
          if (file.exists(path)) {
            report_path <- path
            break
          }
        }
        
        if (!is.null(report_path)) {
          # Set the reactive value to trigger UI update
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

  # Clean up temporary files on session end
  session$onSessionEnded(function() {
    # Clean up any temporary report files
    report_files <- list.files("www", pattern = "multiqc_report_.*\\.html", full.names = TRUE)
    for (file in report_files) {
      if (file.exists(file)) {
        file.remove(file)
      }
    }
    
    if (file.exists(paste0("www/", report_hash))) system2("rm", args = c("-rf", paste0("www/", report_hash)))
    if (file.exists(paste0("www/", nxf_hash))) system2("rm", args = c("-rf", paste0("www/", nxf_hash)))
    isolate({
      users$count <- users$count - 1
      writeLines(as.character(users$count), con = "userlog")
    })
  })
}