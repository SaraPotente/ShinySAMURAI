#### global.R ####

# Load all necessary libraries
libs <- c("shiny", "shinyFiles", "shinyjs", "shinyalert", "shinypop",
          "processx", "stringr", "digest", "dplyr", "fs", "yaml",
          "jsonlite", "httr")
lapply(libs, library, character.only = TRUE)

# Define reactive to track user counts (this is a global reactive value)
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

# Versioni di pipeline e Nextflow
pipeline_info <- get_pipeline_version()
nextflow_version <- get_nextflow_version()
