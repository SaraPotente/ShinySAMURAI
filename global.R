# global.R

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

# Get pipeline info (these will be available globally)
pipeline_info <- get_pipeline_version()
nextflow_version <- get_nextflow_version()

# Define volumes for file/directory selection (used by both UI and Server)
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

# Notification function placeholder (replace with your actual implementation if different)
# If you have a separate file for this, ensure it's sourced here or in server.R
nx_notify_success <- function(message) { message(paste("SUCCESS:", message)) }
nx_notify_warning <- function(message) { warning(paste("WARNING:", message)) }
nx_notify_error <- function(message) { stop(paste("ERROR:", message)) }