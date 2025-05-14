# This code uses google sheets to store the position info
library(googlesheets4)

if (sheet_is_publicly_readable){
  # This tells google sheets to not try and authenticate. Note that this will only
  # work if your sheet has sharing set to "anyone with link can view"
  gs4_deauth()
} else {
  # My info is in a public sheet so there's no need to do authentication but if you want
  # to use a private sheet, then this is the way you need to do it.
  # designate project-specific cache so we can render Rmd without problems
  options(gargle_oauth_cache = ".secrets")
  
  # Need to run this once before knitting to cache an authentication token
  # googlesheets4::gs4_auth()
}
read_data <-
  function(spreadsheet_id, 
           sheet_name, resume = FALSE) {
    selective_cols <-
      c("description", "institution", "location", "name", 
        "links", "authors", "journal")
    data <- googlesheets4::read_sheet(ss = spreadsheet_id, 
                                      sheet = sheet_name) %>% 
      rowwise() %>% 
      mutate(in_resume = as.logical(in_resume),
             title = as.character(title),
             start = ifelse("start" %in% colnames(.), 
                            as.integer(start), 
                            as.integer(year)),
             end = ifelse("end" %in% colnames(.), 
                          as.integer(end), 
                          as.integer(year)),
             start_month = ifelse("start_month" %in% colnames(.), 
                                  as.integer(start_month), 
                                  NA),
             end_month = ifelse("end_month" %in% colnames(.), 
                                as.integer(end_month), 
                                NA),
             year = ifelse("year" %in% colnames(.), 
                           as.integer(year), 
                           as.integer(start))) %>% 
      ungroup() %>% 
      mutate(across(any_of(selective_cols), as.character))
    
    # Apply resume filter if needed
    if(resume) {
      data <- data %>% filter(in_resume)
      
      # Only process details_in_resume if it exists in the data
      if("details_in_resume" %in% colnames(data)) {
        data <- data %>%
          mutate(description = case_when(
            details_in_resume == "yes" ~ description,
            details_in_resume == "no" ~ "",
            TRUE ~ description  # Important: handle NA or other values
          )) %>%
          select(-details_in_resume)
      }
    }
    
    return(data)
  }

# Function to convert month number to abbreviated month name
month_to_name <- function(month_num) {
  if (is.na(month_num)) return(NA)
  month.abb[month_num]  # Uses R's built-in month abbreviations (Jan, Feb, etc.)
}

month_to_roman <- function(month_num) {
  if (is.na(month_num)) return(NA)
  month_names <- c("I", "II", "III", "IV", "V", "VI", 
                   "VII", "VIII", "IX", "X", "XI", "XII")
  month_names[month_num]
}

# Updated print_section function with month support
print_section <- 
  function(spreadsheet_id, 
           section, 
           resume = FALSE,
           include_months = FALSE) {
    templates <-
      c("default" = paste0("### {title} @ {institution}",
                           "\n\n", "N/A",
                           "\n\n", "N/A",
                           "\n\n", "{timeline}", 
                           "\n\n", "{description}", "\n\n\n"),
        "hackathons" = paste0("### {title} @ {institution}, {location}",
                              "\n\n", "N/A",
                              "\n\n", "N/A",
                              "\n\n", "{timeline}", 
                              "\n\n", "", "\n\n\n"),
        "articles" = paste0("### [{title}]({link}) @ {journal}",
                            "\n\n", "N/A",
                            "\n\n", "N/A",
                            "\n\n", "{timeline}", 
                            "\n\n", "{authors}", "\n\n\n"),
        "conferences" = paste0("### {title} @ {name}, {location}",
                               "\n\n", "N/A",
                               "\n\n", "N/A",
                               "\n\n", "{year}", 
                               "\n\n", "", "\n\n\n"),
        "work" = paste0("### {title} @ {institution}, {location}",
                        "\n\n", "N/A",
                        "\n\n", "N/A",
                        "\n\n", "{timeline}", 
                        "\n\n", "{description}", "\n\n\n"),
        "teaching" = paste0("### {title} @ {institution}, {location}",
                            "\n\n", "N/A",
                            "\n\n", "N/A",
                            "\n\n", "{timeline}", 
                            "\n\n", "{description}", "\n\n\n"))
    
    template <-
      ifelse(section %in% names(templates),
             templates[section],
             templates["default"])
    
    data <- read_data(spreadsheet_id, section, resume)
    
    # If include_months is TRUE, format the dates with months
    if (include_months) {
      data <- data %>%
        mutate(
          start_month_roman = sapply(start_month, month_to_name),
          end_month_roman = sapply(end_month, month_to_name),
          end = ifelse(is.na(end), "present", as.character(end)),
          timeline = case_when(
            # When both start and end have month info
            !is.na(start_month) & !is.na(end_month) & end != "present" ~ 
              glue('{start_month_roman} {start} • {end_month_roman} {end}'),
            # When start has month info, end is "present"
            !is.na(start_month) & end == "present" ~ 
              glue('{start_month_roman} {start} • present'),
            # When only start has month info
            !is.na(start_month) & is.na(end_month) & end != "present" ~ 
              glue('{start_month_roman} {start} • {end}'),
            # When only end has month info
            is.na(start_month) & !is.na(end_month) & end != "present" ~ 
              glue('{start} • {end_month_roman} {end}'),
            # Default case (no month info or already handled)
            is.na(start) | start == end ~ end,
            TRUE ~ glue('{start} • {end}')
          )
        )
    } else {
      # Original timeline formatting
      data <- data %>% 
        mutate(end = ifelse(is.na(end), "present", end),
               timeline = ifelse(is.na(start) | start == end,
                                 end,
                                 glue('{end} - {start}')))
    }
    
    # Sort by year and month if available
    if (include_months) {
      data <- data %>%
        # Create a sortable date field (higher number = more recent)
        mutate(
          # For "present" end dates, use a very high number
          sort_end_year = ifelse(end == "present", 9999, as.numeric(end)),
          sort_end_month = ifelse(is.na(end_month), 12, end_month),
          sort_value = sort_end_year * 100 + sort_end_month
        ) %>%
        arrange(desc(sort_value))
    } else {
      data <- data %>% arrange(desc(year))
    }
    
    data %>%
      mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
      glue_data(template)
  }

# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  skills %>% 
    mutate(width_percent = round(100 * level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "var(--indigo-dye) {width_percent}%,",
      "var(--dark-sky-blue) {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}

# Prints out from text_blocks spreadsheet blocks of text for the intro and asides. 
print_text_block <- function(text_blocks, label){
  filter(text_blocks, loc == label)$text %>%
    cat()
} 