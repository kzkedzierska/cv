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

# Read in the data
read_data <-
  function(spreadsheet_id, 
           sheet_name, resume = FALSE) {
    selective_cols <-
      c("description", "institution", "location", "name", 
        "links", "authors", "journal")
    googlesheets4::read_sheet(ss = spreadsheet_id, 
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
             year = ifelse("year" %in% colnames(.), 
                           as.integer(year), 
                           as.integer(start))) %>% 
      ungroup() %>% 
      mutate(across(any_of(selective_cols), as.character)) %>% 
      {if (resume) filter(., in_resume) else .}
  }



# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- 
  function(spreadsheet_id, 
           section, resume = FALSE) {
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
    
    read_data(spreadsheet_id, section, resume) %>%
      arrange(desc(year)) %>% 
      mutate(end = ifelse(is.na(end), "present", end),
             timeline = ifelse(is.na(start) | start == end,
                               end,
                               glue('{end} - {start}'))) %>% 
      mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
      glue_data(template)
  }

# Construct a bar chart of skills
build_skill_bars <- function(skills, out_of = 5){
  # --indigo-dye: #16425b;
  # --dark-sky-blue: #81c3d7;
  bar_color <- "#16425b" #"#5b5f97"
  bar_background <- "#81c3d7" #"#5b5f9740"
  skills %>% 
    mutate(width_percent = round(100 * level/out_of)) %>% 
    glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}

# Prints out from text_blocks spreadsheet blocks of text for the intro and asides. 
print_text_block <- function(text_blocks, label){
  filter(text_blocks, loc == label)$text %>%
    cat()
}
