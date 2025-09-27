library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(janitor)

# ---------- Config ----------
RAW_EXCEL_PATH <- "DATA2x02_survey_2025_Responses.xlsx"
CLEAN_RDS_PATH <- "data/cleaned_data.rds"

# ---------- Cleaning helper ----------
clean_survey <- function(dat_raw) {
  dat <- dat_raw |> janitor::clean_names()
  
  # --- Grade aim (ordered factor; drop paradoxical "fail") ---
  grade_std <- dat$what_final_grade_are_you_aiming_to_achieve_in_data2x02 |>
    as.character() |> str_squish() |> str_to_lower()
  grade_aim <- case_when(
    str_detect(grade_std, "high\\s*dist|\\bhd\\b|high") ~ "High Distinction",
    str_detect(grade_std, "^dist|\\bd\\b|dist")         ~ "Distinction",
    str_detect(grade_std, "credit|\\bc\\b")             ~ "Credit",
    str_detect(grade_std, "^pass|\\bp\\b|pass")         ~ "Pass",
    str_detect(grade_std, "fail|\\bf\\b")               ~ NA_character_,
    TRUE ~ NA_character_
  ) |> factor(levels = c("High Distinction","Distinction","Credit","Pass"), ordered = TRUE)
  
  # --- Alcohol (Low/High) ---
  alc_txt <- dat$how_much_alcohol_do_you_consume_each_week |>
    as.character() |> str_squish() |> str_to_lower()
  alc_num <- readr::parse_number(alc_txt)
  alc_num <- if_else(
    is.na(alc_num) & str_detect(alc_txt, "\\b(none|no|never|zero|nil|don.?t)\\b"),
    0, alc_num
  )
  alcohol_level <- case_when(
    !is.na(alc_num) & alc_num <= 5 ~ "Low",
    !is.na(alc_num) & alc_num >  5 ~ "High",
    TRUE ~ NA_character_
  ) |> factor(levels = c("Low","High"))
  
  # --- Sleep hours / group ---
  sleep_hours <- dat$how_much_sleep_do_you_get_on_avg_per_day |>
    as.character() |> parse_number()
  sleep_hours <- if_else(sleep_hours < 3 | sleep_hours > 14, NA_real_, sleep_hours)
  sleep_group <- if_else(sleep_hours >= 7, "Adequate (≥7h)", "Short (<7h)")
  
  # --- Study hours ---
  study_hours <- dat$how_many_hours_a_week_do_you_spend_studying |>
    as.character() |> parse_number()
  study_hours <- if_else(study_hours < 0 | study_hours > 80, NA_real_, study_hours)
  
  # --- Exercise hours / group ---
  exercise_hours <- suppressWarnings(as.numeric(
    dat$on_average_how_many_hours_each_week_do_you_spend_exercising
  ))
  exercise_hours <- if_else(exercise_hours < 0 | exercise_hours > 100, NA_real_, exercise_hours)
  exercise_group <- case_when(
    !is.na(exercise_hours) & exercise_hours < 3 ~ "Low",
    !is.na(exercise_hours) & exercise_hours >= 3 ~ "High",
    TRUE ~ NA_character_
  )
  
  # --- Assignment on-time (Yes/No) ---
  assign_txt <- dat$do_you_submit_assignments_on_time |>
    as.character() |> str_squish() |> str_to_lower()
  on_time <- case_when(
    str_detect(assign_txt, "always|usually|mostly|most of the time|yes|y\\b|on time") ~ "Yes",
    str_detect(assign_txt, "never|rarely|sometimes|often late|no\\b|late")             ~ "No",
    TRUE ~ NA_character_
  )
  
  tibble::tibble(
    grade_aim,
    alcohol_level,
    sleep_hours,
    sleep_group,
    study_hours,
    exercise_hours,
    exercise_group,
    on_time
  )
}

# ---------- Loader with cache ----------
load_cleaned_data <- function() {
  if (file.exists(CLEAN_RDS_PATH)) {
    return(readRDS(CLEAN_RDS_PATH))
  }
  if (!file.exists(RAW_EXCEL_PATH)) {
    stop(paste0(
      "Raw file not found: ", RAW_EXCEL_PATH, "\n",
      "Place the Excel in the project root or update RAW_EXCEL_PATH in app.R."
    ))
  }
  raw <- readxl::read_excel(RAW_EXCEL_PATH)
  df  <- clean_survey(raw)
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  saveRDS(df, CLEAN_RDS_PATH)
  df
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("DATA2902 Student Survey Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select variables to explore and run hypothesis tests."),
      # Will be filled dynamically after data loads
      uiOutput("ui_cat1"),
      uiOutput("ui_cat2"),
      uiOutput("ui_num"),
      radioButtons("test_type", "Hypothesis test:",
                   choices = c("Chi-square (cat vs cat)" = "chi2",
                               "t-test (num vs cat)"      = "ttest"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("peek")),
        tabPanel("Visualisation", plotOutput("plot")),
        tabPanel("Test Results", verbatimTextOutput("test_output"))
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  df <- shiny::reactive({
    load_cleaned_data()
  })
  
  # Build choices from cleaned data
  observe({
    d <- df()
    
    # categorical: factors or characters with reasonable cardinality
    is_cat <- function(x) is.factor(x) || is.character(x) || is.logical(x)
    cat_candidates <- names(d)[vapply(d, is_cat, logical(1))]
    # keep those with <= 12 unique (to avoid crazy free-text)
    cat_vars <- cat_candidates[vapply(d[cat_candidates], function(x) length(na.omit(unique(x))) <= 12, integer(1))]
    
    # numeric vars
    num_vars <- names(d)[vapply(d, is.numeric, logical(1))]
    
    # prefer our cleaned variables first (reorder)
    preferred_cat <- intersect(c("grade_aim","alcohol_level","sleep_group","exercise_group","on_time"), cat_vars)
    preferred_num <- intersect(c("study_hours","sleep_hours","exercise_hours"), num_vars)
    cat_vars <- unique(c(preferred_cat, setdiff(cat_vars, preferred_cat)))
    num_vars <- unique(c(preferred_num, setdiff(num_vars, preferred_num)))
    
    output$ui_cat1 <- renderUI({
      selectInput("cat1", "Categorical variable:", choices = cat_vars, selected = cat_vars[1])
    })
    output$ui_cat2 <- renderUI({
      selectInput("cat2", "Second categorical variable (for χ²):", choices = cat_vars, selected = cat_vars[min(2, length(cat_vars))])
    })
    output$ui_num <- renderUI({
      selectInput("numvar", "Numeric variable (for t-test):", choices = num_vars, selected = num_vars[1])
    })
  })
  
  # Small head() preview
  output$peek <- renderTable({
    head(df(), 8)
  })
  
  # Placeholder visual (will be enhanced next commits)
  output$plot <- renderPlot({
    plot(1:10, main = "Visuals coming next: bar/boxplot")
  })
  
  # Placeholder test output (real tests in next commits)
  output$test_output <- renderPrint({
    d <- df()
    cat("Data loaded with columns:\n")
    print(names(d))
    invisible()
  })
}

shinyApp(ui = ui, server = server)

