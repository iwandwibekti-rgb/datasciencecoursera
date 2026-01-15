
library(tidyverse)
library(stringr)
library(hablar)

data.hospital <- read.csv("C:/Users/Acer/Desktop/hospital-data.csv")
data.outcome <- read.csv("C:/Users/Acer/Desktop/outcome-of-care-measures.csv", colClasses = "character")


### 1 Plot the 30-day mortality rates for heart attack
data.outcome[,11] <- as.numeric(
  ifelse(
    data.outcome[,11] == "Not Available",
    NA,
    data.outcome[,11]
  )
)

str(data.outcome[,11])
sum(is.na(data.outcome[,11]))

data.outcome.1 <- data.outcome %>% 
  filter(
    !is.na(data.outcome[,11])
  )


hist(data.outcome.1[,11])

### 2 Finding the best hospital in a state

unique(data.outcome$State)
unique(data.outcome$Hospital.Name)

best <- function(state, outcome_input){
  
  # ---- valid values ----
  valid_states <- unique(data.outcome$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  # ---- input validation ----
  if (!state %in% valid_states) {
    stop("invalid state")
  }
  
  if (!tolower(outcome_input) %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  # ---- map outcome to column ----
  outcome_map <- c(
    "heart attack"  = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"     = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  col <- outcome_map[[tolower(outcome_input)]]
  
  data <- data.outcome %>%
    mutate(across(
      c(
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      ),
      as.numeric)
    ) %>% 
    select(
      State,
      Hospital.Name,
      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    ) %>% 
    pivot_longer(
      cols = c(
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
        Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
        Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      ),
      names_to = "outcome",
      values_to = "Mortality_rates"
    ) %>% 
    mutate(
      outcome = case_when(
        str_detect(str_to_lower(outcome), "pneumonia") ~ "pneumonia",
        str_detect(str_to_lower(outcome), "attack") ~ "heart attack",
        str_detect(str_to_lower(outcome), "failure") ~ "heart failure",
        TRUE ~ "other"
      )
    ) %>% 
    filter(
      State == state,
      outcome == outcome_input,
    ) %>% 
    filter(
      !is.na(Mortality_rates)
    ) %>% 
    group_by(
      Hospital.Name
    ) %>% 
    summarise(
      mortality_rates = sum(Mortality_rates),
      .groups = "drop"
    ) %>% 
    arrange((mortality_rates)) %>% 
    slice(1) %>% 
    select(-mortality_rates)
}


y <- best("TX", "heart attack")
y

y <- best("TX", "heart failure")
y

y <- best("BB", "hert attack")
y