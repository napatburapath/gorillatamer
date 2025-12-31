

cols <- c("Store: cumulative_winning", "Store: amount_won", "Participant Public ID",
          "randomiser-mai6", "Response", "display",
          "Spreadsheet: db_rank1", "Spreadsheet: db_rank2",
          "Spreadsheet: db_rank3", "Spreadsheet: db_rank4",
          "Spreadsheet: db_rank5",
          "Store: response_count", "Store: profile_selected",
          "Store: least_likely", "Store: most_likely",
          "Store: second_likely", "Store: third_likely", "Store: fourth_likely",
          "Store: newStore1", "Store: newStore2", "Store: newStore3",
          "Store: newStore4", "Store: newStore5",
          "Store: profile_selected_image")

#' Check attention questions
#'
#' Function to return participant id if failed more than 2 questions.
#'
#' @param data Untouched questionnaire data - short form with name only when downloading from Gorilla. check.names=FALSE.
#' @param questions Vector of exact question as written. Defaults to DAS lab 2025 version.
#' @param correct_answers Vector of correct answers in order. Defaults to DAS lab 2025 version.
#' @return Array of subjID that failed >2 questions, or no message else.
#' @export
check_attention <- function(data,
                            questions = NULL,
                            correct_answers = NULL) {

  if(is.null(questions)) {
    questions <- c(
      'For this question, please select the word "broccoli" from the list of vegetables below.',
      'From the colours listed below please select the colour of snow:',
      'Please select the word that is not a fruit in the list below:',
      'Which of these things do not move:',
      'Please select the answer option “Very happy”:'
    )
  }

  if(is.null(correct_answers)) {
    correct_answers <- c(
      "Broccoli",
      "White",
      "Hammer",
      "Statue",
      "Very happy"
    )

  }

  # Remove 'Quantised' columns to prevent duplicates
  data <- data[, -grep("Quantised", names(data))]

  # Find the column indices for the questions
  questions_str <- paste(questions, collapse = "|")

  # Identify and filter out the 5 questions columns
  attention_cols <- grep(questions_str, data[1,])
  attention_data <- data[-1, attention_cols]

  # Create cols for all questions with empty rows removed
  cols <- data.frame(matrix(ncol = length(attention_cols), nrow = 0))

  for(i in 1:ncol(attention_data)) {
    x <- attention_data[, i]
    x <- x[!is.na(x) & x != ""]

    if(i == 1) {
      cols <- data.frame(x)
    } else {
      cols <- cbind(cols, x)
    }
  }

  subj <- data[ ,"Participant Public ID"]
  subj <- subj[!is.na(subj) & subj != ""]
  subj <- unique(subj)

  data <- cbind(subj, cols)

  # Compare each participant answers
  failed <- c()

  for(row in 1:nrow(data)){
    subjID <- data[row, 1]
    answers <- data[row, -1]

    failed <- sum(answers != correct_answers, na.rm = TRUE)

    if(failed > 2){
      failed <- c(failed, subjID)
    }
  }

  if(length(failed == 0)) {
    return("No failed attention checks > 2")
  } else {
    return(failed)
  }
}



#' Check learning task ranking accuracy
#'
#' Function to check accuracy ranking.
#'
#' @param data Untouched Gorilla task data. check.names=FALSE
#' @param col Vector of column names for first round cleaning. Already includes cols 'Response' 'display' and 'Participant Public ID'. Defaults to treehouse task names.
#' @param bonus double representing bonus payout per correct choice.
#' @return Array of subjID that failed >2 questions, or no message else.
#' @export
check_answers <- function(data, col = NULL, bonus) {

  if(is.null(col)) {
    col <- c("Store: cumulative_winning", "Store: amount_won", "Participant Public ID",
      "randomiser-mai6", "Response", "display",
      "Spreadsheet: db_rank1", "Spreadsheet: db_rank2",
      "Spreadsheet: db_rank3", "Spreadsheet: db_rank4",
      "Spreadsheet: db_rank5",
      "Store: response_count", "Store: profile_selected",
      "Store: least_likely", "Store: most_likely",
      "Store: second_likely", "Store: third_likely", "Store: fourth_likely",
      "Store: newStore1", "Store: newStore2", "Store: newStore3",
      "Store: newStore4", "Store: newStore5",
      "Store: profile_selected_image")
  }

  if(!("Response" %in% col)) {
    col <- c(col, "Response")
  }

  if(!("display" %in% col)) {
    col <- c(col, "display")
  }

  if(!("Participant Public ID" %in% col)) {
    col <- c(col, "Participant Public ID")
  }


  # Get only the Participant Public ID and correct answer per trial
  data <- data %>%
    select(all_of(col)) %>%
    filter(str_detect(display, 'Ranking')) %>%
    group_by(`Participant Public ID`, display) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    select(-c("Response", "Store: response_count", "Store: profile_selected")) %>%
    mutate(r1 = ifelse(`Spreadsheet: db_rank1` == `Store: most_likely`, 1, 0),
           r2 = ifelse(`Spreadsheet: db_rank2` == `Store: second_likely`, 1, 0),
           r3 = ifelse(`Spreadsheet: db_rank3` == `Store: third_likely`, 1, 0),
           r4 = ifelse(`Spreadsheet: db_rank4` == `Store: fourth_likely`, 1, 0),
           r5 = ifelse(`Spreadsheet: db_rank5` == `Store: least_likely`, 1, 0)) %>%
    mutate(correct = rowSums(across(r1:r5), na.rm = TRUE)) %>%
    select(c("Participant Public ID", "correct"))

  # Sum up trials by participant
  data <- data %>%
    mutate(payout = correct * bonus) %>%
    group_by(`Participant Public ID`) %>%
    summarise(
      total = sum(payout, na.rm = TRUE)
    )

  return(data)

}


#' Check learning task ranking accuracy
#'
#' Function to check accuracy ranking.
#'
#' @param data Untouched Gorilla task data. check.names=FALSE
#' @param cols Vector of column names for first round cleaning. Defaults to treehouse task names.
#' @return list(task, ranking) data frames.
#' @export
process_raw_data <- function(raw_task_data, cols=NULL) {

  if(is.null(cols)) {
    cols <- c("Store: cumulative_winning", "Store: amount_won", "Participant Public ID",
             "randomiser-mai6", "Response", "display",
             "Spreadsheet: db_rank1", "Spreadsheet: db_rank2",
             "Spreadsheet: db_rank3", "Spreadsheet: db_rank4",
             "Spreadsheet: db_rank5",
             "Store: response_count", "Store: profile_selected",
             "Store: least_likely", "Store: most_likely",
             "Store: second_likely", "Store: third_likely", "Store: fourth_likely",
             "Store: newStore1", "Store: newStore2", "Store: newStore3",
             "Store: newStore4", "Store: newStore5",
             "Store: profile_selected_image")
  }

  # Only select columns that exist
  cols <- cols[cols %in% names(raw_task_data)]

  task <- raw_task_data %>%
    select(all_of(cols)) %>%
    rename(subjID = "Participant Public ID")

  ranking <- task %>%
    filter(str_detect(display, 'Ranking')) %>%
    group_by(subjID, display) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    select(-c(`Response`, `Store: response_count`, `Store: profile_selected`)) %>%
    mutate(r1 = ifelse(`Spreadsheet: db_rank1` == `Store: most_likely`, 1, 0),
           r2 = ifelse(`Spreadsheet: db_rank2` == `Store: second_likely`, 1, 0),
           r3 = ifelse(`Spreadsheet: db_rank3` == `Store: third_likely`, 1, 0),
           r4 = ifelse(`Spreadsheet: db_rank4` == `Store: fourth_likely`, 1, 0),
           r5 = ifelse(`Spreadsheet: db_rank5` == `Store: least_likely`, 1, 0)) %>%
    mutate(correct = rowSums(across(r1:r5), na.rm = TRUE))

  return(list(
    "task" = task,
    "ranking" = ranking
  ))

}

