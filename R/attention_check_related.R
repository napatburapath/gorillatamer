
#' Check attention questions
#'
#' Function to return participant id if failed more than 2 questions.
#'
#' @param subj_col Name of the column to be used to identify subjects. Defaults to "Participant Public ID".
#' @param data Untouched questionnaire data - short form with name only when downloading from Gorilla. check.names=FALSE.
#' @param questions Vector of exact question as written. Defaults to DAS lab 2025 version.
#' @param correct_answers Vector of correct answers in order of the questions. Defaults to DAS lab 2025 version.
#' @return Array of subjID that failed >2 questions, or no message else.
#' @export
check_attention <- function(data, subj_col = NULL,
                            questions = NULL,
                            correct_answers = NULL) {

  if(is.null(subj_col)) {

    subj_col <- "Participant Public ID"
  }

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

  subj <- data[ ,subj_col]
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
