#' @importFrom dplyr select arrange filter
#' @importFrom magrittr %>%


pascq <- c("My parents/carers let me know they love me",
           "My parents/carers enjoy being with me",
           "My parents/carers are always glad to see me",
           "My parents/carers think I’m special",
           "Sometimes I wonder if my parents/carers like me",
           "My parents/carers think I’m always in the way",
           "My parents/carers make me feel like I’m not wanted",
           "Nothing I do is good enough for my parents/carers",
           "When I want to do something, my parents/carers show me how",
           "When I want to understand how something works, my parents/carers explain it to me",
           "If I ever have a problem, my parents/carers help me to figure out what to do about it",
           "My parents/carers explain the reasons for our family rules",
           "When my parents/carers make a promise, I don’t know if they will keep it",
           "When my parents/carers say they will do something, sometimes they don’t really do it",
           "My parents/carers get mad at me with no warning",
           "My parents/carers punish me for no reason",
           "My parents/carers trust me",
           "My parents/carers accept me for myself",
           "My parents/carers encourage me to be true to myself",
           "My parents/carers try to understand my point of view",
           "My parents/carers are always telling me what to do",
           "My parents/carers boss me",
           "My parents/carers think there is only one right way to do things — their way",
           "My parents/carers say \"no\" to everything")


pascq_scoring <- c("Not at all true", "Not very true", "Sort of true", "Very true", "Prefer not to say")



#' Gather questionnaire data
#'
#' Function to process raw questionnaire csv from Gorilla.
#'
#' @param data raw questionnaire csv as exported per guideline.
#' @param subjID_col colname to represent unique subject ID. Defaults to Participant Public ID.
#' @param remove vector of any questions in between questionnaire items to remove. Defaults to DAS lab 2025 attention check questions.
#' @param sum_only do you want only the sum scores? Defaults to TRUE.
#' @return Processed questionnaire data frame
#' @export
extract_questions <- function(data, subjID_col = NULL, remove = NULL, sum_only = TRUE) {

  # Question definitions
  questions <- list(
    phq8 = c("Little interest or pleasure in doing things",
             "Feeling down, depressed, or hopeless",
             "Trouble falling or staying asleep, or sleeping too much",
             "Feeling tired or having little energy",
             "Poor appetite or overeating",
             "Feeling bad about yourself - or that you are a failure or have let yourself or your family down",
             "Trouble concentrating on things, such as reading the newspaper or watching television",
             "Moving or speaking so slowly that other people could have noticed? Or the opposite - being so fidgety or restless that you have been moving around a lot more than usual"),

    gad7 = c("Feeling nervous, anxious, or on edge",
             "Not being able to stop or control worrying",
             "Worrying too much about different things",
             "Trouble relaxing",
             "Being so restless that it is hard to sit still",
             "Becoming easily annoyed or irritable",
             "Feeling afraid as if something awful might happen"),

    ius12 = c("Unforeseen events upset me greatly",
              "It frustrates me not having all the information I need",
              "Uncertainty keeps me from living a full life",
              "One should always look ahead so as to avoid surprises",
              "A small unforeseen event can spoil everything, even with the best of planning",
              "When it’s time to act, uncertainty paralyses me",
              "When I am uncertain I can’t function very well",
              "I always want to know what the future has in store for me",
              "I can’t stand being taken by surprise",
              "The smallest doubt can stop me from acting",
              "I should be able to organise everything in advance",
              "I must get away from all uncertain situations"),

    o2s3 = c("I worry about the effect I have on other people",
             "I delete my social media posts if I don’t get the responses I wanted",
             "I feel that people generally like me",
             "I feel anxious before posting anything on social media",
             "I always expect criticism",
             "I care about what people feel about me",
             "Initiating contact online with others, such as sending friend requests, is not something I feel comfortable doing",
             "When I update my social media status and no one comments on it, I tend to be disappointed",
             "I check my responses to my social media posts very often",
             "If others knew the real me, they would not like me",
             "My value as a person depends enormously on what others think of me",
             "When I update my social media status, I expect others to comment on it",
             "I worry about being criticized for things I have said or done",
             "I feel others do not understand me",
             "I prefer to post on social media sites where I can stay anonymous (i.e., without revealing my name or identity)",
             "If other people knew what I am really like, they would think less of me",
             "I worry about what others think of me",
             "I feel insecure if I don’t receive positive responses to my social media posts")
  )

  # Scoring mappings
  scoring <- list(
    phq8 = c("Not at all" = 0, "Several days" = 1,
             "More than half the days" = 2, "Nearly every day" = 3,
             "Prefer not to say" = NA),
    gad7 = c("Not at all" = 0, "Several days" = 1,
             "More than half the days" = 2, "Nearly every day" = 3,
             "Prefer not to say" = NA),
    ius12 = c("1" = 0, "2" = 1, "3" = 2, "4" = 3, "5" = 4, "Prefer not to say" = NA),
    o2s3 = c("0 - Strongly disagree" = 0, "1 - Moderately disagree" = 1,
             "2 - Moderately agree" = 2, "3 - Strongly agree" = 3,
             "Prefer not to say" = NA)
  )


  if(is.null(subjID_col)) {
    subjID = "Participant Public ID"
  } else {
    subjID <- subjID_col
  }

  if(is.null(remove)) {
    qu_remove <- c(
      'For this question, please select the word "broccoli" from the list of vegetables below.',
      'From the colours listed below please select the colour of snow:',
      'Please select the word that is not a fruit in the list below:',
      'Which of these things do not move:',
      'Please select the answer option “Very happy”:'
    )
  } else {
    qu_remove <- remove
  }

  std <- function(q) {
    q <- tolower(q)
    q <- gsub("[^a-z ]", "", q)
    gsub("\\s+", " ", trimws(q))  # Normalize whitespace
  }

  # Remove Quantised columns
  colnames(data) <- make.unique(colnames(data))
  data <- data %>%
    select(-matches("(?i)quantise")) %>%
    arrange(.data[[subjID]])

  subjID_id <- which(colnames(data) == subjID)
  colnames(data)[(subjID_id+1):ncol(data)] <- data[1, (subjID_id+1):ncol(data)]

  processed <- data %>%
    select(all_of(subjID)) %>%
    filter(.data[[subjID]] != "") %>%
    unique()

  colnames(data) <- std(colnames(data))

  std_remove <- std(qu_remove)
  cols_to_keep <- !std(colnames(data)) %in% std_remove
  data <- data[, cols_to_keep]

  for(questionnaire in 1:length(questions)) {
    qu_data <- data %>%
      select(any_of(std(questions[[questionnaire]])))

    data <- cbind(
      data, qu_data
    )
  }


  # Extract questions for each scale
  for(qu_name in names(questions)) {

    items <- std(questions[[qu_name]])


    # Find matching columns
    first_idx <- NA
    last_idx <- NA

    for(i in seq_along(colnames(data))) {
      if(grepl(items[1], colnames(data)[i], fixed = TRUE)) {
        first_idx <- i

        break
      }
    }

    for(i in seq_along(colnames(data))) {
      if(grepl(items[length(items)], colnames(data)[i], fixed = TRUE)) {
        last_idx <- i
        break
      }
    }

    if(!is.na(first_idx) && !is.na(last_idx)) {
      df <- data[, first_idx:last_idx]
      df <- df[-1, ]  # Remove header row
      df <- df[df[[1]] != "", ]  # Remove empty rows

      processed <- cbind(processed, df)
    }
  }

  # Apply scoring
  for(qu_name in names(questions)) {

    qu_items <- std(questions[[qu_name]])
    score <- scoring[[qu_name]]

    for(item in qu_items) {
      # Find column matching this item
      col_idx <- which(colnames(processed) == item)

      if(length(col_idx) > 0) {
        col_idx <- col_idx[1]  # Take first match
        processed[[col_idx]] <- score[processed[[col_idx]]]
      }
    }
  }


  new_names <- c(subjID)
  for(qu_name in names(questions)) {
    new_names <- c(new_names, paste(qu_name, 1:length(questions[[qu_name]]), sep = "_"))
  }
  colnames(processed) <- new_names

  for(qu_name in names(questions)) {
    processed <- processed %>%
      mutate(!!paste0(qu_name, "_total") := rowSums(select(., all_of(paste(qu_name, 1:length(questions[[qu_name]]), sep = "_"))), na.rm = TRUE))
  }

  if(sum_only == TRUE) {
    processed <- processed %>%
      select(contains("total"))
  }


  return(processed)
}

















