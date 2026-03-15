# gorillatamer

An R package to make sense of Gorilla task/questionnaire data.

# Formatting data for use

When exporting data from Gorilla.sc, use these settings:
* Combine all task nodes (including shops and games) into one file.
* Combine all questionnaire nodes into one file
* Filetype: CSV (Comma)
* Blinding: Unblind (Important! Currently gorillatamer needs a column named 'Participant Public ID')
* Form (Questionnaires Only): Short Form (one row per participant)
* Set column headers (Questionnaire Builder 2 ONLY): Name only.
* Add additional question text header (Questionnaire Builder 2 ONLY): Add header (Important! gorillatamer uses this row for attention check questions)

##### When importing the csv files, ensure the spaces in column names aren't replaced by '.' by having the 'check.names' argument in read.csv() be FALSE.

# Useful functions

> extract_questions(data, subjID_col = NULL, remove = NULL, sum_only = TRUE)


Function to process raw questionnaire csv from Gorilla. Data should be processed following above guide. 

* @param data raw questionnaire csv as exported per guideline.
* @param subjID_col colname to represent unique subject ID. Defaults to Participant Public ID.
* @param remove vector of any questions in between questionnaire items to remove. Defaults to DAS lab 2025 attention check questions.
* @param sum_only do you want only the sum scores? Defaults to TRUE.
* @return Processed questionnaire data frame


> check_attention <- function(data, subj_col = NULL, questions = NULL, correct_answers = NULL)


Function to return participant id if failed more than 2 questions.

* @param subj_col Name of the column to be used to identify subjects. Defaults to "Participant Public ID".
* @param data Untouched questionnaire data - short form with name only when downloading from Gorilla. check.names=FALSE.
* @param questions Vector of exact question AS WRITTEN. Defaults to DAS lab 2025 version.
* @param correct_answers Vector of correct answers in order of the questions. Defaults to DAS lab 2025 version.
* @return Array of subjID that failed >2 questions, or no message else.


# To install
* Use command: `remotes::install_github("napatburapath/gorillatamer")`. This will download the package into your local environment from Github.
* Run: `library(gorillatamer)`




Things still to do:
* Something...

