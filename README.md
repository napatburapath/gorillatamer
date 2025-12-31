# gorillatamer

An R package to make sense of Gorilla task/questionnaire data.

When exporting data from Gorilla.sc, use these settings:
* Combine all task nodes (including shops and games) into one file.
* Combine all questionnaire nodes into one file
* Filetype: CSV (Comma)
* Blinding: Unblind (Important! Currently gorillatamer needs a column named 'Participant Public ID')
* Form (Questionnaires Only): Short Form (one row per participant)
* Set column headers (Questionnaire Builder 2 ONLY): Name only.
* Add additional question text header (Questionnaire Builder 2 ONLY): Add header (Important! gorillatamer uses this row for attention check questions)

When importing the csv files into RStudio, ensure spaces in column names aren't replaced by . by having the 'check.names' argument in read.csv() be FALSE.



Things still to do:
* 
