library(tidyverse)
library(janitor)



# Make sure the working directory is set to the location of this script!


# Read in details of the tasks
question_details = read_csv("../data-raw/study1/FAC_LGE-tasks.csv") %>%
  select(question_id, question_name) %>% 
  rename(questionid = question_id)

#
# Participant info
#

# Experimental groups
participant_groups = read_csv("../data-raw/study1/FAC1920-groups.csv") %>% 
  clean_names() %>% 
  separate(email_address, c("username", NA), sep = "@") %>% 
  select(username, group) %>% 
  mutate(group = str_extract(group, "LGE Group [:alpha:]{3}")) %>% 
  mutate(group = str_replace_all(group, "X", "G"))

# Consent
participants = read_csv("../data-raw/study1/FAC1920-Course design research-grades.csv") %>% 
  clean_names() %>% 
  # get rid of the summary row at the end
  filter(str_length(email_address) > 0) %>% 
  # extract the username
  separate(email_address, c("username", NA), sep = "@") %>%
  # restrict only to those who consented to participate
  filter(grade_1_00 == 1) %>% 
  select(username) %>% 
  # attach the info about experimental groups
  left_join(participant_groups, by = "username")

#
# Question attempts
#

# Read in all the csv files of attempt data
all_attempt_data =
  list.files(path = "../data-raw/study1/",
             pattern = "^attempts.*.csv",
             full.names = TRUE) %>%
  as_tibble() %>%
  rename(csv_file_name = value) %>% 
  mutate(
    csv_data = purrr::map(csv_file_name, read.csv, stringsAsFactors = FALSE)
  ) %>% 
  unnest(cols = c(csv_data)) %>% 
  # attach details of each question
  left_join(question_details, by = "questionid") %>% 
  # the username field does not exist, so create it
  mutate(username = str_replace(firstname, "firstname", "")) %>% 
  # tidy up the table, and number each attempt at a question by each student
  arrange(question_name, username, timecreated) %>% 
  group_by(question_name, username) %>% 
  mutate(
    attempt_number = row_number()
  ) %>% 
  ungroup() %>% 
  select(csv_file_name, question_name, everything()) %>% 
  # remove unnecessary code from questions with jsxgraph content
  mutate(questionsummary = str_remove(questionsummary, regex("\\[\\[jsxgraph.*/jsxgraph\\]\\]", dotall = TRUE)))


# Restrict to data from those who gave consent
attempt_data = all_attempt_data %>% 
  # only keep attempts by participants
  semi_join(participants, by = "username") %>% 
  # then attach details of which experimental group they were in
  left_join(participants, by = "username") %>% 
  # drop the single student who appears not to be in any group
  filter(str_length(group) > 0)

#
# Write the processed data to .csv files
#

write_attempts_csv = function(DF) {
  write.csv(DF,
            paste0("data-clean/study1/processed-attempts-",unique(DF$question_name),".csv"),
            row.names = FALSE)
  return(DF)
}
attempt_data %>% 
  group_by(question_name) %>% 
  do(write_attempts_csv(.))

# Save an RDS file so that all the previous steps can be skipped later on
attempt_data %>%
  write_rds("data-clean/FAC_LGE-attempt_data.rds")
