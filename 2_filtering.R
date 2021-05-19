library(tidyverse)

attempt_data = read_rds("data-clean/FAC_LGE-attempt_data.rds")


#
# Prepare individual questions for analysis
#

# define the parts of the responsesummary to extract
response_parts = c("ans1", "ans2", "ans3", "prt1", "prt2", "prt3")
stuff_to_remove = paste(paste0(response_parts, ": "), collapse = "|")

attempts_Xa = attempt_data %>% 
  # restrict to a certain question, and only attempts that were graded
  filter(question_name == "Xa",
         str_detect(state, "graded")) %>% 
  # break up the responsesummary into its parts
  separate(responsesummary, sep = "; ",
           into = response_parts) %>% 
  # tidy up these parts, by removing "ans1: " etc, and also "[score]"
  mutate_at(response_parts, ~str_remove_all(., stuff_to_remove)) %>%
  mutate_at(response_parts, ~str_remove_all(., " \\[score\\]")) %>%
  # keep only the interesting columns
  select(username, attempt_number, all_of(response_parts), state, fraction)

attempts_Xa_filtered = attempts_Xa %>% 
  mutate(
    ans1correct = str_detect(prt1, "prt1-1-T"),
    ans2correct = str_detect(prt2, "prt2-4-T"),
    ans3correct = str_detect(prt3, "prt3-4-T"),
    
    keep_first_attempt = case_when(
      
      # We only care about first attempts here
      !attempt_number == 1 ~ 0,
      
      # they got at least one part fully correct
      # (since they visited the "full marks" PRT node)
      ans1correct | ans2correct | ans3correct ~ 1,
      
      # discard if all inputs are the same
      ans1 == ans2 & ans2 == ans3 ~ 0,
      
      # discard if no input contains "n"
      !str_detect(ans1, "n") & !str_detect(ans2, "n") & !str_detect(ans3, "n") ~ 0,
      
      # otherwise, discard
      # TODO: check if this makes sense by looking at the entries with this value set to 10.
      #       If so, replace 10 with 0
      TRUE ~ 1  # going with 1 since it looks like they have all done something involving u_n,
                # which represents an attempt of some sort
    ),
    keep_subsequent_attempt = case_when(
      # if a subsequent attempts contains a correct answer which
      # differs from the teacher answer
      (ans1correct & ans1 != "n+9")
      | (ans2correct & ans2 != "1-1/2^n")
      | (ans3correct & ans3 != "257/16-1/2^n") ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  # For each user, add up the scores above to see if they have any data worth including
  group_by(username) %>% 
  mutate(
    keep_participant = sum(keep_first_attempt) + sum(keep_subsequent_attempt),
    keep_participant = keep_participant > 0
  ) %>% 
  ungroup()

attempts_Xa_filtered %>%
  write.csv("filtered-attempts-Xa.csv", row.names = FALSE)




attempts_Xb = attempt_data %>% 
  # restrict to a certain question, and only attempts that were graded
  filter(question_name == "Xb",
         str_detect(state, "graded")) %>% 
  # break up the responsesummary into its parts
  separate(responsesummary, sep = "; ",
           into = response_parts) %>% 
  # tidy up these parts, by removing "ans1: " etc, and also "[score]"
  mutate_at(response_parts, ~str_remove_all(., stuff_to_remove)) %>%
  mutate_at(response_parts, ~str_remove_all(., " \\[score\\]")) %>%
  # keep only the interesting columns
  select(username, attempt_number, response_parts, state, fraction)

attempts_Xb_filtered = attempts_Xb %>% 
  mutate(
    ans1correct = str_detect(prt1, "prt1-1-T"),
    ans2correct = str_detect(prt2, "prt2-4-T"),
    ans3correct = str_detect(prt3, "prt3-4-T"),
    
    keep_first_attempt = case_when(
      
      # We only care about first attempts here
      !attempt_number == 1 ~ 0,
      
      # they got at least one part fully correct
      # (since they visited the "full marks" PRT node)
      ans1correct | ans2correct | ans3correct ~ 1,
      
      # discard if all inputs are the same
      ans1 == ans2 & ans2 == ans3 ~ 0,
      
      # discard if no input contains "n"
      !str_detect(ans1, "n") & !str_detect(ans2, "n") & !str_detect(ans3, "n") ~ 0,
      
      # otherwise, retain
      TRUE ~ 1 
    ),
    keep_subsequent_attempt = case_when(
      # if a subsequent attempts contains a correct answer which
      # differs from the teacher answer
      (ans1correct & ans1 != "11-n")
      | (ans2correct & ans2 != "1/2^n")
      | (ans3correct & ans3 != "1/2^n+15/16") ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  # For each user, add up the scores above to see if they have any data worth including
  group_by(username) %>% 
  mutate(
    keep_participant = sum(keep_first_attempt) + sum(keep_subsequent_attempt),
    keep_participant = keep_participant > 0
  ) %>% 
  ungroup()

attempts_Xb_filtered %>%
  write.csv("filtered-attempts-Xb.csv", row.names = FALSE)

# define the parts of the responsesummary to extract
response_parts = c("ans1", "ans1_whydifferent", "ans2", "ans3", "prt1", "prt2", "prt3")
stuff_to_remove = paste(paste0(response_parts, ": "), collapse = "|")

attempts_Xc = attempt_data %>% 
  # restrict to a certain question, and only attempts that were graded
  filter(question_name == "Xc",
         str_detect(state, "graded")) %>% 
  # break up the responsesummary into its parts
  separate(responsesummary, sep = "; ",
           into = response_parts) %>% 
  # tidy up these parts, by removing "ans1: " etc, and also "[score]"
  mutate_at(response_parts, ~str_remove_all(., stuff_to_remove)) %>%
  mutate_at(response_parts, ~str_remove_all(., " \\[score\\]")) %>%
  # keep only the interesting columns
  select(username, attempt_number, response_parts, state, fraction)

# correct answer:
# ans1: [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8] [score];
# ans1_whydifferent: "The terms are decimals" [score];
# ans2: [-1,-2,-3,-4,-5,-6,-7,-8] [score]; 
# ans3: [2,4,2,4,2,4,2,4] [score]; 
# prt1: ATGTE_true. | prt1-2-T | prt1-1-T; 
# prt2: ATGTE_true. | prt2-2-T | ATLogic_True | prt2-1-T; 
# prt3: ATGTE_true. | prt3-5-T | ATLogic_True | prt3-6-T | ATLogic_True | prt3-7-T
attempts_Xc_filtered = attempts_Xc %>% 
  mutate(
    ans1correct = str_detect(prt1, "prt1-1-T"),
    ans2correct = str_detect(prt2, "prt2-1-T"),
    ans3correct = str_detect(prt3, "prt3-7-T"),
    
    keep_first_attempt = case_when(
      
      # We only care about first attempts here
      !attempt_number == 1 ~ 0,
      
      # they got at least one part fully correct
      # (since they visited the "full marks" PRT node)
      ans1correct | ans2correct | ans3correct ~ 1,
      
      # discard if all inputs are the same
      ans1 == ans2 & ans2 == ans3 ~ 0,
      
      # discard if no input contains "," meaning they did not enter a list
      !str_detect(ans1, ",") & !str_detect(ans2, ",") & !str_detect(ans3, ",") ~ 0,
      
      # otherwise, retain
      TRUE ~ 1 
    ),
    keep_subsequent_attempt = case_when(
      # if a subsequent attempts contains a correct answer which
      # differs from the teacher answer
      (ans1correct & ans1 != "[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8]")
      | (ans2correct & ans2 != "[-1,-2,-3,-4,-5,-6,-7,-8]")
      | (ans3correct & ans3 != "[2,4,2,4,2,4,2,4]") ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  # For each user, add up the scores above to see if they have any data worth including
  group_by(username) %>% 
  mutate(
    keep_participant = sum(keep_first_attempt) + sum(keep_subsequent_attempt),
    keep_participant = keep_participant > 0
  ) %>% 
  ungroup()

attempts_Xc_filtered %>%
  write.csv("filtered-attempts-Xc.csv", row.names = FALSE)

participant_engagement_X = bind_rows(list("Xa" = attempts_Xa_filtered,
                                          "Xb" = attempts_Xb_filtered,
                                          "Xc" = attempts_Xc_filtered),
                                     .id = "question") %>% 
  select(question, username, attempt_number, keep_first_attempt, keep_subsequent_attempt) %>% 
  # For each user, add up the scores above to see if they have any data worth including
  group_by(username) %>% 
  summarise(
    keep_participant = sum(keep_first_attempt) + sum(keep_subsequent_attempt),
    keep_participant = if_else(keep_participant > 0, "participating", "non-participating")
  ) %>% 
  filter(str_length(keep_participant)>0)

attempt_data_with_participation = attempt_data %>% 
  left_join(participant_engagement_X, by = "username")

# Write this data to file for later use
attempt_data_with_participation %>%
  write_rds("FAC_LGE-attempt_data_filtered.rds")
