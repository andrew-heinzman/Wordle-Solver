if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               furrr,
               here,
               glue)

plan(multisession) # Setting up parallel processing for future_map

# Reading in word lists ---------------------------------------------------
# Worlde answer and guess list from Cyrus Freshman via: https://www.reddit.com/r/wordle/comments/s4tcw8/a_note_on_wordles_word_list/
wordle_answer_list <- read_delim("https://gist.githubusercontent.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b/raw/a9e55d7e0c08100ce62133a1fa0d9c4f0f542f2c/wordle-answers-alphabetical.txt",
                             delim = "\n",
                             col_names = F
                             ) %>% 
  pull()

# Allowed guesses that are not possible answers
wordle_guess_only_list <- read_delim("https://gist.githubusercontent.com/cfreshman/cdcdf777450c5b5301e439061d29694c/raw/975d5100d91ddfe87d99dcee1f01170a43520ea0/wordle-allowed-guesses.txt",
                             delim = "\n",
                             col_names = F
) %>% 
  pull()

wordle_guess_list <- c(wordle_answer_list, wordle_guess_only_list)

# Preliminary functions ---------------------------------------------------
check_guess <- function(guess, answer){
  # Function to return the Wordle output given a guess and correct answer
    # - represents that letter does not occur in the answer
    # G represents that letter occurs in the answer in the given position
    # Y represents that letter occurs in the answer in a different position
  if(guess == answer) return("GGGGG")
  result <- rep("-", 5)
  
  guess_vec <- unlist(strsplit(guess, split = ""))
  answer_vec <- unlist(strsplit(answer, split = ""))

  result[map_lgl(guess_vec, ~ .x %in% answer_vec)] <- "Y"  
  result[guess_vec == answer_vec] <- "G"
  
  return(paste0(result, collapse = ""))
}
check_guess <- Vectorize(check_guess)

calc_score <- function(guess, word_list = wordle_answer_list){
  # Function to calculate the expected number of words in the posterior set after a guess
  
  # generate all of the potential outcomes
  res <- check_guess(guess, word_list) %>% 
    # calculating how often each outcome occurs
    table() %>% 
    # squaring the number of occurrences because the probability of that outcome is
      # number of occurrences / total number of words in word list
    .^2 %>% 
    # dividing to get expected number of words remaining
    sum()/length(word_list)
    
  return(res)
}

# Generating scores for the first guess -----------------------------------
if(!file.exists(here("data", "first_word_scores.csv"))){
  word_scores <- tibble(words = wordle_guess_list) %>%
    mutate(scores = future_map_dbl(words, calc_score),
           potential_answer = words %in% wordle_answer_list) %>%
    arrange(scores)
  
  # write_csv(word_scores, here("data", "first_word_scores.csv"))
}
# 'roate' has the lowest expected number of words in the posterior set (60.4).
# 'raise' has the second lowest expected number of words in the posterior set (61).
# 'raise' is used as the best first guess because 'roate' is not a potential answer
# and the difference in expected posterior words is small.

# Updating after each guess -----------------------------------------------
posterior_words <- function(guess, result, potential_answers){
  # Function that given a prior list of potential answers, a guess, and a result 
  # returns all remaining potential answers
  possible_results <- map_chr(potential_answers, ~check_guess(guess, .x))
  
  remaining_answers <- potential_answers[possible_results == result]
  return(remaining_answers)
}

gen_new_guess <- function(potential_answers, master_guess_list = wordle_guess_list) {
  # Takes a vector of potential_answers and generates the best guess from master_guess_list
  
  # If there are less than two potential answers it is best to guess randomly from answers
  if(length(potential_answers) <= 2){
    return(potential_answers[1])
  }
  
  # Calculating scores for all potential guesses
  new_scores <- map_dbl(master_guess_list, ~ calc_score(.x, potential_answers))
  # Subsetting potential guesses to those with the best scores
  new_guess_options <- master_guess_list[new_scores == min(new_scores)]
  
  # If any of the potential answers is a best guess use the potential answer
  if(any(potential_answers %in% new_guess_options)){
    return(potential_answers[potential_answers %in% new_guess_options][1])
  } else {
    return(new_guess_options[1])
  }
}

# Second round best guesses -----------------------------------------------
# Generating the best second guesses given 'raise' was guessed first
if(file.exists(here("data", "second_word_guesses.csv"))){
  second_word_guesses <- read_csv(here("data", "second_word_guesses.csv"))
} else{
  # Generate the best second round guesses after playing 'raise' in the first round
  second_word_guesses <- tibble("potential_answers" = wordle_answer_list,
                                "results" = map_chr(wordle_answer_list, ~ check_guess("raise", .x))) %>% 
    # grouping together the potential answers by the result after guessing 'raise'
    group_by(results) %>% 
    nest() %>%
    ungroup() %>% 
    # generating the next best guess for each group of potential answers
    mutate(new_guess = future_map_chr(data, ~gen_new_guess(.x$potential_answers)),
           words_remaining = map_int(data , ~dim(.x)[1]))
  
  second_word_guesses %>% 
    select(-data) %>%
    mutate(results = glue(" {results}")) %>% # adding a leading space to make CSV open-able in Excel
    write_csv(here("data", "second_word_guesses.csv"))
}

# Saving best second guesses as a vector that can be easily looked up off of
second_guess <- second_word_guesses$new_guess
names(second_guess) <- second_word_guesses$results

# Round by round Wordle play ----------------------------------------------
play_round <- function(guess, result, prior_word_list, 
                       print_guess = TRUE){
  # Given a guess, the result, and vector of potential answers before the result
  # returns the next bet guess along with the remaining potential answers.
  
  # Update word list based on what the guess and result eliminates
  updated_word_list <- posterior_words(guess, result, prior_word_list)
  
  if(guess == "raise"){ # Using prior calculations for second word to guess
    new_guess <- second_guess[result]
  } else{ 
    # Generating a new guess
    new_guess <- gen_new_guess(updated_word_list)
  }
  
  if(print_guess)  print(glue("{length(updated_word_list)} words remaining, guess: {new_guess}"))
  
  return(list(
    "guess" = new_guess,
    "updated_word_list" = updated_word_list
  ))
}

# rnd1 <- play_round("raise", "--G--", wordle_answer_list)
# rnd2 <- play_round("clint", "Y-G-Y", rnd1$updated_word_list)
# rnd3 <- play_round("thick", "GGGGG", rnd2$updated_word_list)

# Auto-solver -------------------------------------------------------------
wordle_solver <- function(true_answer,
                          master_answer_list = wordle_answer_list,
                          print_guess = TRUE){
  # Function that given a true answer simulates playing Wordle by 
  # generating a series of guesses until the true answer is determined.
  # Returns the number and sequence of guesses the solver made.
  
  # Ensuring the true answer is a 5 letter word in the answer list
  true_answer <- tolower(true_answer)
  stopifnot(nchar(true_answer)==5,
            true_answer %in% master_answer_list)
  
  # Make the initial guess of 'raise' and check the result
  num_guesses <- 1
  updated_word_list <- master_answer_list
  guess <- "raise"
  guess_sequence <- "raise"
  result <- check_guess(guess, true_answer)
  
  # If we were not right on the first try we have to guess again
  if(result != "GGGGG"){
    while(result != "GGGGG" & num_guesses < 50){
      # Generate the updated word list and the next guess
      next_rnd <- play_round(guess, result, updated_word_list, print_guess = print_guess)
      guess <- next_rnd$guess
      updated_word_list <- next_rnd$updated_word_list

      # Check if the new guess is right
      result <- check_guess(guess, true_answer)
      # Adding to number and sequence of guesses
      num_guesses <- num_guesses + 1
      guess_sequence <- paste0(c(guess_sequence, guess), collapse = ", ")
    }
  }
  return(list("total_guesses" = num_guesses,
              "guess_sequence" = guess_sequence))
}

# wordle_solver("thick")

# Calculating turns to solve all potential answers ------------------------
if(!file.exists(here("data", "turns_to_solve.csv"))){
  turns_to_solve <- cbind("true_answer" = wordle_answer_list,
                          future_map_dfr(wordle_answer_list, ~wordle_solver(.x, print_guess = F)))

  write_csv(turns_to_solve, here("data", "turns_to_solve.csv"))
} else {
  turns_to_solve <- read_csv(here("data", "turns_to_solve.csv"))
}

summary(turns_to_solve$total_guesses)
hist(turns_to_solve$total_guesses)
