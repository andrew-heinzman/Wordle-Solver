pacman::p_load(tidyverse,
               furrr,
               here,
               glue,
               tictoc,
               words)

plan(multisession)

# worlde answer and guess list from Cyrus Freshman via: https://www.reddit.com/r/wordle/comments/s4tcw8/a_note_on_wordles_word_list/
wordle_answer_list <- read_delim("https://gist.githubusercontent.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b/raw/a9e55d7e0c08100ce62133a1fa0d9c4f0f542f2c/wordle-answers-alphabetical.txt",
                             delim = "\n",
                             col_names = F
                             ) %>% 
  pull()

wordle_guess_only_list <- read_delim("https://gist.githubusercontent.com/cfreshman/cdcdf777450c5b5301e439061d29694c/raw/975d5100d91ddfe87d99dcee1f01170a43520ea0/wordle-allowed-guesses.txt",
                             delim = "\n",
                             col_names = F
) %>% 
  pull()

wordle_guess_list = c(wordle_answer_list, wordle_guess_only_list)

# Generating vector of all five letter words from the scrabble dictionary
scrabble_word_list <- words %>% 
  filter(word_length == 5) %>% 
  pull(word)

# Function to return the Wordle output given a guess and correct answer
  # - represents that letter does not occur in the answer
  # G represents that letter occurs in the answer in the given position
  # Y represents that letter occurs in the answer in a different position
check_guess <- function(guess, answer){
  if(guess == answer) return("GGGGG")
  result = rep("-", 5)
  
  guess_vec = unlist(strsplit(guess, split = ""))
  answer_vec = unlist(strsplit(answer, split = ""))

  result[map_lgl(guess_vec, ~ .x %in% answer_vec)] = "Y"  
  result[guess_vec == answer_vec] = "G"
  
  return(paste0(result, collapse = ""))
}
check_guess = Vectorize(check_guess)

# Function to calculate the expected number of words in the posterior set after the guess
calc_score <- function(guess, word_list = wordle_answer_list){
  # generates all of the potential outcomes
  res <- check_guess(guess, word_list) %>% 
    # calculating how often each outcome occurs
    table() %>% 
    # squaring the number of occurrences because the probability of that outcome is
      # number of occurrences / total number of words in word list
    .^2 %>% 
    # expected value across all possible outcomes
    sum()/length(word_list)
    
  return(res)
}

# Generating scores for the first guess -----------------------------------
if(!file.exists(here("data", "initial_word_scores.csv"))){
  word_scores <- tibble(words = wordle_guess_list) %>%
    mutate(scores = future_map_dbl(words, calc_score),
           potential_answer = words %in% wordle_answer_list) %>%
    arrange(scores)
  
  write_csv(word_scores, here("data", "initial_word_scores.csv"))
}

# Updating after each guess -----------------------------------------------
# function that given a prior word list, a guess, and an output gives a posterior word list
posterior_words <- function(guess, result, prior_words){
  possible_results <- map_chr(prior_words, ~check_guess(guess, .x))
  
  posterior_list <- prior_words[possible_results == result]
  return(posterior_list)
}

gen_new_guess <- function(potential_answers, master_word_list = wordle_guess_list) {
  # takes a vector of potential_answers and generates the best guess from master_word_list
  if(length(potential_answers) <= 2){
    return(potential_answers[1])
  }
  new_scores = map_dbl(master_word_list, ~ calc_score(.x, potential_answers))
  new_guess_options = master_word_list[new_scores == min(new_scores)]
  # if any of the potential answers is a best guess use that
  if(any(potential_answers %in% new_guess_options)){
    return(potential_answers[potential_answers %in% new_guess_options][1])
  } else {
    return(new_guess_options[1])
  }
}

# Second round best guesses -----------------------------------------------
if(file.exists(here("data", "second_word_guesses.csv"))){
  first_round_results <- read_csv(here("data", "second_word_guesses.csv"))
} else{
  # generate second round guesses after playing 'raise' in the first round
  first_round_results <- tibble("words" = wordle_answer_list,
                                "results" = map_chr(wordle_answer_list, ~ check_guess("raise", .x))) %>% 
    group_by(results) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(new_guess = future_map_chr(data, ~gen_new_guess(.x$words)),
           words_remaining = map_int(data , ~dim(.x)[1]))
  
  first_round_results %>% 
    select(-data) %>%
    ungroup() %>% 
    mutate(results = glue(" {results}")) %>% 
    write_csv(here("data", "second_word_guesses.csv"))
}

# saving second guesses as a vector that can be easily looked up off of
second_guess = first_round_results$new_guess
names(second_guess) = first_round_results$results


# Simulating solving Wordle -----------------------------------------------
play_round <- function(guess, result, prior_word_list, 
                       print_guess = TRUE){

  # update word list based on what guess and result eliminates
  new_word_list = posterior_words(guess, result, prior_word_list)

  # Using prior calculations for second work to guess
  if(guess == "raise"){
    new_guess = second_guess[result]
  } else{
    new_guess = gen_new_guess(new_word_list)
  }
  if(print_guess)  print(glue("{length(new_word_list)} words remaining, guess: {new_guess}"))
  return(list("guess" = new_guess,
         "new_word_list" = new_word_list))
}

## actual play
# rnd1 <- play_round("raise", "Y-GYG", wordle_answer_list)
# rnd2 <- play_round("resod", "YYY--", rnd1$new_word_list)
# rnd3 <- play_round("crest", "-YYY-", rnd2$new_word_list)
# rnd4 <- play_round("spire", "G-GGG", rnd3$new_word_list)

# Auto-solver -------------------------------------------------------------
wordle_solver <- function(true_answer,
                          master_word_list = wordle_answer_list,
                          print_guess = T){
  # ensuring true answer is a 5 letter word
  true_answer = tolower(true_answer)
  stopifnot(nchar(true_answer)==5,
            true_answer %in% master_word_list)
  # stopifnot()
  
  # make the initial guess and check the result
  num_guesses = 1
  updated_word_list = master_word_list
  guess = "raise"
  guess_sequence = "raise"
  result <- check_guess(guess, true_answer)
  
  # if we were not right on the first try we have to guess again
  if(result != "GGGGG"){
    while(result != "GGGGG" & num_guesses < 50){
      # generate the updated word list and the next guess
      next_rnd <- play_round(guess, result, updated_word_list, print_guess = print_guess)
      guess = next_rnd$guess
      updated_word_list = next_rnd$new_word_list
      guess_sequence = paste0(c(guess_sequence, guess), collapse = ", ")
      # check if the new guess is right
      result <- check_guess(guess, true_answer)
      num_guesses = num_guesses + 1
    }
  }
  return(list("total_guesses" = num_guesses,
              "guess_sequence" = guess_sequence))
}
# wordle_solver("shire")

# Calculating how many turns to solve for given answers -------------------
if(!file.exists(here("data", "turns_to_solve.csv"))){
  # takes ~37 minutes
  turns_to_solve <- cbind("true_answer" = wordle_answer_list,
                          future_map_dfr(wordle_answer_list, ~wordle_solver(.x, print_guess = F)))

  write_csv(turns_to_solve, here("data", "turns_to_solve.csv"))
} else {
  turns_to_solve <- read_csv(here("data", "turns_to_solve.csv"))
}

summary(turns_to_solve$total_guesses)
hist(turns_to_solve$total_guesses)
