pacman::p_load(tidyverse,
               furrr,
               here,
               glue,
               # tictoc,
               words)

# pb <- progress_bar$new(total = 100)
plan(multisession)

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

# Function to calculate the expected number of words in the posterior set after the guess
calc_score <- function(guess, word_vec = scrabble_word_list){
  # generates all of the potential outcomes
  res <- map_chr(word_vec, ~check_guess(guess, .x)) %>% 
    # calculating how often each outcome occurs
    table() %>% 
    # squaring the number of occurrences because the probability of that outcome is
      # number of occurrences / total number of words in word list
    map_dbl(~.x^2) %>% 
    # expected value across all possible outcomes
    sum()/length(word_vec)
    
  return(res)
}

# Generating scores for the first guess -----------------------------------
if(!file.exists(here("data", "initial_word_scores.csv"))){
  word_scores <- tibble(words = scrabble_word_list) %>%
    mutate(scores = future_map_dbl(words, calc_score)) %>%
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

# takes a word_list of potential answers and generates the best guess from master_word_list
gen_new_guess <- function(word_list, master_word_list = scrabble_word_list) {
  if(length(word_list) <= 2){
    return(word_list[1])
  }
  new_scores = map_dbl(master_word_list, ~ calc_score(.x, word_list))
  new_guess = master_word_list[new_scores == min(new_scores)][1]
  return(new_guess)
}

# Second round best guesses -----------------------------------------------
if(file.exists(here("data", "second_word_guesses.csv"))){
  first_round_results <- read_csv(here("data", "second_word_guesses.csv"))
} else{
  # generate second round guesses after playing 'lares' in the first round
  first_round_results <- tibble("words" = scrabble_word_list,
                                "results" = map_chr(scrabble_word_list, ~ check_guess("lares", .x))) %>% 
    group_by(results) %>% 
    nest() %>%
    mutate(new_guess = map_chr(data, ~map_chr(.x, gen_new_guess)),
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
                       master_word_list = scrabble_word_list, print_guess = TRUE){

  # update word list based on what guess and result eliminates
  new_word_list = posterior_words(guess, result, prior_word_list)

  
  # Using prior calculations for second work to guess
  if(guess == "lares"){
    new_guess = second_guess[result]
  } else{
    new_guess = gen_new_guess(new_word_list)
  }
  if(print_guess)  print(glue("{length(new_word_list)} words remaining, guess: {new_guess}"))
  return(list("guess" = new_guess,
         "new_word_list" = new_word_list))
}

# actual play
# rnd1 <- play_round("lares", "Y---G", scrabble_word_list)
# rnd2 <- play_round("toils", "--YGG", rnd1$new_word_list)
# rnd3 <- play_round("idyls", "Y--GG", rnd2$new_word_list)

# Auto-solver -------------------------------------------------------------
wordle_solver <- function(true_answer, master_word_list = scrabble_word_list){
  # ensuring true answer is a 5 letter word
  stopifnot(nchar(true_answer)==5)
  true_answer = tolower(true_answer)
  
  # make the initial guess and check the result
  num_guesses = 1
  updated_word_list = master_word_list
  guess = "lares"
  guess_sequence = "lares"
  result <- check_guess(guess, true_answer)
  
  # if we were not right on the first try we have to guess again
  if(result != "GGGGG"){
    while(result != "GGGGG" & num_guesses < 50){
      # generate the updated word list and the next guess
      next_rnd <- play_round(guess, result, updated_word_list, print_guess = F)
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
# wordle_solver("xenon")

# Calculating how many turns to solve for given answers -------------------
if(!file.exists(here("data", "turns_to_solve.csv"))){
  # takes ~30 minutes
  turns_to_solve <- cbind("true_answer" = scrabble_word_list,
                          future_map_dfr(scrabble_word_list, wordle_solver))
  
  
  write_csv(turns_to_solve, here("data", "turns_to_solve.csv"))
} else {
  turns_to_solve <- read_csv(here("data", "turns_to_solve.csv"))
}

turns_to_solve %>% arrange(desc(total_guesses)) %>% 
  head()


