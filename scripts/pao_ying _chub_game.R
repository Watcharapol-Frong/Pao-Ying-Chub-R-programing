library(R6)

# Create R6 Class
PaoYingChub <- R6Class("PaoYingChub",
  public = list(
    user_hands = c("hammer", "scissor", "paper"), # field
    user_score = 0,  # field
    computer_score = 0, # field
    round_number = 1, # field
    difficulty = "Easy", # field for difficulty
    user_history = c(), # field to store user choices
    
    # function start
    initialize = function() {
      message("Welcome to the Pao Ying Chub Game")
    },
    
    # function for choosing difficulty
    choose_difficulty = function() {
      repeat {
        cat("Choose difficulty: Easy or Hard\n")
        input_difficulty <- tolower(trimws(readline("Enter difficulty level: ")))
        
        if (input_difficulty %in% c("easy", "hard")) {
          self$difficulty <- input_difficulty
          cat("Difficulty set to:", self$difficulty, "\n")
          break
        } else {
          message("\033[31mInvalid choice. Please choose 'Easy' or 'Hard'.\033[0m")
        }
      }
    },
    
    # function computer choice with difficulty levels
    computer_choice = function() {
      if (self$difficulty == "easy") {
        return(sample(self$user_hands, 1))  # random
        
      } else if (self$difficulty == "hard") {
        # คำนวณความถี่ของมือผู้เล่น แล้วเลือกมือที่ชนะบ่อยที่สุด
        if (length(self$user_history) > 2) {
          most_common <- names(sort(table(self$user_history), decreasing = TRUE))[1]
          return(switch(most_common, "hammer" = "paper", "scissor" = "hammer", "paper" = "scissor"))
        } else {
          return(sample(self$user_hands, 1))
        }
      }
    },
    
    # function scoring
    determine_winner = function(user_hand, com_hand) {
      if (user_hand == com_hand) {
        return("tie")
      } else if (
        (user_hand == "hammer" && com_hand == "scissor") ||
        (user_hand == "scissor" && com_hand == "paper") ||
        (user_hand == "paper" && com_hand == "hammer")
      ) {
        return("user")
      } else {
        return("computer")
      }
    },
    
    # function play game
    play_game = function() {
      self$choose_difficulty()
      while (TRUE) {
        cat(paste("Round", self$round_number, ": Hammer, Scissor, Paper or End Game\n"))
        input_user_hand <- tolower(trimws(readline("Please choose one option: ")))
        
        if (input_user_hand == "" || grepl("[^a-zA-Z ]", input_user_hand)) {
          message("\033[31mInvalid input, please choose again!\033[0m")
          
        } else {
          matched_hand <- self$user_hands[agrepl(input_user_hand, self$user_hands, ignore.case = TRUE)]
          
          if (length(matched_hand) == 1) {
            input_user_hand <- matched_hand
            self$user_history <- c(self$user_history, input_user_hand) # Store user choice
            com_hand <- self$computer_choice()
            cat("You chose:", input_user_hand, "\n")
            cat("Computer chose:", com_hand, "\n")
            
            winner <- self$determine_winner(input_user_hand, com_hand)
            
            if (winner == "user") {
              cat("You win!\n")
              self$user_score <- self$user_score + 1
            } else if (winner == "computer") {
              cat("Computer wins!\n")
              self$computer_score <- self$computer_score + 1
            } else {
              cat("It's a tie!\n")
            }
            
            cat(paste("Your score:", self$user_score, "\n"))
            cat(paste("Computer score:", self$computer_score, "\n"))
            self$round_number <- self$round_number + 1
            
          } else if (input_user_hand %in% c("end game", "endgame")) {
            cat("Thank you for playing!\n")
            self$show_results()
            return(invisible(NULL))
            
          } else {
            message("\033[31mInvalid choice, please choose again!\033[0m")
          }
        }
      }
    },
    
    # function results
    show_results = function() {
      cat(paste(
        "Final Scores\n",
        "User:", self$user_score, "\n",
        "Computer:", self$computer_score, "\n",
        "Rounds:", self$round_number, "\n"))
    }
  ) # end list
) # end R6 class

game <- PaoYingChub$new()
game$play_game()
