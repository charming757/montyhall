#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' player selects a door
#' 
#' @description
#' `select_door()` chooses a door numbered between 1 and 3 randomly
#' 
#' @details
#' Simulates a players first pick in a monty hall game
#'
#' @param 
#' ... no arguments used by the function
#'
#' @return 
#' The function returns a single value for the chosen door
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a door with goat behind it.
#'
#' @description
#' This function randomly selects from the doors with goats behind them as determined by the create_game() function
#' 
#' @details
#' only "goat" doors can be opened.
#'
#' @param 
#' This function takes parameters game- the output of create_game()-and a.pick the output of select_door()
#'
#' @return
#' This function returns a number corresponding to the door that was opened. 
#'
#' @examples
#' opened_goat_door(my.game, first.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change door function selects final pick depending on player's strategy.
#'
#' @description
#' If the player's strategy is to stay then the number corresponding to the chosen door does not change. If the player's strategy is to "switch then the function selects the door that is neither opened nor the original pick and that door value is now the final pick.
#'
#' @details
#' If the player's strategy is to stay then the number corresponding to the chosen door does not change. If the player's strategy is to "switch then the function selects the door that is neither opened nor the original pick and that door value is now the final pick.
#'
#' @param
#' This function takes 3 parameters, stay which is logical determining if the player's strategy is to stay or switch, opened.door which is the output from opened_door() and a number corresponding to the door that was opened, and a.pick which is the output of select_door() which is a number corresponding to the door that was originally chosen by the player.
#'
#' @return
#' This function returns the number value corresponding to the player's final pick.
#'
#' @examples
#' change_door(stay=T, open.door, first.pick)
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' This function determines if the player won or lost the Monty Hall game.
#' 
#' @description
#' This function evaluates if the player's final pick corresponds to the door with a car behind it. If so the player wins otherwise they lose.
#'
#' @details
#' This function evaluates if the player's final pick corresponds to the door with a car behind it. If so the player wins otherwise they lose.
#'
#' @param
#' This function takes two parameters, final.pick the output of change_door() and the value of the door that was the player's final pick and game which is the output of create_game() and defines what is behind each door. 
#'
#' @return
#' This function returns a string "WIN" if the car door was chosen and "LOSE" if a goat door was chosen.
#'
#' @examples
#' determine_winner(my.final.pick, my.game) 
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' This function plays an entire Monty Hall game all the way through.
#'
#' @description
#' This function uses all the previously defined functions to play a single Monty Hall game all the way through.
#'
#' @details
#' This function uses all the previously defined functions to play a single Monty Hall game all the way through.
#' 
#' @param
#'... this function has no parameters  
#' 
#' @return
#' `play_game()` returns two strings one with the strategy selected either "switch" or "stay" and one with the outcome of the game either "WIN" or "LOSE". 
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' This function plays n Monty Hall games and returns the results as a table.
#'
#' @description
#' This function plays n Monty Hall games and returns the results along side the strategy as a table.This function will allow you to evaluate which strategy "switch" or "stay" yields more wins on average. 
#'
#' @details
#' This function plays n Monty Hall games and returns the results along side the strategy as a table.This function will allow you to evaluate which strategy "switch" or "stay" yields more wins on average. 
#'
#' @param
#' This function takes one parameter n which dictates the number of Monty Hall games played with default 100.  
#'
#' @return
#' Returns a table of counts of wins vs. counts of losses by strategy. 
#'
#' @examples
#' play_n_games(n=1000)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}



