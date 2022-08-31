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
#' 	Creating Doors 

#' @description
#'	 Using '<-' we will create 3 doors and assign it doors

#' @details 
#' 	Three doors are created here. The contestant will select a door. 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'	The function returns a number between 1 and 3, that will be the door that is selected.

#' @examples
#'  return(a.pick)

#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Host Opens Goat Door 
#'
#' @description
#'	The function open_goat_door will open a door with a goat behind it.
#'
#' @details
#'	The host will always open a door w/ a goat behind it. But it can't be a door the #'contestant has already selected. So it must be a door that is not a car and not a #'current conestant selection.
#' @param ... no arguments are used by the function.
#' 
#' @return 
#' The function returns a number between 1 and 3
#'
#' @examples
#' return(opened.door)
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
#' Change Doors 
#'
#' @description
#'  The function change_door is used to create the outcomes for if a contestant stays or #' change the door their initially chose
#'
#' @details
#' The contestant is given the option to change from their initial selection to the other #'door that is still closed. 
#'
#' @param ... no arguments are used by the function
#' 
#' @return 
#' return(final.pick)
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
#' Determining if Contestant Wins
#'
#' @description
#'  return(game.results) will determine if the contestant has won or #'  #' lost. 
#'
#' @details
#'This function will determine if the contestant wins or loses and will #'  report it. 
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'  The function returns a Win or Lose
#' 
#' @examples
#' return(game.results)
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
#'  Playing the Game
#'
#' @description
#' play_game function will go through the code and play the game 
#'
#' @details
#' The commands created above are played out in this code. The options are presented here as switching and staying and the whether the outcome was "win" or "lose". 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return 
#' return(game.results)
#'
#' @examples
#'play_game()
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
#' Adding the Game to a Loop
#'
#' @description
#' play_n_games function will allow us to play the game multiple times, in this case 100 #'times and see the results. 
#'
#' @details
#' If we want to see the results for this game after playing the game many times we can #'add the game to a loop. This will play the game 'n" number of times and report the #'results
#'
#' @param ... no arguments are used by the function.
#' 
#' @return 
#' The function will return a table the number of wins and loses in a table
#'
#' @examples
#' (return(results.df))
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





#' @title
#' Changing Number of Doors, Goats and Cars 
#'
#' @description
#'  build_doors will be used to be able to change the number of doors. 
#'
#' @details
#' We can alter our code to change how many doors, goats, and cars we use. You will #'need to download magrittr package: library(magrittr). For this example we will have 10 doors with 1 cars and 9 goats. 
#'
#' @param ... no arguments are used by the function.
#'#' @return
#' @export
build_doors <- function( n=10 ){ return( 1:n ) }


#' @title
#'  Creating a Game w/ 9 Goats and 1 Car
#'
#' @description
#'    Create_game function will allow us to determine the number of doors, goats and cars. 
#'
#' @details
#' We will have to adjust our code so we can easily change the number of doors, cars, #'and #'goats.
#'
#' @param ... no arguments are used by the function.
# @return
#' Nothing is returned in this chunk of code 
#' @examples
#' return(a.game)
#'
#' @export
create_game <- function( )
{
    a.game <- sample( x=rep( c("goat","car"), c(9,1) ), 
                      size=10, replace=F )
    return( a.game )
}


#' @title
#'  Selecting a Door
#'
#' @description
#'  This function will select a door
#' @param ... no arguments are used by the function.
# @return
#' A door is picked
#' @export
select_door <- function( )
{
  doors <- build_doors() 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  
}


#' @title
#'  Opening a Door
#'
#' @description
#'  The same code we used when using 3 doors is also used for this step. 
#' @param ... no arguments are used by the function.
#' @return
#' This determines what what door is opened
#' @export

open_doors <- function( game, a.pick )
{
   
   doors <- build_doors()
   
   if( game[ a.pick ] == "car" )
   { 
     opened.car.door <- doors[ game == "car" & doors != a.pick ]
     goat.doors <- doors[ game != "car" ] 
     opened.goat.door <- sample( goat.doors, size=1 )
     opened.doors <- c( opened.car.door, opened.goat.door )
   }
   
   if( game[ a.pick ] == "goat" )
   { 
     opened.car.door <- sample( doors[game=="car"], size=1 )
     available.goat.doors <- doors[ game != "car" & doors != a.pick ] 
     opened.goat.door <- sample( available.goat.doors, size=1 )
     opened.doors <- c( opened.car.door, opened.goat.door )
   }
   return( opened.doors ) 
}

#' @title
#' Changing Doors, Determining Winner
#'
#' @description
#'   The code for changing doors and determining a winner are the same as in our first #'example. 
#' @param ... no arguments are used by the function.
#' @return
#' This returns whether the contestant won or lost 
#' @export
change_door <- function( stay=T, opened.doors, a.pick )
{
   doors <- build_doors()
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     available.doors <- 
        doors[  ! ( doors %in% opened.doors | doors == a.pick )  ]
     final.pick  <- sample( available.doors, size=1 ) 
   }
  
   return( final.pick )  # number between 1 and 5
}

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
#'  Play the Game
#'
#' @description
#' To play the game, it is the same code as our first example.
#' @param ... no arguments are used by the function.
#' @return
#' This code plays the game
#' @export

this.game <- create_game()
my.initial.pick <- select_door()
opened.doors <- 
   open_doors( this.game, my.initial.pick )
# save results for both strategies for the game
my.final.pick.stay <- 
   change_door( stay=T, 
                opened.doors=opened.doors, 
                a.pick=my.initial.pick )
my.final.pick.switch <- 
   change_door( stay=F, 
                opened.door=opened.doors, 
                a.pick=my.initial.pick )

game.outcome.stay <- 
   determine_winner( final.pick=my.final.pick.stay, 
                     game=this.game )
game.outcome.switch <- 
   determine_winner( final.pick=my.final.pick.switch, 
                     game=this.game )
                     
#' @title
#'  Showing the outcome: Swithc vs Stay
#'
#' @description
#' play_game function will go through the code and play the game and show the outcome of switching vs staying
#' @details
#' The game will be played and it will show us the outcome of switching vs staying
#' @param ... no arguments are used by the function.
#' @return
#' This returns the outcome for each strategy
#' The commands created above are played out in this code. The options are presented here as switching and staying and the whether the outcome was "win" or "lose". 
#'
#'
#' @examples
#'play_game()
#'
#' @export
 play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_doors( new.game, first.pick )

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

play_game()

#' @title
#' Adding the Game to a Loop
#'
#' @description
#' play_n_games function will allow us to play the game multiple times, in this case 100 #'times and see the results. 
#' @details
#' If we want to see the results for this game after playing the game many times we can #'add the game to a loop. This will play the game 'n" number of times and report the #'results
#'
#' @param ... no arguments are used by the function.
#' @return 
#' The function will return a table the number of wins and loses in a table
#'
#' @examples
#' (return(results.df))
#'
#' @export               
results.df <- NULL   

for( i in 1:10000 )  
{
  game.outcome <- play_game()
  # binding step
  results.df <- rbind( results.df, game.outcome )
}

table( results.df ) 

table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 )
