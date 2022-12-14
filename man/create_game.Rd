% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem (1).R
\name{create_game}
\alias{create_game}
\title{Create a new Monty Hall Problem game.}
\usage{
create_game()

create_game()
}
\arguments{
\item{...}{no arguments are used by the function.
Nothing is returned in this chunk of code}
}
\value{
The function returns a length 3 character vector
indicating the positions of goats and the car.
}
\description{
\code{create_game()} generates a new game that consists of two doors
with goats behind them, and one with a car.

Create_game function will allow us to determine the number of doors, goats and cars.
}
\details{
The game setup replicates the game on the TV show "Let's
Make a Deal" where there are three doors for a contestant
to choose from, one of which has a car behind it and two
have goats. The contestant selects a door, then the host
opens a door to reveal a goat, and then the contestant is
given an opportunity to stay with their original selection
or switch to the other unopened door. There was a famous
debate about whether it was optimal to stay or switch when
given the option to switch, so this simulation was created
to test both strategies.

We will have to adjust our code so we can easily change the number of doors, cars, #'and #'goats.
}
\examples{
  create_game()

return(a.game)

}
