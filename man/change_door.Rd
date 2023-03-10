% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{change_door}
\alias{change_door}
\title{Change door function selects final pick depending on player's strategy.}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{This}{function takes 3 parameters, stay which is logical determining if the player's strategy is to stay or switch, opened.door which is the output from opened_door() and a number corresponding to the door that was opened, and a.pick which is the output of select_door() which is a number corresponding to the door that was originally chosen by the player.}
}
\value{
This function returns the number value corresponding to the player's final pick.
}
\description{
If the player's strategy is to stay then the number corresponding to the chosen door does not change. If the player's strategy is to "switch then the function selects the door that is neither opened nor the original pick and that door value is now the final pick.
}
\details{
If the player's strategy is to stay then the number corresponding to the chosen door does not change. If the player's strategy is to "switch then the function selects the door that is neither opened nor the original pick and that door value is now the final pick.
}
\examples{
change_door(stay=T, open.door, first.pick)

}
