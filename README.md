# EPL_project
Use R programming to create a function with the inputs of date and season that pulls English Premier League data sets from the web (https://www.football-data.co.uk/englandm.php). Then manipulate the data to return a data frame with stats about the match outcomes for the date and season specified. The parameters of the function (in order) are date, in the mm/dd/yyyy format, and
season specified by yyyy/yy, such as 2017/18.


Along with the team name, the data frame returned by the function contains the following columns where the column name is specified in parentheses:

Record as wins-loses-ties (Record), home record (HomeRec), away record (AwayRec), matches played (MathchesPlayed), points (Points), points per match (PPM), point percentage = points / 3 * the number of games played, (PtPct), goals scored (GS), goals scored per match (GSM), goals allowed (GA), goals allowed per match (GAM), the team’s record over the last 10 games played (Last10), and the team’s current streak where W6 would mean the team has won their last six games but they have not won their last 7 games, L2 indicates a team has lost their last two games but they have not lost their last 3 games, while T1 denotes the team has tied their most recent game but did not tie their last two games. (Streak)
