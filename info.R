# info.php
getMaxPlayerID <- function() {
	as.integer(fromJSON("http://cross-tables.com/rest/info.php")["maxplayerid"])
}

getMaxGameID <- function() {
	as.integer(fromJSON("http://cross-tables.com/rest/info.php")["maxgameid"])
}

getMaxTourneyID <- function() {
	as.integer(fromJSON("http://cross-tables.com/rest/info.php")["maxtourneyid"])
}