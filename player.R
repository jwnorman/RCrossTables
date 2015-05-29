# player.php
player <- function(playerID=NULL, naspaID=NULL, upcoming=FALSE, partial=FALSE, add=TRUE, asdf=FALSE) {
	require(RJSONIO)
	prefix <- "http://cross-tables.com/rest/player.php"

	# Check required parameters
	if (is.null(playerID) && is.null(naspaID)) {
		stop("Either playerID or naspaID must be provided.")
	}

	# Check class types
	if (!(is.null(playerID) || class(playerID) == "numeric" || class(playerID) == "integer" || class(playerID) == "character")) {
		stop("'playerID' must be NULL or of class 'numeric', 'integer', or 'character'")
	}
	if (!(is.null(naspaID) || class(naspaID) == "character")) {
		stop("'naspaID' must be NULL or  of class 'character'")
	}
	if (!(class(partial) == "logical")) {
		stop("'partial' must be of class 'logical'")
	}
	if (!(class(add) == "logical")) {
		stop("'add' must be of class 'logical'")
	}
	if (!(class(asdf) == "logical")) {
		stop("'asdf' must be of class 'logical'")
	}

	# Find max length
	maxLength <- max(c(length(playerID), length(naspaID), length(upcoming), length(partial), length(add)))
	if (!is.null(playerID)) playerID <- rep(playerID, length.out=maxLength)
	if (!is.null(naspaID)) naspaID <- rep(naspaID, length.out=maxLength)
	upcoming <- rep(upcoming, length.out=maxLength)
	partial <- rep(partial, length.out=maxLength)
	add <- rep(add, length.out=maxLength)

	# Paste links together
	if (!is.null(playerID)) {
		id <- paste("?player=", playerID, sep='')
	} else {
		id <- paste("?naspa=", naspaID, sep='')
	}
	link <- paste(prefix, id, sep='')
	link <- ifelse(add, paste(link, "&addresults=1", sep=''), link)
	link <- ifelse(upcoming, paste(link, "&upcoming=1", sep=''), link)
	link <- ifelse(partial, paste(link, "&partialresults=1", sep=''), link)

	# Access API and convert to R object
	playerList <- lapply(link, function(url) {
		out <- tryCatch(fromJSON(url, nullValue = ""),
			error = function(cond) {
				return("Connection Error. Retry.")
			},
			warning = function(cond) {
				return("Connection Error. Retry.")
			},
			finally = {
				
			})
		})

	# Return
	if (!asdf) invisible(playerList)
	else invisible(as.data.frame(t(as.data.frame(playerList))))
}

# portioned calls; time estimates
numPlayers <- getMaxPlayerID()
begintime <- Sys.time()
allplayers <- player(1:getMaxPlayerID()) # 1.637622 hours
endtime <- Sys.time()
totaltime <- endtime - begintime; totaltime
avgtime <- totaltime/numPlayers; avgtime # .35532
getMaxPlayerID()*avgtime/60/60 # 2 hours 23 minutes
save(allplayers, file="~/Documents/Scrabble/Studies/RCrossTables/playerslist.Rda")

# convert to data frame and clean
allplayers2 <- allplayers[-5286] # problems with playerid=5286 because it uses null instead of ""
playerDataFrame <- as.data.frame(t(as.data.frame(allplayers2)))
missingPlayerids <- grep("no player with playerid", playerDataFrame$playerid)
playerDF <- playerDataFrame[-missingPlayerids, ]
save(playerDF, file="~/Documents/Scrabble/Studies/RCrossTables/player.Rda")