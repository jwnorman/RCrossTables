# game.php
games <- function(id=NULL, minid=NULL, maxid=NULL, asdf=FALSE) {
	require(RJSONIO)
	prefix <- "http://cross-tables.com/rest/games.php"

	# Check required parameters
	# Either id needs to be provided or minid and maxid
	if (is.null(id) && (is.null(minid) || is.null(maxid))) {
		stop("Either id or minid/maxid must be provided.")
	}

	# Check class types
	# All ids need to be integer or numeric
	if (!(class(id) == "integer" || class(id) == "numeric" || is.null(id))) {
		stop("'id' must be of class 'integer' or 'numeric'")
	}
	if (!(class(minid) == "integer" || class(minid) == "numeric" || is.null(minid))) {
		stop("'minid' must be of class 'integer' or 'numeric'")
	}
	if (!(class(maxid) == "integer" || class(maxid) == "numeric" || is.null(maxid))) {
		stop("'maxid' must be of class 'integer' or 'numeric'")
	}
	
	# Cross-tables REST asks for no more than 1000 games at a time
	if (!(is.null(minid) && is.null(maxid))) {
		if (maxid - minid > 1000) {
			stop("No more than 1000 games at a time, please.")
		}
	}

	# Paste links together
	if (!is.null(id)) {
		suffix <- paste("?id=", as.integer(id), sep='')
	} else {
		suffix <- paste("?minid=", as.integer(minid), "&maxid=", as.integer(maxid), sep='')
	}
	link <- paste(prefix, suffix, sep='')
	cat(link, "\n")
	# Access API and convert to R object
	gamesList <- tryCatch(fromJSON(link, nullValue = ""),
		error = function(cond) {
			return("Connection Error. Retry.")
		},
		warning = function(cond) {
			return("Connection Error. Retry.")
		},
		finally = {
			
		}
	)

	# Return
	if (!asdf) invisible(gamesList)
	else {
		gamesDF <- as.data.frame(t(as.data.frame(gamesList)))
		rownames(gamesDF) <- 1:nrow(gamesDF)
		invisible(gamesDF)
	}
}

numTotGames <- getMaxGameID()
allgames <- NULL
start <- 1
while (start < numTotGames - 999) {
	 temp <- games(minid=start, maxid=start+999, asdf=TRUE)
	 allgames <- as.data.frame(rbind(allgames, temp))
	 start <- start + 999 + 1
}

save(allgames, file="~/Documents/Scrabble/Studies/RCrossTables/games.Rda")