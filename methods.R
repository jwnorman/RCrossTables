require(RJSONIO)

# player.php
player <- function(playerID=NULL, naspaID=NULL, upcoming=FALSE, partial=FALSE, add=TRUE) {
	prefix <- "http://cross-tables.com/rest/player.php"
	if (is.null(playerID) && is.null(naspaID)) {
		stop("Either playerID or naspaID must be provided.")
	}
	if (!is.null(playerID)) {
		id <- paste("?player=", playerID, sep='')
	} else {
		id <- paste("?naspa=", naspaID, sep='')
	}
	link <- paste(prefix, id, sep='')
	if(add) {
		link <- paste(link, "&addresults=1", sep='')
	}
	if (upcoming) {
		link <- paste(link, "&upcoming=1", sep='')
	}
	if (partial) {
		link <- paste(link, "&partialresults=1", sep='')
	}
	p <- fromJSON(link)
	return(p)
}

# to do
# deal with partial, and upcoming
# deal with vector inputs (if playerID is "1024" versus "1:1024")
