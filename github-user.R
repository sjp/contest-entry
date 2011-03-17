## Simply initialises a data frame filled with data from "data.txt"
## userID : repoID
user.repo.data = read.table("data.txt", sep=":", col.names=c("userID", "repoID"), colClasses=c(integer(), integer()))

## Creates a vector full of users that require recommendation
user.recommendation = read.table("test.txt", col.names=c("userID"), colClasses=c(integer()))
listOfUsers = user.recommendation$userID


## This is the main processing method that does all of the recommendation work
## It requires a user to recommend for and a data frame of userIDs and repoIDs
userRecommendation = 	function(userID, data.df) {

							## BEGIN USER SIMILARITY WEIGHTING
							## **** IMPORTANT **** TWEAK WEIGHTING ON SIZE OF CANDIDATE USER SET
							## Tweaking variable = candidateSetSize
							candidateSetSize = 50

							# Obtains the repositories watched by the given user
							currentUserRepos = data.df[data.df$userID == userID, "repoID"]
							vectorOfUsers = integer()

							# See which users share the same repositories as the specified user
							for(repo in currentUserRepos) {
								vectorOfUsers = append(vectorOfUsers, data.df[data.df$repoID == repo, "userID"])
							}
							
							# Remove the current user from the selection of users as we do not want to
							# recommend to the same user
							vectorOfUsers = vectorOfUsers[vectorOfUsers != userID]
							uniqueCounts = integer()
							uniqueUsers = unique(vectorOfUsers)
							
							# See how many times a given user appears in the list we obtained from shared repositories
							# A user will appear each time a repository is shared between the specified user and a user in our dataset
							for(user in uniqueUsers) {
								uniqueCounts = append(uniqueCounts, length(vectorOfUsers[vectorOfUsers == user]))
							}

							# Store the userID and the association count in a data frame, descending sort on association count
							output.df = data.frame(userID = uniqueUsers, count = uniqueCounts)
							output.df = output.df[ rev(order(output.df$count)), ]
							# Grab the 50 most common users, these will be our candidate users that we'll grab
							# repository recommendations from
							candidateUsers = output.df[1:candidateSetSize, 1]
							candidateRepos = integer()

							# Grab all of the repositories that our candidate users watch
							for(user in candidateUsers) {
								candidateRepos = append(candidateRepos, data.df[data.df$userID == user, "repoID"])
							}

							# Select only repositories that our current user does *not* have
							candidateRepos = candidateRepos[!candidateRepos %in% currentUserRepos]
							uniqueCandidates = unique(candidateRepos)
							candidateCount = integer()
														
							# Count up the number of appearances for each repository
							for(repo in uniqueCandidates) {
								candidateCount = append(candidateCount, length(candidateRepos[candidateRepos == repo]))
							}
							
							# Store in a data from the candidate repositories and their frequency
							final.df = data.frame(repoID = uniqueCandidates, count = candidateCount)
							
							## END USER SIMILARITY WEIGHTING
														
							# Order the counts so that the most frequent appear at the top
							final.df = final.df[ rev(order(final.df$count)), ]
							
							##  **** FINAL PRINT OUT ****
							## Print out the repoIDs that we are going to recommend from smallest to largest
							sort(final.df[1:10, 1])
						}
						
## This function will be the main one we call to write to results.txt
## All it needs is a vector of users to recommend for
allRecommendations =	function(listOfUsers) {
							results = character()
							
							# Simply go through every user in the list and store the result
							# in recommendationTxt, it will be formatted nicely for file storage
							for(user in listOfUsers) {
								userRecommendations = userRecommendation(user, user.repo.data)
								recommendationTxt = paste(paste(user, ":", sep=""), paste(userRecommendations, sep=",", collapse=","), sep="")
								results = append(results, recommendationTxt)
							}							

							# Finally write out the results to results.txt in the format specified in readme.txt
							write(results, file="results.txt")
						}
