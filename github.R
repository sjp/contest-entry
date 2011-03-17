## Simply initialises a data frame filled with data from "data.txt"
## userID : repoID
user.repo.data = read.table("data.txt", sep=":", col.names=c("userID", "repoID"), colClasses=c(integer(), integer()))

## Creates a vector full of users that require recommendation
user.recommendation = read.table("test.txt", col.names=c("userID"), colClasses=c(integer()))
listOfUsers = user.recommendation$userID



## Creates a data frame of repoIDs and the IDs that they were forked from
## repoID : forkedID
temp.repo.data = read.table(file="repos.txt", col.names=c("repoID", "forkedID"), colClasses=character(), sep=":")

## Grabs a string of repo information, instead of a factor
vectorOfForks = as.character(temp.repo.data$forkedID)

## Declaring variables required for our loop
forkRepoID = integer()
forkForkedID = integer()
iter = 1

## For each repository in repos.txt
for(repo in vectorOfForks) {
	
	# Collects the amount of times a comma appears in repo info
	amount = length(gregexpr(",", repo)[[1]])
		# When we have 3 commas, meaning we have a forked repository id available
	if(amount == 2) {
		# Split the string into three parts by commas
		splitString = strsplit(repo, ",")[[1]]
		
		# Store the associated repository ID for the appearance of the forked repository ID
		forkRepoID = append(forkRepoID, temp.repo.data$repoID[iter])
		
		# Store the forked repository ID
		forkForkedID = append(forkForkedID, as.integer(splitString[3]))
	}
	
	iter = iter + 1
}

## Finally store in a data frame all of the necessary information for forks
fork.data = data.frame(repoID=forkRepoID, forkedID=forkForkedID)

## END FORMAT FORK ASSOCIATION DATA


## Creates a data frame of repoIDs and the languages (and the amount of lines) they use
## repoID : language : lines
temp.lang.data = read.table(file="lang.txt", col.names=c("repoID", "langData"), colClasses=c(integer(), character()), sep=":")

lang.data = data.frame(repoID=integer(), language=character(), lines=numeric())
temp.langData = temp.lang.data$langData
temp.langData = as.character(temp.langData)
iter = 1

for(repo in temp.lang.data$repoID) {
	langPlusLines = as.character(strsplit(temp.langData[iter], ",")[[1]])
	
	for(language in langPlusLines) {
		finalLangLines = strsplit(language, ";")[[1]]
		
		combinedInsert = data.frame(repoID=repo, language=as.character(finalLangLines[1]), lines=as.numeric(finalLangLines[2]))
		
		lang.data = rbind(lang.data, combinedInsert)
	}
	
	iter = iter + 1
}

## Now we have a list in the form repoID : language : lines
## The number of lines needs to be transformed into a proportion
## and not an absolute number of lines. 1.0 for 100% of lines begin in that
## language, 0.15 for 15% in that language.

for(repo in unique(lang.data$repoID)) {
	df.Indices = which(lang.data$repoID == repo)
	lineSum = sum(lang.data$lines[df.Indices])
	
	for(i in 1:length(df.Indices)) {
		lang.data$lines[df.Indices[i]] = lang.data$lines[df.Indices[i]] / lineSum
	}
}

## Because not all repositories have associated language information
## we should average the language information over all repositories.
## The idea behind this is that the language association weighting would
## otherwise be zero if no information is available. In this case we
## compare to the average proportion of a language out of all repositories.
## This information will be stored in average.lang.set

average.lang.set = data.frame(language=character(), lines=numeric())

for (lang in unique(as.character(lang.data$language))) {

	# Extracting languages and lines where the language matches the language in the loop
	langlines.subset = lang.data[which(lang.data$language == lang), "lines"]
	lines.average = sum(langlines.subset) / length(langlines.subset)
	average.set = rbind(average.lang.set, data.frame(language=lang, lines=lines.average))
}

## END FORMAT LANGUAGE ASSOCIATION DATA




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
							
							## BEGIN REPOSITORY FORKING WEIGHTING (likely to be quite a low occurrence)
							## **** IMPORTANT **** TWEAK WEIGHTING ON THE FORK ASSOCIATION BY A DESIRED AMOUNT
							## Tweaking variable = forkAssociation
							forkAssociation = 5
							
							# Grab the repositories that the current user has
							currentUserRepos = data.df[data.df$userID == userID, "repoID"]

							# See which repositories that the user has, that have been forked
							matchingForkedIDs = intersect(currentUserRepos, fork.data$forkedID)
							
							# See where in the list these matches occur
							matchingIndices = which(matchingForkedIDs %in% fork.data$forkedID)
							
							# Store the repositories that have been forked from our user's repositories
							# and see whether any of them are shared with our candidate repositories
							matchingRepoIDs = fork.data$repoID[matchingIndices]
							matchingCandidateRepoIDs = intersect(uniqueCandidates, matchingRepoIDs)
							
							# If we do have a matching candidate with the set of forks
							# we want to multiply the count by our 'forkAssociation' variable
							# The size of the variable determines how much we value a fork
							for(repo in matchingCandidateRepoIDs) {
								final.df$count[which(final.df$repoID == repo)] = final.df$count[which(final.df$repoID == repo)] * forkAssociation
							}
							
							## END REPOSITORY FORKING WEIGHTING
							
							## BEGIN REPOSITORY LANGUAGE ASSOCIATION WEIGHTING
							
							## The only magic number is how highly we weight an association when multiplying
							## against the previous information.
							## Magic variable: tweakForkWeighting
							tweakForkWeighting = 5
							
							# Data frame to store user language information in
							currentUserLangs = data.frame(repoID=integer(), language=character(), lines=numeric())
							
							# Collecting the language data for the repositories that our user watches
							for(repo in currentUserRepos) {
								repo.subset = lang.data[which(lang.data$repoID == repo), c("language", "lines")]
								
								# If there is a matching subset, there exists the possibility that there is language info
								if(length(repo.subset$language) > 0) {
									for(i in 1:length(repo.subset$lines)) {
										currentUserLangs = rbind(currentUserLangs, data.frame(repoID=repo, language=as.character(repo.subset$language[i]), lines=repo.subset$lines[i]))
									}
								}
							}
							
							# Data frame to store *final* language proportion information in
							userLanguageWeightings = data.frame(language=character(), lines=integer())
							
							# Grouping by language and storing the average proportion of code for each language
							for(lang in as.character(unique(currentUserLangs$language))) {
								current.subset = currentUserLangs[which(currentUserLangs$language == lang), "lines"]
								
								subset.sum = sum(current.subset)
								subset.length = length(current.subset)
								calculatedProportion = subset.sum / subset.length
								
								# Appending the language to userLanguageWeightings
								userLanguageWeightings = rbind(userLanguageWeightings, data.frame(language=lang, lines=calculatedProportion))
							}
							
							# For each repository in final.df, we want to see how similar the repos are.
							# First we restrict the calculations only to languages that the user has in their set of repos
							# We then calculate the sum of absolute differences across languages.
							# This figure is then divided by the amount of languages.
							# This is our language association weighting
							for(repo in final.df$repoID) {
								
								# Collecting information on the languages that the candidate repository has
								repo.subset = lang.data[which(lang.data$repoID == repo), c("language", "lines")]
								
								# Seeing whether there are any shared languages between the main user and the candidate repository
								langAmount = length(intersect(lang.data$language, userLanguageWeightings$language))
								
								# Initialising the accumulator
								accumulator = 0
								
								# Test variable in case there is no information on 
								userHasNoLangs = !(length(userLanguageWeightings$lines) > 0)
								
								# We have a shared repository
								if(langAmount > 0) {
								
									# See which languages appear in both the user's set of repositories and the candidate repository, extract a subset
									matchingLanguages = intersect(lang.data$language, userLanguageWeightings$language)
									matching.subset = repo.subset[which(repo.subset$language %in% matchingLanguages), ]
									
									# For each shared language
									for(langMatch in matching.subset$language) {
										
										# Add the proportional difference to the accumulator
										userLangProportion = userLanguageWeightings$lines[which(userLanguageWeightings$language == langMatch)] 
										repoLangProportion = matching.subset$lines[which(matching.subset$language == langMatch)]
										absDiff = abs(userLangProportion - repoLangProportion)
										accumulator = accumulator + absDiff
									}
								}
								else {
									
									# If there is information on *any* language that a user has
									if(!userHasNoLangs) {
										
										# For each language that a user has in their repos
										for(langMatch in matching.subset$language) {
										
											# Add the proportional difference to the accumulator
											userLangProportion = userLanguageWeightings$lines[which(userLanguageWeightings$language == langMatch)] 
											avgLangProportion = average.lang.set$lines[which(average.lang.set$language == langMatch)]
											absDiff = abs(userLangProportion - avgLangProportion)
											accumulator = accumulator + absDiff
										}
									}
								}
								
								# Dividing the accumulator to account for the number of matching languages
								# The final calculated weighting should be diff's complement to 1	
								if(userHasNoLangs) {
									finalRepoLangWeighting = 1
								} else {
									nonzero = !(is.na(accumulator) || is.na(langAmount))
									
									diff = ifelse(nonzero, accumulator / langAmount, 1)
									
									# Rounding *up* to the nearest tenth
									truncated = as.integer(diff * 10)
									multiplied = diff * 10
									roundingOccurs = truncated != multiplied
	
									diff = ifelse(roundingOccurs, (truncated + 1) / 10, 1)
									
									finalRepoLangWeighting = 1 + (tweakForkWeighting * (1 - diff))
								}
								
								# Finally modify the weighting on the repository to account for the language weighting
								final.df$lines[which(final.df$repoID == repo)] = final.df$lines[which(final.df$repoID == repo)] * finalRepoLangWeighting
							}
							
							## END REPOSITORY LANGUAGE ASSOCIATION WEIGHTING
							
							# Order the counts so that the most frequent appear at the top
							final.df = final.df[ rev(order(final.df$count)), ]
							
							##  **** FINAL PRINT OUT ****
							## Print out the repoIDs that we are going to recommend from smallest to largest
							sort(final.df[1:10, 1])
						}
						
## This function will be the main one we call to write to results.txt
## All it needs is a vector of users to recommend for
allRecommendations =	function(listOfUsers) {
							library(multicore)

							results = character()
							
							# Simply go through every user in the list and store the result
							# in recommendationTxt, it will be formatted nicely for file storage
							parallelResultList = list()
							counterSet = c(1:5)
							for(fiveUsers in listOfUsers[counterSet]) {
								for(user in fiveUsers) {
									parallel(paste(paste(user, ":", sep=""), paste(userRecommendation(user, user.repo.data), sep=",", collapse=","), sep=""))
								}
								
								if(counterSet[5] + 5 > length(listOfUsers)) {
									exceedsBy = (counterSet[5] + 5) - length(listOfUsers)
									
								}
								else {
									counterSet = counterSet + 5
								}
							}
							parallelResultList = collect()

							for (user in parallelResultList) {
								results = append(results, user[1])
							}

							# Finally write out the results to results.txt in the format specified in readme.txt
							write(results, file="results.txt")
						}
