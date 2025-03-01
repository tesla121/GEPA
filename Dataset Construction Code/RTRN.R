#code to check tweet return count and get graph information
library(httpuv)
library(rtweet)
library(readr)

app_name <- "shafaat app 2"
api_key <- "btE43oqjHVBJ0rLwK4X721GOR"
api_secret_key <- "o31n9h01hOXKo3s8geukgCq5z6hAKIt969p1axmoTBdk7Cl4C6"
access_token <- "47053762-jZ2t085JYpHvDNDLo5GgfVKzy7L2kZwXdY11e8QQ7"
access_token_secret <- "IbZpUEYrBNx5YgXiLINiGJQWCJC0QWaght8perzwFXZBJ"

# authenticate  
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

# RTRN count code
ds_gs1=read.table(file="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/retCount15_100_Dataset-I",header=T,sep='\t',numerals='no.loss')
return_count=c()
tweetID=as.character(ds_gs1[,1])
#tweetID='1224506134100230144'
i=1
rt=rate_limit()
reqs_left=as.numeric(rt[73,]$remaining)
while(i<=length(tweetID)){
	if(reqs_left==0){
		rt=rate_limit()
		req=as.numeric(rt[73,]$reset)
		reqs_left=as.numeric(rt[73,]$remaining)
		if(reqs_left==0){
			print(paste('reqs_left exhausted.'))
			print(paste("going to sleep for ",req+1,"minutes"))
			Sys.sleep((req+1)*60)
			rt=rate_limit()
			req=as.numeric(rt[73,]$reset)
			reqs_left=as.numeric(rt[73,]$remaining)
			print(paste('new time window started. reqs_left is',reqs_left,'and time left is',req))
		}
	}
	timestamps=c()
	userIDs=c()
	statusIDs=c()
	# quo_tweets <- search_tweets( # get quoted retweets
	#   tweetID, n = 1000, include_rts=T  # get n dynamically by checking the number of retweets on the original tweet
	# ) # 180 requests /15 minutes window
	tweet=lookup_statuses(tweetID[i], parse = TRUE, token = NULL) # get the original tweet // 900 requests /15 minutes window
	statusIDs=append(statusIDs,tweet$status_id)
	userIDs=append(userIDs,tweet$user_id[1])
	timestamps=append(timestamps,tweet$created_at[1])
	retweets=get_retweets(tweetID[i], n = 100, parse = TRUE, token = NULL) # at most 100 user IDs are returned by Twitter
	statusIDs=append(statusIDs,retweets$status_id)
	# 75 reqs /15 minutes window
	reqs_left=reqs_left-1
	retweeters=c()
	retweeters=append(retweeters,tweet$screen_name)
	retweeters=append(retweeters,retweets$screen_name) # get the retweeters screen name for friendship comparison later
	#retweeters=append(retweeters,quo_tweets$screen_name)
	timestamps=append(timestamps,retweets$created_at)	# as.list(retweets$created_at))
	#timestamps=append(timestamps,as.list(quo_tweets$created_at)) # make timestamps list of all the retweets
	userIDs=unlist(append(userIDs,as.list(retweets$user_id)))
	return_count=append(return_count,length(retweeters))

	#userIDs=unlist(append(userIDs,as.list(quo_tweets$user_id))) # make user ID list of all the retweeters
	# retweeters, userIDs and timestamps contain the information of the original tweet/tweeter and the retweets/retweeters

	# check for redundancy in userIDs 
	# this redundancy might arise from using both the normal retweets function and quoted retweets function
	# remove this redundancy along with the corresponding timestamp value
	# for timestamps based second phase graph construction
	df=NULL
	df=as.data.frame(userIDs)
	df=as.data.frame(cbind(df,statusIDs))
	df=as.data.frame(cbind(df,retweeters))
	df=as.data.frame(cbind(df,timestamps))
	print(i)
	print(head(df))
	print(paste('retweets returned:',length(retweeters)))
	# library(tidyverse)
	# dup=df[,1][duplicated(df[,1])] #check redundancy
	# remove=max(which(df[,1]==as.character(dup)))
	# df1=df[-remove,] # remove redundancy
	write.table(df,paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/GraphInfo",i,tweetID[i],'.txt'),sep='\t',row.names=F)
	i=i+1
}
data=read.table(file="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/retCount15_100_Dataset-I", sep='\t', header = T, numerals='no.loss')
df=as.data.frame(data)
df=as.data.frame(cbind(df,return_count))
df[,1]=as.character(df[,1])
write.table(df,"./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/RTRN_retCount15_100_Dataset-I",sep='\t',row.names=F)
