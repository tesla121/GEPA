#code to check whether retweet count is between 15 and 100
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


# check retweet count in range 20 and 100
ds_gs1=read.delim("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Dataset-I",header=T,sep='\t',dec = ".")
tweetID=as.character(ds_gs1[,1])

i=1
rt=rate_limit()
reqs_left=as.numeric(rt[76,]$remaining)
indices=c()
ret_count=c()
while(i<=length(tweetID)){
	if(reqs_left==0){
		rt=rate_limit()
		req=as.numeric(rt[76,]$reset)
		reqs_left=as.numeric(rt[76,]$remaining)
		if(reqs_left==0){
			print(paste('reqs_left exhausted.'))
			print(paste("going to sleep for ",req+1,"minutes"))
			Sys.sleep((req+1)*60)
			rt=rate_limit()
			req=as.numeric(rt[76,]$reset)
			reqs_left=as.numeric(rt[76,]$remaining)
			print(paste('new time window started. reqs_left is',reqs_left,'and time left is',req))
		}
	}
	print(paste("DS_GS, i is: ",i))
	tweet=as.data.frame(lookup_statuses(tweetID[i], parse = TRUE, token = NULL))
	reqs_left=reqs_left-1
	test4zero=tweet$retweet_count
	if(length(test4zero)<1){
		print("nothing returned")
		i=i+1
		next
	}
	if(test4zero>=15){ # &&test4zero<=110
		print("HIT")
		indices=append(indices,i)
		ret_count=append(ret_count,test4zero)
		print(paste(tweetID[i],' ',test4zero))
		print(paste("total hits: ",length(indices)))
		i=i+1
		next
	}
	print("MISS")
	print(paste(tweetID[i],' ',test4zero))
	i=i+1
}
ds_gs2=ds_gs1[indices,]
ds_gs2[,1]=as.character(ds_gs2[,1])
ds_gs2=cbind(ds_gs2,ret_count)
ds_gs2=cbind(ds_gs2,indices)
write.table(ds_gs2,"./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/retCount15_100_Dataset-I",sep='\t',row.names=F)


