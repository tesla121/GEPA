#code to get uninfected lists
# get the most recent tweets of the accumulated user IDs
#lookup_users(userIDs, parse = TRUE, token = NULL)
library(igraph)
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


file_list_IG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/")
t=1
while(t<=121){ #121 for covid-19 and 234 for us elections 2020
	print(paste('t is:',t))
	uninfected_list = list()
	node_list = list()
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/",file_list_IG[t],sep=''))
	userIDs=V(g)$name
	ug=g
	print(paste('number of nodes:',length(userIDs)))
	k=1
	d=1
	while(k<=length(userIDs)){
		print(paste('k is:',k))
		x=tryCatch({
			timeline=get_timelines(userIDs[k], n = 500, max_id = NULL, home = FALSE, parse = TRUE, check = TRUE, token = NULL) # n max is 3200
			},
			error=function(err){
				message(paste('error occured in k:',err))
				y=1
				while(y<=10){
					alarm()
					Sys.sleep(1)
					y=y+1
				}
				input=readline(prompt = 'press any key to continue : ')
				token <- create_token(
					app = app_name,
					consumer_key = api_key,
					consumer_secret = api_secret_key,
					access_token = access_token,
					access_secret = access_token_secret)
				return ("CNCTN_BRK")
			},
			warning=function(warn){
				message(paste('warning occured:',(warn)))
				#alarm()
				#input=readline(prompt = 'press any key to continue : ')
				return ("PDNE")
			}
		)
		if(class(x)[1]!='tbl_df'){
			if(x=="PDNE"){
				print('page not found. moving to next k')
				k=k+1
				timeline=NULL
				next
			}
			else{
				next
			}
		}
		# 900 requests /15 minutes window
		#timeline1=timeline
		#print('..')
		timeline=as.data.frame(timeline)
		delete=c()
		delete=which(timeline$is_retweet==T)
		delete=append(delete,which(!is.na(timeline$reply_to_user_id)))
		timeline=timeline[-delete,]
		# this overall process will get us no-reply and no-retweeted tweets from a user, but there will be quoted retweets
		# now extract the status IDs and find all the retweeters of these status IDs (perhaps top/recent 10 or 20)
		# get the number of unioned or intersected users
		# this will give us number of uninfected nodes of a user
		if(nrow(timeline)>=25){
			statusIDs=timeline$status_id[1:25]
		}
		else{
			statusIDs=timeline$status_id
		}
		if(length(statusIDs)==0){
			print('no status IDs found. moving to next k')
			k=k+1
			timeline=NULL
			next
		}
		i=1
		p=1
		retweeters1=list()
		
		while(1){
			w=tryCatch({
				rt=rate_limit()},
				error=function(err){
					message(paste('error occured in rate_limit:',err))
					y=1
					while(y<=10){
						alarm()
						Sys.sleep(1)
						y=y+1
					}
					input=readline(prompt = 'press any key to continue : ')
					token <- create_token(
						app = app_name,
						consumer_key = api_key,
						consumer_secret = api_secret_key,
						access_token = access_token,
						access_secret = access_token_secret)
					return ("CNCTN_BRK")
			},
				warning=function(warn){
					message(paste('warning occured:',(warn)))
					#alarm()
					#input=readline(prompt = 'press any key to continue : ')
					return ("PDNE")
				}
			)
			if(class(w)[1]!='tbl_df'){
				if(w=="CNCTN_BRK"){
					print('rate_limit error occured')
					next
				}
				else{
					print('rate_limit warning occured')
					next
				}
			}
			else{
				break
			}
		}

		reqs_left=as.numeric(rt[67,]$remaining)
		while(i<=length(statusIDs)){
			tryCatch({
			if(reqs_left==0){
				rt=rate_limit()
				req=as.numeric(rt[67,]$reset)
				reqs_left=as.numeric(rt[67,]$remaining)
				if(reqs_left==0){
					print(paste('reqs_left exhausted.'))
					print(paste("going to sleep for ",req+1,"minutes"))
					Sys.sleep((req+1)*60)
					rt=rate_limit()
					req=as.numeric(rt[67,]$reset)
					reqs_left=as.numeric(rt[67,]$remaining)
					print(paste('new time window started. reqs_left is',reqs_left,'and time left is',req))
				}
			}
			#retweets1=get_retweets(statusIDs[i], n = 100, parse = TRUE, token = NULL) # at most 100 user IDs are returned by Twitter
			# 75 reqs /15 minutes window
			#retweeters[i]=retweets1$screen_name # get the retweeters screen name for friendship comparison later on
			
			test4zero=unlist(as.list(get_retweeters(statusIDs[i], n = 100, parse = TRUE, token = NULL))[1])
			reqs_left=reqs_left-1
			if(length(test4zero)<1){
				i=i+1
				next
			}
			retweeters1[[p]]=test4zero
			#print(retweeters1[[p]])
			p=p+1
			# at most 100 user IDs are returned by Twitter
			# 75 reqs /15 minutes window
			#print(i)
			i=i+1
			test4zero=NULL
			},
			error=function(err){
				message(paste('error occured in i/rate_limit:',err))
				y=1
				while(y<=60){
					alarm()
					Sys.sleep(1)
					y=y+1
				}
				input=readline(prompt = 'press any key to continue : ')
				token <- create_token(
					app = app_name,
					consumer_key = api_key,
					consumer_secret = api_secret_key,
					access_token = access_token,
					access_secret = access_token_secret)
			},
			warning=function(warn){
				message(paste('warning occured:',(warn)))
			}
		)
		}
		if(length(retweeters1)<=1){
			print('number of retweeters list is less than or equal to 1. moving to next k')
			k=k+1
			timeline=NULL
			retweeters1=NULL
			next
		}
		max_index=order(unlist(lapply(retweeters1,length)))[length(retweeters1)]

		q=1
		followers=c()
		while(q<=length(retweeters1)) {
			if(q==max_index){
				q=q+1
				next
			}
			followers=append(followers,intersect(as.character(retweeters1[[max_index]]),as.character(retweeters1[[q]])))
			q=q+1
		}
		followers=unique(followers)
		followers=setdiff(followers,intersect(userIDs,followers))
		followers1=setdiff(followers,V(ug)$name)
		ug <- ug + vertices(followers1)
		u=1
		while(u<=length(followers)){
			ug <- ug + edges(userIDs[k], followers[u])
			u=u+1
		}
		# Isolated = which(degree(G)==0)
		# G2 = delete.vertices(G, Isolated)
		print(paste('graph is: ',file_list_IG[t]))
		print(paste('user ID is: ',userIDs[k]))
		print(paste('number of followers are', length(followers)))
		print(paste('Infection graph node count',length(V(g))))
		print(paste('Infection graph edge count',length(E(g))))
		print(paste('Underlying graph node count',length(V(ug))))
		print(paste('Underlying graph edge count',length(E(ug))))
		print(paste('Underlying graph components',components(ug)$no))
		uninfected_list[[d]]=retweeters1
		node_list[d]=userIDs[k]
		d=d+1
		k=k+1
		ug=simplify(ug, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))
		timeline=NULL
		retweeters1=NULL
	}
	save(uninfected_list,file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Uninfected Lists/UL_",file_list_IG[t],'.RData',sep=''))
	save(node_list,file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Uninfected Lists/NL_",file_list_IG[t],'.RData',sep=''))
	alarm()
	t=t+1
}
