library(httpuv)
library(rtweet)
library(readr)

# authenticate  
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)
# comparisons for primary graph construction (based on follower/friendship/followee relation in the underlying Twitter network)
file_list <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Graph Info/")
t=222
while(t<=121){ #121 for covid-19 and 234 for US Elections 2020
		print(paste('t is:',t))
		data=read.table(file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Graph Info/",file_list[t],sep=''), sep='\t', header = T, numerals='no.loss')
		retweeters=as.character(data$retweeters)
		userIDs=as.character(data$userIDs)
		#links=c()
		n=ceiling(length(retweeters)*(length(retweeters)-1))/2
		#print(paste('total comparisons:',n))
		remaining_sg_UN=c()
		remaining_sg_ID=c()
		sourceID=c()
		targetID=c()
		sourceUN=c()
		targetUN=c()
		noc=0
		misscount=0
		hitcount=0
		j=1
		while(j<=(length(retweeters)-1)){
			#print(paste('j is: ',j))
			i=1
			flag=F
			tryCatch({
			rt=rate_limit()
			reqs_left=as.numeric(rt[22,]$remaining)
			while(i<=(length(retweeters)-j)){
				tryCatch({
				if(reqs_left==0){
					rt=rate_limit()
					req=as.numeric(rt[22,]$reset)
					reqs_left=as.numeric(rt[22,]$remaining)
					if(reqs_left==0){
						print(paste('reqs_left exhausted.'))
						print(paste("going to sleep for ",req+1,"minutes"))
						Sys.sleep((req+1)*60)
						rt=rate_limit()
						req=as.numeric(rt[22,]$reset)
						reqs_left=as.numeric(rt[22,]$remaining)
						#print(paste('new time window started. reqs_left is',reqs_left,'and time left is',req))
					}
				}
				friendships=lookup_friendships(retweeters[j], retweeters[i+j], parse = TRUE, token = NULL) # check friendships between users
				# print(i)
				# print(j)
				# print(friendships)
				noc=noc+1
				# print(paste('number of comparisons made: ',noc))
				# print(paste('number of comparisons left',n-noc))
				reqs_left=reqs_left-1
			# 180 comparisons requests /15 minutes window
				fol=which(friendships$variable=='following')[1]
				folby=which(friendships$variable=='followed_by')[1]
				ToF=as.logical(friendships$value[fol]) || as.logical(friendships$value[folby])
				if(dim(friendships)[1]==0){
					ToF=F
				}
				flag=flag || ToF
				if(ToF==T){
					sourceID=append(sourceID,userIDs[j])
					targetID=append(targetID,userIDs[i+j])
					sourceUN=append(sourceUN,retweeters[j])
					targetUN=append(targetUN,retweeters[i+j])
					hitcount=hitcount+1
					#print(paste('number of hits:',hitcount))
					#links=append(links,ToF)
				}
				# i=4
				# l=c()
				# while(i<=length(friendships$value)){ # for preliminary graph construction
				# 	l=append(l,(as.logical(friendships$value[i]) || as.logical(friendships$value[i+1])))
				# 	i=i+12
				# }
				# print(unlist(lapply(split(l,f=l),length)))
				i=i+1
				},
				error=function(err){
					alarm()
					message(paste('error occured in i:',err))
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
					message(paste('warning occured:',warn))
					}
				)
			}
			if(flag==F){
				remaining_sg_UN=append(remaining_sg_UN,retweeters[j])
				remaining_sg_ID=append(remaining_sg_ID,userIDs[j])
				#print(paste('remaining nodes:',length(remaining_sg_ID)))
			}
			j=j+1
			print(paste('total comparisons:',n))
			print(paste('number of comparisons made: ',noc))
			print(paste('number of hits:',hitcount))
			print(file_list[t])
			},
			error=function(err){
				alarm()
				message(paste('error occured in j:',err))
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
				message(paste('warning occured:',warn))
				}
			)
			#print(paste('number of comparisons left',n-noc))
		}
		remaining_sg_UN=append(remaining_sg_UN,retweeters[j])
		remaining_sg_ID=append(remaining_sg_ID,userIDs[j])
		df=as.data.frame(sourceID)
		df=as.data.frame(cbind(df,targetID))
		df=as.data.frame(cbind(df,sourceUN))
		df=as.data.frame(cbind(df,targetUN))
		if(dim(df)[1]!=0){ # if some of the nodes of the graph get connected, do the following. 
			remaining_sg_UN=setdiff(remaining_sg_UN,intersect(as.character(remaining_sg_UN),as.character(union(df[,3],df[,4]))))
			remaining_sg_ID=setdiff(remaining_sg_ID,intersect(as.character(remaining_sg_ID),as.character(union(df[,1],df[,2]))))
			library(igraph)
			g=graph_from_data_frame(df[,1:2], directed = F, vertices = NULL)
			comp=components(g)$no
			print(paste('total components:',comp))
			plot(g)
			dev.off()
		} # else if no nodes get connected, graphs will not be saved
		df1=as.data.frame(remaining_sg_ID)
		df1=as.data.frame(cbind(df1,remaining_sg_UN))
		write.table(df,paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Primary Graph Info/Primary",file_list[t]),sep='\t',row.names=F)
		write.table(df1,paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Secondary Graph Info/Secondary",file_list[t]),sep='\t',row.names=F)
		t=t+1
#t=t+1
}
