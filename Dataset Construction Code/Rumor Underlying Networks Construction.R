# get the most recent tweets of the accumulated user IDs
#lookup_users(userIDs, parse = TRUE, token = NULL)
library(igraph)
library(httpuv)
library(rtweet)
library(readr)

#change paths according to Covid 19 and US Elections
file_list_IG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/")
UL_files <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Uninfected Lists/")
t=1
while(t<=121){ #121 is the number of networks for covid-19; for us elections 2020, it is 234
	print(paste('t is:',t))
	#uninfected_list = list()
	#node_list = list()
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/",file_list_IG[t],sep=''))
	load(file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Uninfected Lists/",UL_files[t+121],sep=''))	#to get uninfected node list (windows default sorting)
	load(file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Uninfected Lists/",UL_files[t],sep='')) #to get infected node list (windows default sorting)
	userIDs=unlist(node_list)
	ug=g
	print(paste('number of nodes:',length(userIDs)))
	k=1
	d=1

	while(k<=length(userIDs)){

		print(paste('k is:',k))
		
		retweeters1=uninfected_list[[k]]
		max_index=order(unlist(lapply(retweeters1,length)))[length(retweeters1)]
		max_index2=order(unlist(lapply(retweeters1,length)))[length(retweeters1)-1]
		print(retweeters1)
		print(order(unlist(lapply(retweeters1,length))))
		print(max_index)
		print(max_index2)
		q=1
		followers=c()
		#next line is the change 
		inter=intersect(as.character(retweeters1[[max_index]]),as.character(retweeters1[[max_index2]]))
		while(q<=length(retweeters1)) {
			if(q==max_index){
				q=q+1
				next
			}
			#followers=append(followers,intersect(as.character(retweeters1[[max_index]]),as.character(retweeters1[[q]])))
			followers=append(followers,intersect(as.character(inter),as.character(retweeters1[[q]])))
			q=q+1
		}
		nodes=V(g)$name
		followers=unique(followers)
		followers=setdiff(followers,intersect(nodes,followers))
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
		# #uninfected_list[[d]]=retweeters1
		#node_list[d]=userIDs[k]
		d=d+1
		k=k+1
		ug=simplify(ug, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))
		#timeline=NULL
		#retweeters1=NULL
	}
	save(ug,file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Underlying Networks/UG_",file_list_IG[t],'.RData',sep=''))	

	t=t+1
}

