file_list_gi <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Graph Info/")
file_list_pgi = list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Primary Graph Info/")
file_list_sgi = list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Secondary Graph Info/")
t=1
conn=0
N_conn=0
IG_stats <- data.frame(GraphName=character(),
                 n_nodes=numeric(), 
                 n_TS_edges=numeric(),
                 n_netw_edges=numeric(),
                 n_comps=numeric(), 
                 n_isolated=numeric(),
                 stringsAsFactors=FALSE) 
while(t<=length(file_list_pgi)){
	print(paste('t is: ',t))
	gi=read.table(file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Graph Info/",file_list_gi[t],sep=''), sep='\t', header = T, numerals='no.loss')
	pgi=read.table(file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Primary Graph Info/",file_list_pgi[t],sep=''), sep='\t', header = T, numerals='no.loss')
	sgi=read.table(file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Secondary Graph Info/",file_list_sgi[t],sep=''), sep='\t', header = T, numerals='no.loss')

	gi[,1]=as.character(gi[,1])
	gi[,2]=as.character(gi[,2])
	gi[,3]=as.character(gi[,3])
	gi[,4]=as.character(gi[,4])
	pgi[,1]=as.character(pgi[,1])
	pgi[,2]=as.character(pgi[,2])
	sgi[,2]=as.character(sgi[,2])
	sgi[,1]=as.character(sgi[,1])

	df=as.data.frame(pgi[,1:2])
	library(igraph)
	g=graph_from_data_frame(df[,1:2], directed = F, vertices = NULL)
	g <- g + vertex(sgi[,1])
	comp=components(g)$no
	IG_stats=as.data.frame(rbind(IG_stats,c(file_list_gi[t],length(V(g)),comp-1,length(E(g)),comp,nrow(sgi))))
	print(paste('edges in original graph',length(E(g))))
	print(paste('components are:',comp))
	if(comp==1){
		print("already connected")
		conn=conn+1
	}
	else{
		print("not connected")
		N_conn=N_conn+1
	}
	while(comp!=1){
		print('*******************')
		comp_nos=as.numeric(components(g)$membership)
		userIDs=attributes(components(g)$membership)$names
		comp_userIDs=list()
		max=max(comp_nos)
		i=1
		while(i<=max){
			comp_userIDs[[i]]=userIDs[which(comp_nos==i)]
			i=i+1
		}
		comp_userIDs_TS=list()
		i=1
		while(i<=max){
			#comp_userIDs_TS[[i]]=gi[which(gi$userIDs==comp_userIDs[[i]]),4]
			j=1
			comp_userIDs_TS[[i]]=rep(0,length(comp_userIDs[[i]])) 
			while(j<=length(comp_userIDs[[i]])){
				comp_userIDs_TS[[i]][j]=gi[which(gi$userIDs==comp_userIDs[[i]][j])[1],4]
				j=j+1
			}
			i=i+1
		}
		k=1
		mini_comp_TS=c()
		mini_comp_TS_index=c()
		while(k<=length(comp_userIDs_TS)){
			temp=gsub("[: -]", "" , comp_userIDs_TS[[k]], perl=TRUE)
			mini_comp_TS_index=append(mini_comp_TS_index,which(temp==min(temp))[1])
			mini_comp_TS=append(mini_comp_TS,comp_userIDs_TS[[k]][which(temp==min(temp))[1]])
			k=k+1
		}
		sorted_comp_index=order(mini_comp_TS)
		ind=which(mini_comp_TS==min(mini_comp_TS))
		for(p in sorted_comp_index[2]){
			uID1=comp_userIDs[[p]][mini_comp_TS_index[p]]
			time1=mini_comp_TS[p]
			timediffs=abs(as.numeric(difftime(time1,comp_userIDs_TS[[ind]],units = "secs")))
			index=which(timediffs==min(timediffs))
			uID_main=comp_userIDs[[ind]][index]
			g <- g + edge(uID_main[1], uID1)
			comp=components(g)$no
		}
	}
	#plot(g)
	print(components(g))
	print(paste('edges in connected graph',length(E(g))))
	save(g,file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/IG_",file_list_gi[t],'.RData',sep=''))
	print(paste('total connected: ',conn, 'total disconnected:',N_conn))
	#input=readline(prompt = 'press any key to continue : ')
	t=t+1
}
colnames(IG_stats)=c('Graph_Name','n_nodes','n_TS_edges','n_netw_edges','n_comps','n_isolated')
write.csv(IG_stats,'./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Cov_Infection Graph Stats.csv')
