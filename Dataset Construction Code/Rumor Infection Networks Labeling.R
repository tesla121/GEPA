#code to label nodes in networks
library(igraph)
file_list_IG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/")
t=1
while(t<=length(file_list_IG)){
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Infection Networks/",file_list_IG[t],sep=''))
	#print(g)
	p=1
	while(p<=length(V(g))){
		V(g)[p]$label=as.numeric(p)
		p=p+1
	}
	save(g,file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/Labeled_",file_list_IG[t],'.RData',sep=''))	
	t=t+1
}
