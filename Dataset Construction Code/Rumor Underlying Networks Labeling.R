#code to label underlying networks
library(igraph)
file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Underlying Networks/")
file_list_LIG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/")
t=1
while(t<=length(file_list_UG)){
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/",file_list_LIG[t],sep=''))
	load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Rumor Underlying Networks/",file_list_UG[t],sep=''))
	#print(g)
	ug=simplify(ug, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))
	p=1
	while(p<=length(V(g))){
		V(ug)[which(V(ug)$name==V(g)[p]$name)]$label=as.numeric(p)
		#V(g)[p]$label=p
		p=p+1
	}
	while(p<=length(V(ug))){
		V(ug)[p]$label=as.numeric(p)
		p=p+1
	}
	save(ug,file=paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks/Labeled_",file_list_UG[t],'.RData',sep=''))	
	t=t+1
}
