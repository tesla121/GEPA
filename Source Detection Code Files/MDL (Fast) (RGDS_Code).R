#Give paths to input underlying graphs and infection graphs
#Set the working directory

# Load rumor underlying network files (Comment/uncomment for Beta=3 and Beta=4)
# (uncomment line below to execute for Beta=3)
# file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=3)/")
# Beta = 4 (comment out line below to execute for Beta=3)
file_list_UG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=4)/")

# Load rumor infection network files (Common for both Beta=3 and Beta=4)
file_list_LIG <- list.files(path="./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/")

library(igraph)
library(RSpectra)

	mdl_FB_Ht_2=list() 
	mdl_sum=0
	i=1
	shift_list_estimated=c()
	while(i<=length(file_list_UG)){

		# Load rumor underlying network (Comment/uncomment for Beta=3 and Beta=4)
		# (uncomment line below to execute for Beta=3)
		# load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=3)/",file_list_UG[i],sep=''))
		# Beta = 4 (comment out line below to execute for Beta=3)
		load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Underlying Networks (Beta=4)/",file_list_UG[i],sep=''))

		# Load rumor infection network (Common for both Beta=3 and Beta=4)
		load(file = paste("./Datasets, Code and Results/Rumor Networks Datasets/Covid 19/Labeled Rumor Infection Networks/",file_list_LIG[i],sep=''))

		source=V(g)[1]$label
		sg=g
		graph=ug
		A=as.matrix(get.adjacency(graph)) 
		D=diag(rowSums(A))
		L=D-A
		Lm=L[c(V(g)),c(V(g))]
		e=eigs(Lm,1,which="SM")
		si=which(max(abs(e$vector))==abs(e$vector))		
		x=V(g)[si]
		estimated_mdl=V(graph)[x]$label
		d_mdl=distances(sg, v = (estimated_mdl), to = (source), mode = c("all"), weights=NULL, algorithm = c("unweighted"))

		samp=sample(length(d_mdl),1)
		mdl_sum=mdl_sum+as.numeric(d_mdl[samp])
		mdl_FB_Ht_2[[i]]=as.numeric(d_mdl[samp])
		
		i=i+1

		centers=names(which(min(sort((eccentricity(sg, vids = V(sg), mode = c("all")))))==sort((eccentricity(sg, vids = V(sg), mode = c("all"))))))
		est_source=dimnames(d_mdl)[[1]][samp]
		shift=distances(sg, v = as.character(centers), to = as.character(est_source) , mode = c("all"), weights=NULL, algorithm = c("unweighted"))
		
		shift_list_estimated=append(shift_list_estimated,ceiling(min(shift)))
	}



df=as.data.frame(cbind(file_list_UG,unlist(mdl_FB_Ht_2),shift_list_estimated))
colnames(df)=c('graph','MDL','shift_list_estimated')

# Save results for Beta=3
# write.csv(df,"./Covid 19 Results MDL (Beta=3).csv")

# Save results for Beta=4
write.csv(df,"./Covid 19 Results MDL (Beta=4).csv")

