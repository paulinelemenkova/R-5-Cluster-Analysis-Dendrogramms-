# ЧАСТЬ 1: делаем data.frame
	# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")

	# шаг-2. чистим датафрейм от NA значений
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(MDF) # смотрим очищенный датафрейм. теперь с ним работаем.

# ЧАСТЬ-2. Иерархический кластерный анализ, дендрограмма.
library(dendextend)
	# шаг-3. строим 1-ю дендрограмму (здесь: по 25 кластерам)
dend <- MDF[1:25,] %>%  scale %>% dist %>% # calculate a distance matrix, 
	hclust (method = "average") %>% 
	as.dendrogram %>% 
	set("labels", c(("profile"), rep(1:25), sep="")) %>%
	set("labels_col","blue") %>% set("labels_cex", c(.7)) %>%
	set("branches_k_color", k=5) %>% set("branches_lwd", 1) %>% 	
	set("nodes_pch", 19) %>%  set("nodes_cex", 1) %>%
	set("nodes_col", "plum1") %>%
	set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red")) 
dend %>% plot(main = "Mariana Trench: \nCluster Analysis Dendrogramm-1 of the Bathymetric Profiles \nUnsorted Dendrogramm")

	# шаг-4. создаем 2-ю дендрограмму из 1-й, отсортированной по величине кластеров 
dend2 <- sort(dend)
dend2 %>%  set("branches_k_color", k=3) %>% set("branches_lwd", 1) %>%    	
	set("labels_col","blue") %>% set("labels_cex", c(.7)) %>%
	set("branches_k_color", k=5) %>% set("branches_lwd", 1) %>% 	
	set("nodes_pch", 19) %>%  set("nodes_cex", 1) %>%
	set("nodes_col", "plum1") %>% 	
	set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
dend2 %>% plot(main = "Mariana Trench: \nCluster Analysis Dendrogramm-2 of the Bathymetric Profiles \nSorted Dendrogramm")

	# шаг-5. сравниваем две дендрограммы (отсортированную с неотсортированной)
tanglegram(dend, dend2)
tanglegram(dend, dend2) %>% plot(main = "Mariana Trench: \nComrapison of the Cluster Dendrogramms 1 and 2")

	# шаг-6. Hierarchical Clustering with P-Values via Multiscale Bootstrap Resampling
data(MDF) 
set.seed(518) 
result <- pvclust(MDF, method.dist="cor", method.hclust="average", nboot=10)
# Default plot of the result 
plot(result, main = "Mariana Trench Bathymetric Profiles 1-25: \nHierarchical Clustering with P-Values (AU/BP, %) \nvia Multiscale Bootstrap Resampling")
pvrect(result)

	# шаг-7. pvclust and dendextend - результаты в виде отсортированной дендрограммы
result %>% as.dendrogram %>% 
	set("branches_k_color", k = 5, value = c("purple", "orange", "cyan1", "firebrick1", "springgreen")) %>%
 	plot(main = "Mariana Trench Bathymetric Profiles 1-25: Cluster Dendrogram\nwith AU/BP Values (%). nAU: Approximately Unbiased p-Value \n and BP: Bootstrap Probability")
result %>% text 
result %>% pvrect
# конец. результаты можно сохранять как pdf через "Save As" (здесь: 5 картинок для каждого шага с 3-го по 7-й)

######################################## дополнительно
# можно узнать сколько элементов в каждом узле
dend %>% get_nodes_attr("members", id = c(2,5))

dend <- MDF[1:25,] %>%  scale %>% dist %>% 
	hclust %>% as.dendrogram
set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%   
set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>%     
set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red")) 
plot(dend)

# моя первая дендрограмма (через hclust: мало контроля, но работает.через dendextend лучше)
model <- hclust(dist(MDF), "ave")
dhc <- as.dendrogram(model)
ddata <- dendro_data(dhc, type = "rectangle")

pclusters <- ggplot(segment(ddata1)) +    
	geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +    
	coord_flip() +    
	scale_y_reverse(expand = c(0.2, 0)) 
pclusters # выдал по x -518 точек, по y - глубины