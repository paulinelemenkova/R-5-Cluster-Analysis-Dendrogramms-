# QQ t.test
# шаг-1. вчитываем таблицу. делаем из нее датафрейм.
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")

# шаг-2. чистим датафрейм от NA значений
df<- na.omit(MDepths) 
row.has.na <- apply(df, 1, function(x){any(is.na(x))}) # проверяем, удалил ли все NA
sum(row.has.na) # суммируем все NA, должно получиться: [1] 0
head(df) # смотрим очищенный датафрейм. теперь с ним работаем.

#шаг-3. создаем ячейки датафрейма, вчитываем туда столбцы-колонки из таблицы
nr_point<- as.numeric(df$observ)
profile1 <- as.numeric(df$profile1)
profile2 <- as.numeric(df$profile2)
profile3 <- as.numeric(df$profile3)
profile4 <- as.numeric(df$profile4)
profile5 <- as.numeric(df$profile5)
profile6 <- as.numeric(df$profile6)
profile7 <- as.numeric(df$profile7)
profile8 <- as.numeric(df$profile8)
profile9 <- as.numeric(df$profile9)
profile10 <- as.numeric(df$profile10)
profile11 <- as.numeric(df$profile11)
profile12 <- as.numeric(df$profile12)
profile13 <- as.numeric(df$profile13)
profile14 <- as.numeric(df$profile14)
profile15 <- as.numeric(df$profile15)
profile16 <- as.numeric(df$profile16)
profile17 <- as.numeric(df$profile17)
profile18 <- as.numeric(df$profile18)
profile19 <- as.numeric(df$profile19)
profile20 <- as.numeric(df$profile20)
profile21 <- as.numeric(df$profile21)
profile22 <- as.numeric(df$profile22)
profile23 <- as.numeric(df$profile23)
profile24 <- as.numeric(df$profile24)
profile25 <- as.numeric(df$profile25)

# шаг-4. формируем датафрейм
MDF<- data.frame(nr_point, profile1, profile2, profile3, profile4, profile5, profile6, profile7, profile8, profile9, profile10, profile11, profile12, profile13, profile14, profile15, profile16, profile17, profile18, profile19, profile20, profile21, profile22, profile23, profile24, profile25)
head(MDF)

############################## ДЕНДРОГРАММЫ, Иерархический кластерный анализ

library(dendextend)
# шаг-1. строим 1-ю дендрограмму (здесь: по 25 кластерам)
dend <- MDF[1:25,] %>%  scale %>% dist %>% # calculate a distance matrix, 
	hclust (method = "average") %>% 
	as.dendrogram %>% 
	set("labels", c("profile1", "profile2", "profile3", "profile4", "profile5", "profile6", "profile7", "profile8", "profile9", "profile10","profile11", "profile12", "profile13", "profile14", "profile15", "profile16", "profile17", "profile18", "profile19", "profile20","profile21", "profile22", "profile23", "profile24", "profile25")) %>%
	set("labels_col","blue") %>% set("labels_cex", c(.7)) %>%
	set("branches_k_color", k=5) %>% set("branches_lwd", 1) %>% 	
	set("nodes_pch", 19) %>%  set("nodes_cex", 1) %>%
	set("nodes_col", "plum1") %>%
	set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red")) 
dend %>% plot(main = "Mariana Trench: \nCluster Analysis Dendrogramm-1 of the Bathymetric Profiles \nUnsorted Dendrogramm")
#dend %>% rect.dendrogram(k = 4, border = 8, lty = 5, lwd = 2) %>% plot(horiz = TRUE, main = "Mariana Trench: \nCluster Analysis Dendrogramm-1 of the Bathymetric Profiles \nUnsorted Dendrogramm")


# шаг-2. сортируем 1-ю дендрограмму по величине кластеров, создаем из нее 2-ю
dend2 <- sort(dend)
dend2 %>%  set("branches_k_color", k=3) %>% set("branches_lwd", 1) %>%    	
	set("labels_col","blue") %>% set("labels_cex", c(.7)) %>%
	set("branches_k_color", k=5) %>% set("branches_lwd", 1) %>% 	
	set("nodes_pch", 19) %>%  set("nodes_cex", 1) %>%
	set("nodes_col", "plum1") %>% 	
	set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
dend2 %>% plot(main = "Mariana Trench: \nCluster Analysis Dendrogramm-2 of the Bathymetric Profiles \nSorted Dendrogramm")

# шаг-3. сравниваем 2-ю дендрограмму с 1-й (отсортированную с первичной)
tanglegram(dend, dend2)
tanglegram(dend, dend2) %>% plot(main = "Mariana Trench: \nComrapison of the Cluster Dendrogramms 1 and 2")

#шаг-4. Hierarchical Clustering with P-Values via Multiscale Bootstrap Resampling
data(MDF) 
set.seed(518) 
result <- pvclust(MDF, method.dist="cor", method.hclust="average", nboot=10)
# Default plot of the result 
plot(result, main = "Mariana Trench Bathymetric Profiles 1-25: \nHierarchical Clustering with P-Values (AU/BP, %) \nvia Multiscale Bootstrap Resampling")
pvrect(result)

# pvclust and dendextend - результаты в виде отсортированной дендрограммы
result %>% as.dendrogram %>% 
	set("branches_k_color", k = 5, value = c("purple", "orange", "cyan1", "firebrick1", "springgreen")) %>%
 	plot(main = "Mariana Trench Bathymetric Profiles 1-25: Cluster Dendrogram\nwith AU/BP Values (%). nAU: Approximately Unbiased p-Value \n and BP: Bootstrap Probability")
result %>% text 
result %>% pvrect

###################### конец. результаты можно сохранять как pdf через "Save As"

### дополнительно
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
