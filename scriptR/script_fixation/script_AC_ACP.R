data1 <- read_csv("data/JAR_translate.csv")
data_finales <- read_csv("data/data_fix_finales/data_supp/data_finales_supp_classes.csv")
data_finales <- data_finales[1:402,]

data2 <- data_finales%>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("fix"), ends_with("tps")) %>% 
  dplyr::select(classe,id, pays,num_stimulus, starts_with("pct")) %>% 
  left_join(data1, by = c("id", "num_stimulus")) %>%
  mutate(stimulus_classe = paste(pays, "_", classe))# %>% 
#dplyr::select(id, classe, num_stimulus, pays)

data3 <- data2 %>% 
  dplyr::select(stimulus_classe, ends_with("fix")) %>% 
  group_by(stimulus_classe) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>%
  summarise(pct_legumes_fix = mean(pct_legumes_fix), pct_fruits_fix = mean(pct_fruits_fix),pct_proteines_fix = mean(pct_proteines_fix),pct_feculents_fix = mean(pct_feculents_fix)) %>% 
  data.frame()

data4 <- data_finales %>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("fix"),-starts_with("pct"), -cat_max_fix) %>% 
  mutate(stimulus_classe = paste(pays, "_", classe)) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>% 
  group_by(stimulus_classe) %>% 
  summarise(feculents_fix = sum(feculents_fix), legumes_fix = sum(legumes_fix), fruits_fix = sum(fruits_fix), proteines_fix = sum(proteines_fix)) %>% 
  column_to_rownames("stimulus_classe")

data5 <- data_finales %>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("fix"),-starts_with("pct"), -cat_max_fix) %>% 
  mutate(stimulus_classe = paste(pays, "_", classe)) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>% 
  group_by(classe) %>% 
  summarise(feculents_fix = sum(feculents_fix), legumes_fix = sum(legumes_fix), fruits_fix = sum(fruits_fix), proteines_fix = sum(proteines_fix)) %>% 
  column_to_rownames("classe")

data6 <- data_finales %>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("fix"),-starts_with("pct"), -cat_max_fix) %>% 
  mutate(stimulus_classe = paste(pays, "_", classe)) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>% 
  group_by(pays) %>% 
  summarise(feculents_fix = sum(feculents_fix), legumes_fix = sum(legumes_fix), fruits_fix = sum(fruits_fix), proteines_fix = sum(proteines_fix)) %>% 
  column_to_rownames("pays")

data_agreg <- rbind(data4, data5, data6)

res.des <- descfreq(data4)
res.CA <- CA(data_agreg,row.sup = 27:38, graph = FALSE )
plot.CA(res.CA, invisible = "row")
res.CA$eig

data7 <- data_finales %>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("tps")) %>% 
  dplyr::select(classe,id, pays, num_stimulus, starts_with("pct"),) %>% 
  mutate(stimulus_classe = paste(pays, "_", classe)) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>% 
  group_by(stimulus_classe) %>% 
  summarise(pct_feculents_tps = sum(pct_feculents_tps), pct_legumes_tps = sum(pct_legumes_tps), pct_fruits_tps = sum(pct_fruits_tps), pct_proteines_tps = sum(pct_proteines_tps)) %>% 
  column_to_rownames("stimulus_classe")

data8 <- data_finales %>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("tps")) %>% 
  dplyr::select(classe,id, pays, num_stimulus, starts_with("pct"),) %>% 
  mutate(stimulus_classe = paste(pays, "_", classe)) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>% 
  group_by(classe) %>% 
  summarise(pct_feculents_tps = sum(pct_feculents_tps), pct_legumes_tps = sum(pct_legumes_tps), pct_fruits_tps = sum(pct_fruits_tps), pct_proteines_tps = sum(pct_proteines_tps)) %>% 
  column_to_rownames("classe")

data9 <- data_finales %>% 
  dplyr::select(classe,id, pays, num_stimulus,ends_with("tps")) %>% 
  dplyr::select(classe,id, pays, num_stimulus, starts_with("pct"),) %>% 
  mutate(stimulus_classe = paste(pays, "_", classe)) %>%
  mutate(stimulus_classe = as.factor(stimulus_classe)) %>% 
  group_by(pays) %>% 
  summarise(pct_feculents_tps = sum(pct_feculents_tps), pct_legumes_tps = sum(pct_legumes_tps), pct_fruits_tps = sum(pct_fruits_tps), pct_proteines_tps = sum(pct_proteines_tps)) %>% 
  column_to_rownames("pays")

data_agreg2 <- rbind(data7, data8, data9)

res.CA <- CA(data_agreg2,row.sup = 27:38, graph = FALSE )
plot.CA(res.CA, invisible = "row")
res.CA$eig

catdes(data3, num.var = 1)
colnames(data3)

res.pca <- PCA(data_agreg2, ind.sup = 27:38 )
plot.PCA(res.pca, invisible = "ind")  

cor(data_finales$feculents_fix,data_finales$feculents_tps)
cor(data_finales$fruits_fix,data_finales$fruits_tps)
cor(data_finales$legumes_fix,data_finales$legumes_tps)
cor(data_finales$proteines_fix,data_finales$proteines_tps)
