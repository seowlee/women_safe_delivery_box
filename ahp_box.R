# install.packages("ahpsurvey")
# install.packages("data.tree")
# install.packages("tidyr")
# install.packages("knitr")
library(ahpsurvey)
library(data.tree)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

# ================================================================
## 참조 https://cran.r-project.org/web/packages/ahpsurvey/vignettes/my-vignette.html

# input data
# 일인가구비_생활인구 <- c(2, 2, 2, 3, 1)
# 일인가구비_방범cctv <- c(2, 3, 3, 1, 1)
# 일인가구비_건물가점 <- c(4, 2, 3, 3, 3)
# 일인가구비_파출소거리 <- c(1, 1, 1, 1, 1)
# 일인가구비_백미터반경주택 <- c(1, 1, 1, 1, 1)
# 
# 생활인구_방범cctv <- c(2, 2, 3, 2, 1)
# 생활인구_건물가점 <- c(3, 3, 3, 3, 3)
# 생활인구_파출소거리 <- c(1, 1, 1, 1, 1)
# 생활인구_백미터반경주택 <- c(0.5, 0.5, 0.5, 0.5, 0.5)
# 
# 방범cctv_건물가점 <- c(3, 2, 4, 3, 3)
# 방범cctv_파출소거리 <- c(2, 1, 3, 2, 2)
# 방범cctv_백미터반경주택 <- c(0.5, 0.5, 0.5, 0.5, 0.5)
# 
# 건물가점_파출소거리 <- c(1, 1, 1, 1, 1)
# 건물가점_백미터반경주택 <- c(0.5, 0.5, 0.5, 0.5, 0.5)
# 
# 파출소거리_백미터반경주택 <- c(1, 1, 1, 1, 1)

# ======================
# input data
일인가구비_생활인구 <- c(-2, -2, -2, -3, -1)
일인가구비_방범cctv <- c(-3, -1, -3, -2, -1)
일인가구비_건물가점 <- c(-5, -4, -3, -5, -3)
일인가구비_파출소거리 <- c(2, 2, 3, 2, 3)
일인가구비_백미터반경주택 <- c(3, 2, 1, 2, 2)

생활인구_방범cctv <- c(-3, -2, 2, 2, 1)
생활인구_건물가점 <- c(3, -2, 3, -4, -3)
생활인구_파출소거리 <- c(3, 2, 4, 2, 3)
생활인구_백미터반경주택 <- c(6, 5, 4, 4, 5)

방범cctv_건물가점 <- c(-4, -3, -5, -3, -2)
방범cctv_파출소거리 <- c(3, 2, 3, 1, 2)
방범cctv_백미터반경주택 <- c(5, 4, 5, 4, 5)

건물가점_파출소거리 <- c(2, 3, 4, 2, 3)
건물가점_백미터반경주택 <- c(6, 5, 4, 5, 5)

파출소거리_백미터반경주택 <- c(3, 2, 2, 2, 2)

# to dataframe
df_box <- data.frame(일인가구비_생활인구, 일인가구비_방범cctv, 일인가구비_건물가점, 일인가구비_파출소거리, 일인가구비_백미터반경주택, 생활인구_방범cctv, 생활인구_건물가점, 생활인구_파출소거리, 생활인구_백미터반경주택, 방범cctv_건물가점, 방범cctv_파출소거리, 방범cctv_백미터반경주택, 건물가점_파출소거리, 건물가점_백미터반경주택, 파출소거리_백미터반경주택)

head(df_box)

atts <- c("일인가구비", "생활인구", "방범cctv", "건물가점", "파출소거리", "백미터반경주택")

dict <- c("일인가구비" = "1인가구비",
          "생활인구" = "생활인구", 
          "방범cctv" = "방범cctv", 
          "건물가점" = "건물가점", 
          "파출소거리" = "파출소거리", 
          "백미터반경주택" = "100m반경주택")

df_box %>% 
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  head(3)

box_ahp <- df_box %>%
  ahp.mat(atts, negconvert = T)
eigentrue <- ahp.indpref(box_ahp, atts, method = "eigen")
geom <- ahp.indpref(box_ahp, atts, method = "arithmetic")
error <- data.frame(id = 1:length(box_ahp), maxdiff = apply(abs(eigentrue - geom), 1, max))
error %>%
  ggplot(aes(x = id, y = maxdiff)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "gray50") +
  scale_x_continuous("Respondent ID") +
  scale_y_continuous("Maximum difference") +
  theme_minimal()

amean <- ahp.aggpref(box_ahp, atts, method = "arithmetic")
amean

mm <- ahp.aggpref(box_ahp, atts, method = "eigen")
mm

eigentrue
geom
# t() 가중치 table
mean <- df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.aggpref(atts, method = "arithmetic")

sd <- df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")

t(data.frame(mean, sd)) %>% kable()
write.csv(t(data.frame(mean, sd)), file = "C:/Users/user/big_project/atts_weight.csv")

emean <- df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.aggpref(atts, method = "eigen")

esd <- df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.aggpref(atts, method = "eigen", aggmethod = "sd")

t(data.frame(emean, esd)) %>% kable()
write.csv(t(data.frame(emean, esd)), file = "C:/Users/user/big_project/atts_weight_eigen.csv")

cr <- df_box %>%
  ahp.mat(atts, negconvert = TRUE) %>%
  ahp.cr(atts)
table(cr <= 0.1)

# 변수별 가중치 시각화
thres <- 0.1

cr.df <- df_box %>%
  ahp.mat(atts, negconvert = TRUE) %>%
  ahp.cr(atts) %>%
  data.frame() %>%
  mutate(rowid = 1:length(cr), cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)
cr.df

# eigen
df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.indpref(atts, method = "eigen") %>%
  mutate(rowid = 1:nrow(eigentrue)) %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(일인가구비, 생활인구, 방범cctv, 건물가점, 파출소거리, 백미터반경주택, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(df_box), ",", "Mean CR =",
                             round(mean(cr),3)))+
  theme_minimal()

# arithmetic
df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>%
  ahp.harker(atts, iterations = it, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
  ahp.indpref(atts, method = "arithmetic") %>%
  mutate(rowid = 1:nrow(eigentrue)) %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(일인가구비, 생활인구, 방범cctv, 건물가점, 파출소거리, 백미터반경주택, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (arithmetic)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(df_box), ",", "Mean CR =",
                             round(mean(crmat[,it]),3)))+
  theme_minimal()

crmat <- matrix(NA, nrow = 5, ncol = 11)
crmat
colnames(crmat) <- 0:10
crmat[,1] <- df_box %>%
  ahp.mat(atts, negconvert = TRUE) %>%
  ahp.cr(atts)
crmat[,1]
for (it in 1:10){
  crmat[,it+1] <- df_box %>%
    ahp.mat(atts, negconvert = TRUE) %>%
    ahp.harker(atts, iterations = it, stopcr = 0.1, 
               limit = T, round = T, printiter = F) %>%
    ahp.cr(atts)
}

data.frame(table(crmat[,1] <= 0.1), 
           table(crmat[,3] <= 0.1),
           table(crmat[,5] <= 0.1)) %>% 
  select(Var1, Freq, Freq.1, Freq.2) %>%
  rename("Consistent?" = "Var1", "No Iteration" = "Freq",
         "2 Iterations" = "Freq.1", "4 Iterations" = "Freq.2")
crmat %>% 
  as.data.frame() %>%
  gather(key = "iter", value = "cr", `0`, 1,2,3,4,5,6,7,8,9,10,11) %>%
  mutate(iter = as.integer(iter)) %>%
  ggplot(aes(x = iter, y = cr, group = iter)) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed")+
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "turquoise4") +
  geom_boxplot(fill = "transparent", color = "#808080", outlier.shape = NA) + 
  scale_x_continuous("Iterations", breaks = 0:10) +
  scale_y_continuous("Consistency Ratio") +
  theme_minimal()
crmat[, 1]


for (it in 1:10){
  print(crmat[,it])
  print(round(mean(crmat[,it]),3))
}

for (it in 1:10){
  print(crmat[,it])
  print(round(mean(crmat[,it]),3))
}

best_m <- df_box %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.harker(atts, iterations = it, stopcr = 0.1, limit = T, round = T, printiter = F) %>%
  ahp.aggpref(atts, method = "arithmetic") 
best_m
