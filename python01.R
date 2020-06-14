library(tidyverse)

set.seed(123) # 乱数を固定

# 初期値の設定
year <- 10  # 期間
sigma <- 0.3 # 標準偏差
mu = 0.045 # 平均
r = 0.0 # 割引率 ここでは思い切って0と仮定
samples = 100000# 試行回数 PCが非力なら少なめに


# シミュレーション
CFmat <- matrix(NA_real_,nrow=year,ncol=samples) # 空の10*100000行列
for (m in 1:year){ # シミュレーション
  for (n in 1:samples){
    CFmat[m,n] <- 100 * rlnorm(1,mu,sigma)
  }
}

# データをtidy dataに
CF <- CFmat %>% 
  as_tibble() %>%  # tibble化 
  dplyr::mutate(
    FY = c(1:10) # 年のデータを追加
  ) %>% # やたら遅い?
  tidyr::pivot_longer(cols=-FY, names_to = "trial", values_to ="amount")# ワイドをロング

# 図の見た目を設定
mystyle <- list(
theme_calc(base_family = "HiraMaruProN-W4") # mac用
)

# 試行1回目のCF
CF_V1 <- CF %>% 
  dplyr::filter(trial == "V1") # 第1回試行を抽出
g <- ggplot(CF_V1, aes(x=FY, y=amount)) + geom_bar(stat = "identity",fill="lightblue", colour = "blue")
g <- g + ylab("1st trial's Cash Flow") + xlab("year") + mystyle
print(g)

# ヒストグラム
CF_FY10 <- CF %>% dplyr::filter(FY==10) # 10期の結果10万個のヒストグラム
g2 <- ggplot(CF_FY10, aes(amount)) + geom_histogram(fill="lightblue", colour = "blue") 
g2 <- g2 + ylab("FY10's CF") + xlab("year") + mystyle
print(g2)

# 割引因子の作成
DF <- rep(1,10) # 1を10個
for (t in 1:year) {
  DF[t+1] <- DF[t]*exp(-0.05)
}

# 10年分のCFを割引現在価値に
CF$trial <- as.factor(CF$trial) 
df_NPV <- CF %>% 
  dplyr::group_by(trial) %>% # 試行ごとに以下を計算
  dplyr::mutate(DCF = amount * DF[2:11]) %>%  # 金額にディスカウント・ファクターを乗じる 
  dplyr::summarise(NPV = sum(DCF)) # 試行ごとにDCFを合計しNPVを計算

# 10万回の試行で作られたNPVのヒストグラム
xinter <- sum(100*DF[2:11])
g3 <- ggplot(df_NPV, aes(NPV)) + geom_histogram(binwidth=25,fill="lightblue", colour = "blue")
g3 <- g3 + ylab("Simulated NPV") + mystyle
g3 <- g3 + geom_vline(xintercept=sum(100*DF[2:11]), colour="red", size=1)
print(g3)


# 年ごとの分布
library("ggridges")
g <- ggplot(CF,aes(x=amount, y = FY, fill=FY))
g <- g + geom_density_ridges(aes(point_color=FY, point_fill=FY), alpha = 0.3)
g4 <- g   + coord_flip() + mystyle + theme(legend.position = 'none')
print(g4)

