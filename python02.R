# ライブラリのインポート
library(tidyverse) # tidyなライブラリ
library(lubridate) # tidyな日付操作

# 記述統計のデータ

# 売上データの読み込みと変数名の変更
df <- readr::read_csv("http://www.chuokeizai.co.jp/acc/202007/uriage_sample.csv") %>% 
  dplyr::rename(
    shop = "店舗名",
    date = "日付",
    sale = "売上金額"
  )

# 先頭の5行を表示
head(df)
# データの型を確認
str(df)

# 日付を文字列から日付系に変換
df$date <- lubridate::parse_date_time2(df$date, orders='Y/m/d')

# 月と年を抽出し，集計する
df <- df %>% 
  dplyr::mutate(# 変数作成
    month = lubridate::month(date), # 月
    year = lubridate::year(date)    # 年
  ) 
# 年月ごとに売上合計を計算
df_month <- df %>% 
  dplyr::group_by(month, year) %>% 
  dplyr::summarise(monthly = sum(sale))

# 棒グラフの作図
mystyle <- list(
  theme_bw(base_family = "HiraMaruProN-W4")
)

g <- ggplot(df_month, aes(x=factor(month), y = monthly/1000000, group=factor(year), fill=factor(year))) 
g <- g + geom_bar(stat="identity", position = "dodge") # 横並び棒グラフ dodge
g <- g + scale_y_continuous(expand = c(0,0), limits = c(0,40)) # y軸下の余白を削除
g <- g + xlab("月") + ylab("月次売上高合計(単位：百万円)") + labs(fill = "年度") + mystyle # タイトル
plot(g)


# 基本関数でポワソン分布作成
k <- c(1:30)
lamb <- 10
pmf_pois <- dpois(k,lamb)
barplot(pmf_pois)

# ggplot2でポワソン分布作成
df <- as_tibble(k,pmf_pois) # データをtibbleに変換
ggplot(df,aes(y=pmf_pois, x=k)) + geom_bar(stat="identity") + mystyle

# 属性サンプリング

# 二項分布におけるサンプルサイズの算定関数の作成
sample_binom <- function(pt, alpha ,ke) {
  k = c(1:10) # 試行
  i = 1 #初期値
  while (dbinom(ke, i, pt) > alpha) {
    i <- i + 1
  }
  print(i)
}
# 数値の設定
pt = 0.1
alpha = 0.05
ke = 0
# 自作関数でサンプルサイズを計算 ここでは29を返す
sample_binom(pt, alpha, ke)


# 金額単位サンプリング
## データの読み込み
df_poi <- readr::read_csv("http://www.chuokeizai.co.jp/acc/202007/sample-receivable.csv",col_names=FALSE) %>% 
  dplyr::rename(
    name = X1,
    amount =X2
  )
# 型を確認
str(df_poi)

# 母集団の売上合計を計算
(N = sum(df_poi$amount))

# 関数作成
sample_poisson <- function(N, pm, ke, alpha) {
  k = ke # 逸脱数
  pt = pm / N # 期待逸脱率
  n = 1 # カウンター
  while (dpois(k, I(n*pt)) > alpha) { # 条件がFalseになるまでループ
    n = n + 1 
  }
  print(n)
}

pm <- 4*10^7 # 重要性の基準値
ke <- 0 # 予想虚偽表示金額
alpha = 0.05 # 信頼度

# サンプルサイズsizeの算出 ここでは 57 を返す
size <- sample_poisson(N, pm, ke, alpha)

# グラフで確認
df2 <- data.frame(
  n <- 0:100,
  pois <- dpois(0,I(n*pt)),
  cond <- ifelse(pois > 0.05, "insufficient", "sufficient")
)
ggplot(df2, aes(n,pois, group=cond, fill = cond)) + geom_bar(stat="identity") + geom_hline(yintercept = 0.05)


# サンプリング

# ランダムに並び替え
df_re <- df_poi[sample(nrow(df_poi)),]

#サンプリング区間の算定
m <-  N / size # 売上合計金額をサンプルサイズで割る.

# 売上高の累積和を列に追加
# 累積和をサンプリング区間で除することでグループ化
df_re <- df_re %>% 
  dplyr::mutate(
    cum_amount = cumsum(amount), # 累積和 cumsum
    group = cum_amount %/% m # グループ
  )
# グループの先頭データのみを抽出
df_sub <- df_re %>% 
  dplyr::group_by(group) %>% # groupごと
  dplyr::slice(which.min(cum_amount)) %>%  
  dplyr::select(-cum_amount)

# サンプリング後のデータdf_sub　が完成
