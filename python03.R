# ライブラリのインポート
library(tidyverse)
library(lubridate) # tidyな日付操作
library(forecast) # 時系列分析
library(ggthemes) #  グラフのデザイン

# グラフとfontの指定
font <- "HiraMaruProN-W4" # for mac
mystyle <- list(
  theme_calc(base_family = font),
  theme(legend.position = 'none')
)

# 点過程データの読み込み
df_ten <- readr::read_csv("http://www.chuokeizai.co.jp/acc/202008/tenkatei.csv") 
# 作図 fig01
ggplot(df_ten,aes(x=time,y=count)) + geom_bar(stat="identity") + mystyle

# 時系列データの読み込み
df_ts <- readr::read_csv("http://www.chuokeizai.co.jp/acc/202008/demodata2.csv")
df_ts$time <- lubridate::parse_date_time2(df_ts$time, orders='Y/m/d')

# tsクラスに変更
y <- ts(df_ts$amount/10000, start = c(2015,4), frequency = 12 )

# 作図 fig02
autoplot(y) + xlab("年") + ylab("万円") + ggtitle("時系列データの例") + mystyle

# 作図 fig04
autoplot(y, series="Data") +
  autolayer(ma(y,12), series="12-MA") +
  xlab("年") + ylab("万円") + ggtitle("移動平均系列を重ねたグラフ") +
  scale_colour_manual(values=c("Data"="grey50","12-MA"="red"), breaks=c("Data","12-MA")) + mystyle

# 季節を考慮した作図 1
ggseasonplot(y, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("万円") + ggtitle("Seasonal plot") + theme_calc(base_family = font)
# 季節を考慮した作図 1
ggseasonplot(y, polar=TRUE) +
  ylab("万円") + ggtitle("Seasonal plot") + theme_calc(base_family = font)

# 分解
y %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") + ggtitle("時系列データの分解")

# 季節指数の作成 ここから急にdata.frame
df_mon <- df_ts %>% 
  dplyr::mutate(mon = month(time)) %>%  # 月
  dplyr::group_by(mon) %>% # 月ごとに
  dplyr::summarise(amount_sum = sum(amount)/10000) # 合計

# 4月スタートに並び替えて作図 fig05
april <- c("4","5","6","7","8","9","10","11","12","1","2","3")
df_mon <- transform(df_mon, mon = factor(mon, levels = april))
ggplot(df_mon, aes(y=amount_sum, x=mon, group=1)) + geom_line() + geom_point() +
  xlab("月") + ylab("万円") + mystyle

# ここから再度 ts
# 訓練データ
y_train <- window(y, start(y), c(2019,6))
# SARIMAモデル SARIMA(2,1,3)[12]
fit <- y_train %>% 
  Arima(order=c(2,1,3), seasonal=list(order=c(1,1,1), period=12))
# 3年先まで予測
y_forecast <- fit %>% forecast(h=36)
# 現系列と予測系列の図示 fig06
autoplot(y) + autolayer(y_forecast$mean)+  xlab("年") + ylab("万円") + mystyle
