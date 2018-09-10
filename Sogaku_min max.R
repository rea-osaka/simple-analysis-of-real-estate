#########   地区毎の取引総額の範囲  ##############
############  取引価格情報を利用　################
#表示桁数
options(digits=10)
options(scipen=100)

# 取引価格情報の存在するフォルダをして、適宜修正
setwd("C:/Users/_____/Desktop/torihiki")
library(tidyverse)

###　ファイル名の取得 
fnames <- dir(pattern="^\\d{2}_.+_\\d{5}_\\d{5}.csv")  #ファイル名の取得
fnames <- fnames[c(27)]  # c( )内に数字　京都と兵庫c(26, 28) 三重と大阪～和歌山c(24,27:30)
fnames

### ファイルの結合
dl <- lapply(fnames, read.csv,encoding = "Shift_JIS",na.strings = c("","NULL"))
data0 <- do.call(rbind, dl)
rm(dl)
data0 <- data0[, -1 ]
colnames(data0) <- c("種類","地域","市区町村コード","都道府県名","市区町村名","地区名","最寄駅名称","最寄駅分","取引総額","坪単価","間取り","面積","取引価格の単価","土地の形状","間口","延床面積","建築年","構造","用途","今後","前面道路の方位","前面道路の種類","前面道路幅員","都市計画","建蔽率","容積率","取引時点","改装","備考")
data0$市地区 <- paste0(data0$市区町村名, data0$地区名)
data0$駅分 <- as.numeric(as.character( data0$最寄駅分))
data0$地積 <- as.numeric(as.character( data0$面積))
data0$取引年 <- as.integer(str_sub(as.character(data0$取引時点),3,4))+1988

### 絞り込み
data1 <- subset(data0, 種類 =="宅地(土地)" | 種類 == "宅地(土地と建物)")
# 以下、絞り込みの例。コメントの#を削除して利用ください。
# data1 <- subset(data1, 地域 == "住宅地")
# data1 <- subset(data1, 地積 <= 400)
# data1 <- subset(data1, 最寄駅名称 == "" | 最寄駅名称 == "")
# data1 <- subset(data1, 最寄駅分 >= 5 & 最寄駅分 <= 15)
# data1 <- subset(data1, 市区町村名 == "東大阪市")
 
##### 地区毎に最小～最大　
chiku_sogaku <- data1 %>% group_by(市地区) %>% summarise(
    n = n(), mean =mean(取引総額, na.rm= TRUE),
    min = min(取引総額, na.rm = TRUE ),
    p01 = quantile(取引総額, 0.01, na.rm = TRUE  ), p05 = quantile(取引総額, 0.05, na.rm = TRUE ),
    p10 = quantile(取引総額, 0.10, na.rm = TRUE ), p25 = quantile(取引総額, 0.25, na.rm = TRUE ),
    p50 = quantile(取引総額, 0.50, na.rm = TRUE ), p75 = quantile(取引総額, 0.75, na.rm = TRUE ),
    p90 = quantile(取引総額, 0.90, na.rm = TRUE ), p95 = quantile(取引総額, 0.95, na.rm = TRUE ),
    p99 = quantile(取引総額, 0.99, na.rm = TRUE ),
    max = max(取引総額, na.rm = TRUE)
    )
write.csv(chiku_sogaku, "chiku_sogaku.csv")    # setwdのフォルダに保存

###### 駅ごとに最小～最大
eki_sogaku <- group_by(data1, 最寄駅名称) %>% summarise(
  n = n(), mean =mean(取引総額, na.rm= TRUE),
  min = min(取引総額, na.rm = TRUE ),
  p01 = quantile(取引総額, 0.01, na.rm = TRUE  ), p05 = quantile(取引総額, 0.05, na.rm = TRUE ),
  p10 = quantile(取引総額, 0.10, na.rm = TRUE ), p25 = quantile(取引総額, 0.25, na.rm = TRUE ),
  p50 = quantile(取引総額, 0.50, na.rm = TRUE ), p75 = quantile(取引総額, 0.75, na.rm = TRUE ),
  p90 = quantile(取引総額, 0.90, na.rm = TRUE ), p95 = quantile(取引総額, 0.95, na.rm = TRUE ),
  p99 = quantile(取引総額, 0.99, na.rm = TRUE ),
  max = max(取引総額, na.rm = TRUE)
)
write.csv(eki_sogaku, "eki_sogaku.csv")

############# 例　東大阪市

higashiosaka <- chiku_sogaku %>% subset(str_detect(市地区, "東大阪市")) %>% droplevels
# サンプル数が10年余で10件未満は無視
higashiosaka_over10 <- higashiosaka %>% subset(n >= 10)
# 最大値maxで並び替え
higashiosaka_over10 <- higashiosaka_over10[order(higashiosaka_over10$max),]
# 結果として、東部の生駒山麓部の宅地の総額が低いことが確認できる
# ファイルに保尊
write.csv(higashiosaka_over10, "higashiosaka_over10.csv")

## グラフ化
# 総額が下から15番目までの箱ひげ図的なグラフ
# 地区名を取り出す
chiku15 <- higashiosaka_over10$市地区[1:15]
higashiosaka_chiku15 <- data1[data1$市地区 %in% chiku15,]
higashiosaka_chiku15$市地区 <- factor(higashiosaka_chiku15$市地区,levels = chiku15)
higashiosaka_chiku15 %>% ggplot(aes(市地区,取引総額)) + geom_boxplot(outlier.colour = NA) + geom_point(size=5, alpha=0.2)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

