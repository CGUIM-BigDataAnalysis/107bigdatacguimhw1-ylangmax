library(jsonlite)
library(dplyr)
library(readxl)
X103 <- read_excel(
  "~/Downloads/103年各教育程度別初任人員經常性薪資─按大職類分.xlsx",
  col_types = c(
    "numeric",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)



X106 <-
  read_excel(
    "~/Downloads/106年各教育程度別初任人員每人每月經常性薪資─按大職類分.xlsx",
    col_types = c(
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )


jointable <- inner_join(X103, X106, by = "大職業別")

##ＱＡ職業是只要大標題還是全部都要+為何要分析出現次數
jointable<-jointable[,c(1,2,11,15,24)]
jointable$SalaryRate <- jointable$`大學-薪資.y` / jointable$`大學-薪資.x`
jointable <- filter(jointable, jointable$SalaryRate > 1)
jointable <-arrange(jointable,desc(jointable$SalaryRate))


JTCareer <-
  jointable[!grepl("-", jointable$大職業別), ]   ##請問106年度薪資較103年度薪資高的職業有哪些?
JTCareer$大職業別
##並用文字說明結果(10分)。

jointable105 <-
  filter(jointable, jointable$SalaryRate > 1.05)##提高超過5%的的職業有哪些(5分)?
jointableCarrer <- gsub("-專業人員|-技術員及助理專業人員|-技術員及助理專業人員|-服務及銷售工作人員|-技藝、機械設備操作及組裝人員|-事務支援人員","",jointable105$大職業別)
table(jointableCarrer)

JTCareer1 <-
  jointable105[!grepl("-", jointable105$大職業別),]## 主要的職業種別是哪些種類呢(5分)? 這題作法？
JTCareer1$大職業別



#2
library(readxl)
X104 <- read_excel(
  "~/Downloads/104年各教育程度別初任人員經常性薪資─按大職類分.xlsx",
  col_types = c(
    "numeric",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)

X105 <-
  read_excel(
    "~/Downloads/105年各教育程度別初任人員每人每月經常性薪資─按大職類分.xlsx",
    col_types = c(
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )
library(dplyr)
# 男生薪資較多 女/男<100
X103SalaryRate <- filter(X103, X103$`大學-女/男` < 100) %>%
  arrange(`大學-女/男`)
X103SalaryRate<-filter(X103,X103$`大學-女/男`<100) %>%
  arrange(`大學-女/男`)
X103SalaryRate<-X103SalaryRate[,c(1,2,11,12)]


X104SR <- filter(X104, X104$`大學-女/男` < 100) %>%
  arrange(`大學-女/男`)
X105SR <- filter(X105, X105$`大學-女/男` < 100) %>%
  arrange(`大學-女/男`)
X106SR <- filter(X106, X106$`大學-女/男` < 100) %>%
  arrange(`大學-女/男`)
head(X103SalaryRate, 10)

# 女生薪資較多 女/男>100
X103SRGirl <- filter(X103, X103$`大學-女/男` > 100) %>%
  arrange(desc(`大學-女/男`))
X104SRG <- filter(X104, X104$`大學-女/男` > 100) %>%
  arrange(desc(`大學-女/男`))
X105SRG <- filter(X105, X105$`大學-女/男` > 100) %>%
  arrange(desc(`大學-女/男`))
X106SRG <- filter(X106, X106$`大學-女/男` > 100) %>%
  arrange(desc(`大學-女/男`))
head(X103SRGirl, 10)

#3
X106<-X106[,c(1,2,11,13)]
X106$SalaryRate <- X106$`研究所及以上-薪資` / X106$`大學-薪資`
X106 <- arrange(X106, desc(SalaryRate))
X106 <- X106[complete.cases(X106$SalaryRate), ]
head(X106 , 10)

X106Difference<-X106[,c(1,2,11,13)]
X106Difference$SalaryRate <- X106Difference$`研究所及以上-薪資` / X106$`大學-薪資`
X106Difference <- arrange(X106Difference, desc(SalaryRate))
X106Difference<- X106Difference[complete.cases(X106$SalaryRate), ]
head(X106Difference ,10)
#4
X106Like <-
  subset(X106,
         大職業別 == "資訊及通訊傳播業-專業人員" | 大職業別 == "資訊及通訊傳播業" |
           大職業別 == "藝術_娛樂及休閒服務業-專業人員")
X106Like <- X106Like[,c(1,2,11,13)]
X106Like$difference <- X106Like$`研究所及以上-薪資` - X106Like$`大學-薪資`
