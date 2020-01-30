
    options(scipen=100)
    library(psych)
    library(corrplot) 
    library(tidyverse)
    library(caret)
    library(ranger)
    library(randomForest)
    library(ROSE)
    library(reshape2)
    library(stringr)
    library(ggplot2)
    library(geosphere)
    library(lubridate)
    select <- dplyr::select
    
    #dat = read.csv("C:/Users/jeeyeon/Desktop/final_real_select_0616.csv")
    dat = read.csv("C:/Users/jeeyeon/Desktop/data/DM/final_real_select_0617_outlier.csv")
    
    dat_all = read.csv("C:/Users/jeeyeon/Desktop/final_real_0617_outlier.csv")
    
    dat_1 = final6 %>% unique()
    #dat_1 = bigdata %>% unique()
    #### '행사용' 카페였던 경우를 제외하기
    dat_1 = dat_1[-grep("행사", dat_1$close_reason),]
    dat_1 = dat_1[-grep("한시", dat_1$close_reason),]
    dat_1 = dat_1[-grep("단기", dat_1$close_reason),]
    dat_1 = dat_1[-grep("박람회", dat_1$close_reason),]
    dat_1 = dat_1 %>% 
      select(train_test_label,close,days,gu,area_st_year,lat,lon, # 모델링 제외 
             y_oneyear,area_st_date,area_st_month,   # modeling 에 필요한 변수
             #dong,ro,adm_name,road_name,area_st_date,close_date,close, 
             area_st_month,area_st_season,  # 시작일 
             floor,total_floor,total_area,franchise,
             near,near_new_cafe,near_franchise_pop,near_starbucks, # 주변정보
             #서울시상권분석
             rental_fee, total_sales,
             floating_pop,living_pop,office_pop,female, # 유동인구
             income_grade,household_num,   
             #위도경도 
             parking_lot,crosswalk,subway,blog,    
             #코스피
             kospi_avg_lag1,kospi_avg_lag2,kospi_avg_lag3,kospi_avg_lag4,
             kospi_dec,
             kospi_avg_lag34,kospi_avg_lag23 ,kospi_avg_lag12,kospi_avg_lag14,
             #집객시설 #fac,medical,airport
             gov,bank,gen_hosp,hospi,kinder,element,middle,high,univer,depart,
             super,cinema,accom,rail,bus_term,bus_stop,medical,airport)
    
    
#### x 끼리의 correlation ####
    #x = dat %>% select(near,nearcafe,near_franchise)
    x = dat %>% select(near,nearcafe,near_new_cafe,near_franchise) 
    colnames(x) =c("음식점수","카페수","개업카페수","프랜차이즈카페수")

    x = dat_1 %>% select(gen_hosp,gov,medical,airport,bus_stop)
    x = x[complete.cases(x),]
    colnames(x) = c("일반병원","관공서","약국","공항버스","버스정류장")
    
    x = select_if (temp , is.numeric)
    dat_cor <- cor(x)
    
  # # method 1
  #   corrplot(dat_cor, method="number")
  # # method 2
  #   pairs.panels(x, 
  #                method = "pearson", # correlation method
  #                hist.col = "#00AFBB",
  #                density = TRUE,  # show density plots
  #                ellipses = TRUE) # show correlation ellipses
    # method 3
    #correlation plot
    corr <- melt(round(dat_cor, 2))
    ggplot(corr, aes(Var1, Var2)) + geom_tile(aes(fill = value), colour = "white") +
      geom_text(aes(label = sprintf("%1.2f", value)),size=6, vjust = 1) +
      scale_fill_gradient(low = "white", high = "steelblue") + theme_bw()+
      labs(x="", y="") +
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(angle=45,hjust=1,size=15,face="bold"),#x축숫자크기
                       axis.text.y=element_text(size=20,face="bold"),#y축숫자크기 
                       axis.title=element_text(size=20,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    
    
#### y : 영업일수 그래프  ####
    
#1년내 
    dat_sample_1 = dat_all %>% mutate(area_st_date = ymd(area_st_date)) %>% 
      filter(area_st_date < ymd("20180523"))
    nrow(dat_sample_1)
    sum(dat_sample_1$y_oneyear==1)
    sum(dat_sample_1$y_oneyear==1)/nrow(dat_sample_1)
    
    ggplot(dat_sample_1,aes(days))+geom_histogram(bins=30,color="white") +
      #geom_text(aes(fontface=2,size=6),vjust=2) + 
      xlab("총 영업일수") + ylab("카페 수")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=15),#x축숫자크기
                       axis.text.y=element_text(size=15),#y축숫자크기 
                       axis.title=element_text(size=20,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    
    ggplot(dat_sample_1,aes(x=factor(y_oneyear))) + geom_bar(stat="count")+theme_bw() +
      xlab("폐업여부") + ylab("카페 수")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=15),#x축숫자크기
                       axis.text.y=element_text(size=15),#y축숫자크기 
                       axis.title=element_text(size=20,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    # ggplot(dat, aes(x=1,y=near)) + geom_boxplot() +coord_flip()+
    #   #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
    #   ylab("주변 음식점 수") + 
    #   theme_bw()+theme(legend.position="none",
    #                    axis.text=element_text(size=15),  #축제목크기
    #                    #axis.text.x=element_text(size=12),#x축숫자크기
    #                    axis.text.y=element_text(size=12),#y축숫자크기 
    #                    axis.title=element_text(size=15,face="bold"),
    #                    plot.title=element_text(size=20,face="bold"))
    
    dat_sample_2 = dat_all %>% mutate(area_st_date = ymd(area_st_date),
                                      y_twoyear = ifelse(days<730,"1","0")) %>% 
      filter(area_st_date < ymd("20170523"))
    
    nrow(dat_sample_2)
    sum(dat_sample_2$y_twoyear==1)
    sum(dat_sample_2$y_twoyear==1)/nrow(dat_sample_2)
    
    ggplot(dat_sample_2,aes(x=factor(y_oneyear))) + geom_bar(stat="count")+theme_bw() +
      xlab("폐업여부") + ylab("카페 수")+ylim(c(0,7000))+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=15),#x축숫자크기
                       axis.text.y=element_text(size=15),#y축숫자크기 
                       axis.title=element_text(size=20,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
#후
    mydata = final5 %>% filter(area_st_date > ymd("20150401")) %>% 
      filter(days>30&!is.na(close_date))
    mydata = mydata[-grep("행사", mydata$close_reason),]
    mydata = mydata[-grep("한시", mydata$close_reason),]
    mydata = mydata[-grep("단기", mydata$close_reason),]
    mydata = mydata[-grep("박람회", mydata$close_reason),]
    ggplot(mydata,aes(days))+geom_histogram(bins=30,color="white") +
      #geom_text(aes(fontface=2,size=6),vjust=2) +
      xlab("총 영업일수") + ylab("카페 수")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=15),#x축숫자크기
                       axis.text.y=element_text(size=15),#y축숫자크기
                       axis.title=element_text(size=20,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    mydata = mydata%>% 
      filter(area_st_date < ymd("20180523"))
    ggplot(mydata, aes(x=factor(y_oneyear),y=near_new_cafe)) + geom_boxplot()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("주변 음식점 수") + xlab("1년내폐업여부") + 
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    # cor(dat[complete.cases(dat$total_sales),]$total_sales,
    #     dat[complete.cases(dat$total_sales),]$rental_fee)
    
    
  ### 
    ggplot(mydata,aes(x=days)) +geom_histogram()
    
    mydata = dat %>%mutate(area_st_date = ymd(area_st_date)) %>% 
      filter(area_st_date < ymd("20170523"))
    
    
    
    ggplot(dat %>% filter(days <500),aes(days))+geom_histogram(bins=30) 
    
    closing = data %>% filter(!is.na(close_date))
    non_closing= data %>% filter(is.na(close_date))
    ggplot(closing,aes(days))+geom_density()
    ggplot(non_closing,aes(days))+geom_density()
    
    
#### x , y 와의 관계      ####

  ############## all_boxplot######   ############## 
    ggplot(dat, aes(x=factor(y_oneyear),y=total_floor)) +geom_boxplot()
    ggplot(dat,aes(total_floor))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=total_area)) +geom_boxplot() ####
    ggplot(dat, aes(x=factor(y_oneyear),y=total_area)) +geom_violin() 
    ggplot(dat, aes(x=factor(y_oneyear),y=log(total_area))) +geom_boxplot()
    ggplot(dat,aes(total_area))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=near)) +geom_boxplot() ###
    ggplot(dat,aes(near))+geom_histogram(bins=30)
    ggplot(dat, aes(x=factor(y_oneyear),y=near_new_cafe)) +geom_boxplot()
    ggplot(dat,aes(near_new_cafe))+geom_histogram(bins=30)
    ggplot(dat, aes(x=factor(y_oneyear),y=near_franchise_pop)) +geom_boxplot()
    
    ggplot(dat,aes(near_franchise_pop))+geom_histogram(bins=30)
    ggplot(dat, aes(x=factor(y_oneyear),y=near_franchise_pop)) +geom_violin() 
    
    ggplot(dat, aes(x=factor(y_oneyear),y=near_starbucks)) +geom_boxplot()
    ggplot(dat,aes(near_starbucks))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=rental_fee)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=log(rental_fee)))+geom_boxplot()###
    ggplot(dat,aes(rental_fee))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=floating_pop)) +geom_boxplot()
    ggplot(dat,aes(floating_pop))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=living_pop)) +geom_boxplot()
    ggplot(dat,aes(living_pop))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=office_pop)) +geom_boxplot()
    ggplot(dat,aes(office_pop))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=female)) +geom_boxplot()
    ggplot(dat,aes(female))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=income_grade)) +geom_boxplot()
    ggplot(dat,aes(income_grade))+geom_histogram(bins=30)
    ggplot(dat, aes(x=factor(y_oneyear),y=household_num)) +geom_boxplot()
    ggplot(dat,aes(household_num))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=parking_lot)) +geom_boxplot()
    ggplot(dat,aes(parking_lot))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=crosswalk)) +geom_boxplot()
    ggplot(dat,aes(crosswalk))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=subway)) +geom_boxplot()
    ggplot(dat,aes(subway))+geom_histogram(bins=30)
    
    ggplot(dat %>% filter(blog<1400), aes(x=factor(y_oneyear),y=blog)) +geom_boxplot() 
    ggplot(dat, aes(x=factor(y_oneyear),y=blog)) +geom_violin() +geom_jitter(alpha=0.5)
    ggplot(dat, aes(x=factor(y_oneyear),y=log(blog))) +geom_boxplot()
    ggplot(dat,aes(blog))+geom_histogram(bins=30)
    
    ggplot(dat, aes(x=factor(y_oneyear),y=kospi_dec)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=kospi_avg_lag34)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=kospi_avg_lag23)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=kospi_avg_lag12)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=kospi_avg_lag14)) +geom_boxplot()
    
    ggplot(dat, aes(x=factor(y_oneyear),y=gov)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=bank)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=gen_hosp)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=hospi)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=kinder)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=element)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=middle)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=high)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=univer)) +geom_boxplot()
    
    ggplot(dat, aes(x=factor(y_oneyear),y=depart)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=super)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=cinema)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=rail)) +geom_boxplot() ###
    ggplot(dat, aes(x=factor(y_oneyear),y=accom)) +geom_boxplot()
    ggplot(dat, aes(x=factor(y_oneyear),y=bus_term)) +geom_boxplot() ###
    ggplot(dat, aes(x=factor(y_oneyear),y=bus_stop)) +geom_boxplot()
    
  ############## PPT boxplot######   ############
    ### 울타리 대체 변수들 : near, blog, depart, cinema
    ggplot(dat, aes(x=1,y=near)) + geom_boxplot() +coord_flip()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("주변 음식점 수") + 
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    ggplot(dat, aes(x=1,y=blog)) + geom_boxplot() +coord_flip()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("블로그 건수") + 
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    ggplot(dat, aes(x=1,y=depart)) + geom_boxplot() +coord_flip()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("백화점 수") + 
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    ggplot(dat, aes(x=1,y=cinema)) + geom_boxplot() +coord_flip()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("극장수") +
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    ##### 딱 한개의 동떨어진 값 있는 변수 들 : kinder, bus stop
    ggplot(dat, aes(x=1,y=kinder)) + geom_boxplot() +coord_flip()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("유치원수") +
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    ggplot(dat, aes(x=1,y=bus_stop)) + geom_boxplot() +coord_flip()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("버스정류장수") +
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    ggplot(dat, aes(x=factor(y_oneyear),y=near_new_cafe)) + geom_boxplot()+
      #geom_hline(yintercept=c(summary(dat$near)[6]), color="red")+
      ylab("주변 음식점 수") + xlab("1년내폐업여부") + 
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       #axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
########################################## 
    
######## 매출액,임대료#####  ####
  # 1층 임대료
    DM_rental_fee = read.csv("C:/Users/jeeyeon/Desktop/data/DM/rental_fee_final.csv")
    
    names(DM_rental_fee) = c("adm_name","one_floor_fee","remain_floor_fee","area_st_year","quarter")
    DM_rental_fee = DM_rental_fee %>% mutate(adm_name = as.character(adm_name),
                                             area_st_year = as.numeric(area_st_year))
    DM_rental_fee = DM_rental_fee %>% mutate(adm_name = as.character(adm_name),
                                             year_quater=paste(area_st_year,quarter,sep="-"))
    df_seq = data.frame(year_quater=paste(dat_all$area_st_year,dat_all$quarter,sep="-")) %>%
      unique()
    df_seq = data.frame(year_quater=sort(df_seq$year_quater)) %>% 
      mutate(year_quater_seq = 1:nrow(df_seq))
    
    DM_rental_fee = left_join(DM_rental_fee, df_seq, by = "year_quater")
    DM_rental_fee = DM_rental_fee %>% mutate(year_quater_seq_pre1 = year_quater_seq-1)
    DM_rental_fee = DM_rental_fee %>% select(adm_name,year_quater_seq_pre1,
                                             one_floor_fee,remain_floor_fee)
    DM_rental_fee = DM_rental_fee %>% gather(floor,rental_fee,3:4)
    one_rental_fee = DM_rental_fee %>% filter(floor=="one_floor_fee") %>% 
      select(adm_name,year_quater_seq_pre1,rental_fee)
    
  # 매출액 
    sales = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_sales.csv")
    sales = sales %>% select(1,2,5,9)
    names(sales) = c("year","quarter","code","total_sales")
    
    match1 = read.csv("C:/Users/jeeyeon/Desktop/data/DM/FAC_adm_code_name.csv")
    match1 = match1 %>% filter(시도명 == "서울특별시") %>% select(1,3,5,7) 
    names(match1) = c("sggcode","admcode","gu","adm_name_temp")
    match1 = match1 %>% transmute(adm_code = paste(sggcode,admcode,sep=""), 
                                  gu = gu, adm_name_temp=as.character(adm_name_temp))
    match1 = match1 %>% 
      mutate(adm_name = ifelse(gu=="강남구"&adm_name_temp=="신사동","신사동_강",
                               ifelse(gu=="관악구"&adm_name_temp=="신사동","신사동_관",adm_name_temp)),
             adm_name = gsub("제1동","1동",adm_name), adm_name = gsub("제2동","2동",adm_name),
             adm_name = gsub("제3동","3동",adm_name), adm_name = gsub("제4동","4동",adm_name),
             adm_name = gsub("제5동","5동",adm_name), adm_name = gsub("제6동","6동",adm_name),
             adm_name = gsub("제7동","7동",adm_name), adm_name = gsub("제8동","8동",adm_name),
             adm_name = gsub("제9동","9동",adm_name), adm_name = gsub("제10동","10동",adm_name),
             adm_name = gsub("홍1동","홍제1동",adm_name), adm_name = gsub("홍2동","홍제2동",adm_name),
             adm_name = gsub("홍3동","홍제3동",adm_name), 
             adm_name = gsub("제3.8동","3.8동",adm_name),
             adm_name = gsub("중계제23동","중계2.3동",adm_name),
             adm_name = gsub("보래매동","보라매동",adm_name))
    match1 = match1 %>% select(adm_code,adm_name)
    
    match2 = read.csv("C:/Users/jeeyeon/Desktop/data/DM/FAC_adm_code,sang_code.csv")
    match2 = match2 %>% select(3,8) 
    names(match2) = c("code","adm_code")
    match2$adm_code = as.character(match2$adm_code)
    
    # match : adm_code(행정동코드), adm_name(행정동명), code_name(상권코드명)
    match2_sales = left_join(match2,sales, by="code")
    match1_match2_sales = left_join(match1, match2_sales,by="adm_code" ) %>% unique()
    
    match1_match2_sales = match1_match2_sales %>% group_by(adm_name,year,quarter) %>% 
      summarize(total_sales = mean(total_sales,na.rm=T))
    
    match1_match2_sales = match1_match2_sales %>% mutate(year_quater=paste(year,quarter,sep="-"))
    match1_match2_sales = left_join(match1_match2_sales, df_seq, by = "year_quater")
    match1_match2_sales = match1_match2_sales %>% mutate(year_quater_seq_pre1 = year_quater_seq-1) %>% ungroup()
    match1_match2_sales = match1_match2_sales %>% select(adm_name,year_quater_seq_pre1,total_sales)
  
  # join
    one_rental_fee = one_rental_fee %>% group_by(adm_name) %>% summarize(rental_fee = mean(rental_fee,na.rm=T))
    sales = match1_match2_sales %>% group_by(adm_name) %>% summarize(total_sales = mean(total_sales,na.rm=T))
    
    gu_adm_name = dat_all %>% group_by(gu,adm_name) %>% count() %>% select(gu,adm_name)

#### 구별 매출액,임대료 ####
    mydata = left_join(gu_adm_name, one_rental_fee,by = "adm_name")
    mydata = left_join(mydata, sales,by = "adm_name")
    mydata = mydata %>% group_by(gu) %>% 
      summarize(rental_fee = mean(rental_fee,na.rm=T), total_sales = mean(total_sales,na.rm=T))
    ggplot(mydata,aes(x=rental_fee,y=total_sales,color=gu,size=6,label=gu)) +geom_point() +
      geom_text(aes(fontface=2,size=6),vjust=2) + 
      xlab("평당임대료") +ylab("한달 평균 매출액")+ggtitle("구별 매출액,임대료")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    write.csv(mydata, "C:/Users/jeeyeon/Desktop/temp.csv",row.names=FALSE)
    
#### 행정동별 매출액,임대료 ####
    mydata = left_join(gu_adm_name, one_rental_fee,by = "adm_name")
    mydata = left_join(mydata, sales,by = "adm_name")
    # graph 
    mydata = mydata %>% group_by(gu,adm_name) %>% 
      summarize(rental_fee = mean(rental_fee,na.rm=T), total_sales = mean(total_sales,na.rm=T))
    ggplot(mydata,aes(x=rental_fee,y=total_sales,color=gu,size=6,label=adm_name)) +geom_point() +
      geom_text(aes(fontface=2,size=6),vjust=2) + 
      xlab("평당임대료") +ylab("한달 평균 매출액")+ggtitle("행정동별 매출액,임대료")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=12),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    
############################################################ 
    
####### x 살펴보기 ######################################### 
    
#### 집객시설수 #### 
    ggplot(dat,aes(gov))+geom_histogram(bins=30)
    ggplot(dat,aes(bank))+geom_histogram(bins=30)
    ggplot(dat,aes(kinder))+geom_histogram(bins=30)
    
    ggplot(dat,aes(x=total_floor,y=total_area)) +geom_point()
    ggplot(dat_all,aes(x=area_st_date,y=days)) +geom_point()
    
    dat = dat %>% mutate(floating_pop =as.numeric(gsub(",","",floating_pop)))
    mydata = dat %>% group_by(gu) %>% summarize(floating_pop=mean(floating_pop,na.rm =T))
    ggplot(mydata,aes(x=reorder(gu),y=floating_pop)) +geom_bar(stat = "identity")
    
#### 스타벅스   #### 
    # name = gsub(pattern = "\\s",replacement = "",x = dat_all$name) # 공백제거
    # name = gsub("까페", "카페", name) # 까페 -> 카페로 통일
    # fran_index = c()
    # fran_index = sort(c(fran_index, grep("커피빈",name)))
    # starbucks = dat_all %>% mutate(n=1:nrow(dat_all),
    #                                starbucks = ifelse(n %in% fran_index,1,0),
    #                                franchise = as.numeric(as.character(franchise))) %>%
    #                                select(-n)
    # 
    # sum(starbucks$starbucks)/nrow(starbucks)  #전체 데이터 중에 스타벅스 비율 
    # sum(starbucks$franchise)/nrow(starbucks)
    # sum(starbucks$starbucks)/sum(starbucks$franchise)
    
  # 카페 비율
    fran_data = c("스타벅스","커피빈","투썸","이디야","달콤커피","커피베이","할리스","탐앤탐스","폴바셋","커피나무",
                  "파스쿠찌","파스쿠치","빽다방","빈스빈스","엔제리너스","엔젤리너스","커피명가","카페베네","커피스미스","드롭탑",
                  "커피마마","더착한커피","더카페","커피에반하다","매머드커피","주커피","토프레소","카페보니또","전광수커피","그라찌에","띠아모")
    name = gsub(pattern = "\\s",replacement = "",x = dat_all$name) # 공백제거
    name = gsub("까페", "카페", name) # 까페 -> 카페로 통일
    
    fran_index = list()
    for(i in 1:length(fran_data)){
      fran_index[[i]] = c(grep(fran_data[i],name))
      }
    # data = data %>% mutate(franchise =ifelse(n %in% fran_index,1,0)) %>% select(-n)
    
    dim(fran_index)
    franchise_table = data.frame("name"=fran_data,"number"=sapply(fran_index,length) )
    
  #1974~ 현재까지 안망한 곳 스타벅스 비율 
    allcafe = read.csv("C:/Users/jeeyeon/Desktop/data/DM/cafe.csv")
    names(allcafe) = c("sgg_code","type_code","year","id","type_name",
                       "adm_date","name","road_name","ad_name","working_area",
                       "call_number", "corp_st_date","corp_name","area_st_date","adm_name",
                       "close_date","close","close_reason","type","top_start",
                       "top_fin", "bott_start","bott_fin","total_floor", "location",
                       "cook_area","room_area","hall_area","etc_area","bath_area",
                       "joint_bath_area","fitting_area","seat_area","work_area","lab_area",
                       "counter_area","storage_area")
    allcafe = allcafe %>% filter(is.na(close_date)) # close_date NA 인 곳이니까 현재도 안망한곳임
    name = gsub(pattern = "\\s",replacement = "",x = allcafe$name) # 공백제거
    name = gsub("까페", "카페", name) # 까페 -> 카페로 통일
    
    fran_index = list()
    for(i in 1:length(fran_data)){
      fran_index[[i]] = c(grep(fran_data[i],name))
    }
    # data = data %>% mutate(franchise =ifelse(n %in% fran_index,1,0)) %>% select(-n)
    
    dim(fran_index)
    franchise_table = data.frame("name"=fran_data,"number"=sapply(fran_index,length) )
    
#### 직장인 인구 &여성인구  #### 
    mydata = dat %>% group_by(gu) %>% summarize(female=mean(female,na.rm =T),
                                                office_pop=mean(office_pop,na.rm=T),
                                                living_pop=mean(living_pop,na.rm=T),
                                                floating_pop=mean(floating_pop,na.rm=T))
    ggplot(mydata) + geom_bar(aes(x=reorder(gu,female),y=female),stat = "identity")
    ggplot(mydata) + geom_bar(aes(x=reorder(gu,living_pop),y=femalliving_pope),stat = "identity")
    ggplot(mydata) + geom_bar(aes(x=reorder(gu,office_pop),y=office_pop),stat = "identity")
    ggplot(mydata) + geom_bar(aes(x=reorder(gu,floating_pop),y=floating_pop),stat = "identity")
    
#### 직장인 인구 & 임대시세 #### 
    mydata = dat %>%  filter(floor %in% c("1층","1층+2층이상","지하+지상")) %>% 
      group_by(adm_name) %>% summarize(x=)
    ggplot(dat) + geom_point(aes(x=floating_pop,y=rental_fee))
    
#### 역별 블로그 건수 TOP10 #### 
    subway_blog = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_subway_blog.csv") 
    mydat = subway_blog %>% mutate(year_month = paste(year,month,"-")) %>% 
      group_by(station) %>% summarize(mean_blog = mean(blog,na.rm=T))%>% 
      arrange(desc(mean_blog))
    
    ggplot(mydat[1:10,],aes(x=reorder(station,mean_blog),y=mean_blog)) + theme_light() +
      geom_bar(stat="identity")+ 
      xlab("지하철역") +ylab("평균 블로그 건수")+ggtitle("블로그 건수 TOP10 지하철역")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=12,angle=45,hjust=1),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    ggplot(mydat[11:20,],aes(x=reorder(station,mean_blog),y=mean_blog)) + theme_light() +
      geom_bar(stat="identity")+ 
      xlab("지하철역") +ylab("평균 블로그 건수")+ggtitle("블로그 건수 TOP11-20 지하철역")+
      theme_bw()+theme(legend.position="none",
                       axis.text=element_text(size=15),  #축제목크기
                       axis.text.x=element_text(size=12,angle=45,hjust=1),#x축숫자크기
                       axis.text.y=element_text(size=12),#y축숫자크기 
                       axis.title=element_text(size=15,face="bold"),
                       plot.title=element_text(size=20,face="bold"))
    
    top10 = mydat[1:10,]$station
    top11_20 = mydat[11:20,]$station
    
    mydat = subway_blog %>% 
      mutate(year = as.character(year), month = as.character(month),
            month = ifelse(month =="1","01",month),month = ifelse(month =="2","02",month),
            month = ifelse(month =="3","03",month),month = ifelse(month =="4","04",month),
            month = ifelse(month =="5","05",month),month = ifelse(month =="6","06",month),
            month = ifelse(month =="7","07",month),month = ifelse(month =="8","08",month),
            month = ifelse(month =="9","09",month),ym = paste(year, month,sep ="")) %>% 
      arrange(desc(blog)) 
    
    mydat1 = mydat %>% filter(station %in% top10)
    ggplot(mydat1,aes(x=ym,y=blog,colour=station,group=station))+geom_line(size=2)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    mydat2 = mydat %>% filter(station %in% top11_20)
    ggplot(mydat2,aes(x=ym,y=blog,colour=station,group=station))+geom_line(size=2)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
##### 성신여대점 ######
    sungsin = final6 %>% unique()
    #### '행사용' 카페였던 경우를 제외하기
    sungsin = sungsin[-grep("행사", sungsin$close_reason),]
    sungsin = sungsin[-grep("한시", sungsin$close_reason),]
    sungsin = sungsin[-grep("단기", sungsin$close_reason),]
    sungsin = sungsin[-grep("박람회", sungsin$close_reason),]
    
    sungsin = sungsin %>% 
      filter(adm_name%in%c("동선동","동소문동1가","동소문동2가","동소문동3가","삼선동","정릉1동",
                           "동소문동4가","동소문동6가","동소문동7가","안암동","보문동")|
               dong %in% c("삼선동4가","동선동1가","동선동2가","동선동3가","동선동4가","동선동5가",
                           "돈암동"))
    sungsin = sungsin %>% group_by(area_st_date) %>% summarize(n=n())
    
    ggplot(sungsin,aes(x=area_st_date,y=n))+geom_line(size=2)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    
############################################################ 

    
    
####### MAP ################################################
#### map  ####
    
    # AIzaSyBMIgVnbPgsXv8LgljlesIn6-ulN_mZ1ew
    library(ggmap)
    library(ggplot2)
    library(raster)
    
    # 구글 지도 API 인증키를 등록
    register_google(key ='mykey')
    
    # 한번 그래프 그려볼까
    ggmap(get_map(location='south korea', zoom=7))
    map <- get_map(location='Seoul,korea', zoom=7, maptype='roadmap', color='bw')
    ggmap(map) + geom_point(data=final, aes(x=lon, y=lat, color=gu))
    
    gc=geocode("seoul,korea",source="google")
    gc
    center = as.numeric(gc)
    center
    
    map = get_googlemap(center=center,language = "ko-KR",color="bw",scale=2)
    ggmap(map,extent="device")
    
    
    kor=get_map("seoul",zoom=11,maptype="terrain")
    kor.map=ggmap(kor)+geom_point(data=dat_all,aes(x=lat,y=lon,color=floor,size=3))
    kor.map
    library(devtools)
    install_github("ggmap", "haven-jeon")
    
    cent = c(mean(dat_all$lat),mean(dat_all$lon))
    bmap <- ggmap(get_navermap(center = cent, level = 6, baselayer = "default",
                  overlayers = c("anno_satellite"), marker = data.frame(cent[1], cent[2]),
                  key = "AIzaSyCOGOjnN1pOl8FmyscQhF5Nvn7KvN31Eb4", uri = "www.r-project.org"), extent = "device",
                  base_layer = ggplot(dat_all, aes(x = lon, y = lat, colour = gu)))
    

    
# -------------------------- 기타 ------------
    # 모든 카페y보려고
    # allcafe = read.csv("C:/Users/jeeyeon/Desktop/data/DM/cafe.csv")
    # names(allcafe) = c("sgg_code","type_code","year","id","type_name",
    #                    "adm_date","name","road_name","ad_name","working_area",
    #                    "call_number", "corp_st_date","corp_name","area_st_date","adm_name",
    #                    "close_date","close","close_reason","type","top_start",
    #                    "top_fin", "bott_start","bott_fin","total_floor", "location",
    #                    "cook_area","room_area","hall_area","etc_area","bath_area",
    #                    "joint_bath_area","fitting_area","seat_area","work_area","lab_area",
    #                    "counter_area","storage_area")
    # allcafe = allcafe %>% filter(!is.na(close_date))
    # allcafe = allcafe %>% mutate(area_st_date = ymd(area_st_date),
    #                              close_date=ymd(close_date),
    #                              days=close_date-area_st_date)
    # 
    # ggplot(allcafe,aes(days))+geom_histogram(bins=30)
    # ggplot(allcafe%>% filter(days <500 & !(year%in%c(2018,2019))),aes(days))+
    #   geom_histogram(bins=30) 
    # ggplot(allcafe %>% filter(days <500),aes(days))+geom_histogram(bins=30)
    
    
    
    
    
    
    
    
    
    