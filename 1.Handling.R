library(tidyverse)
library(stringr)
library(ggplot2)
library(geosphere)
library(lubridate)
select <- dplyr::select

######## [total_area] ######
    data = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_cafe_2015.csv")
    names(data)
    names(data) = c("sgg_code","type_code","year","id","type_name",
                    "adm_date","name","road_name","ad_name","working_area",
                    "call_number", "corp_st_date","corp_name","area_st_date","adm_name",
                    "close_date","close","close_reason","type","top_start",
                    "top_fin", "bott_start","bott_fin","total_floor", "location",
                    "cook_area","room_area","hall_area","etc_area","bath_area",
                    "joint_bath_area","fitting_area","seat_area","work_area","lab_area",
                    "counter_area","storage_area")
    for(i in 26:37){ data[is.na(data[,i]), i] = 0 } # NA -> 0 으로
    
    data = data %>% mutate(total_area=cook_area+room_area+hall_area+etc_area+bath_area+joint_bath_area+
                             fitting_area+seat_area+work_area+lab_area+counter_area+storage_area)
    # data %>% select(working_area, total_area) # working_area 계산할 때 NA값있으면 NA로 놨군, 버리자
    
    data = data %>% select(-working_area,-cook_area,-room_area,-hall_area,-etc_area,-bath_area,-joint_bath_area,
                             -fitting_area,-seat_area,-work_area,-lab_area,-counter_area,-storage_area) %>% 
                    mutate(road_name = as.character(road_name), name = as.character(name), n=1:nrow(data))
    #data = data %>% mutate(road_name_merge =paste(si,gu,ro,adnum,sep=""))
    
#### Franchise변수생성
    #### 프랜차이즈 출처 http://www.reputation.kr/news/articleView.html?idxno=1684
    fran_data = c("스타벅스","커피빈","투썸","이디야","달콤커피","커피베이","할리스","탐앤탐스","폴바셋","커피나무",
                  "파스쿠찌","파스쿠치","빽다방","빈스빈스","엔제리너스","엔젤리너스","커피명가","카페베네","커피스미스","드롭탑","커피마마","더착한커피",
                  "더카페","커피에반하다","매머드커피","주커피","토프레소","카페보니또","전광수커피","그라찌에","띠아모")
    name = gsub(pattern = "\\s",replacement = "",x = data$name) # 공백제거
    name = gsub("까페", "카페", name) # 까페 -> 카페로 통일
    fran_index = c()
    for(i in 1:length(fran_data)){fran_index = sort(c(fran_index, grep(fran_data[i],name)))}
    data = data %>% mutate(franchise = ifelse(n %in% fran_index,1,0)) %>% select(-n)
    
#### road_name -> si, gu, ro, etc_ad, dong 변수추가
    
    #####################
    ### 다음엔 str_split 말고 colsplit로 해보기 ( 그러면 for문 안돌려도 됨 !!! )
    #####################
    
    remove_jiha = gsub("지하 ", "", data$road_name) # "지하"때문에 지번이 제대로 안들어와서 없애기
    remove_jiha = gsub("지하", "", remove_jiha)
    split_road = str_split(remove_jiha, " ", n = 5)# 시, 군, 로, 도로명숫자, 나머지로 분리
    split_ad = str_split(data$ad_name, " ", n = 4) # 동추출
    si = c(); gu = c(); ro = c(); dong = c(); adnum = c();remain_ad = c()
    for (i in 1: nrow(data)){
      si[i]=split_road[[i]][1]
      gu[i]=split_road[[i]][2]
      ro[i]=split_road[[i]][3]
      adnum[i] = split_road[[i]][4]
      remain_ad[i] = split_road[[i]][5]
      dong[i]=split_ad[[i]][3]
    }
    data = data %>% mutate(si = si, gu= gu, dong = dong, ro = ro, adnum = adnum, remain_ad = remain_ad)
    

######## [lat,lon] 도로명주소전체 #########

#1. lat, lon 추가하기(단위:도로명주소전체)
    DM_latlon = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_latlon.csv")
    names(DM_latlon) = c("si","sgg_code","gu","dong_hangjung","dong_raw","ad_name", "road", "build_name", "road_name","lon","lat")
    DM_latlon = DM_latlon %>% filter(si == "서울특별시") %>% 
                              mutate(road_name_merge = gsub(pattern = "\\s",replacement = "",x = road_name)) %>% 
                              select(road_name_merge,lat,lon) 
    DM_latlon = DM_latlon[-which(duplicated(DM_latlon$road_name_merge)),] #중복행 제거
    
    final = left_join(data, DM_latlon,by=c("road_name_merge"))
    sum(is.na(final$lat))
    sum(is.na(final$lat))/nrow(final)
    # road_name 없는거 처리, 위경도 조금 수정
    # write.csv(final,"C:/Users/jeeyeon/Desktop/cafe_2015_latlon.csv") 
    # 위도,경도 빈값 추가하려고 데이터 내보냄! 
    
    
    
############################################################################ 

######## [lat,lon] 추가 처리 #####
    #### road_name 직접 채운것도 있어서 다시 si,gun,ro생성해주자
    final=read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_cafe_2015_latlon.csv")#수정한데이터
    final=final %>% select(-X) %>% mutate(adm_name = as.character(adm_name))
    remove_jiha = gsub("지하 ", "", final$road_name) # "지하"때문에 지번이 제대로 안들어와서 없애기
    remove_jiha = gsub("지하", "", remove_jiha)
    split_road = str_split(remove_jiha, " ", n = 5)# 시, 군, 로, 도로명숫자, 나머지로 분리
    split_ad = str_split(final$ad_name, " ", n = 4) # 동추출
    si = c(); gu = c(); ro = c(); dong = c(); adnum = c();remain_ad = c()
    for (i in 1: nrow(final)){
      si[i]=split_road[[i]][1]
      gu[i]=split_road[[i]][2]
      ro[i]=split_road[[i]][3]
      adnum[i] = split_road[[i]][4]
      remain_ad[i] = split_road[[i]][5]
      dong[i]=split_ad[[i]][3]
    }
    final = final %>% mutate(si = si, gu= gu, dong = dong, ro = ro, adnum = adnum, remain_ad = remain_ad)
    sum(is.na(final$lat))
    sum(is.na(final$lat))/nrow(final) # 14% 결측치 존재
    
#2. ro 단위 위도경도 join
    ##### 결측치 채우기 위해서 final 에서 NA 값 가진 subset 분리해서 동단위 join
    # final 에서 lat lon 기준 missing인 값들만 ro 기준으로 위도경도 join
    final_NA_latlon = final[is.na(final$lat),] %>% select(-lat, -lon)

    DM_doro_latlon = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_doro_latlon.csv")
    names(DM_doro_latlon) = c("sgg_code","seq_num","ro","width","length","base","cross","lat","lon")
    DM_doro_latlon = DM_doro_latlon %>% mutate(ro = as.character(ro)) %>% group_by(ro) %>% 
                     summarize(lat = median(lat), lon= median(lon))
    final_NA_latlon = final_NA_latlon %>% mutate(ro = as.character(ro))
    DM_doro_latlon_ver2 = DM_doro_latlon%>%select(ro,lat,lon) %>% mutate(ro = as.character(ro))
    final_NA_latlon = left_join(final_NA_latlon,DM_doro_latlon_ver2 ,by= "ro")
    sum(is.na(final_NA_latlon$lat))
    # 합쳤는데도 25개 결측치 존재-> 
    
#3. 파일 output해서 직접 넣자 
    # write.csv(final_NA_latlon,"C:/Users/jeeyeon/Desktop/final_NA_latlon.csv") # 다 넣었어
    final_NA_latlon_complete =read.csv("C:/Users/jeeyeon/Desktop/data/DM/final_NA_latlon_complete.csv")
    final_NA_latlon_complete =final_NA_latlon_complete %>% select(-X, -X.1)
    sum(is.na(final_NA_latlon_complete$lat)) # 성공
    
#4. 완성!! 최종 위경도 다 있는 것
    complete = final[!is.na(final$lat),]
    final = rbind( complete, final_NA_latlon_complete) # 13518
    # write.csv(final, "C:/Users/jeeyeon/Desktop/final_0601.csv",row.names=FALSE)
    
    
######## [floating_pop,living_pop,office_pop,female,income_grade,household_num] #########
### 조인을 위해 행정동명 수정
    library(lubridate)
    final = final %>% mutate(area_st_date = ymd(area_st_date), area_st_year = year(area_st_date),
                             adm_name = ifelse(gu=="강남구"& dong=="신사동","신사동_강",adm_name),
                             adm_name = ifelse(gu=="강남구"& dong=="압구정동","신사동_강",adm_name),
                             adm_name = ifelse(gu=="관악구"& dong=="신사동","신사동_관",adm_name),
                             adm_name = ifelse(gu=="관악구"& dong=="신림동","신사동_관",adm_name),
                             adm_name = gsub("제1동","1동",adm_name), adm_name = gsub("제2동","2동",adm_name),
                             adm_name = gsub("제3동","3동",adm_name), adm_name = gsub("제4동","4동",adm_name),
                             adm_name = gsub("제5동","5동",adm_name), adm_name = gsub("제6동","6동",adm_name),
                             adm_name = gsub("제7동","7동",adm_name), adm_name = gsub("제8동","8동",adm_name),
                             adm_name = gsub("홍1동","홍제1동",adm_name), adm_name = gsub("홍2동","홍제2동",adm_name),
                             adm_name = gsub("홍3동","홍제3동",adm_name), 
                             adm_name = gsub("제3.8동","3.8동",adm_name))
    #행정동명 수정없이 조인했더니 오류, 아래와 같은 과정을 거친후 adm_name수정(위코드보다아래를먼저수행)
    #write.csv(final, "C:/Users/jeeyeon/Desktop/final_0601.csv",row.names=FALSE)
    #nogada = final %>% filter(area_st_year !="2019")
    #nogada = nogada[is.na(nogada$floating_pop),]
    #write.csv(nogada, "C:/Users/jeeyeon/Desktop/nogada.csv",row.names=FALSE)
    nogada = read.csv("C:/Users/jeeyeon/Desktop/data/DM/nogada.csv") # 공백으로 들어있는거 대체
    
    na_table = final %>% filter(is.na(adm_name))  # NA 테이블 
    na_table[1,"adm_name"] = "논현1동" 
    na_table[2,"adm_name"] = "삼성1동" 
    na_table[3,"adm_name"] = "삼성1동" 
    complete = final %>% filter(str_length(adm_name) != 0) %>% filter(!is.na(adm_name)) # 완벽한 table
    final = rbind(complete,nogada,na_table)# 완성
    
### 소재지시작일 이전 분기값 넣기 위해 year_quater_seq 생성
    final =final %>%  mutate(days = ifelse(is.na(close_date), as.Date("2019-05-23")-as.Date(area_st_date), 
                                           as.Date(close_date)-as.Date(area_st_date)),
                             month = month(area_st_date),
                             quarter = ifelse(month %in% c(1,2,3),1,
                                              ifelse(month %in% c(4,5,6),2,
                                              ifelse(month %in% c(7,8,9),3,4))))
    df_seq = data.frame(year_quater=paste(final$area_st_year,final$quarter,sep="-")) %>% unique()
    df_seq = data.frame(year_quater=sort(df_seq$year_quater)) %>% 
      mutate(year_quater_seq = 1:nrow(df_seq))
    
    final = final %>% mutate(year_quater = as.factor(paste(area_st_year,quarter,sep="-")))
    final = left_join(final, df_seq, by = "year_quater")
    final = final %>% mutate(year_quater_seq_pre1 = year_quater_seq-1)

### DM_pop 유동인구,거주인구,직장인구(단위:동)
    DM_pop = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_pop_ver3.csv")
    names(DM_pop) = c("adm_name","floating_pop","living_pop","office_pop","area_st_year","quarter") 
    DM_pop = DM_pop %>% mutate(adm_name = as.character(adm_name),
                               year_quater=paste(area_st_year,quarter,sep="-"))
    DM_pop = left_join(DM_pop, df_seq, by = "year_quater")
    DM_pop = DM_pop %>% mutate(year_quater_seq_pre1 = year_quater_seq-1)
    DM_pop = DM_pop %>% select(adm_name,year_quater_seq_pre1,floating_pop,living_pop,office_pop)
    
### DM_pop 여성인구비율
    DM_female = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_female.csv")
    names(DM_female)= c("date","gu","adm_name","female")
    DM_female = DM_female %>% mutate(area_st_year = substr(date,1,4),quarter = substr(date,5,5)) %>% select(-date)
    DM_female = DM_female %>% mutate(adm_name = as.character(adm_name),
                                     adm_name = ifelse(gu=="강남구"&adm_name=="신사동","신사동_강",
                                                       ifelse(gu=="관악구"&adm_name=="신사동","신사동_관",adm_name)),
                               year_quater=paste(area_st_year,quarter,sep="-"))
    
    DM_female = left_join(DM_female, df_seq, by = "year_quater")
    DM_female = DM_female %>% mutate(year_quater_seq_pre1 = year_quater_seq-1) %>% 
              filter(year_quater != "2014-4")
    DM_female = DM_female %>% select(adm_name,year_quater_seq_pre1,female)
    
#### DM_income 소득분위,가구수,연도(단위:동)
    DM_income= read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_income_ver3.csv")
    names(DM_income) = c("adm_name","income_grade","household_num","area_st_year","quarter") 
    DM_income = DM_income %>% mutate(adm_name = as.character(adm_name))
    DM_income = DM_income %>% mutate(adm_name = as.character(adm_name),
                               year_quater=paste(area_st_year,quarter,sep="-"))
    DM_income = left_join(DM_income, df_seq, by = "year_quater")
    DM_income = DM_income %>% mutate(year_quater_seq_pre1 = year_quater_seq-1)
    DM_income = DM_income %>% select(adm_name,year_quater_seq_pre1,income_grade,household_num)
    
#### table join
    final1 = left_join(final, DM_pop, by = c("adm_name","year_quater_seq_pre1"))
    final1 = left_join(final1, DM_female, by = c("adm_name","year_quater_seq_pre1"))
    final1 = left_join(final1, DM_income, by = c("adm_name","year_quater_seq_pre1"))

 
######## [parking_lot,crosswalk,subway,blog] ######## 
#### 데이터
    # 주차장
    DM_parking_lot = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_parking_lot.csv")
    names(DM_parking_lot) = c("parking_name","ad_name","lat","lon")
    
    # 횡단보도
    DM_crosswalk = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_crosswalk.csv")
    DM_crosswalk = DM_crosswalk %>% select("지번","위도","경도")
    names(DM_crosswalk) = c("ad_name","lat","lon")
    
    # 지하철
    subway_blog = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_subway_blog.csv")  
    subway_blog_latlon <- read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_subway_blog_latlon.csv")  
    
#### 변수생성 
    f <- function(x, d){ sum(x < d) }
    
    ## 반경 500m 내 주차장 수
    table1 = final1
    table2 = filter(DM_parking_lot[complete.cases(DM_parking_lot),], !duplicated(ad_name)) #
    dist = distm(cbind(table1$lon, table1$lat), cbind(table2$lon, table2$lat), fun=distHaversine)
    final1$parking_lot = apply(dist, 1, f, 500) # final에 row수만큼
    
    ## 반경 50m 내 횡단보도수
    table3 = filter(DM_crosswalk[complete.cases(DM_crosswalk),], !duplicated(ad_name)) #
    dist = distm(cbind(table1$lon, table1$lat), cbind(table3$lon, table3$lat), fun=distHaversine)
    final1$crosswalk = apply(dist, 1, f, 50) # final에 row수만큼
    
    ## 반경 1000m 내 지하철역 수
    table4 <- subway_blog_latlon
    dist <- distm(cbind(table1$lon, table1$lat), cbind(table4$lon, table4$lat), fun=distHaversine)
    final1$subway <- apply(dist, 1, f, 1000)
    
    ## 반경 500m 내 지하철역 카페 블로그 건수
    f2 <- function(x, d){ as.character(subway_blog_latlon[x < d, "station"]) }
    stations <- apply(dist, 1, f2, 1000)  # 반경 1000m 내 지하철역 이름 추출
    blog <- c()
    for(i in 1:nrow(final1)){
      Xmonth <- ymd(final1$area_st_date[i])-months(1)
      f <- filter(subway_blog, year==year(Xmonth) & month==month(Xmonth) & station %in% stations[[i]])
      blog[i] <- ifelse(length(stations[[i]])==0, 0, sum(f$blog))
    }
    final1$blog = blog
    # write.csv(final1, "C:/Users/jeeyeon/Desktop/final_0606.csv",row.names=FALSE)

    
######## [floor,totalfloor] 결측채우기 ######## 
    ms = read.csv("C:/Users/jeeyeon/Desktop/data/DM/final_floor0607.csv") %>% unique()
     # 파이썬으로 미리 채워 놓은 파일
    
#### floor 결측 채우기
    floor_miss = ms %>% filter(str_length(floor) == 0|is.na(floor))
    for_data = floor_miss
    
    for(i in 1:nrow(for_data)){
      # 지하 포함된경우(특히 지상도 포함된경우는 지하+지상으로,아니면지하)
      if(grepl("지하|지층",gsub("\\s","",for_data$ad_name[i]))|grepl("지하|지층",gsub("\\s","",for_data$road_name[i]))){
        if(grepl("지상",gsub("\\s","",for_data$ad_name[i]))|grepl("지상",gsub("\\s","",for_data$road_name[i]))){
          for_data$floor[i] ="지하+지상"
        }
        else{for_data$floor[i] = '지하'}}
      
      # 지하,지층 포함되지않은경우 1층이 포함되어있다면(만약1~이런게포함되어있으면 1층+1층이상으로 아니면 1층)
      else if(grepl("1층|1~2층|1~3층|1~4층|1~5층|1-2층|1.2층",gsub("\\s","",for_data$ad_name[i]))|
              grepl("1층|1~2층|1~3층|1~4층|1~5층|1-2층|1.2층",gsub("\\s","",for_data$road_name[i]))){ #'지하'없는1층은 지상1층이니까
        if(grepl("1~2층|1~3층|1~4층|1~5층|1-2층|1.2층",gsub("\\s","",for_data$ad_name[i]))|
           grepl("1~2층|1~3층|1~4층|1~5층|1-2층|1.2층",gsub("\\s","",for_data$road_name[i]))){
          for_data$floor[i] ="1층+2층이상"
        }
        else{for_data$floor[i] ="1층"}
      }
      
      # 지하,지층포함안되있고 1층도 포함안됨->2층이상으로 분리
      else if(str_length(for_data$floor[i]) == 0){
        for_data$floor[i] = "2층이상"
      }
    }
    nrow(for_data %>% filter(str_length(floor) == 0))
    table(for_data$floor)
    
    complete = ms %>% filter(str_length(floor) != 0)
    final_ms = rbind( complete, for_data) 


#### total_floor 결측 채우기
    totalfloor_miss = final_ms %>% filter(is.na(total_floor))
    for_data = totalfloor_miss
    
    for(i in 1:nrow(for_data)){
      if(grepl("1~2층|1-2층|1.2층",gsub("\\s","",for_data$ad_name[i]))|
         grepl("1~2층|1-2층|1.2층",gsub("\\s","",for_data$road_name[i]))){
        
        for_data$total_floor[i] =2
      }
      else if(grepl("1~3층",gsub("\\s","",for_data$ad_name[i]))|
              grepl("1~3층",gsub("\\s","",for_data$road_name[i]))){
        for_data$total_floor[i] =3
      }
      else if(grepl("1~4층",gsub("\\s","",for_data$ad_name[i]))|
              grepl("1~4층",gsub("\\s","",for_data$road_name[i]))){
        for_data$total_floor[i] =4
      }
      else if(grepl("1~5층",gsub("\\s","",for_data$ad_name[i]))|
              grepl("1~5층",gsub("\\s","",for_data$road_name[i]))){
        for_data$total_floor[i] =5
      }
      else{for_data$total_floor[i] =1}
    }
    nrow(for_data %>% filter(is.na(total_floor)))
    table(for_data$total_floor)
    
    complete = final_ms %>% filter(!is.na(total_floor))
    final_ms = rbind(complete, for_data)

# 이제까지 겹치는거 있었는데 unique한것만 뽑기로
  #total_floor 
    final1_complete = final1 %>% filter(!is.na(total_floor)) 
    
    final1_not_complete = final1 %>% filter(is.na(total_floor)) 
    final1_not_complete = left_join(final1_not_complete %>% select(-total_floor),
                                    final_ms %>% select(road_name,total_floor) %>% unique(),
                                    by=c("road_name"))#total_floor missing 채우기
    
    final_total_floor = rbind(final1_complete, final1_not_complete) 
    
  # floor 새로 만든거 채우기 
    final2 = left_join(final_total_floor,
                       final_ms %>% select(road_name,floor) %>% unique(),
                       by=c("road_name") )

######## [DM_rental_fee] 총임대료,1층임대료,나머지층임대료(단위:동) ################
    DM_rental_fee = read.csv("C:/Users/jeeyeon/Desktop/data/DM/rental_fee_final.csv")
    names(DM_rental_fee) = c("adm_name","one_floor_fee","remain_floor_fee","area_st_year","quarter")
    DM_rental_fee = DM_rental_fee %>% mutate(adm_name = as.character(adm_name),
                                             area_st_year = as.numeric(area_st_year))
    DM_rental_fee = DM_rental_fee %>% mutate(adm_name = as.character(adm_name),
                               year_quater=paste(area_st_year,quarter,sep="-"))
    DM_rental_fee = left_join(DM_rental_fee, df_seq, by = "year_quater")
    DM_rental_fee = DM_rental_fee %>% mutate(year_quater_seq_pre1 = year_quater_seq-1)
    DM_rental_fee = DM_rental_fee %>% select(adm_name,year_quater_seq_pre1,
                                             one_floor_fee,remain_floor_fee)
    DM_rental_fee = DM_rental_fee %>% gather(floor,rental_fee,3:4)
    one_rental_fee = DM_rental_fee %>% filter(floor=="one_floor_fee") %>% 
      select(adm_name,year_quater_seq_pre1,rental_fee)
    remain_rental_fee = DM_rental_fee %>% filter(floor=="remain_floor_fee") %>% 
      select(adm_name,year_quater_seq_pre1,rental_fee)
    
    mysubset1 = final2 %>% filter(floor %in% c("1층","1층+2층이상","지하+지상"))
    mysubset1 = left_join(mysubset1,one_rental_fee, by = c("adm_name","year_quater_seq_pre1"))
    mysubset2 = final2 %>% filter(!floor %in% c("1층","1층+2층이상","지하+지상"))
    mysubset2 = left_join(mysubset2,remain_rental_fee, by = c("adm_name","year_quater_seq_pre1"))
    
final3 = rbind(mysubset1,mysubset2)

######## 'gu' 도로명주소로부터 없던거 지번주소로 채우기 ########
    notin_gu = final3 %>% filter(str_length(gu)==0)
    split_road = str_split(notin_gu$ad_name, " ", n = 3)
    gu = c()
    for (i in 1: nrow(notin_gu)){
      gu[i]=split_road[[i]][2]
    }
    notin_gu$gu = gu
    
    final3 = rbind(final3 %>% filter(str_length(gu)!=0), notin_gu)
    table(final3$gu) #good !!


######## [fac,gov,bank,gen_hosp,hospi,medical,kinder,element,...] ####### 
# match1 : adm_code(행정동코드), adm_name(행정동명)
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
    
    # fac1 : 상권 배후지 최종목표 집객시설 데이터 
    fac1 = read.csv("C:/Users/jeeyeon/Desktop/data/DM/FAC_fac_broad.csv")
    names(fac1) = c("years","gubun1","gubun2","code","code_name","fac","gov","bank",
                    "gen_hosp","hospi","medical",
                    "kinder","element","middle","high","univer",
                    "depart","super","cinema","accom","airport",  
                    "rail","bus_term","subway","bus_stop","bungi")
    fac1 = fac1 %>% select(-gubun1,-gubun2,-subway,-bungi,-code_name)
    fac1 = fac1 %>% group_by(code) %>% 
      summarize(fac=mean(fac,na.rm=T),gov=mean(gov,na.rm=T),bank=mean(bank,na.rm=T),
                gen_hosp=mean(gen_hosp,na.rm=T),hospi=mean(hospi,na.rm=T),medical=mean(medical,na.rm=T),
                kinder=mean(kinder,na.rm=T),element=mean(element,na.rm=T),middle=mean(middle,na.rm=T),
                high=mean(high,na.rm=T),univer=mean(univer,na.rm=T),
                depart=mean(depart,na.rm=T),super=mean(super,na.rm=T),cinema=mean(cinema,na.rm=T),
                accom=mean(accom,na.rm=T),airport=mean(airport,na.rm=T),
                rail=mean(rail,na.rm=T),bus_term=mean(bus_term,na.rm=T),bus_stop=mean(bus_stop,na.rm=T))
    for(i in 2:ncol(fac1)){ fac1[is.na(fac1[,i]), i] = 0}
    
    # match : adm_code(행정동코드), adm_name(행정동명), code_name(상권코드명)
    match2_fac1 = left_join(match2,fac1, by="code")
    match2_fac1 = match2_fac1 %>% group_by(adm_code) %>% 
      summarize(fac=sum(fac,na.rm=T),gov=sum(gov,na.rm=T),bank=sum(bank,na.rm=T),
                gen_hosp=sum(gen_hosp,na.rm=T),hospi=sum(hospi,na.rm=T),medical=sum(medical,na.rm=T),
                kinder=sum(kinder,na.rm=T),element=sum(element,na.rm=T),middle=sum(middle,na.rm=T),
                high=sum(high,na.rm=T),univer=sum(univer,na.rm=T),
                depart=sum(depart,na.rm=T),super=sum(super,na.rm=T),cinema=sum(cinema,na.rm=T),
                accom=sum(accom,na.rm=T),airport=sum(airport,na.rm=T),
                rail=sum(rail,na.rm=T),bus_term=sum(bus_term,na.rm=T),bus_stop=sum(bus_stop,na.rm=T))
    
    match1_match2_fac1 = left_join(match1, match2_fac1,by="adm_code" ) %>% unique()
    
    match1_match2_fac1 = match1_match2_fac1 %>% group_by(adm_name) %>% 
      summarize(fac=sum(fac,na.rm=T),gov=sum(gov,na.rm=T),bank=sum(bank,na.rm=T),
                gen_hosp=sum(gen_hosp,na.rm=T),hospi=sum(hospi,na.rm=T),medical=sum(medical,na.rm=T),
                kinder=sum(kinder,na.rm=T),element=sum(element,na.rm=T),middle=sum(middle,na.rm=T),
                high=sum(high,na.rm=T),univer=sum(univer,na.rm=T),
                depart=sum(depart,na.rm=T),super=sum(super,na.rm=T),cinema=sum(cinema,na.rm=T),
                accom=sum(accom,na.rm=T),airport=sum(airport,na.rm=T),
                rail=sum(rail,na.rm=T),bus_term=sum(bus_term,na.rm=T),bus_stop=sum(bus_stop,na.rm=T))
    
    
final4_1 = left_join(final3,match1_match2_fac1,by="adm_name")

temp = final4_1[is.na(final4_1$bus_stop),]
table(temp$adm_name)
    
      # near관련 오래걸림 주의 
######## [near,nearcafe,near_new_cafe,near_franchise] #######
      # '로' 없는거 채우기위해  
      #final_miss_ro = final4 %>% filter(str_length(ro)==0) 
      # write.csv(final_miss_ro, "C:/Users/jeeyeon/Desktop/final_miss_ro.csv",row.names=FALSE)
      final_ro_perfect = final4_1 %>% filter(str_length(ro)!=0) 
      final_ro_not_perfect  = final4_1 %>% filter(str_length(ro)==0) %>% select(-ro)
      final_miss_ro = read.csv("C:/Users/jeeyeon/Desktop/data/DM/final_miss_ro_complete.csv") %>% 
        select(ro,name) %>% unique()
      final_ro_not_perfect = left_join(final_ro_not_perfect, final_miss_ro,by="name")
      
      final5 = rbind(final_ro_perfect, final_ro_not_perfect) 
      
      final5 = final5 %>% unique()
  #### ro2 생성하기 
      final5$area_st_date<-as.Date(final5$area_st_date)
      
      month<-c("20150101","20150201","20150301","20150401","20150501","20150601",
               "20150701","20150801","20150901","20151001","20151101","20151201",
               "20160101","20160201","20160301","20160401","20160501","20160601",
               "20160701","20160801","20160901","20161001","20161101","20161201",
               "20170101","20170201","20170301","20170401","20170501","20170601",
               "20170701","20170801","20170901","20171001","20171101","20171201",
               "20180101","20180201","20180301","20180401","20180501","20180601",
               "20180701","20180801","20180901","20181001","20181101","20181201",
               "20190101","20190201","20190301","20190401","20190501")
      month<-as.Date(month, "%Y%m%d")
      
      
      ## 원 데이터 도로명 주소 바꾸기 --> ro2로
      ## ro2는 좀더 일반화된 도로명주소 (예: 신촌역로 12길 -> 신촌역로)
      ## grep(b, a) 비가 에이의 리스트들중에 어디에 포함되는지!
      
      final5$ro2<-as.character(final5$ro)
      #sum((final5$ro2)=="")
      #a<-sort(unique(final5$ro2))
      #View(a)
      
      final5$ro2[final5$ro2=="공원로"]<-"그냥공원로"
      final5$ro2[final5$ro2=="공원로7길"]<-"그냥공원로"
      final5$ro2[final5$ro2=="공원로8길"]<-"그냥공원로"
      final5$ro2[final5$ro2=="공원로8길"]<-"그냥공원로"
      final5$ro2[final5$ro2=="공원로8길"]<-"그냥공원로"
      
      final5$ro2[final5$ro2=="중앙로"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로10길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로15길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로1길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로29길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로36길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로3길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로51길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로6길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로8길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙로32길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙2길"]<-"그냥중앙로"
      final5$ro2[final5$ro2=="중앙길"]<-"그냥중앙로"
      
      
      for(i in 1:nrow(final5)){
        final5[grep(final5$ro2[i], final5$ro2),"ro2"]<-final5$ro2[i]
        if(i%%100==0){
          print(i/100)
        }
      }
      
      final5$ro2<-gsub('[0-9]+', '', final5$ro2)
      
      final5$ro2[which(final5$ro2==".로")]<-"4.19로"
      final5$ro2[which(final5$ro2=="로")]<-"63로"
      final5$ro2[which(final5$ro2=="종로")]<-"그냥종로"
      final5$ro2<-as.character(final5$ro2)
      final5$ro2<-substr(final5$ro2, 1, nchar(final5$ro2)-1)
      
      for(i in 1:nrow(final5)){
        final5[grep(final5$ro2[i], final5$ro2),"ro2"]<-final5$ro2[i]
        if(i%%100==0){
          print(i/100)
        }
      }
      #a<-sort(unique(final5$ro2))
      #View(a)
      
      
  #### 새로운 near등의 변수 채우기
      near<-read.csv("C:/Users/jeeyeon/Desktop/data/DM/near_final_3.csv")
      cafe<-read.csv("C:/Users/jeeyeon/Desktop/data/DM/nearcafe_0615.csv")
      franchise<-read.csv("C:/Users/jeeyeon/Desktop/data/DM/nearfranchise_0615.csv")
      starbucks = read.csv("C:/Users/jeeyeon/Desktop/data/DM/nearstarbucks_0615.csv")
    
      final5$ro2<-as.character(final5$ro2)
      final5$near<-0
      final5$nearcafe<-0
      final5$near_franchise<-0
      final5$near_starbucks<-0
      
      for(i in 1:nrow(final5)){
        if(paste(substr(final5$area_st_date[i],1,8),"01",sep="")==month[1]){
          colnum<-2  # 2015년 1월 이면 그냥 넣기 위해서
        }
        else{
          colnum<-which(month==paste(substr(final5$area_st_date[i],1,8),"01",sep="")) #이전 달
        }
        
        rownum1<-ifelse(which(near$ro==final5$ro2[i]),which(near$ro==final5$ro2[i]),0)
        rownum2<-ifelse(which(cafe$ro==final5$ro2[i]),which(cafe$ro==final5$ro2[i]),0)
        rownum3<-ifelse(which(franchise$ro==final5$ro2[i]),which(franchise$ro==final5$ro2[i]),0)
        rownum4<-ifelse(which(starbucks$ro==final5$ro2[i]),which(starbucks$ro==final5$ro2[i]),0)
        
        if(length(rownum1)>0){
          final5$near[i]<-near[rownum1,colnum]
        }
        if(length(rownum2)>0){
          final5$nearcafe[i]<-cafe[rownum2,colnum]
        }
        if(length(rownum3)>0){
          final5$near_franchise[i]<-franchise[rownum3,colnum]
        }
        if(length(rownum4)>0){
          final5$near_starbucks[i]<-starbucks[rownum4,colnum]
        }
        if(i%%100==0){
          print(i/100)
        }
      }
      #View(final5)

  #### 이전달에 개업한 카페 수
      library(geosphere)
      
      # pre month 
      final5 = final5 %>% select(-corp_st_date,-corp_name) %>% unique()
      
      final5$pre<-as.Date(final5$area_st_date)
      for(i in 1:nrow(final5)){
        d<-paste(substr(final5$area_st_date[i],1,8),"01",sep="")
        if(d==month[1]){
          final5$pre[i]<-as.Date(month[1], "%Y-%m-%d")
        }
        else{
          final5$pre[i]<-as.Date(month[which(month==paste(substr(final5$area_st_date[i],1,8),"01",sep=""))-1],"%Y-%m-%d")
        }
      }
      
      temp<-c()
      f <- function(x, d){ sum(x < d) }
      
      for(i in 1:nrow(final5)){
        mypre<-final5[i,"pre"]
        table1<-final5%>%filter(pre==mypre)
        dist<-distm(cbind(final5[i,"lon"], final5[i,"lat"]), cbind(table1$lon, table1$lat), fun=distHaversine)
        temp[i]<-apply(dist, 1, f, 500) 
        
        if(i%%100==0){
          print(i/100)
        }
      }
      
      summary(temp-1)
      final5$near_new_cafe<-temp-1

      #write.csv(final5,"C:/Users/jeeyeon/Desktop/final_visual.csv",row.names=FALSE)
      
      
######## [kospi_lag1,kospi_lag2,kospi_lag3,kospi_lag4,kospi_dec] #######
    # 30일 미만 제외
      dat<-read.csv("C:/Users/jeeyeon/Desktop/data/DM/KOSPI.csv", header=T, na.strings="NA")
      dat$Date<-as.Date(dat$Date, "%Y-%m-%d")
      dat<-dat%>%mutate(year=year(Date), week=week(Date))
      dat$KOSPI<-as.numeric(as.character(dat$KOSPI)) #3개의 null값
      kospi<-dat%>%group_by(year, week)%>%summarise(kospi_avg=mean(KOSPI, na.rm=T))
      
      kospi<-kospi%>%unite(start_year_week, year, week)
      kospi<-kospi%>%mutate(kospi_avg_lag1=lag(kospi_avg),kospi_avg_lag2=lag(kospi_avg,2),
                            kospi_avg_lag3=lag(kospi_avg,3),kospi_avg_lag4=lag(kospi_avg,4))
      
   ###
      final6 = final5 %>% filter(days>30 & area_st_date > ymd("20150401"))
      final6 = final6 %>% mutate(start_year=year(area_st_date),start_week=week(area_st_date),
                        start_week = ifelse(start_week==53,52,start_week))
      final6 = final6 %>% unite(start_year_week, start_year, start_week)
      final6 = left_join(final6, kospi, by="start_year_week")
      
      final6 = final6 %>% 
        mutate(lag43 = ifelse(kospi_avg_lag4>kospi_avg_lag3,1,0),
               lag32 = ifelse(kospi_avg_lag3>kospi_avg_lag2,1,0),
               lag21 = ifelse(kospi_avg_lag2>kospi_avg_lag1,1,0),
               kospi_dec_dummy = paste(lag43,lag32,lag21,sep=""),
               kospi_dec = ifelse(kospi_dec_dummy=="111",3,
                                ifelse(kospi_dec_dummy=="011",2,
                                       ifelse(kospi_dec_dummy%in%c("101","001"),1,0))))
######## [total_sales] ###############
      
      sales = read.csv("C:/Users/jeeyeon/Desktop/data/DM/DM_sales.csv")
      sales = sales %>% select(1,2,5,9)
      names(sales) = c("year","quarter","code","total_sales")
      
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
      
      final6 = left_join(final6,match1_match2_sales,by=c("adm_name","year_quater_seq_pre1"))
      
      #write.csv(final6, "C:/Users/jeeyeon/Desktop/final6_0614.csv",row.names=FALSE)
      
#-------------------------------------------------------------- final_real

      
      
############################################################################ 
      
      final_final=final6
      
######## 추가작업 ######
    final_real = final6 %>% unique()
    
#### '행사용' 카페였던 경우를 제외하기
    final_real = final_real[-grep("행사", final_real$close_reason),]
    final_real = final_real[-grep("한시", final_real$close_reason),]
    final_real = final_real[-grep("단기", final_real$close_reason),]
    final_real = final_real[-grep("박람회", final_real$close_reason),]
    
#### [area_st_month,area_st_season,train_test_label,y_oneyear] 
    final_real = final_real %>% filter(area_st_date < ymd("20180523"))
    final_real = final_real %>% 
            mutate(area_st_month = month(area_st_date),
                   area_st_season = ifelse(area_st_month %in% c(3,4,5),"1",
                                           ifelse(area_st_month %in% c(6,7,8),"2",
                                                  ifelse(area_st_month %in% c(9,10,11),"3","4"))),
                   train_test_label = ifelse(area_st_date<ymd("20170430"),"train","test"),
                   y_oneyear =ifelse(days>365,0,1))
    
#### 자료형 변환
    final_real = final_real %>% mutate(floating_pop=as.numeric(gsub(",","",floating_pop)),
                                       living_pop = as.numeric(as.character(living_pop)),
                                       office_pop = as.numeric(gsub(",","",floating_pop)),
                                       income_grade = as.numeric(gsub("분위","",income_grade)),
                                       area_st_month = as.factor(area_st_month),
                                       area_st_season= as.factor(area_st_season),
                                       franchise = as.factor(franchise),
                                       near_franchise_pop = near_franchise/nearcafe,
                                       kospi_avg_lag34 = (kospi_avg_lag3-kospi_avg_lag4)/kospi_avg_lag4,
                                       kospi_avg_lag23 = (kospi_avg_lag2-kospi_avg_lag3)/kospi_avg_lag3,
                                       kospi_avg_lag12 = (kospi_avg_lag1-kospi_avg_lag2)/kospi_avg_lag2,
                                       kospi_avg_lag14 = (kospi_avg_lag1-kospi_avg_lag4)/kospi_avg_lag4,
                                       near_franchise_pop = ifelse(is.na(near_franchise_pop),0,near_franchise_pop))
    
### 최종자료처리 
    final_real = final_real[complete.cases(final_real$floating_pop),]
    final_real = final_real[complete.cases(final_real$fac),]
    
### floor , total_floor 
    final_real = final_real %>% select(-total_floor, -floor) 
    floor_re = read.csv("C:/Users/jeeyeon/Desktop/data/DM/floor_re.csv")
    floor_re = floor_re %>% select(road_name,ad_name,floor,total_floor) %>% unique()
    
    final_real = left_join(final_real,floor_re,by=c("road_name","ad_name"))
    
    
### outlier 처리!!!
    final_real<-final_real %>% filter(total_area<400)  %>% mutate(rental_fee = log(rental_fee))
    final = final_real
    
    ### 울타리 대체 변수들 : near, blog, depart, cinema
    # 1) near
    Q3<-as.numeric(summary(final$near)[5]); Q1<-as.numeric(summary(final$near)[2])
    fence<-Q3+1.5*(Q3-Q1)
    final[which(final$near>fence),"near"]<-fence
    # 2) blog
    Q3<-as.numeric(summary(final$blog)[5]); Q1<-as.numeric(summary(final$blog)[2])
    fence<-Q3+1.5*(Q3-Q1)
    final[which(final$blog>fence),"blog"]<-fence
    # 3) depart
    Q3<-as.numeric(summary(final$depart)[5]); Q1<-as.numeric(summary(final$depart)[2])
    fence<-Q3+1.5*(Q3-Q1)
    final[which(final$depart>fence),"depart"]<-fence
    # 4) cinema
    Q3<-as.numeric(summary(final$cinema)[5]); Q1<-as.numeric(summary(final$cinema)[2])
    fence<-Q3+1.5*(Q3-Q1)
    final[which(final$cinema>fence),"cinema"]<-fence
    
    ### 딱 한개의 동떨어진 값 있는 변수 들 : kinder, bus stop
    # 1) kinder
    Q3<-as.numeric(summary(final$kinder)[5]); Q1<-as.numeric(summary(final$kinder)[2])
    fence<-Q3+1.5*(Q3-Q1)
    final[which(final$kinder>150),"kinder"]<-fence
    # 2) bus stop
    Q3<-as.numeric(summary(final$bus_stop)[5]); Q1<-as.numeric(summary(final$bus_stop)[2])
    fence<-Q3+1.5*(Q3-Q1)
    final[which(final$bus_stop>250),"bus_stop"]<-fence
    
    final_real = final
    
    
    write.csv(final_real, "C:/Users/jeeyeon/Desktop/final_real_0617_outlier.csv",row.names=FALSE)
    
    
######## 최종 사용 변수선택 ########
    final_real_select = final_real %>% 
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
                super,cinema,accom,rail,bus_term,bus_stop)
    
    write.csv(final_real_select, "C:/Users/jeeyeon/Desktop/final_real_select_0617_outlier.csv",row.names=FALSE)
    
    
######## train vs test  #############
############################# BIGDATA CAMPUS COMPETETION ###################
    
    bigdata_all = read.csv("C:/Users/jeeyeon/Desktop/data/DM/final_real_0617_outlier.csv")
    bigdata = read.csv("C:/Users/jeeyeon/Desktop/data/DM/final_real_select_0617_outlier.csv")
    
######## 여성 직장인구 수 ->앞서 집객시설(fac) join table 가져와서 함 
    ofemale = read.csv("C:/Users/jeeyeon/Desktop/data/DM/female_office_pop_prop.csv")
    ofemale = ofemale[,-4]
    names(ofemale) = c("area_st_year","quarter","code","ofmale")
    
  #### step1
    match2_ofemale = left_join(match2,ofemale, by="code")
    match2_ofemale = match2_ofemale %>% group_by(adm_code,area_st_year,quarter) %>% 
      summarize(ofmale=sum(ofmale,na.rm=T))
    
    match1_match2_ofemale = left_join(match1, match2_ofemale,by="adm_code" ) %>% unique()
    match1_match2_ofemale = match1_match2_ofemale %>% group_by(adm_name) %>% 
      summarize(fac=sum(fac,na.rm=T))
    
    # final4_1 = left_join(final3,match1_match2_fac1,by="adm_name")
    # temp = final4_1[is.na(final4_1$bus_stop),]
    # table(temp$adm_name)
    
    
  #### step2 :분기별
    # join위해 필요한 seq 데이터셋 
    df_seq = data.frame(year_quater=paste(bigdata_all$area_st_year,bigdata_all$quarter,sep="-")) %>% unique()
    df_seq = data.frame(year_quater=sort(df_seq$year_quater)) %>% 
      mutate(year_quater_seq = 1:nrow(df_seq))
    
    names(DM_female)= c("date","gu","adm_name","female")
    DM_female = DM_female %>% mutate(area_st_year = substr(date,1,4),quarter = substr(date,5,5)) %>% select(-date)
    DM_female = DM_female %>% mutate(adm_name = as.character(adm_name),
                                     adm_name = ifelse(gu=="강남구"&adm_name=="신사동","신사동_강",
                                                       ifelse(gu=="관악구"&adm_name=="신사동","신사동_관",adm_name)),
                                     year_quater=paste(area_st_year,quarter,sep="-"))
    
    
    DM_female = left_join(DM_female, df_seq, by = "year_quater")
    DM_female = DM_female %>% mutate(year_quater_seq_pre1 = year_quater_seq-1) %>% 
      filter(year_quater != "2014-4")
    DM_female = DM_female %>% select(adm_name,year_quater_seq_pre1,female)
    
    
    
    
    
    
    
    
    
######## pca ######## 
    dataset = scale(bigdata[,41:56])
    
    library(corrplot)
    # correlation matrix --------------------------------------------------# 
    M = dataset[complete.cases(dataset),]
    M = cor(M)
    corrplot(M, type="upper",  tl.col="black", tl.srt=45, method="number")
    # --------------------------------------------------------------------# 
    
    PCA1 = prcomp(dataset,center = T, scale. = T)             # method1  ->PCA$rotation[,1]  #비정칙치분해 이용 주성분 구함
    # x들 마다 분산은 유의하게 다르기 때문에 scale = TRUE 옵션을 사용 
    PCA2 = principal(dataset, nfactors = 2, rotate="varimax") # method2  ->PCA$communality 
    FACTOR = factanal(dataset, factors = 1, rotation = "varimax")   #    ->FACTOR$loadings[,1]
    
    summary(PCA1)
    plot(PCA1,type = "l")
    PCA1$rotation[,1]
    PCA2$communality 
    FACTOR$loadings[,1]
    biplot(PCA1)
    biplot(PCA2)

############################################################################     
    
    data = read.csv("C:/Users/jeeyeon/Desktop/폐업률_상위하위_10퍼.csv")

    ggplot(data, aes(x=blog))+geom_histogram(fill="white", colour="black")+
      facet_grid(close~.)
    
    
    ndata = select_if (data , is.numeric)
    
for (i in 3:(ncol(ndata)-1)){
  i=ggplot(ndata, aes(x=ndata[,i]))+geom_histogram(fill="white", colour="black")+
    facet_grid(close~.)}
  print(i)
  # upper, lower 하는 이유가 
  # imbalance [0,1]로 
  # cutoff :
  
  
  # 입지제안
  # 입시 선정 했을 때, 그 곳의 위험률을 제안 가능
  
  ## 



  
  
  
  
  