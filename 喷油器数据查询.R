library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(flexdashboard)
library(RMySQL)
library(magrittr)
library(xlsx)
library(knitr)
library(data.table)
library(formattable)
library(RODBC)  
library(tibble)
library(bookdownplus)


#查询最近数据，数据排版

con <- dbConnect(MySQL(),user='recedit',password='8409',dbname='qualidata',host="172.16.67.163",DBMSencoding="UTF8") #连接数据库
channel<-readLines("D:\\xianghp\\common_rail_injector\\test_r.sql")#读取sql命令
result<-dbGetQuery(con, paste(channel,collapse=""))#读取mysql数据
dbDisconnect(con) #断开连接


con <- dbConnect(MySQL(),user='recedit',password='8409',dbname='qualidata',host="localhost") #连接数据库
channel<-readLines("C:\\Documents and Settings\\Administrator\\Desktop\\QueryDbFiles\\test_r.sql")#读取sql命令
result<-dbGetQuery(con, paste(channel,collapse=""))#读取mysql数据
dbDisconnect(con) #断开连接


con <- dbConnect(MySQL(),user='root',password='643100xhp',dbname='qualidata',host="localhost") #连接数据库
channel<-readLines("D:\\xianghp\\common_rail_injector\\test_r.sql")#读取sql命令
result<-dbGetQuery(con, paste(channel,collapse=""))#读取mysql数据

con <- dbConnect(MySQL(),user='root',password='643100xhp',dbname='qualidata',host="localhost") #连接数据库
channel<-readLines("D:\\xianghp\\common_rail_injector\\test_r.sql")#读取sql命令
result<-dbGetQuery(con, paste(channel,collapse=""))#读取mysql数据library(Rserve)


summary(result)
dbSendQuery(conn,'SET NAMES gbk') 
Encoding(result) <- "UTF-8" ##转换格式为UTF-8
result$recipe_name
result_volume$recipe_name



result_volume <- result %>%
  filter(qualification_date_time > "2018-01-18 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('^045',rownames(table(result$injector_number)),value = TRUE))%>%
  select(qualification_date_time,injector_number,rail_pressure_cons,pulse_length,aver_inj_volume,bench_channel,recipe_name)%>%  
  unite(pressure_pulse,rail_pressure_cons,pulse_length,sep ="_")%>%  
  spread(pressure_pulse,aver_inj_volume)
write.csv(result_volume, file = "C:\\Users\\xianghp\\Desktop\\最近一周油量.csv")#保存数据csv格 







result_volume <- result %>%
  filter(qualification_date_time > "2019-05-07 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('^021',rownames(table(result$injector_number)),value = TRUE))%>%
  #filter(injector_number %in% c("906 83040024" ,"906 83040218", "906 830887JZ" ,"906 830888JZ"))%>%
  #filter(injector_number %in% c("909 86090186" ,"909 86010051", "909 86050074" ,"909 86040176"))%>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  # filter(recipe_name %in% grep('^ncr905i',rownames(table(result$recipe_name)),value = TRUE))%>%
  #filter(recipe_name == "ncr905i QT棰勫垎閫<89>",value = TRUE)%>%
  select(qualification_date_time,injector_number,rail_pressure_cons,pulse_length,aver_inj_volume,bench_channel,recipe_name)%>%  
  unite(pressure_pulse,rail_pressure_cons,pulse_length,sep ="_")%>%  
  spread(pressure_pulse,aver_inj_volume)
write.csv(result_volume, file = "C:\\Users\\xianghp\\Desktop\\喷油器021油量.csv")#保存数据csv格 




result_volume <- result %>%
  filter(qualification_date_time>"2018-04-20 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('^905',rownames(table(result$injector_number)),value = TRUE))%>%
  #filter(injector_number %in% c("906 83040024" ,"906 83040218", "906 830887JZ" ,"906 830888JZ"))%>%
  filter(injector_number %in% c("90584210534" ,"905 84210138", "905 84210809" ,"905 84210713","905 84210265","905 84210063",
                                "905 84210058"))%>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
 # filter(recipe_name %in% grep('^ncr905i',rownames(table(result$recipe_name)),value = TRUE))%>%
  #filter(recipe_name == "ncr905i QT棰勫垎閫<89>",value = TRUE)%>%
  select(qualification_date_time,injector_number,rail_pressure_cons,pulse_length,aver_inj_volume,bench_channel,recipe_name)%>%  
  unite(pressure_pulse,rail_pressure_cons,pulse_length,sep ="_")%>%  
  spread(pressure_pulse,aver_inj_volume)
  
  result_data <- result_volume %>% mutate(rowid = rowidv(result_volume,cols="injector_number")) %>%  
  mutate(registered = "TRUE")%>% 
    select("日期" = qualification_date_time,"喷油器编号" = injector_number,"250bar_1000us" = `250_1000`,"1000bar_250us"= `1000_250`,"1000bar_600us" = `1000_600`,"1600bar_2000us" =`1600_2000`,"缸号" = bench_channel, "测试次数" = rowid)

  write.csv(result_data, file = "C:\\Users\\xianghp\\Desktop\\喷油器909油量.csv")#保存数据csv格 
  
  
  
  
injector_data  

names(injector_data)
mode(injector_data)

DT::datatable(injector_data)

formattable(injector_data, list( 
  
 
  
  
 
   )) 



bar1000_us250 = color_tile("white", "orange")
registered = formatter( "span", style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))        

area(col = c(bar1000_us250)) ~ normalize_bar("pink", 0.2)
bar1000_250us = formatter("span",  style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),  x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))) ,

bar1000_250us = formatter( "span", style = x ~ ifelse( x == 2, style(color = "green", font.weight = "bold"), NA)),           



write.csv(injector_data, file = "C:\\Users\\xianghp\\Desktop\\喷油器基准件油量.csv")#保存数据csv格


#select(qualification_date_time,injector_number,rail_pressure_cons,pulse_length,aver_inj_volume,std_inj_volume,aver_opening_delay,std_opening_delay,aver_closing_delay,std_closing_delay,recipe_name,bench_channel)%>%


df <- data.frame(  
  id = 1:10,  
  name = c("Bob", "Ashley", "James", "David", "Jenny", "Hans", "Leo", "John", "Emily", "Lee"),   
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),  
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),  
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),  
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),  
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),  
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),  stringsAsFactors = FALSE)

df

DT::datatable(df)

formattable(df, list( 
  age = color_tile("white", "orange"),               
  grade = formatter( "span", style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA) ),            
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),             
  final_score = formatter("span",  style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),  x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),            
  registered = formatter( "span", style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) 
  )) 



DT::datatable(result_data)
row.names(table(result_data$injector_number))
names(result_data)

formattable(result_data, list( 
  bench_channel = color_tile("white", "orange"),   
  '1000_160_vol' = formatter( "span", style = x ~ style(color = ifelse(x >= 0.4 & x <= 6, "green", "red"), font.weight = "bold", NA)),
  '1600_1700_vol_max' = formatter( "span", style = x ~ ifelse(x == "TRUE", style(color = "green", font.weight = "bold"), NA) ),            
  area(col = c('1000_160_std' ,'1000_160_open', '1000_160_clos')) ~ normalize_bar("pink", 0.2),             
  judge_result = formatter( "span", style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) 
)) 



result_v <- result %>%
  filter(qualification_date_time>"2018-05-10 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('911',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(recipe_name %in% grep('^ncr911i',rownames(table(result$recipe_name)),value = TRUE))%>%
 filter(injector_number %in% c("911-bosch-boschvalve"  ,"911 bocsh DC1.07-2","911 bocsh DC1.07-3"))

result_911 <- result_v %>%  #南岳自制阀组件
  bind_rows(result_v %>% filter(injector_number == "911-bosch-boschvalve") %>% 
              mutate( injector_number = "911-bs-(348.97, 336.11, 1.038)"))%>%
  bind_rows(result_v %>% filter(injector_number == "911 bocsh DC1.07-2") %>% 
              mutate( injector_number = "911-ny-(342.51, 333.97, 1.026)"))%>%
  bind_rows(result_v %>% filter(injector_number == "911 bocsh DC1.07-3") %>% 
              mutate( injector_number = "911-ny-(363.62, 338.14, 1.075)"))%>%
  filter(injector_number %in% c("911-bs-(348.97, 336.11, 1.038)","911-ny-(342.51, 333.97, 1.026)","911-ny-(363.62, 338.14, 1.075)"))












result_v <- result %>%
  filter(qualification_date_time>"2018-05-10 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('911',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(recipe_name %in% grep('^ncr911i',rownames(table(result$recipe_name)),value = TRUE))%>%
  filter(injector_number %in% c("911-bosch-boschvalve"  ,"911 85110052","911-bosch-DC1.07","911-bosch-DC370-1.03"))
  
result_911 <- result_v %>%  
  bind_rows(result_v %>% filter(injector_number == "911-bosch-boschvalve") %>% 
  mutate( injector_number = "911-bs-(348.97, 336.11, 1.038)"))%>%
  bind_rows(result_v %>% filter(injector_number == "911 85110052") %>% 
  mutate( injector_number = "911-dc-(356.13, 345.90, 1.030)"))%>%
  bind_rows(result_v %>% filter(injector_number == "911-bosch-DC1.07") %>% 
  mutate( injector_number = "911-dc-(372.42, 346.85, 1.070)"))%>%
  bind_rows(result_v %>% filter(injector_number == "911-bosch-DC370-1.03") %>% 
  mutate( injector_number = "911-dc-(383.42, 370.29, 1.035)"))%>%
  filter(injector_number %in% c("911-bs-(348.97, 336.11, 1.038)","911-dc-(356.13, 345.90, 1.030)","911-dc-(372.42, 346.85, 1.070)","911-dc-(383.42, 370.29, 1.035)"))


result_021 <- result %>%
  filter(qualification_date_time>"2018-09-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('021',rownames(table(result$injector_number)),value = TRUE))
  #filter(recipe_name %in% grep('^ncr911i',rownames(table(result$recipe_name)),value = TRUE))%>%
  #filter(injector_number %in% c("911-bosch-boschvalve"  ,"911 85110052","911-bosch-DC1.07","911-bosch-DC370-1.03"))


result_014 <- result %>%
  filter(qualification_date_time>"2018-09-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^014',rownames(table(result$injector_number)),value = TRUE))
#filter(recipe_name %in% grep('^ncr911i',rownames(table(result$recipe_name)),value = TRUE))%>%
#filter(injector_number %in% c("911-bosch-boschvalve"  ,"911 85110052","911-bosch-DC1.07","911-bosch-DC370-1.03"))
write.csv(result_014, file = "C:\\Users\\xianghp\\Desktop\\014.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格



result_905 <- result %>%
  filter(qualification_date_time>"2018-11-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('905',rownames(table(result$injector_number)),value = TRUE))

result_907 <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('907',rownames(table(result$injector_number)),value = TRUE))

result_901 <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('901',rownames(table(result$injector_number)),value = TRUE))

result_903 <- result %>%
  filter(qualification_date_time>"2018-11-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('903',rownames(table(result$injector_number)),value = TRUE))

result_904 <- result %>%
  filter(qualification_date_time>"2018-07-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('904',rownames(table(result$injector_number)),value = TRUE))

result_911 <- result %>%
  filter(qualification_date_time>"2018-07-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('911',rownames(table(result$injector_number)),value = TRUE))

result_915JZ <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('915',rownames(table(result$injector_number)),value = TRUE))
write.csv(result_915JZ, file = "C:\\Users\\xianghp\\Desktop\\result_915JZ.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_915 <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('915',rownames(table(result$injector_number)),value = TRUE))
write.csv(result_915, file = "C:\\Users\\xianghp\\Desktop\\result_915.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格



result_932JZ <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('932',rownames(table(result$injector_number)),value = TRUE))



result_933JZ <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('933',rownames(table(result$injector_number)),value = TRUE))

result_909JZ <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('909',rownames(table(result$injector_number)),value = TRUE))

result_931JZ <- result %>%
  filter(qualification_date_time>"2018-03-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('931',rownames(table(result$injector_number)),value = TRUE))



result_935 <- result %>%
  filter(qualification_date_time>"2018-07-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('935',rownames(table(result$injector_number)),value = TRUE))

result_905zf <- result %>%
  filter(qualification_date_time>"2018-09-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^905',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% c("9057C010042"))
write.csv(result_905zf, file = "C:\\Users\\xianghp\\Desktop\\905zf.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_912luosai <- result %>%
  filter(qualification_date_time>"2018-05-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^912',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_912luosai, file = "C:\\Users\\xianghp\\Desktop\\912luosai.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


result_943 <- result %>%
  filter(qualification_date_time>"2019-05-09 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^943',rownames(table(result$injector_number)),value = TRUE))
  #filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_912luosai, file = "C:\\Users\\xianghp\\Desktop\\943.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


result_095 <- result %>%
  filter(qualification_date_time>"2019-09-01 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^095',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_095, file = "C:\\Users\\xianghp\\Desktop\\NCR095I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_096 <- result %>%
  filter(qualification_date_time>"2019-11-26 16:29:50") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^096',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_096, file = "C:\\Users\\xianghp\\Desktop\\NCR096I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


result_097 <- result %>%
  filter(qualification_date_time>"2019-11-26 08:40:10") %>%
  filter(qualification_date_time<"2019-11-26 08:40:18") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") 
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  #filter(injector_number %in% grep('^097',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_097, file = "C:\\Users\\xianghp\\Desktop\\NCR097I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


result_043 <- result %>%
  filter(qualification_date_time>"2018-01-01 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^043',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))


result_037 <- result %>%
  filter(qualification_date_time>"2019-01-01 08:00:00") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^037',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_037, file = "C:\\Users\\xianghp\\Desktop\\NCR037I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格



result_071 <- result %>%
  filter(qualification_date_time>"2020-03-15 14:19:19") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^071',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_071, file = "C:\\Users\\xianghp\\Desktop\\NCR071I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_960 <- result %>%
  filter(qualification_date_time>"2019-10-08 13:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^960',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_960, file = "C:\\Users\\xianghp\\Desktop\\NCR960I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_091 <- result %>%
  filter(qualification_date_time>"2020-07-17 13:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^091',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_091, file = "C:\\Users\\xianghp\\Desktop\\NCR091I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_933 <- result %>%
  filter(qualification_date_time>"2020-01-18 13:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(qualification_date_time  == "2020-01-19 11:20:04") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^933',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_933, file = "C:\\Users\\xianghp\\Desktop\\NCR933I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


result_075 <- result %>%
  filter(qualification_date_time>"2020-05-21 13:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  filter(qualification_date_time  == "2020-05-21 22:31:55") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^075',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_075, file = "C:\\Users\\xianghp\\Desktop\\NCR075I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_074 <- result %>%
  filter(qualification_date_time>"2020-09-01 10:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  #filter(qualification_date_time  == "2020-05-21 11:25:07") %>%
  #filter(rail_pressure_cons != "1250") %>%
  #filter(pulse_length != "1500") %>%
  #filter(pulse_length != "0") %>%
  #filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^074',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_074, file = "C:\\Users\\xianghp\\Desktop\\NCR074I-2020-9-16.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


result_620 <- result %>%
  filter(qualification_date_time>"2020-05-21 10:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  #filter(qualification_date_time  == "2020-05-21 11:25:07") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^620',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_620, file = "C:\\Users\\xianghp\\Desktop\\NCR620I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_680 <- result %>%
  filter(qualification_date_time>"2020-08-11 10:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  #filter(qualification_date_time  == "2020-05-21 11:25:07") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^680',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_680, file = "C:\\Users\\xianghp\\Desktop\\NCR680I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_017 <- result %>%
  filter(qualification_date_time>"2020-07-07 10:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  #filter(qualification_date_time  == "2020-05-21 11:25:07") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^017',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_017, file = "C:\\Users\\xianghp\\Desktop\\NCR017I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_944 <- result %>%
  filter(qualification_date_time>"2018-05-27 10:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  #filter(qualification_date_time  == "2020-05-21 11:25:07") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^944',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_944, file = "C:\\Users\\xianghp\\Desktop\\NCR944I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格

result_400 <- result %>%
  filter(qualification_date_time>"2020-09-12 10:14:05") %>%
  #filter(qualification_date_time<"2018-02-28 16:10:00") %>%
  #filter(qualification_date_time  == "2020-05-21 11:25:07") %>%
  filter(rail_pressure_cons != "1250") %>%
  filter(pulse_length != "1500") %>%
  filter(pulse_length != "0") %>%
  filter(pulse_length != "100") %>%
  #filter(injector_number %in% grep('JZ',rownames(table(result$injector_number)),value = TRUE))%>%
  filter(injector_number %in% grep('^400',rownames(table(result$injector_number)),value = TRUE))
#filter(injector_number %in% c("912 86110269","912 86110143","912 86130143"))
write.csv(result_400, file = "C:\\Users\\xianghp\\Desktop\\NCR400I.csv", fileEncoding = "UTF-16LE", row.names = TRUE)#保存数据csv格


#【】分析数据电装喷油器_960
#〈1〉脉宽 油量 折线图
ggplot(result_960,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR960I_volume")+scale_x_continuous(limits=c(0,2100),breaks=seq(0,2100,100))+scale_y_continuous(limits=c(0,180),breaks=seq(0,180,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR960I_volume.png"),width = 12, height = 8)
#〈2〉脉宽 关闭延时 折线图+轨压分布
ggplot(result_960,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR960I_closing_delay")+scale_x_continuous(limits=c(0,2200),breaks=seq(0,2200,200))+scale_y_continuous(limits=c(0,1800),breaks=seq(0,1800,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR960I_closing_delay.png"),width = 12, height = 6)
#〈3〉脉宽 开启延时 折线图+轨压分布
ggplot(result_960,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR960I_opening_delay")+scale_x_continuous(limits=c(0,2100),breaks=seq(0,2100,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR960I_opening_delay.png"),width = 12, height = 6)
#〈4〉脉宽 油量散差 折线图+轨压分布
ggplot(result_960,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR960I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR960I_std_inj_volume.png"),width = 12, height = 6)			

#【】分析数据电装喷油器_096
#〈1〉脉宽 油量 折线图
ggplot(result_096,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR096I_volume")+scale_x_continuous(limits=c(0,1300),breaks=seq(0,1300,100))+scale_y_continuous(limits=c(0,150),breaks=seq(0,150,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR096I_volume.png"),width = 12, height = 8)
#〈2〉脉宽 关闭延时 折线图+轨压分布
ggplot(result_096,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR096I_closing_delay")+scale_x_continuous(limits=c(0,1400),breaks=seq(0,1400,200))+scale_y_continuous(limits=c(0,1800),breaks=seq(0,1800,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR096I_closing_delay.png"),width = 12, height = 6)
#〈3〉脉宽 开启延时 折线图+轨压分布
ggplot(result_096,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR096I_opening_delay")+scale_x_continuous(limits=c(0,1400),breaks=seq(0,1400,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR096I_opening_delay.png"),width = 12, height = 6)
#〈4〉脉宽 油量散差 折线图+轨压分布
ggplot(result_096,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR096I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR096I_std_inj_volume.png"),width = 12, height = 6)			

#【】分析数据电装喷油器_097
#〈1〉脉宽 油量 折线图
ggplot(result_097,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR097I_volume")+scale_x_continuous(limits=c(0,1300),breaks=seq(0,1300,100))+scale_y_continuous(limits=c(0,150),breaks=seq(0,150,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR097I_volume.png"),width = 12, height = 8)
#〈2〉脉宽 关闭延时 折线图+轨压分布
ggplot(result_097,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR097I_closing_delay")+scale_x_continuous(limits=c(0,1400),breaks=seq(0,1400,200))+scale_y_continuous(limits=c(0,1800),breaks=seq(0,1800,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR097I_closing_delay.png"),width = 12, height = 6)
#〈3〉脉宽 开启延时 折线图+轨压分布
ggplot(result_097,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR097I_opening_delay")+scale_x_continuous(limits=c(0,1400),breaks=seq(0,1400,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR097I_opening_delay.png"),width = 12, height = 6)
#〈4〉脉宽 油量散差 折线图+轨压分布
ggplot(result_097,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR097I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR097I_std_inj_volume.png"),width = 12, height = 6)			



#【】分析数据电装喷油器_071
#〈1〉脉宽 油量 折线图
ggplot(result_071,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR071I_volume")+scale_x_continuous(limits=c(0,1000),breaks=seq(0,1000,100))+scale_y_continuous(limits=c(0,110),breaks=seq(0,110,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR071I_volume.png"),width = 12, height = 8)
#〈2〉脉宽 关闭延时 折线图+轨压分布
ggplot(result_071,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR071I_closing_delay")+scale_x_continuous(limits=c(0,2200),breaks=seq(0,2200,200))+scale_y_continuous(limits=c(0,1800),breaks=seq(0,1800,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR071I_closing_delay.png"),width = 12, height = 6)
#〈3〉脉宽 开启延时 折线图+轨压分布
ggplot(result_071,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR071I_opening_delay")+scale_x_continuous(limits=c(0,2100),breaks=seq(0,2100,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR071I_opening_delay.png"),width = 12, height = 6)
#〈4〉脉宽 油量散差 折线图+轨压分布
ggplot(result_071,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR071I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR071I_std_inj_volume.png"),width = 12, height = 6)			



#【15】分析数据泗洪阀组件_911
    #〈1〉脉宽 油量 折线图
    ggplot(result_911,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("博世_鼎诚阀组件_911_volume")+scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,100))+scale_y_continuous(limits=c(0,210),breaks=seq(0,210,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_鼎诚阀组件_911_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_911,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("博世_鼎诚阀组件_911_closing_delay")+scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_鼎诚阀组件_911_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_911,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("博世_鼎诚阀组件_911_opening_delay")+scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_鼎诚阀组件_911_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_911,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("博世_鼎诚阀组件_911_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_鼎诚阀组件_911_std_inj_volume.png"),width = 12, height = 6)			
    
    
#【16】分析数据南岳阀组件_911
    #〈1〉脉宽 油量 折线图
    ggplot(result_911,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("博世_南岳阀组件_911_volume")+scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,100))+scale_y_continuous(limits=c(0,210),breaks=seq(0,210,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_南岳阀组件_911_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_911,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("博世_南岳阀组件_911_closing_delay")+scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_南岳阀组件_911_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_911,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("博世_南岳阀组件_911_opening_delay")+scale_x_continuous(limits=c(0,1800),breaks=seq(0,1800,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_南岳阀组件_911_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_911,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("博世_南岳阀组件_911_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"博世_南岳阀组件_911_std_inj_volume.png"),width = 12, height = 6)			
    
 
#【17】分析数据南岳阀组件_911
    #〈1〉脉宽 油量 折线图
    ggplot(result_035,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR035I_S1_S2_volume")+scale_x_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+scale_y_continuous(limits=c(0,120),breaks=seq(0,120,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR035I_S1_S2_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_035,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR035I_S1_S2_closing_delay")+scale_x_continuous(limits=c(0,1200),breaks=seq(0,1200,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR035I_S1_S2_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_035,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR035I_S1_S2_opening_delay")+scale_x_continuous(limits=c(0,1200),breaks=seq(0,1200,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR035I_S1_S2_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_035,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR035I_S1_S2_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR035I_S1_S2_std_inj_volume.png"),width = 12, height = 6)			
  
    
#【18】分析数据南岳阀组件_091
    #〈1〉脉宽 油量 折线图
    ggplot(result_091,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR091I_S1_volume")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,100))+scale_y_continuous(limits=c(0,250),breaks=seq(0,250,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR091I_S1_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_091,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR091I_S1_closing_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR091I_S1_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_091,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR091I_S1_opening_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR091I_S1_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_091,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR091I_S1_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR091I_S1_std_inj_volume.png"),width = 12, height = 6)			

#【18】分析数据南岳阀组件_933
    #〈1〉脉宽 油量 折线图
    ggplot(result_933,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR933I_volume")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,100))+scale_y_continuous(limits=c(0,200),breaks=seq(0,200,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR933I_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_933,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR933I_closing_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR933I_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_933,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR933I_opening_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR933I_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_933,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR933I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR933I_std_inj_volume.png"),width = 12, height = 6)			
    
#【19】分析数据南岳阀组件_075
    #〈1〉脉宽 油量 折线图
    ggplot(result_075,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR075I_volume")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,100))+scale_y_continuous(limits=c(0,200),breaks=seq(0,200,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR075I_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_075,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR075I_closing_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR075I_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_075,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR075I_opening_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR075I_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_075,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR075I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR075I_std_inj_volume.png"),width = 12, height = 6)			
    
#【20】分析数据南岳阀组件_074
    #〈1〉脉宽 油量 折线图
    ggplot(result_074,aes(pulse_length,aver_inj_volume,col=injector_number,shape=factor(rail_pressure_cons)))+geom_point()+geom_line()+ggtitle("NCR074I_volume")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,100))+scale_y_continuous(limits=c(0,200),breaks=seq(0,200,10))+xlab("pulse_length(us)")+ylab("aver_inj_volume(mm^3)")+labs(col="injector_number", shape="rail_pressure_cons")+theme(legend.position = c(.25, .85),legend.direction ="vertical",legend.box ="horizontal",legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#映射数据，x轴，y轴，颜色，线图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR074I_volume.png"),width = 12, height = 8)
    #〈2〉脉宽 关闭延时 折线图+轨压分布
    ggplot(result_074,aes(pulse_length,aver_closing_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR074I_closing_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1400),breaks=seq(0,1400,100))+xlab("pulse_length(us)")+ylab("aver_closing_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR074I_closing_delay.png"),width = 12, height = 6)
    #〈3〉脉宽 开启延时 折线图+轨压分布
    ggplot(result_074,aes(pulse_length,aver_opening_delay,col=injector_number))+geom_point()+geom_line()+facet_grid(.~rail_pressure_cons)+ggtitle("NCR074I_opening_delay")+scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,200))+scale_y_continuous(limits=c(0,1200),breaks=seq(0,1200,100))+xlab("pulse_length(us)")+ylab("aver_opening_delay(us)")+theme(legend.position = c(.88, .2),legend.background=element_blank(),legend.key=element_blank(),axis.text.x = element_text(angle=60, hjust=1))#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR074I_opening_delay.png"),width = 12, height = 6)
    #〈4〉脉宽 油量散差 折线图+轨压分布
    ggplot(result_074,aes(factor(pulse_length),std_inj_volume,fill=injector_number))+geom_bar(stat = "identity", position = "dodge",width=0.5)+facet_grid(rail_pressure_cons~.)+ggtitle("NCR074I_std_inj_volume")+scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+xlab("pulse_length(us)")+ylab("std_inj_volume")+theme(legend.position = c(.15, .88),legend.direction ="vertical",legend.background=element_blank(),legend.key=element_blank())#以轨压分别作图
    ggsave(file = paste(format(Sys.time(), "%Y-%b-%d-%H-%M-%S"),"NCR074I_std_inj_volume.png"),width = 12, height = 6)			
    
    
###连接oracle数据库 
    
    library(ggplot2)
    library(plotly)
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(flexdashboard)
    library(RMySQL)
    library(magrittr)
    library(xlsx)
    library(knitr)
    library(data.table)
    library(formattable)
    library(RODBC)  
    library(tibble)
    
    
    #加载RODBC包，连接数据库SQL server
    
    library(RODBC)  #建立一个ODBC连接数据库SQL server,连接名称为NCRI
    
    #创建链接
    channel<-odbcConnect("NCRI",uid="zpuser",pwd="123456",believeNRows=FALSE) ##建立Oracle连接,uid为用户名，pwd为密码,服务器172.16.52.13
    
    
    #查看数据表
    
    sqltable <- sqlTables(channel, errors = FALSE, as.is = TRUE)
    sqltable
    
    #创建查询
    
   labodi_sql <- "SELECT TEC101_Report.ID_REPORT,  
    TEC101_Report.DATETIME_INSERT , 
    TEC101_Report.OPERATOR , 
    TEC101_Report.SERIAL_NUMBER , 
    TEC101_Report.INJ_TYPE , 
    TEC101_Report.POSITION , 
    TEC101_Report.POINTS , 
    TEC101_Report.PRESSURE , 
    TEC101_Report.VALUE, 
    TEC101_Report.TARGET,  
    TEC101_Report.TOLERANCE ,  
    TEC101_Report.DELTA_TARGET ,  
    TEC101_Report.STDDEV,  
    TEC101_Report.MAX_STDDEV,  
    TEC101_Report.OIL_TEMP ,  
    TEC101_Report.RESULT_TEST_POINT ,  
    TEC101_Report.SHOTS_NUMBER ,   
    TEC101_Report.INJ_RESULT ,   
    TEC101_Report.CURRENT_POINT,  
    TEC101_Report.PULSE  
    FROM TXM.dbo.TEC101_Report TEC101_Report 
    WHERE (TEC101_Report.DATETIME_INSERT>={ts '2018-06-20 00:00:00'}) AND (TEC101_Report.SERIAL_NUMBER Not Like '%X%') AND (TEC101_Report.SERIAL_NUMBER Not Like '%--%')
    ORDER BY TEC101_Report.ID_REPORT DESC "
    
    
    result_labodi <- sqlQuery(channel, labodi_sql)  #Rabotti
    
    
    result_labodi <- sqlFetch(channel, "TEC101_Report") # 拉宝地数据
    result_research_efs <- sqlFetch(channel,"研发试验台测试喷油器记录") #研发EFS数据
    result_zp <- sqlFetch(channel,"测量数据") #喷油器装配参数
    result_research_efs <- sqlFetch(channel,"efs_n102912610_product_change_vo") #研发EFS数据
    result_production_efs <- sqlFetch(channel,"efs8409") #生产型EFS试验台数据
    result_bill <- sqlFetch(channel,"喷油器主要物料清单") #喷油器电磁铁类别
    result_leakage <- sqlFetch(channel,"喷油器_测回油") #回油量合格状态
    result_passrate <- sqlFetch(channel,"算合格率1_测性能") #算合格率1_测性能
    
    
    
    
    
    
    names(result_labodi)
    
    
    
    
    TEC101_Report.ID_REPORT as "ID号", 
    TEC101_Report.DATETIME_INSERT as "测量日期时间", 
    TEC101_Report.OPERATOR as "操作者", 
    TEC101_Report.SERIAL_NUMBER as "产品编号", 
    TEC101_Report.INJ_TYPE as "喷油器型号", 
    TEC101_Report.POSITION as "工位号", 
    TEC101_Report.POINTS as "工况数", 
    TEC101_Report.PRESSURE as "轨压值", 
    TEC101_Report.VALUE as "油量值", 
    TEC101_Report.TARGET as "油量标准值", 
    TEC101_Report.TOLERANCE as "油量公差值", 
    TEC101_Report.DELTA_TARGET as "油量偏差值", 
    TEC101_Report.STDDEV as "标准差", 
    TEC101_Report.MAX_STDDEV as "标准差公差", 
    TEC101_Report.OIL_TEMP as "油温", 
    TEC101_Report.RESULT_TEST_POINT as "工况合格状态", 
    TEC101_Report.SHOTS_NUMBER as "次数", 
    TEC101_Report.INJ_RESULT as "合格状态", 
    TEC101_Report.CURRENT_POINT as "工况号", 
    TEC101_Report.PULSE as "脉宽"
    
    
    
    
    
    result_labodi <- odbcConnect("Labodi_EFS",uid="zpuser",pwd="123456") %>% # 拉宝地测试数据
      sqlFetch( "TEC101_Report")  %>%  
      select(ID_REPORT,qualification_date_time = DATETIME_INSERT,injector_number =SERIAL_NUMBER ,
             rail_pressure_cons= PRESSURE ,pulse_length = PULSE ,aver_inj_volume = VALUE ,
             std_inj_volume = STDDEV ,recipe_name =INJ_TYPE,bench_channel = POSITION,CURRENT_POINT,
             POINTS,DELTA_TARGET,OIL_TEMP, TARGET,TOLERANCE,RESULT_TEST_POINT,SHOTS_NUMBER,INJ_RESULT) %>% 
      #na.omit()%>% 
      filter(qualification_date_time>"2019-04-01 00:00:00") %>%
      filter(rail_pressure_cons != 0) %>%
      #filter(recipe_name == "NCR909I") %>%
      #filter(CURRENT_POINT >= 2) %>%
      filter(injector_number %in% grep('^071',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      #filter(injector_number %in% c("903 84140935" ,"903 84140606" ,"903 84140488" ,"903 84140669" ,"903 7C250336" ,"903 82050210" ,"903 82040106" ,"903 84070242" ,"903 84070081" ,"903 84060294" ,"903 84060165" ,"903 84070139" ,"903 84150092" ,"903 84140863" ,"903 84140206" ,"903 84140243" ,"903 84140660" ,"903 84140198" ,"903 84140193" ))%>%
      #filter(!injector_number %in% grep('bs',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      #filter(!injector_number %in% grep('bz',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      #filter(!injector_number %in% grep('bosch',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      #filter(!injector_number %in% grep('BOSCH',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      #filter(!injector_number %in% grep('jz',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      #filter(!injector_number %in% grep('JZ',rownames(table(result_labodi$injector_number)),value = TRUE))%>%
      filter(!injector_number %in% c("909 86020067" ,"909 86040166"  ,"909 86090041" ,"909 86040186","909 86040172"))%>%
      #filter(!qualification_date_time %in% c("2018-06-06 10:07:06"  ))%>%
       #select(qualification_date_time,injector_number,rail_pressure_cons,pulse_length,aver_inj_volume,std_inj_volume,bench_channel,CURRENT_POINT,recipe_name)%>%
       select(qualification_date_time,injector_number,rail_pressure_cons,pulse_length,aver_inj_volume,bench_channel,CURRENT_POINT,recipe_name)%>%
       unite(pressure_pulse,rail_pressure_cons,pulse_length,CURRENT_POINT,sep ="_") %>% 
       #unite(injector_data,aver_inj_volume,std_inj_volume,sep ="/") %>%
       #spread(pressure_pulse,injector_data)#%>%
       spread(pressure_pulse,aver_inj_volume)
    
    
       separate(`250_800_4`,sep = "/", c('250_800_vol', '250_800_std'),convert = TRUE)%>%
       separate(`1000_160_2`,sep = "/", c("1000_160_vol", "1000_160_std"),convert = TRUE)%>%
       separate(`600_700_5`,sep = "/", c("600_700_vol", "600_700_std"),convert = TRUE)%>%
       separate(`1600_1700_6`,sep = "/", c("1600_1700_vol", "1600_1700_std"),convert = TRUE)%>%
       separate(`1600_1700_0`,sep = "/", c("1600_1700_leakage", "1600_1700_0_std"),convert = TRUE)%>%
       separate(`1650_0_1`,sep = "/", c("1650_0_leakage", "1650_0_1_std"),convert = TRUE) 
    
    result_labodi <- result_labodi %>%  mutate( rowid = rowidv(result_labodi,cols="injector_number")) %>%
       rownames_to_column(var = "serial_number")%>%
       select(serial_number,qualification_date_time,injector_number,rowid,bench_channel,recipe_name,'250_800_vol','1000_160_vol','600_700_vol','1600_1700_vol','250_800_std','1000_160_std','600_700_std','1600_1700_std','1650_0_leakage','1600_1700_leakage')%>% na.omit()
     

    write.csv(result_labodi, file = "C:\\Users\\xianghp\\Desktop\\喷油器NCR071I拉宝地油量.csv")#保存数据csv格 
    
    
    
    #关闭连接
    
    odbcClose(channel)
    
    #显示查询结果
    
    print( result_labodi)
    
    
    
    
    
    
    
    
    
    
    
    con <- dbConnect(MySQL(),user='recedit',password='8409',dbname='qualidata',host="172.16.67.163",DBMSencoding="UTF8") #连接数据库
    dbListTables <- dbListTables(con)
    ipod_config <- dbReadTable(con, "ipod_config" ) #电流驱动设置
    recipe <- dbReadTable(con, "recipe"  )#  工况参数设置目录
    recipe_parameters <- dbReadTable(con, "recipe_parameters"  )
    recipe_step <- dbReadTable(con, "recipe_step"  ) #  工况参数
    write.csv(recipe_step, file = "C:\\Users\\xianghp\\Desktop\\喷油器工况参数.csv")#保存数据csv格 
    
    channel <- readLines("D:\\xianghp\\common_rail_injector\\test_r.sql")#读取sql命令
    result <- dbGetQuery(con, paste(channel,collapse=""))#读取mysql数据
    
    write.csv(ipod_config, file = "C:\\Users\\xianghp\\Desktop\\喷油器909驱动电流.csv")#保存数据csv格 
    
    recipe_857 <- dbReadTable(con, "recipe_step"  ) %>% filter(idRecipe %in% c("857"))
    write.csv(recipe_857, file = "D:\\xianghp\\2019年\\NCR071I全柴4C2\\喷油器071测试工况.csv")#保存数据csv格
    
    
    
    
    
    
    
    
    
    
