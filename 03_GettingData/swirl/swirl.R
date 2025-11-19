setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\Getting and Cleaning Data\\swirl")
swirl()
mydf<-read.csv(path2csv,stringsAsFactors = FALSE)
dim(mydf)
head(mydf)
library(dplyr)
packageVersion("dplyr")
cran <- tbl_df(mydf)
rm("mydf")
cran
?select
select(cran, ip_id, package, country)
5:20
select(cran, r_arch:country)
select(cran, country:r_arch)
cran
select(cran, -time)
csize (X:size)
size (X:size)
select(cran,X:size)
select(cran,-X:-size)
select(cran,-X:size)
select(cran,-(X:size))
-5:20
-(5:20)
select(cran,-(X:size))
filter(cran, package == "swirl")
filter(cran, r_version == "3.1.1", country == "US")
?Comparison
filter(cran, r_version =< "3.1.1", country == "US")
filter(cran, r_version <= "3.1.1", country == "US")
filter(cran, r_version <= "3.1.1")
filter(cran, r_version =< "3.1.1", country == "IN")
filter(cran, r_version <= "3.1.1", country == "IN")
filter(cran, r_version <= "3.0.2", country == "IN")
filter(cran, country == "US" | country == "IN")
filter(cran, size > 100500 , r_os == "linux-gnu")
is.na(c(3, 5, NA, 10))
point(is.na(c(3, 5, NA, 10)))
points(is.na(c(3, 5, NA, 10)))
point(is.na(c(3, 5, NA, 10)))
!is.na(c(3, 5, NA, 10))
filter(cran, !is.na(r_version))
arrange(cran,cran2,size:ip_id)
arrange(cran,select(cran2,size:ip_id))
arrange(cran2,select(cran,size:ip_id))
?arrange
cran2<-select(cran,size:ip_id))
cran2<-select(cran,size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country,desc(r_version),ip_id)
cran3<-select(cran,ip_id, package, size)
cran3
mutate(cran3,size_mb=size/2^20)
mutate(cran3,size_mb=size/2^20,size_gb=size_mb/2^10)
mutate(cran3,correct_size=size+1000)
summarize(cran, avg_bytes = mean(size))
swirl()
swirl()
library(dplyr)
cran<-tbl_df(mydf)
rm("mydf")
cran
?group_by()
?group_by
by_package<-group_by(cran,package)
by_package
summarize(by_package,mean(size))
submit()
submit()
tbl
pack_sum
quantile(pack_sum$count, probs = 0.99)
filter(cran,count>679)
top_counts<-filter(cran,count>679)
top_counts<-filter(pack_sum,count>679)
top_counts
View(top_counts)
?arrange
?desc
top_counts_sorted<-arrange(top_counts,desc(count))
View(top_counts_sorted)
quantile(pack_sum$unique, probs = 0.99)
top_counts<-filter(pack_sum,unique>465)
top_unique<-filter(pack_sum,unique>465)
View(top_unique)
arrange(top_unique,unique)
top_unique_sorted<-arrange(top_unique,unique)
top_unique_sorted<-arrange(top_unique,desc(unique))
view(top_unique_sorted)
View(top_unique_sorted)
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/summarize2.R')
submit()
submit()
submit()
View(result3)
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/chain1.R')
submit()
submit()
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/chain3.R')
submit()
submit()
library(tidyr)
sudents
students
?gather
gather(students,sex,count,-grade)
student2
students2
res<-gather(students2,sex_class='key',count='value',-grade)
res<-gather(students2,key,value,-grade)
res<-gather(students2,sex_class,count,-grade)
res
?separate
separate(res,col=sex_class,into=c("sex","class"))
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script1.R')
submit
submit()
student3
students3
submit()
?spread
submit()
examples(spread)
example(spread)
submit()
extract_numeric("class5")
submit()
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script4.R')
print
mutate(extract_numeric(class))
students3 %>%
        gather(class, grade, class1:class5, na.rm = TRUE) %>%
        spread(test, grade) %>%
        mutate(extract_numeric(class))
students3 %>%
        gather(class, grade, class1:class5, na.rm = TRUE) %>%
        spread(test, grade) %>%
        class<-mutate(extract_numeric(class))
class=mutate(extract_numeric(class))
?mutate
example(mutate)
mutate(class=extract_numeric(class))  %>%
        submit()
students4
submit()
submit()
?unique
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
unique(id,incomparables=T,fromLast=T) %>%
        source('~/.active-rstudio-document')
source('~/.active-rstudio-document')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
student_info <- students4 %>%
        select(id, name, sex) %>%
        unique(id, name, sex, incomparables != FALSE, fromLast=T) %>%
        print
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
reset()
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
submit()
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
student_info <- students4 %>%
        select(id, name, sex) %>%
        unique
### Your code here %>%
print
student_info
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
student_info <- students4 %>%
        select(id , name, sex) %>%
        print(student_info)
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script5.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script5.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script6.R')
submit()
submit()
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script7.R')
submit()
submit()
passed
failed
mutate(passed,status=passed)
mutate(passed,status="passed")
passed<-mutate(passed,status="passed")
failed<-mutate(failed,status="failed")
bind_rows(failed,passed)
bind_rows(passed,failed)
SAT
ssat
sat
submit()
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script8.R')
source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script8.R')
select(-c(read_total, math_total, write_total)) %>%
        source('C:/Users/lenovo/AppData/Local/Temp/Rtmp2lqkgb/script8.R')
source('~/.active-rstudio-document')
source('~/.active-rstudio-document')
?select
source('~/.active-rstudio-document')
source('~/.active-rstudio-document')
sat
source('~/.active-rstudio-document')
submit()
submit()
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")
swirl()
swirl()
Sys.getlocale("LC_TIME")
library(lubridate)
help(package = lubridate)
this_day<-today()
this_day
year()
year(this_day)
wday(this_day)
wday(this_day,label=T)
wday(this_day,label=TRUE)
this_moment<-now()
this_moment
hour(this_moment)
my_date<-ymd("1989-05-17")
my_date
class(my_date)
ymd("1989-05-17")
ymd("1989 May 17")
ymd(my_date)
ymd("March 12, 1975")
ymd("March 12, 1975",y,m,d)
?ymd
parse_date_time(my_date)
ymd(my_date)
mdy(my_date)
mdy("March 12, 1975")
parse(25081985)
mdy(25081985)
dmy(25081985)
dmy(192012)
mdy(192012)
ymd("192012")
ymd(192012)
ymd(1920/1/2)
ymd("1920/1/2")
ymd(19200102)
dt1
ymd_hms(dt1)
hms("03:22:14")
dt2
ymd(dt2)
update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment
this_moment<-update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment
now("America/New_York")
nyc<-now("America/New_York")
nyc
nyc + days(2)
depart<-nyc + days(2)
depart
update(depart, hours = 17, minutes = 34)
depart<-update(depart, hours = 17, minutes = 34)
depart
depart + hours(15)+min(50)
arrive<-depart + hours(15)+minutes(50)
?with_tz
with_tz(arrive,tzone="Asia/Hong_Kong")
arrive<-with_tz(arrive,tzone="Asia/Hong_Kong")
arrive
singapore<-with_tz(arrive,tz ="Singapore")
last_time<-mdy("June 17, 2008", tz = "Singapore")
last_time
last_time<-with_tz(arrive,tz ="Singapore")
last_time
?new_interval
how_long<-new_interval(last_time,arrive)
as.period(how_long)
stopwatch()
savehistory("C:/Users/lenovo/Desktop/Rlearning/Getting and Cleaning Data/swirl/new.Rhistory")
