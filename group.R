library(dplyr)

df <- read.csv(file = "./Desktop/sample_data.csv",stringsAsFactors = F, fileEncoding = "UTF-8-BOM")


### 何かしらの変形で、Ageごと(xごと)の率とcountを出した時点

df <- df %>%
      dplyr::mutate(class = dplyr::ntile(df$Age, 5))
df_new <- df %>% 
          dplyr::group_by(class) %>%
          dplyr::summarise(Age = mean(Age), mean = mean(mean), count = sum(count))


#グラフの山が指定された個数以下にするようにグルーピングする関数
grouping <- function(df, peak, class_n){
  peak_count = 0
  up_flag=0
  df <- df %>%
    dplyr::mutate(class = dplyr::ntile(df$Age, class_n))
  df_new <- df %>% 
    dplyr::group_by(class) %>%
    dplyr::summarise(Age = mean(Age), mean = mean(mean), count = sum(count))
  for (i in 2:max(df_new$class)){
    if(df_new$mean[i] > df_new$mean[i-1]){
        up_flag = 1
    }
    else{
        if(up_flag == 1){
          peak_count = peak_count + 1
        }
        up_flag = 0
   }
    if(peak_count > peak){
      break;
    }
  }
  if(peak_count > peak){
    return (grouping(df, peak, class_n-2))
  }
  else{
    return (df_new)
  }
}

df_new <- grouping(df, 3, 24)
