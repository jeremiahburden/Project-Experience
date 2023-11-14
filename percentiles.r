#percentile calcs
pacman::p_load(bizdays,anytime,tidyr,timestamp,lubridate,clock,dplyr,plotly,stringr,ggplot2)
 
load_rmetrics_calendars(2023)
business_calendar <- create.calendar('my_calendar', weekdays = c('saturday','sunday'))
 
setwd("C:/Users/JeremiahBurden/Documents")
 
# import csv, split into settlement prices and intraday bar prices, insert the settles appropriately, reformat trade date (data cleaning)
down_clean <- function(csv_choose, date_choose,product,lookback,tick_multiplier){
  #csv_choose<-'nq_5min.csv'
  csv_down <- read.csv(csv_choose)
  names(csv_down)<-c('date_p','mid_price','settle_date','settle_price')
  price_df <- csv_down[,1:2]
  price_df$date_p<-gsub('/23 ','/2023 ',price_df$date_p)
  full_df <- separate(price_df, col=date_p, into = c("date","time_period"), sep = " ")
  full_df$timedate<-paste0(full_df$date,' ',full_df$time_period)
  full_df$timedate<- as.POSIXct(full_df$timedate,format='%m/%d/%Y %H:%M')
  full_df$trade_date <- ifelse(16 <=  hour(full_df$timedate), paste0(anydate(full_df$date)+1), paste0(anydate(full_df$date)))
  
  find_weekend<- which(is.bizday(full_df$trade_date,business_calendar) != TRUE)
  for(i in find_weekend){
	full_df$trade_date[i] <- paste0(anydate(full_df$trade_date[i])+days(2)) 
  }
  # return calculations
  current_date <- full_df$trade_date[1]
  first_index <- 1
  for(i in 1:nrow(full_df)){
	if(i==1){
  	full_df$cumulative_return[i]<-0
  	full_df$simple_return[i]<-0
	}
	else if(current_date == full_df$trade_date[i]){
  	full_df$cumulative_return[i]<-full_df$mid_price[i]/full_df$mid_price[first_index]-1
  	full_df$simple_return[i]<-full_df$mid_price[i]/full_df$mid_price[i-1]-1
	}
	else{
  	full_df$cumulative_return[i]<-0
  	full_df$simple_return[i]<-0
  	current_date<-full_df$trade_date[i]
  	first_index<-i
	}
  }
  return_list <- list('full_df'=full_df,'date_choose'=date_choose, 'product'=product, 'lookback'=lookback,'tick_multiplier'=tick_multiplier)
  return(return_list)
}
fgh<-down_clean('gold_5min_std.csv','7/18/2023','GC',30,100)
fgh<-down_clean('nq_5min.csv','7/6/2023','NQ',30,20)
fgh<-down_clean('crude_5min.csv','7/18/2023','CL',30,100)
fgh<-down_clean('crude_mid_price.csv','7/18/2023','CL',30,100)
 
full_df<- fgh$full_df
date_choose<-fgh$date_choose
product<-fgh$product
lookback<-fgh$lookback
tick_multiplier<-fgh$tick_multiplier
 
 
# subset full_df
lookback_len <- lookback	# how many days to look back
sub_df <-  subset(full_df,anydate(full_df$trade_date)>(anydate(date_choose)-lookback_len))
df_date <- subset(sub_df,anydate(sub_df$trade_date)==anydate(date_choose))
sub_df <- subset(sub_df,anydate(sub_df$trade_date)<(anydate(date_choose)))
times<- unique(full_df$time_period)
 
 
#cumulative
q<- c(0.0,0.05,0.15,0.25,0.5,0.75,0.85,0.95,1.0)
quart <- sub_df %>% 
  group_by(time_period) %>% 
  summarize(quart0 = quantile(cumulative_return,probs=q[1]),
        	quart05 = quantile(cumulative_return,probs=q[2]),
        	quart15 = quantile(cumulative_return,probs=q[3]),
        	quart25 = quantile(cumulative_return,probs=q[4]),
        	quart50 = quantile(cumulative_return,probs=q[5]),
        	quart75 = quantile(cumulative_return,probs=q[6]),
        	quart85 = quantile(cumulative_return,probs=q[7]),
        	quart95 = quantile(cumulative_return,probs=q[8]),
        	quart100 = quantile(cumulative_return,probs=q[9]))
q_prices_df <- quart
q_prices_df[-1]<-q_prices_df[-1]+1
 
 
ref_price <- df_date$mid_price[1] #reference price
q_prices_df[-1] <- (quart[-1]+1)*ref_price
 
q_prices_df <- q_prices_df[order(match(q_prices_df$time_period,times)),]
 
# p&l calculations
#creates the check for what quartile the price is currently in
for(i in 1:nrow(df_date)){
  if(df_date$mid_price[i]>q_prices_df$quart100[i]){
	df_date$q_check[i]<-100
  }
  else if(df_date$mid_price[i]>q_prices_df$quart95[i]&&df_date$mid_price[i]<q_prices_df$quart100[i]){
	df_date$q_check[i]<-95
  }
  else if(df_date$mid_price[i]>q_prices_df$quart85[i]&&df_date$mid_price[i]<q_prices_df$quart95[i]){
	df_date$q_check[i]<-85
  }
  else if(df_date$mid_price[i]>q_prices_df$quart75[i]&&df_date$mid_price[i]<q_prices_df$quart85[i]){
	df_date$q_check[i]<-75
  }
  else if(df_date$mid_price[i]<q_prices_df$quart25[i]&&df_date$mid_price[i]>q_prices_df$quart15[i]){
	df_date$q_check[i]<-25
  }
  else if(df_date$mid_price[i]<q_prices_df$quart15[i]&&df_date$mid_price[i]>q_prices_df$quart05[i]){
	df_date$q_check[i]<-15
  }
  else if(df_date$mid_price[i]<q_prices_df$quart05[i]&&df_date$mid_price[i]>q_prices_df$quart0[i]){
	df_date$q_check[i]<-5
  }
  else if(df_date$mid_price[i]<q_prices_df$quart0[i]){
	df_date$q_check[i]<-0
  }
  else{df_date$q_check[i]<-50}
}
entry<- data.frame(matrix(ncol=5,nrow=0))
colnames(entry)<-c('time_period','mid_price','exit_price','buy_sell','pos')
exit<-data.frame(matrix(ncol=5,nrow=0))
colnames(exit)<-c('time_period','mid_price','exit_price','buy_sell','pos')
current_position<-50
for(i in 1:nrow(df_date)){
  if(i==1){next}
  else if(df_date$q_check[i]==75&&current_position<75){
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],1,75)
	current_position<-75
  }
  else if(df_date$q_check[i]==75&&current_position>75){
	current_position<-75
  }
  else if(df_date$q_check[i]==85&&current_position<85){
	if(current_position<75){
  	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],1,75)
	}
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart75[i],1,85)
	current_position<-85
  }
  else if(df_date$q_check[i]==85&&current_position>85){
	current_position<-85
  }
  else if(df_date$q_check[i]==95&&current_position<95){
	if(current_position<85){
  	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart75[i],1,85)
  	if(current_position<75){
    	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],1,75)
  	}
	}
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart85[i],1,95)
	current_position<-95
  }
  else if(df_date$q_check[i]==95&&current_position>95){
	current_position<-95
  }
  # else if(df_date$q_check[i]==100){
  #   if(current_position<95){
  # 	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart85[i],1,95)
  # 	if(current_position<85){
  #   	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart75[i],1,85)
  #   	if(current_position<75){
  #     	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],1,75)
  #   	}
  # 	}
  #   }
  #   entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],df_date$mid_price[i],-1,100)
  #   break
  # }
  else if(df_date$q_check[i]==25&&current_position>25){
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],-1,25)
	current_position<-25
  }
  else if(df_date$q_check[i]==15&&current_position>15){
	if(current_position>25){
  	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],-1,25)
	}
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart25[i],-1,15)
	current_position<-15
  }
  else if(df_date$q_check[i]==05&&current_position>05){
	if(current_position>15){
  	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart25[i],-1,15)
  	if(current_position>25){
    	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],-1,25)
  	}
	}
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart15[i],-1,05)
	current_position<-05
  }
  else if(df_date$q_check[i]==0){
	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],df_date$mid_price[i],1,0)
	if(current_postion>05){
  	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart15[i],-1,05)
  	if(current_position>15){
    	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart25[i],-1,15)
    	if(current_position>25){
      	entry[nrow(entry)+1,]<- c(df_date$time_period[i],df_date$mid_price[i],q_prices_df$quart50[i],-1,25)
    	}
  	}
	}
	break
  }
}
 
for(i in 1:nrow(entry)){
  if((as.numeric(entry$pos[i]==0))|(as.numeric(entry$pos[i]==100))){
	exit[nrow(exit)-2,]<- c(entry$time_period[i],entry$mid_price[i],NA,as.numeric(entry$buy_sell[i]),NA)
	exit[nrow(exit)-1,]<- c(entry$time_period[i],entry$mid_price[i],NA,as.numeric(entry$buy_sell[i]),NA)
	exit[nrow(exit),]<- c(entry$time_period[i],entry$mid_price[i],NA,as.numeric(entry$buy_sell[i]),NA)
	entry<-entry[-nrow(entry),]
	break
  }
  else if(as.numeric(entry$pos[i])>50){
	pull<-df_date[first(which((df_date$time_period>entry$time_period[i]) & (df_date$mid_price<entry$exit_price[i]))),]
	exit[nrow(exit)+1,]<- c(pull$time_period,pull$mid_price,NA,0-as.numeric(entry$buy_sell[i]),NA)
  }
  else{
	pull<-df_date[first(which((df_date$time_period>entry$time_period[i]) & (df_date$mid_price>entry$exit_price[i]))),]
	exit[nrow(exit)+1,]<- c(pull$time_period,pull$mid_price,NA,0-as.numeric(entry$buy_sell[i]),NA)
  }
}
 
pnl<-0
for(i in 1:nrow(entry)){
  value1<-as.numeric(entry$mid_price[i])*as.numeric(entry$buy_sell[i])
  value2<-as.numeric(exit$mid_price[i])*as.numeric(exit$buy_sell[i])
  pnl<-value1+value2+pnl
}
pnl<-pnl*tick_multiplier
pnl<-round(pnl,2)
 
xform <- list(categoryorder = "array", 
          	categoryarray = times)
 
 
figure <- plot_ly(data =df_date, 
              	x = ~time_period, 
              	y = ~mid_price,
              	name = 'Mid Price',
              	type = 'scatter', 
              	mode = 'lines+markers',
              	color = I('black')) %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart0,color=I('tomato4'),type='scatter',mode='lines',name='0%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart05,color=I('tomato3'),type='scatter',mode='lines',name='5%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart15,color=I('tomato2'),type='scatter',mode='lines',name='15%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart25,color=I('tomato1'),type='scatter',mode='lines',name='25%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart50,color=I('darkgreen'),type='scatter',mode='lines',name='50%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart75,color=I('skyblue1'),type='scatter',mode='lines',name='75%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart85,color=I('skyblue2'),type='scatter',mode='lines',name='85%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart95,color=I('skyblue3'),type='scatter',mode='lines',name='95%') %>% 
  add_trace(data=q_prices_df,x=~time_period,y=~quart100,color=I('skyblue4'),type='scatter',mode='lines',name='100%') %>% 
  plotly::add_annotations(text=paste0('PnL: ',pnl,'$'),showarrow=F,x=12,y=max(q_prices_df$quart100),font=list(size=14,color='gray')) %>%
  layout(
	title = paste0('Cumulative Return Percentiles ', product,' over ',lookback,' days ' ,date_choose), 
	showlegend = TRUE, 
	xaxis = xform,
	#yaxis = list(title = 'Standard Deviations',zerolinewidth=2,range=c(-3.5,3.5))
	yaxis=list(title='Mid Prices') 
  )
figure
 
boxplot<- ggplot(sub_df,aes(time_period,cumulative_return))+geom_boxplot()
boxplot + ggtitle('Distribution of Cumulative Returns over Lookback')
 
violin<- ggplot(sub_df,aes(time_period,cumulative_return))+geom_violin()
violin + ggtitle('Distribution of Cumulative Returns over Lookback')
violin + geom_line(data=df_date,aes(y=cumulative_z))
 
ggplot()+geom_path(data=df_date,aes(x=time_period,y=cumulative_z))
 
fig <- sub_df %>% 
  plot_ly(
	x=~time_period,
	y=~cumulative_return,
	split=~day,
	type='box',
	box=list(visible=F)
  ) %>% 
  add_trace(data=df_date,x=~time_period,y=~cumulative_return,color = I('green'),type='scatter',mode='lines',line=list(width=2)) %>% 
  layout(
	title = paste0('Cumulative Return Box Spread ', product,' over ',lookback,' days ' ,date_choose), 
	showlegend = TRUE, 
	xaxis = xform,
	yaxis = list(title = 'Returns',zerolinewidth=2))
 
fig
