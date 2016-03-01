library(dplyr)
library(ggplot2)
library(RColorBrewer)
col_pal <-brewer.pal(n = 12, name = "Paired")


setwd('~/data_science/atlanta_crime')
df = read.csv('atlanta_crime_data.csv')
names(df) <- c('date','time','adress','type','neighborhood','x','y')
df$date <- as.Date(df$date,"%m/%d/%Y")
df$type <- sapply(df$type,function(x){tolower(gsub("-"," ",x))}) #format text for presentation
df <- df[df$date>"2008-12-31",] # data before 2009 is not good quality 
                                # (including data from 1916)

# grouping by date
by_date <- df %>% 
          group_by(date) %>% 
          summarise(count = n()) %>%
          arrange(date)

ggplot(by_date, aes(date,count )) + geom_line(color = col_pal[3]) + 
  geom_smooth(color = 'black') +
  labs(x = '', y ='Number of crimes', title = 'Crime in Atlanta 2009-2016')+
theme_bw()+theme(panel.border = element_blank(), 
                     axis.ticks = element_blank(),
                     title = element_text(size = 20),
                     text = element_text(size = 15))
ggsave('~/lfjover.github.io/figures/crimes_date.png')

# grouping by day of the week
by_day <- df %>% select(date) %>%
            mutate(day = weekdays(date)) %>%
            group_by(day) %>%
            summarise(count = n()) %>%
            mutate(percentage = count/sum(count)*100)
by_day$day <- factor(by_day$day, levels= c( "Monday", "Tuesday", "Wednesday", 
                                    "Thursday", "Friday", "Saturday","Sunday"))
by_day <- by_day[order(by_day$day), ]

ggplot(by_day,aes(x= day, y = percentage)) + 
  geom_bar(stat = 'identity', fill = col_pal[1]) +
  theme(text= element_text(size = 10), title = element_text(size = 12))+ 
  labs(y = '% crimes', x = '') 
ggsave('~/lfjover.github.io/figures/crime_by_day.png', width = 6, height = 3)

# grouping by type of crime
by_type <- df %>% group_by(type) %>%
                  summarise(count = n()) %>%
                  arrange(count)
ggplot(by_type,aes(x = reorder(type,count), y = count)) + 
  geom_bar(stat = 'identity', fill = col_pal[2]) + coord_flip() + 
  theme(text= element_text(size = 18)) + labs(x = " ")
ggsave('~/lfjover.github.io/figures/crime_by_type.png')

# grouping by day of the week and type
by_day_type <- df %>% select(date, type) %>%
  mutate(day = factor(weekdays(date), levels= c( "Monday", "Tuesday",
                 "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))) %>%
  group_by(day,type) %>%
  summarise(count = n()) %>%
  group_by(type) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(by_day_type,aes(x= day, y = percentage)) + 
  geom_bar(stat = 'identity', fill = col_pal[4]) + 
  facet_wrap(~ type, nrow = 6, scale = 'free')+
  labs(x ='')
ggsave('~/lfjover.github.io/figures/crime_by_type_day.png', height = 10, width = 10)

by_day_type <- df %>% 
  group_by(date,type) %>% 
  summarise(count = n()) %>%
  arrange(date)

by_day_type <- by_day_type %>% 
  group_by(type) %>% 
  mutate(proportion = count/max(count))

ggplot(by_day_type, aes(date,count )) + geom_line(color = "#669966") + 
  facet_wrap(~ type, nrow = 6, scales = "free")
#
ggsave('test.png',dpi=300, width=4, height=3)
  
## Crime in atlanta has a clear  downwards trend. There is also a clear annual seaonality in the
## data. Crimes go down around January/February. It looks like criminals wan't to deal
## with the cold weather.

## time series for last year
ggplot(tail(by_date,365), aes(date,count )) + geom_line(color = col_pal[4]) + 
  geom_smooth(color = 'black') +
  labs(x = '', y ='Number of crimes', title = 'Crime in Atlanta over one year')+
  theme_bw()+theme(panel.border = element_blank(), 
                   axis.ticks = element_blank(),
                   title = element_text(size = 20),
                   text = element_text(size = 15))
ggsave('~/lfjover.github.io/figures/crimes_last_year.png')

# autocorrelation plot
bacf <- acf(by_date$count, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + 
  labs(y = 'autocorrelation', title = 'Atlanta crime time series') +
  theme(title = element_text(size = 18),
        text = element_text(size = 13))
ggsave('~/lfjover.github.io/figures/acf.png')


# forecast using bats
library(forecast)
y <- msts(by_date$count, seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit,h = 365)
fc_df <- data.frame(date = as.Date(1:365,origin = tail(by_date$date,1)),
      forecast = fc$mean, upper = fc$upper[,1], lower = fc$lower[,1])
plot(fc,xlim = c(1,9))
plot.tbats(fit,main = 'time series decomposition', xlim = c(7,9))


ggplot(fc_df) + 
  geom_line(data = fc_df, aes(x=date,y=forecast), color = "blue") +
  geom_ribbon(data = fc_df, aes(x=date, ymin=lower,ymax = upper),color = "grey", alpha = 0.5) +
  geom_line(data = by_date, aes(x=date,y=count))+
  labs(x = '', y ='Number of crimes', title = 'Crime in Atlanta 2009-2016')+
  theme(panel.border = element_blank(), 
                   axis.ticks = element_blank(),
                   title = element_text(size = 20),
                   text = element_text(size = 15))
ggsave('~/lfjover.github.io/figures/forecast.png')

# grouping by month 
by_month_year <- df %>%
            select(date) %>%
            filter(date< as.Date("2016-01-01")) %>%
            mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
            group_by(month,year) %>%
            summarise(count = n()) %>%
            arrange(year,month)
#total number of days for each month
days_by_month_year <- data.frame(date = seq(from = as.Date("2009-01-01"), to = as.Date("2015-12-31"),by=1)) %>%
                  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
                 group_by(month,year) %>%
                 summarise(count = n()) %>%
                 arrange(year,month)
                
# crimes per day in each month                 
by_month_year_norm <- cbind(by_month_year, total_days = days_by_month_year$count) %>%
                  mutate(norm_count = count/total_days)
                  
ggplot(by_month_year_norm,aes(x= month, y = norm_count)) + 
  geom_bar(stat = 'identity', fill = col_pal[4]) +
  theme(text= element_text(size = 10), title = element_text(size = 12))+ 
  labs(y = 'Crimes per day', x = '')+
  facet_wrap(~ year, nrow = 6, scales = "free")

ggplot(by_month_year_norm, aes(x = month, y = year, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = col_pal[4])
            
  
  