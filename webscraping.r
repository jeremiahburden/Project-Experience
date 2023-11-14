label <- c('TSLA', 'AMZN', 'AAPL') # choosing the tickers to be scraped

url <- paste0('https://finviz.com/quote.ashx?t=',label,'&p=d') # selecting the websites of interest
webpage <- read_html(url)
label_html <- html_nodes(webpage,'.adhesion_collapse , .snapshot-td2-cp') # leveraging SelectorGadget to grab the appropriate data
labels <- html_text(label_html)
values_html1 <- html_nodes(webpage, '.snapshot-td2')
data <- html_text(values_html)
df <- data.frame(labels,data) # saving the data into a table
df
