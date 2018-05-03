library("tidyverse")

#Loading the data
US_Cost <- readxl::read_xlsx("us_avg_tuition.xlsx",1)

#Tidy the data
US_Cost <- gather(US_Cost,year,avg_tuition,-State)

#Plotting data for states
ggplot(data=US_Cost %>% 
         geom_path(aes(x=year, y=avg_tuition, group="all")) +
         facet_wrap(~State) + 
         labs(x="Year", y="Avg Tuition Cost") +
         theme(axis.text.x=element_text(angle=90)))
