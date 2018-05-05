library("tidyverse")
library("here")
library(readxl)
library(glue)
library("ggrepel")

#Loading the data
US_Cost <- readxl::read_xlsx("us_avg_tuition.xlsx",1)

#Tidy the data
US_Cost <- gather(US_Cost,year,avg_tuition,-State)

#Plotting data for states in faceted boxes (not great visual)
ggplot(data=US_Cost) + 
         geom_path(aes(x=year, y=avg_tuition, group="all")) +
         facet_wrap(~State) + 
         labs(x="Year", y="Avg Tuition Cost") +
         theme(axis.text.x=element_text(angle=90))

#adding 5 yr change rate
US_Cost_5 <- US_Cost %>% 
  group_by(State) %>%
  mutate(lag=lag(avg_tuition, 5), pct.change=(avg_tuition-lag)/lag) %>%
  na.omit

#Plotted barbell graph
US_Cost_5 %>%
  filter(year %in% c("2010-11", "2015-16")) %>%
  ggplot(aes(x=avg_tuition, y=fct_reorder(State, avg_tuition, min), color=year, group=State))+
  geom_line() +
  geom_point() +
  scale_x_continuous(labels=scales::dollar) +
  guides(color=FALSE) +
  labs(title="Tuition Growth from 2010 to 2016", x="Tuition", y="State") + 
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))


nat_avg <- US_Cost %>%
  filter(year %in% c("2010-11", "2015-16")) %>%
  group_by(year) %>%
  summarize(avg_tuition=mean(avg_tuition)) %>%
  mutate(state="National Average")

plot <- US_Cost %>%
  filter(year %in% c("2010-11", "2015-16")) %>%
  left_join(select(nat_avg, year, nat_avg=avg_tuition), by="year") %>%
  bind_rows(nat_avg)

labels <- plot %>%
  group_by(State) %>%
  filter(all(avg_tuition > nat_avg)) %>%
  pull(State) %>% 
  unique()

plot_data <- plot %>%
  ggplot(., aes(x=year, y=avg_tuition, group=State)) +
  geom_text_repel(data=filter(plot, State %in% labels, year=="2015-16"), aes(label=State), 
                  direction="y", nudge_x=0.1, segment.size=0.1, family="Oxygen", size=3) +
  geom_path(color="grey50", size=0.5, alpha=0.5) +
  geom_point(color="grey50") +
  geom_path(data=nat_avg, color="red", size=1) +
  geom_point(data=nat_avg, color="red") +
  scale_y_continuous(labels=scales::dollar) + 
  labs(x=NULL, y=NULL, title="Avg US Tuition 2010 vs 2016") +
  theme_minimal(base_family="Oswald Light") +
  theme(panel.grid.minor=element_blank())

plot_data
