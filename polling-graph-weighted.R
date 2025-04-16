library(ggplot2)
library(dplyr)
library(lubridate)
setwd("~/Documents/GitHub/Vic-poll-Charts") # replace with your own working directory
polling2226 <- read.csv("polling2226.csv")
pp2226 <- read.csv("pp2226.csv")
allansat <- read.csv("allan_sat2226.csv")
spansize <- 0.2

election22 <- data.frame(c("LNP","ALP","GRN","OTH"), c(34.4,37.0,11.5,17.1))
#election25 <- data.frame(c("LNP","ALP","GRN","OTH"), c())
election22tpp <- data.frame(c("LNP","ALP"), c(45.0,55.0))
#election25tpp <- data.frame(c("LNP","ALP"), c())
names(election22) <- c("party", "vote")
#names(election25) <- c("party", "vote")
names(election22tpp) <- c("party", "vote")
#names(election25tpp) <- c("party", "vote")


polling2226$sample_size[is.na(polling2226$sample_size)] <- round(mean(polling2226$sample_size, na.rm=TRUE))

polling2226 <- polling2226 %>%
  arrange(desc(as.Date(last_date, '%d %b %Y')))

minss <- min(polling2226$sample_size)
max_date <- max(as.Date(polling2226$last_date, '%d %b %Y')) + days(7)

primary_votes <- ggplot(polling2226, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=pv_alp, size=sample_size), colour="red3", alpha = 3/10) +
  geom_smooth(aes(y=pv_alp, colour="ALP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_lnp, size=sample_size), colour="blue4", alpha = 3/10) +
  geom_smooth(aes(y=pv_lnp, colour="LNP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=pv_grn, size=sample_size), colour="green4", alpha = 3/10) +
  geom_smooth(aes(y=pv_grn, colour="GRN", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_smooth(aes(y=pv_oth, colour="OTH", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(data = election22, aes(x = as.Date('2022-11-26', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election22, aes(x = as.Date('2022-11-26', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  #geom_point(data = election25, aes(x = as.Date('2025-05-??', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  #geom_point(data = election25, aes(x = as.Date('2025-05-??', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  scale_y_continuous(limits=c(0, 50), breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(limits=c(as.Date('2022-11-26', '%Y-%m-%d'), max_date), date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = "1 month", expand = c(0,0)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, shape = 16, size = 3))) +
  theme(legend.key = element_rect(colour = NA, fill = NA), legend.text=element_text(size=12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle=45, vjust=0.5, size=12)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Labor", "Greens", "Liberal-National Coalition", "Other"), 
                     values = c("ALP"="red3", "GRN"="green4", "LNP"="blue4", "OTH"="gray60"))

primary_votes + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12)) +
  guides(colour = guide_legend(order=1, override.aes = list(size = 0, shape = 15)), size = guide_legend(order=2)) +
  scale_size_area(name = "Sample size:")

tpp <- ggplot(polling2226, aes(x=as.Date(last_date, '%d %b %Y'))) +
  theme_bw() +
  geom_point(aes(y=tpp_lnp, size=sample_size), colour="blue4", alpha = 3/10) +
  geom_smooth(aes(y=tpp_lnp, colour="LNP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(aes(y=tpp_alp, size=sample_size), colour="red3", alpha = 3/10) +
  geom_smooth(aes(y=tpp_alp, colour="ALP", weight=sqrt(sample_size)), span = spansize, se = FALSE) +
  geom_point(data = election22tpp, aes(x = as.Date('2022-11-26', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  geom_point(data = election22tpp, aes(x = as.Date('2022-11-26', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  #geom_point(data = election25tpp, aes(x = as.Date('2025-05-??', '%Y-%m-%d'), y = vote, colour = party), shape=23, stroke=0.5, fill = "#FFFFFF", size=4) +
  #geom_point(data = election25tpp, aes(x = as.Date('2025-05-??', '%Y-%m-%d'), y = vote, colour = party), shape=18, size=3) +
  scale_y_continuous(limits=c(35, 65), breaks=c(40,45,50,55,60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(limits=c(as.Date('2022-11-26', '%Y-%m-%d'), max_date), date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = "1 month", expand = c(0,0)) +
  theme(legend.key = element_rect(colour = NA, fill = NA), legend.text=element_text(size=12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle=45, vjust=0.5, size=12)) +
  labs(y="Voters (%)", x= NULL) +
  scale_colour_manual(name="", 
                     labels = c("Australian Labor Party", "Liberal-National Coalition"), 
                     values = c("ALP"="red3", "LNP"="blue4"))
tpp + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12)) +
  guides(colour = guide_legend(order=1, override.aes = list(size = 0, shape = 15)), size = guide_legend(order=2)) +
  scale_size_area(name = "Sample size:")

ppm <- ggplot(pp2226, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=allan), colour="red3", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=allan, colour="Allan"), span = spansize, se = FALSE) +
  geom_point(aes(y=battin), colour="blue4", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=battin, colour="Battin"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="grey20", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Unknown"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(10, 20, 30, 40, 50, 60), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="Support (%)", x= NULL) +
  scale_colour_manual(name="",
                     labels = c("Allan", "Battin", "Don't Know"),
                     values = c("Allan"="red3", "Battin"="blue4", "Unknown" = "grey60"))
ppm + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

alp_sat <- ggplot(allansat, aes(x=as.Date(date, '%d-%b-%y'))) +
  theme_bw() +
  geom_point(aes(y=satisfied), colour="#02e03d", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=satisfied, colour="Satisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=dissatisfied), colour="#f74888", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=dissatisfied, colour="Dissatisfied"), span = spansize, se = FALSE) +
  geom_point(aes(y=unknown), colour="#b3b3b3", size=2.5, alpha = 3/10) +
  geom_smooth(aes(y=unknown, colour="Don't Know"), span = spansize, se = FALSE) +
  scale_y_continuous(limits=c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70), minor_breaks = NULL, expand = c(0,0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=12), axis.text.y = element_text(size=12), axis.title.y = element_text(size=14)) +
  labs(y="% satisfaction", x= NULL, title = "Jacinta Allan approval rating") +
  scale_colour_manual(name="",
                      breaks = c("Satisfied", "Dissatisfied", "Don't Know"),
                      labels = c("Satisfied", "Dissatisfied", "Don't Know"),
                      values = c("Satisfied"="#02e03d", "Dissatisfied"="#f74888", "Don't Know" = "#b3b3b3"))
alp_sat + theme(legend.position="bottom", legend.box = "horizontal", legend.text = element_text(size=12))

