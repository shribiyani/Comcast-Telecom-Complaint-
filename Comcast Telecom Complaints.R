

library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Importing & loading dataset as -

comcast_data <- read.csv("E:/DATA ANALYTICS/Projects/R Projects/2. Comcast telecom complaints data/Comcast Telecom Complaints data.csv", header = TRUE)

# manipulating column names

names(comcast_data)<- stri_replace_all(regex = "\\.",replacement = "",str = names(comcast_data))
head(comcast_data)
View(comcast_data)

# finding NA or null in data

na_vec<- is.na(comcast_data)
length(na_vec[na_vec==T])

# modification in date

comcast_data$Date<- dmy(comcast_data$Date)

# Extracting monthly ticket count

monthly_count <- summarise(group_by(comcast_data, Month = as.integer(month(Date))),count = n())
monthly_count <- arrange(monthly_count,Month)
View(monthly_count)

# Comparing Monthly Complaints

ggplot(data = monthly_count,aes(Month,count,label = count))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = monthly_count$Month)+
  labs(title = "Monthly Ticket Count",x = "Monthly", y = "No.of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))

# Extracting Daily Ticket counts

daily_count<- summarise(group_by(comcast_data,Date),count =n())
View(daily_count)

# Comparing Daily Ticket Complaints

ggplot(data = daily_count,aes(as.POSIXct(Date),count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

# Complaints Types as -

network_tickets<- contains(comcast_data$CustomerComplaint,match = 'network',ignore.case = T)
table(network_tickets)
View(network_tickets)

internet_tickets<- contains(comcast_data$CustomerComplaint,match = 'internet',ignore.case = T)
table(internet_tickets)
View(internet_tickets)

billing_tickets<- contains(comcast_data$CustomerComplaint,match = 'bill',ignore.case = T)
table(billing_tickets)
View(billing_tickets)

email_tickets <- contains(comcast_data$CustomerComplaint,match = 'email',ignore.case = T)
table(email_tickets)
View(email_tickets)

charges_tickets<- contains(comcast_data$CustomerComplaint,match = 'charge',ignore.case = T)
table(charges_tickets)
View(charges_tickets)


comcast_data$ComplaintType[internet_tickets]<- "Internet"
comcast_data$ComplaintType[network_tickets]<- "Network"
comcast_data$ComplaintType[email_tickets]<- "Email"
comcast_data$ComplaintType[charges_tickets]<- "Charges"
comcast_data$ComplaintType[billing_tickets]<- "Billing"
comcast_data$ComplaintType[-c(internet_tickets,network_tickets,
                              billing_tickets,charges_tickets,email_tickets)]<- "Others"

table(comcast_data$ComplaintType)
summary(comcast_data$ComplaintType)
View(comcast_data$ComplaintType)
View(comcast_data)

# Creating new variable Complaint status with Open and Closed

open_complaints<- (comcast_data$Status == "Open"|comcast_data$Status =="Pending")
table(open_complaints)
View(open_complaints)

closed_complaints<- (comcast_data$Status == "Closed"|comcast_data$Status =="Solved")
table(closed_complaints)
View(closed_complaints)

comcast_data$ComplaintStatus[open_complaints]<-"Open"
comcast_data$ComplaintStatus[closed_complaints]<-"Closed"

View(comcast_data$ComplaintStatus[open_complaints])
View(comcast_data$ComplaintStatus[closed_complaints])

View(comcast_data)

# Complaint Type wise Status 

comcast_data<- group_by(comcast_data,ComplaintType,ComplaintStatus)
chart_data<- summarise(comcast_data,Count = n())
View(chart_data)

ggplot(as.data.frame(chart_data),mapping = aes(ComplaintType,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16, colour = "#0073C2FF"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Ticket Status",
       x = "Complaint Type",y = "No. of Tickets",
       color = "#0073C2FF")

# Creating Stacked Bar chart for Complaints based on State & Status

comcast_data<- group_by(comcast_data,State,ComplaintStatus)
chart_data<- summarise(comcast_data,Count = n())
View(chart_data)

ggplot(as.data.frame(chart_data),mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16, colour = "#0073C2FF"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "State wise Ticket Status",
       x = "States",y = "No. of Tickets",
       fill = "Status")

# Finding State which has Highest number of Unsolved Tickets

chart_data%>%
  filter(ComplaintStatus == "Open")->
  open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]

chart_data %>%
  filter(ComplaintStatus == "Closed") ->
  closed_complaints
closed_complaints[closed_complaints$Count == max(closed_complaints$Count/100*100),c(1,3)]

# Calculating Resolution Percentage based on Total & Category

resolved_data <- group_by(comcast_data,ComplaintStatus)
total_resolved<- summarise(resolved_data,percentage =(n()/nrow(resolved_data)))
View(total_resolved)

resolved_data <- group_by(comcast_data,ReceivedVia,ComplaintStatus)
Category_resolved<- summarise(resolved_data, percentage =(n()/nrow(resolved_data)))
View(Category_resolved)

# Pie Chart for Resolved vs Category Resolved

par(mfrom = c(1,2))

# Pie Chart  for Resolved

ggplot(total_resolved,
       aes(x="",y=percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic() +theme(axis.line = element_blank(),
                         axis.text = element_blank(),
                         axis.ticks = element_blank())

# Pie chart for Category wise Ticket Satus

ggplot(Category_resolved,
       aes(x="",y=percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(ReceivedVia,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic() +theme(axis.line = element_blank(),
                         axis.text = element_blank(),
                         axis.ticks = element_blank())
