library(tidyverse)
library(lubridate)
library(scales)

setwd("~/Documents/Coursera/Data_Science_Johns_Hopkins/4_Exploratory_Analysis/week1/project")

tb <- read_delim(
        file = "household_power_consumption.txt", 
        delim = ";",
        #n_max = 10000,
        na = "?",
        col_types = cols_only(
                `Date` = col_date(format = "%d/%m/%Y"),
                `Time` = col_time(),
                `Global_active_power` = col_double(),
                `Global_reactive_power` = col_double(),
                `Voltage` = col_double(),
                `Sub_metering_1` = col_double(),
                `Sub_metering_2` = col_double(),
                `Sub_metering_3` = col_double()
        )
)

tb <- tb %>%
        filter(
                Date >= "2007-02-01",
                Date <= "2007-02-02"
        ) 

tb <- tb %>% 
        mutate(
                day_time = ymd_hms(paste(tb$Date, tb$Time))
        )


p1 <- ggplot(tb, aes(x = day_time, y = Global_active_power)) + 
        geom_line() + 
        scale_x_datetime(breaks = date_breaks("1 day"), date_labels = "%a") + 
        labs(x = "", y = "Global Active Power")

tb_new <- tb %>% select(
        day_time,
        Sub_metering_1,
        Sub_metering_2,
        Sub_metering_3
)


p2 <- ggplot(tb, aes(x = day_time, y = Voltage)) + 
        geom_line()+
        scale_x_datetime(breaks = date_breaks("1 day"), date_labels = "%a") + 
        scale_y_continuous(breaks = pretty_breaks(n = 6)) + 
        labs(x = "datetime", y = "Voltage")


tb_new <- melt(tb_new, id.vars = c("day_time"))

p3 <- ggplot(tb_new, aes(x = day_time, y = value)) + 
        geom_line(aes(colour = variable)) + 
        scale_x_datetime(breaks = date_breaks("1 day"), date_labels = "%a") + 
        labs(x = "", y = "Energy sub metering") + 
        scale_colour_discrete(name = "Sub metering",
                              breaks = c(
                                      "Sub_metering_1", 
                                      "Sub_metering_2",
                                      "Sub_metering_3"
                              ),
                              labels = c(
                                      "Sub_metering_1", 
                                      "Sub_metering_2",
                                      "Sub_metering_3"
                              )
        )
        scale_shape_discrete(name = "Sub metering",
                     breaks = c(
                             "Sub_metering_1", 
                             "Sub_metering_2",
                             "Sub_metering_3"
                     ),
                     labels = c(
                             "Sub_metering_1", 
                             "Sub_metering_2",
                             "Sub_metering_3"
                     )
)
        
p4 <- ggplot(tb, aes(x = day_time, y = Global_reactive_power)) + 
        geom_line() + 
        scale_x_datetime(breaks = date_breaks("1 day"), date_labels = "%a") + 
        labs(x = "datetime", y = "Global Reactive Power")


library(gridExtra)
 p <- grid.arrange(p1, p2, p3, p4, ncol = 2)

ggsave(
        filename = "plot4.png",
        device = "png",
        height = 10,
        width = 10,
        units = "in",
        dpi = 48, 
        plot = p
)