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


ggplot(tb, aes(x = day_time, y = Global_active_power)) + 
        geom_line() + 
        scale_x_datetime(breaks = date_breaks("1 day"), date_labels = "%a") + 
        labs(x = "", y = "Global Active Power (kilowatts)")
        ggsave(
                filename = "plot2.png",
                device = "png",
                height = 4,
                width = 4,
                units = "in",
                dpi = 120
        )