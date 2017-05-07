library(tidyverse)
library(scales)

setwd("~/Documents/Coursera/Data_Science_Johns_Hopkins/4_Exploratory_Analysis/week1/project")

tb <- read_delim(
                        file = "household_power_consumption.txt", 
                        delim = ";",
                        #n_max = 10000,
                        na = "?",
                        col_types = cols_only(
                                `Date` = col_date(format = "%d/%m/%Y"),
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


ggplot(tb, aes(Global_active_power)) + 
        geom_histogram(
                binwidth = 0.5,
                boundary = -0.5,
                fill = "red",
                col = "black"
        )+
        labs(title = "Global Active Power") + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        labs(x = "Global Active Power (kilowatts)", y = "Frequency")+  
        scale_y_continuous(breaks = pretty_breaks(n = 7)) + 
        ggsave(
                filename = "plot1.png",
                device = "png",
                height = 4,
                width = 4,
                units = "in",
                dpi = 120
                )