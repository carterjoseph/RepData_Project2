Data <- read.csv("repdata_data_StormData.csv", header = TRUE)

Data$EVTYPE<- toupper(Data$EVTYPE)

Data$EVTYPE[grep("TSTM WIND", Data$EVTYPE)] <- "THUNDERSTORM WIND"
Data$EVTYPE[grep("THUNDERSTORM", Data$EVTYPE)] <- "THUNDERSTORM WIND"


Fatalities <- Data %>%  
        group_by(EVTYPE) %>% 
                summarise(Total_Fatalities = sum(FATALITIES))


Injuries <- Data %>% 
        group_by(EVTYPE) %>% 
                summarise(Total_Injuries = sum(INJURIES))



Population_Health<- merge(Fatalities, Injuries, by = "EVTYPE")

Population_Health <- Population_Health %>% 
        mutate("Total_Damage" = Total_Fatalities + Total_Injuries) %>% 
                arrange(desc(Total_Damage)) %>% 
                        slice(1:10)
library(ggplot2)
ggplot(data=Population_Health, aes(x=(reorder(EVTYPE, -Total_Damage)), y=Total_Damage)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Events that are most harmful to human population in US") +
        labs(x = "Events type", y = "Total injuries and fatalities") +
        theme_classic(base_family = "Arial") +
        coord_flip() 
        
Data2 <- select(Data, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)


Data2$PROPDMGEXP[grep("K", Data2$PROPDMGEXP)] <- "1000"
Data2$PROPDMGEXP[grep("M", Data2$PROPDMGEXP)] <- "1000000"
Data2$PROPDMGEXP[grep("B", Data2$PROPDMGEXP)] <- "1000000000"
Data2$PROPDMGEXP <- as.numeric(Data2$PROPDMGEXP)

Data2$CROPDMGEXP[grep("K", Data2$CROPDMGEXP)] <- "1000"
Data2$CROPDMGEXP[grep("M", Data2$CROPDMGEXP)] <- "1000000"
Data2$CROPDMGEXP[grep("B", Data2$CROPDMGEXP)] <- "1000000000"
Data2$CROPDMGEXP <- as.numeric(Data2$CROPDMGEXP)

Data2 <- mutate(Data2, "Total_Prop_Dmg" = PROPDMG * PROPDMGEXP )
Data2 <- mutate(Data2, "Total_Crop_Dmg" = CROPDMG * CROPDMGEXP )

Data2Summary <- Data2 %>% group_by(EVTYPE) %>% 
        summarise(TotalProp = sum(Total_Prop_Dmg, na.rm = TRUE), TotalCrop = sum(Total_Crop_Dmg, na.rm = TRUE)) %>%
                mutate(CombinedDamage = TotalProp + TotalCrop) %>% arrange(desc(CombinedDamage)) %>% 
                         slice(1:10)

ggplot(data=Data2Summary, aes(x=(reorder(EVTYPE, -CombinedDamage)), y=CombinedDamage/1000000)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Events that are most harmful to crops and properties in US") +
        labs(x = "Event type", y = "Total property and crop damage (millions of $)") +
        theme_classic(base_family = "Arial") +
        coord_flip() 




