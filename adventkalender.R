library(tidyverse)
library(googlesheets4)
library(ggridges)

f.ggsave.goldenratio <- function(ff) {
  ggsave(file=paste0(ff, ".png"), width = 30, height = 30/((1+sqrt(5))/2), units = "cm")
}

#get sheet data
gg <- Sys.getenv("Schanz.ggsheet") 
gg.dta <- read_sheet(gg)

# wrangle data; set up date format
dta <- gg.dta %>% 
  mutate(
    date = as.Date(paste(Year, "12", Day, sep="-")),
    Year = as.factor(Year)
  ) %>%
  relocate(Year, Day, date)

# descriptive statistics
dta %>% 
  group_by(Year) %>% 
  summarise(
    n = n(), 
    sum = sum(Offer),
    mean = mean(Offer),
    sd = sd(Offer), 
    min = min(Offer), 
    max = max(Offer)
    ) 

# total by year
dta %>% ggplot(aes(x=Year, y=Offer, fill=Year))+
  geom_bar(stat="identity")+
  theme_minimal()+
  labs(
    title = "Sum of offers per year (Euros)",
    subtitle = "Schanz Adventskalender",
    x = ""
  )

f.ggsave.goldenratio("sumperyear")

# split by year and show distribution
ggplot(dta, aes(x=Offer, y=Year, group=Year, fill=Year))+
  geom_density_ridges(stat = "binline", scale = 0.9)+
  scale_x_continuous(limits = c(0, 1200))+
  theme_minimal()+
  labs(
    title = "Distribution of offers per year (only offers of <1200 Euros)",
    subtitle = "Schanz Adventskalender",
    y = ""
  )

f.ggsave.goldenratio("densityperyear")

