setwd("C:/Users/sergi/Downloads/ITQB/Plate readings/Exp20_3plates_17012024")

library(tidyverse)

library(readxl)
Exp20 <- Exp20_3plates_17012024 <- read_excel("Exp20_3plates_17012024_final_final.xlsx")
View(Exp20) 

Exp20_v1 <- Exp20 %>%
  pivot_longer(names_to = "Condition", values_to = "OD_578", cols = -Time_h)
Exp20_v1
View(Exp20_v1)


Exp20_v2 <- separate(Exp20_v1, Condition, 
                     c("Strain",
                       "Transfer_medium",
                       "Zn_condition", 
                       "Tech_Repl",
                       "Biol_Repl",
                       "TPEN_conc",
                       "Well_number"), 
                     sep = "_", remove = FALSE,
                     convert = FALSE)
View(Exp20_v2)

Exp20_v2$OD_578 <- as.numeric (Exp20_v2$OD_578) #Because OD_578 values are character

Exp20_v2$TPEN_conc <- factor(Exp20_v2$TPEN_conc, levels = c("ctrTPENcells",
                                                            "ctrTPENwZn",
                                                            "ctrTPEN",
                                                            "3.6",
                                                            "4.8",
                                                            "6",
                                                            "7.2",
                                                            "8.4",
                                                            "9.6",
                                                            "10.8",
                                                            "12",
                                                            "14",
                                                            "16",
                                                            "18",
                                                            "20",
                                                            "22",
                                                            "24",
                                                            "26",
                                                            "28"))

Exp20_v2 <- Exp20_v2 %>% mutate(TPEN_conc = recode (TPEN_conc,
                                                    "ctrTPENcells" = "blank",
                                                    "ctrTPENwZn" = "blankZn",
                                                    "ctrTPEN" = "0 uM",
                                                    "3.6"= "18 uM",
                                                    "4.8"= "24 uM",
                                                    "6" = "30 uM",
                                                    "7.2" = "36 uM",
                                                    "8.4" = "42 uM",
                                                    "9.6" = "48 uM",
                                                    "10.8" = "54 uM",
                                                    "12" = " 60 uM",
                                                    "14" = "70 uM",
                                                    "16" = "80 uM",
                                                    "18" = "90 uM",
                                                    "20" = "100 uM",
                                                    "22" = "110 uM",
                                                    "24" = "120 uM",
                                                    "26" = "130 uM",
                                                    "28" = "140 uM"))

Exp20_v2$Strain <- factor(Exp20_v2$Strain, levels = c("5011",
                                                      "5019",
                                                      "5026",
                                                      "5032",
                                                      "5071",
                                                      "12004",
                                                      "14076",
                                                      "HM04"))

# Exp20_v2 %>%
#   ggplot(aes(x= Time_h, y=OD_578, color = Tech_Repl)) +
#   geom_line() +
#   facet_grid (Strain ~ TPEN_conc)


Exp20_v2 %>% filter (Transfer_medium == "mGAM") %>%
  ggplot(aes(x= Time_h, y=OD_578, color = Tech_Repl)) +
  geom_line() +
  facet_grid (Strain ~ TPEN_conc)

Exp20_v2 %>% filter (Transfer_medium == "M8") %>%
  ggplot(aes(x= Time_h, y=OD_578, color = Tech_Repl)) +
  geom_line() +
  facet_grid (Strain ~ TPEN_conc)


Exp20_v3<- Exp20_v2 %>% group_by(Strain, Transfer_medium, Zn_condition, Tech_Repl, Biol_Repl, Well_number) %>%
  mutate(blank = Time_h ==0)

View(Exp20_v3)

Exp20_v4 <- Exp20_v3 %>%
  group_by(Strain, Transfer_medium, Zn_condition, Tech_Repl, Biol_Repl, Well_number) %>%
  mutate(OD = OD_578 - OD_578[blank]) %>%
  ungroup()


View(Exp20_v4)

#Obtain the mean and sd
Exp20_v5 <- Exp20_v4 %>% group_by(Time_h, Strain, Transfer_medium, TPEN_conc) %>%
  summarise(ODmean = mean(OD), sdOD = sd(OD), n())

Exp20_v5$Strain <- factor(Exp20_v5$Strain, levels = c("5011",
                                                      "5019",
                                                      "5026",
                                                      "5032",
                                                      "5071",
                                                      "12004",
                                                      "14076",
                                                      "HM04"))


Exp20_v5 <- Exp20_v5 %>% mutate(TPEN_conc = recode (TPEN_conc,
                                                    "ctrTPENcells" = "blank",
                                                    "ctrTPENwZn" = "blankZn",
                                                    "ctrTPEN" = "0 uM",
                                                    "3.6"= "18 uM",
                                                    "4.8"= "24 uM",
                                                    "6" = "30 uM",
                                                    "7.2" = "36 uM",
                                                    "8.4" = "42 uM",
                                                    "9.6" = "48 uM",
                                                    "10.8" = "54 uM",
                                                    "12" = "60 uM",
                                                    "14" = "70 uM",
                                                    "16" = "80 uM",
                                                    "18" = "90 uM",
                                                    "20" = "100 uM",
                                                    "22" = "110 uM",
                                                    "24" = "120 uM",
                                                    "26" = "130 uM",
                                                    "28" = "140 uM"))


# stocks conc / 0.001(mM --> uM) / d.f (=2)--> conc TPEN (now in the table)


Exp20_v5  %>% filter (Transfer_medium == "M8") %>%
  ggplot(aes(x = Time_h,
             y = ODmean,
             color = Transfer_medium
  )) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=ODmean-sdOD, ymax=ODmean+sdOD, fill = Transfer_medium),
              alpha = 0.3, colour = NA) +
  facet_grid (Strain ~ TPEN_conc) +
  theme_bw()


Exp20_v5  %>% filter (Transfer_medium == "mGAM") %>%
  ggplot(aes(x = Time_h,
             y = ODmean,
             color = Transfer_medium
  )) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin=ODmean-sdOD, ymax=ODmean+sdOD, fill = Transfer_medium), 
              alpha = 0.3, colour = NA) + 
  facet_grid (Strain ~ TPEN_conc) +
  theme_bw()
