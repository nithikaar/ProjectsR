---
  title: "STAT184Team - Grace Wagner, Nithika Radhakrishnan, Tariq Lisse "
output:
  pdf_document: default
html_document: default
date: "2022-12-02"
---
  
  # How does the Acquisition of a New Player Affect the overall Teams Performance ?
  
  ## Relating the Absence of a Star Player to the Overall Teams Performance
  
  "There is no I in team...I know, but there's an M.E in it...The late and great Kobe Bryant once spoke these words to his teammate Shaquille O'neal when asked why he is not passing the ball to his teammates.This is no slander towards teammates who believe the game revolves solely around them and that the ball should always be in their hands in order to guarantee a win for the team. These are facts to show that a star players absence whether it is due to injuries or a trade, does have an affect on the overall teams performance. 

## Purpose

Men lie, women lie, but numbers do not. These days, it is evident just how much not having the leading performer on a team around can either make the team better or make the team worse.

In this project, we need to access how the absence of one player due to injury or trade affects the overall performance of the team. 

### Case 1:

In this particular case, we looked at the Miami Dolphins team. There are different varieties of injuries which have occurred but one can analyze the performance of the players based on that one injury. We are ultimately trying to answer how far does injury negatively impact player stats, and in turn messes with the team statistics as well. 

We will focus our attention for this case on Tua Tagovailoa. Tua Tagovailoa is the starting Quarterback for the Miami Dolphins. According to espn.com, Nfl.com and the official Miami Dolphins team website, Tua is the leading passer on the team. He accounts for more than half of the total offensive yards for the team throughout this season. Out of the total 3,659 passing yards from the team this season, Tua is responsible for 2,859 yards. Also, with the team having a total of 4,562 offensive yards, Tua is responsible for 2,894 total offensive yards if you include passing and rushing yards by the Quarterback.

#### Figure 1 : Miami Dolphins win and loss record through week 12 without Tua Tagavailoa in the lineup
```{r, echo=FALSE, warning=FALSE, message=FALSE}
#load packages
library(dplyr)
library(rvest)
library(ggplot2)
###Scraping
#Scraping for Dolphins data
miamiDolphinsData <- read_html(
x = "https://www.pro-football-reference.com/teams/mia/2022.htm"
)%>%
  html_elements(css = "table")%>%
  html_table()
#getting correct table from data 
miamiDolphins <- miamiDolphinsData[[2]]
#remove unwanted/problematic rows
miamiDolphins <- miamiDolphins[-1,]
miamiDolphins <- miamiDolphins[-11,]
miamiDolphins <- miamiDolphins[-c(12:17),]
#changing week values from characters to numbers
miamiDolphins[[1]] <- as.numeric(as.character(miamiDolphins[[1]]))
#making new data frame
miamiDolphins <- bind_cols(miamiDolphins[[1]], miamiDolphins[[6]])
miamiDolphins <- miamiDolphins
colnames(miamiDolphins) <- c("Weeks", "Outcome")
#making ggplot visualization
ggplot(miamiDolphins) +
  aes(x = Outcome, y = Weeks) +
  geom_point(shape = "circle", size = 1.5, colour = "#112446") +
  scale_y_continuous(breaks = seq(ceiling(min(1)), floor(max(12)), by = 1))+
  theme_minimal()+
  labs(title = "Miami Dolphin's Losing Streak after Quarterback Tua Tagovailoa was Injured", caption = "Stats from the 2022-2023 season, up to week 12.\nWeek 11 was a bye week.", subtitle = "Tua Tagovailoa was injured with a concussion in week 4 and did not return until week 7.",  x = "Wins and Losses", y = "Weeks")
```

#### Figure 2: Miami Dolphins offensive Turnovers throughout Week 12 without Tua in the lineup
```{r setup, include=FALSE}
## Loading in our packages
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

miamiDolphinsData <- read_html(
x = "https://www.pro-football-reference.com/teams/mia/2022.htm"
) %>%
  html_elements(css = "table") %>%
  html_table()
#getting correct table from data 
miamiDolphins <- miamiDolphinsData[[2]]
#remove unwanted/problematic rows
miamiDolphins <- miamiDolphins[-1,]
miamiDolphins <- miamiDolphins[-11,]
miamiDolphins <- miamiDolphins[-c(12:17),]

#changing week values from characters to numbers
miamiDolphins[[1]] <- as.numeric(as.character(miamiDolphins[[1]]))
# Changing offensive turnover values from characters to numbers
miamiDolphins[[17]] <- as.numeric(as.character(miamiDolphins[[17]]))

#making new data frame
miamiDolphins <- bind_cols(miamiDolphins[[1]], miamiDolphins[[17]])

miamiDolphins <- miamiDolphins
colnames(miamiDolphins) <- c("Weeks", "Turnovers")

```
```{r, echo=FALSE, warning=FALSE}
#making ggplot visualization
ggplot(data = miamiDolphins,
       mapping = aes(x = Weeks,
                     y = Turnovers)) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = seq(ceiling(min(1)), floor(max(12)), by = 1)) +
  scale_y_continuous(breaks = seq(ceiling(min(1)), floor(max(3)), by = 1)) +
  theme_minimal() +
  labs(title = "Miami Dolphins Offensive Turnovers Week 1 - Week 12", caption = "Week 11 represents a bye week (no games)", subtitle = "Tua leaves with concussion in week 4 and an ankle injury during week 12", x = "Weeks", y = "Turnovers")
```

## Data Explanation :

When looking at figure 1 and figure 2, one can physically see how Tua's absence leads to the overall team struggle. In figure 1, Tua Tagavailoa was injured with a concussion in Week 4 and did not return until week 7. In the span of the time that he was gone, the team lost three straight games in a row leading them to have an overall record of 3-3. As Tua returns in week 7, the team goes on a five game winning streak leading the team to have an overall record of 8-3. The stats show that with Tua leading the team and being in the starting lineup, the team plays better as they are able to score enough points to secure a W in the win column.

In figure 2, we are shown the offensive turnovers of the team through week 12. As we already know, Tua was injured in week 4 with a concussion and returned in week 7. The study shows that the team is more liable to consistently have turnovers without Tua in the lineup. Their ultimate high in offensive turnovers was three which occurred in week 6, the week before Tua's return. Tua, in comparison to other quarterbacks on his team seems to have a positive impact on his team as the turnovers are not as consistent and not as abundant when he is in the lineup. 

### Data Visual Table :

Below is a representation of the Quarterback statistics for the Miami Dolphins this season based on amount of games played, completion percentation, total passing yards, passing touchdowns, and interceptions.

```{r, echo=FALSE, warning=FALSE, message=FALSE}
# Loading in correct packages
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

## Reading in data from ESPN site

espnMiamiDolphinsTable <- read_html(
  x = "https://www.espn.com/nfl/team/stats/_/name/mia/miami-dolphins"
) %>%
  html_elements(css = "table") %>%
  html_table()

## We want a list of element 1 
espnMiamiPassing <- bind_cols(espnMiamiDolphinsTable[[1]], espnMiamiDolphinsTable[[2]])

## Cleaning Espn Data and converting passing yards to numeric
espnMiamiPassing <- espnMiamiPassing %>%
  filter(Name != "Total") %>%
  tidyr::separate(
    col = Name,
    into = c("First", "Last", "Position")
  ) %>%
  mutate(
    "Player" = paste(First, Last, sep = " "),
    .before = First
  ) %>%
  mutate(
    YDS = parse_number(YDS)
  )
```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
## Loading in correct packages
library(dplyr)
library(kableExtra)
library(knitr)
library(ggplot2)
passingStats <- espnMiamiPassing %>%
  group_by(Player) %>%
  select(GP, `CMP%`, YDS, TD, INT)
passingStats %>%
  kable() %>%
  kable_styling()
```

# Final Implications

Tua Tagovailoa came to Miami as a number 5 draft pick. He came from the university of Alabama where he was the offensive MVP of the 2018 College Football National Championship and also received the Maxwell and Walter Camp Awards as a sophomore there.

The accomplishments made by Tua explains the reasons for the way he outperforms the other quarterbacks on his team leading to him playing and starting in more games and the reason for his numbers to be higher despite him being the youngest quarterback on his roster. Men lie, women lie, but numbers do not. These numbers show us that Tua is the best quarterback for the Miami Dolphins and his impact on the team is a positive one. This is understandable, as without Tua there would have not been such a major impact made by the team.

### Case 2:

In the case of the NBA, one can mention a lot of greats such as Kobe Bryant, Michael Jordan, Kareem Abdul-Jabbar, and Bill Russell to name a few. However, when you think of someone who revolutionized the point guard position and the way the sport is played in this position, one can mention Steve Nash. 

We will focus our attention on Steve Nash to show how he after being traded to the Phoenix Suns from the Dallas Mavericks, he helped improve the team.

#### Figure 1 : Phoenix Suns Wins & Losses (2003-04) (year prior to acquiring Steve Nash)
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# load packages
library(dplyr)
library(rvest)
library(ggplot2)
library(readr)
library(tidyr)
### Scraping
# Scraping for Phoenix Suns 2003 - 2004 data
phoenixSuns2003 <- read_html(
  x = "https://www.landofbasketball.com/results_by_team/2003_2004_suns.htm"
) %>%
  html_elements(css = "table") %>%
  html_table()

## Getting correct table from data
phoenixSunsStats <- phoenixSuns2003[[1]]

## Removing unwanted / problematic rows
phoenixSunsStats <- phoenixSunsStats[-32,]
phoenixSunsStats <- phoenixSunsStats[-62,]
phoenixSunsStats <- phoenixSunsStats[-1,]

## Changing Game values from character to numeric
phoenixSunsStats[[1]] <- as.numeric(as.character(phoenixSunsStats[[1]]))

## Making a new data frame
phoenixSunsStats <- bind_cols(phoenixSunsStats[[1]], phoenixSunsStats[[6]])
phoenixSunsStats <- phoenixSunsStats
colnames(phoenixSunsStats) <- c("Game", "Outcome")

## Making ggplot visualization
ggplot(phoenixSunsStats) +
  aes(x = Outcome, y = Game) +
  geom_point(shape = "diamond", size = 1.0, colour = "darkorange2") +
  scale_y_continuous(breaks = seq(ceiling(min(1)), floor(max(82)), by = 2)) +
  theme_minimal() +
  labs(title = "Phoenix Suns Win/Loss record before acquiring Steve Nash")
```

As you can see from the data visualization, in the 2003-04 season, the Phoenix Suns lost more consistently than they won which resulted in them ending the season with a record of 29-53 and finishing 6th in the NBA Pacific Division and 13th in the Western Conference.

#### Figure 2 : Phoenix Suns Wins & Losses (2004-05) (With Steve Nash)
```{r, echo=FALSE, warning=FALSE, message=FALSE}
# load packages
library(dplyr)
library(rvest)
library(ggplot2)
library(readr)
library(tidyr)
### Scraping
# Scraping for Phoenix Suns 2004 - 2005 data
phoenixSuns2004 <- read_html(
  x = "https://www.landofbasketball.com/results_by_team/2004_2005_suns.htm"
) %>%
  html_elements(css = "table") %>%
  html_table()

## Getting correct table from data
phoenixSunsStats <- phoenixSuns2004[[1]]

## Removing unwanted / problematic rows
phoenixSunsStats <- phoenixSunsStats[-32,]
phoenixSunsStats <- phoenixSunsStats[-62,]
phoenixSunsStats <- phoenixSunsStats[-1,]

## Changing Game values from character to numeric
phoenixSunsStats[[1]] <- as.numeric(as.character(phoenixSunsStats[[1]]))

## Making a new data frame
phoenixSunsStats <- bind_cols(phoenixSunsStats[[1]], phoenixSunsStats[[6]])
phoenixSunsStats <- phoenixSunsStats
colnames(phoenixSunsStats) <- c("Game", "Outcome")

## Making ggplot visualization
ggplot(phoenixSunsStats) +
  aes(x = Outcome, y = Game) +
  geom_point(shape = "diamond", size = 1.0, colour = "darkorange2") +
  scale_y_continuous(breaks = seq(ceiling(min(1)), floor(max(82)), by = 2)) +
  theme_minimal() +
  labs(title = "Phoenix Suns Win/Loss record after acquiring Steve Nash")
```

With the acquisition of Steve Nash, Steve Nash would lead the Phoenix Suns to have 33 more wins than the year before. They ended the 2004-05 season with a 62-20 record and becoming 1st in the Western Conference. With the winning record, the team makes it all the way to the Western Conference finals but lost and had a playoff record of 9-6. 

# Final Thoughts

Many sports require a team of more than one player to operate together to achieve a goal. I am certain that despite the acquisition of Tua Tagovailoa and Steve Nash to their teams did have a positive impact on the team, the teams overall success would not have been possible if it was a one-man team. 

The team sports serve to show the effects of collaboration in the world and that when players from different backgrounds, cultures and ideologies come together with one goal in mind, they can be winners.

## Sources Used

https://www.pro-football-reference.com/teams/mia/2022.htm - Miami Dolphins Data which gave us information on the injury report on players and team statistical information regarding each game for each week.

https://www.espn.com/nfl/team/stats/_/name/mia/miami-dolphins - Another source regarding the Miami Dolphins statistical information for the overall team performance and the performance of each player according to their position. We focused on the three Quarterbacks in their lineup.

https://www.landofbasketball.com/results_by_team/2003_2004_suns.htm - One of the few sources that was able to give win and loss information dating back to the Phoenix Suns franchise in 2003-04.

https://www.landofbasketball.com/results_by_team/2004_2005_suns.htm - Another source for the Phoenix Suns team in the 2004-05 season. This gave us the specific wins and losses as well as the win streak and losing streak of the team.
