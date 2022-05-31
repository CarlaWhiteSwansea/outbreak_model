library(outbreak)
library(dplyr)
library(ggplot2)

results <- scenario_sim(n.sim = 500,
                    num.initial.cases = 1,
                    prop.asym=0,
                    cap_cases = 4500,
                    cap_max_days = 90,
                    r0isolated = 0.7,
                    r0community = 2.3,
                    disp.com = 0.36,
                    disp.iso = 0.8,
                    dist_shape = 2.101547,
                    dist_scale = 11.29064,
                    delay_shape = 1.347738,
                    delay_scale = 4.360796,
                    k = 0)


# Plot of daily cases
gg <- results %>%
  ggplot()+
  geom_line(aes(x=day, y=cumulative, col = as.factor(sim)),alpha=0.6,show.legend = FALSE)+
  scale_y_continuous(name="Number of cases")

outbreak_sims <- results %>%
  filter(day==90 & cumulative > 10) %>%
  select(sim)

outbreak_results <- results %>%
  filter(sim %in% outbreak_sims$sim)

outbreak_mean <- outbreak_results %>%
  group_by(day) %>%
  summarise(mean=mean(cumulative),median=median(cumulative))

gg <- gg+
  geom_line(data=outbreak_mean,aes(x=day,y=mean),colour="black")+
  geom_line(data=outbreak_mean,aes(x=day,y=median),colour="blue")

data <- read.csv("data/monkeypox_uk.csv")

gg <- gg+
  geom_point(data=data,aes(x=day-7,y=cumulative))

gg
