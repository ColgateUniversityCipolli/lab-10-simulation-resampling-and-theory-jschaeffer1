library(tidyverse)

##########################################################
#####             PART 1: BASIC SIMULATION          ######
##########################################################

#Generating resamples, but making values percentage instead of raw # satisfied
part1.sample1 = rbinom(10000, 1004, .39)/1004 
#view(part1.sample1)
sample1_df = tibble(value = part1.sample1)

#Making a histogram with a superimposed density plot
part1_plot1 = ggplot() + 
  geom_histogram(aes(part1.sample1, y=after_stat(density))) +
  geom_density(aes(part1.sample1)) + 
  geom_hline(yintercept=0) +
  theme_bw() +
  xlab("Percentage Satisfied with US") +
  ylab("Density")

#Making a summary to determine the upper and lower bounds
sample1_summary = sample1_df |>
  summarize(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    MOE = (upper-lower)/2
  )
#3% margin of error

#DOUBLING SAMPLE SIZE
#Generating resamples, but making values percentage instead of raw # satisfied
part1.sample2 = rbinom(10000, 2008, .39)/2008

sample2_df = tibble(value = part1.sample2)

#Making a histogram with a superimposed density plot
part1_plot2 = ggplot() + 
  geom_histogram(aes(part1.sample2, y=after_stat(density))) +
  geom_density(aes(part1.sample2)) + 
  geom_hline(yintercept=0) +
  theme_bw() +
  xlab("Percentage Satisfied with US") +
  ylab("Density")


#Making a summary to determine the upper and lower bounds
sample2_summary = sample2_df |>
  summarize(
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    MOE = (upper-lower)/2
  )
#2.1% margin of error

##########################################################
#####             PART 2: RESAMPLING                ######
##########################################################
#Making a tibble representative of the original data
gallup_df = tibble(
  value = c(rep(1, round(1004*.39)), rep(0, 1004-round(1004*.39)))
)

#Setitng up resampling
R <- 1000
resamples <- tibble(p.hat = numeric(R))

for( i in 1:R){ #Running for loop to take 1000 resamples
  # Take a resample
  curr.resample <- sample(x = gallup_df$value,
                          size = nrow(gallup_df),
                          replace = T)
  # compute the stat on the resample
  resamples$p.hat[i] <- mean(curr.resample)
}
#view(resamples)

#Plotting a histogram and density line of the resample mean
resample_plot = ggplot() + 
  geom_histogram(aes(resamples$p.hat, y=after_stat(density))) +
  geom_density(aes(resamples$p.hat)) + 
  geom_hline(yintercept=0) +
  theme_bw() +
  xlab("Percentage Satisfied with US") +
  ylab("Density")

#Checking what the 95% spread of the resamplings are
resample_summary = resamples |>
  summarize(
    lower = quantile(p.hat, 0.025),
    upper = quantile(p.hat, 0.975),
    MOE = (upper-lower)/2
  )
#3% margin of error

##########################################################
#####       PART 3: SIMULATION OVER N AND P         ######
##########################################################
#Initializing n and p
n = seq(100, 3000, by=10)
p = seq(0.01, 0.99, by=0.01)
sims = 10000

#Initialing tibble
sims_df = tibble(
  n = numeric(),  
  p = numeric(),  
  MOE = numeric()
)


for(i in n){ #Looping over each n value
  
  for (j in p){ #Looping over each percentage
    sample_sim = rbinom(sims, i, j)/i #Generating simulation
    temp_df = tibble(value= sample_sim) #Converting into a tibble for summary
    
    curr_summary = temp_df |>
      summarize(
        MOE = (quantile(value, 0.975)-quantile(value, 0.025))/2
      )
    #Adding new row to original tibble
    sims_df = bind_rows(sims_df, tibble(n=i, p=j, MOE=curr_summary$MOE))
  }
}
#Turning the margin of error into a percentage
sims_df = sims_df |>
  mutate(MOE = MOE*100) |>
  mutate(p = p*100)

#view(sims_df)
sim_plot = ggplot(sims_df, aes(n,p)) +
  geom_raster(aes(fill = MOE))+
  scale_fill_viridis_c() +
  xlab("Sample Size") +
  ylab("Percentage Estimated")


##########################################################
#####   PART 4: ACTUAL MARGIN OF ERROR CALCULATION   #####
##########################################################
#Initializing sample sizes and probabilities
n = seq(100, 2000, by=10)
p = seq(0.01, 0.99, by=0.01)
z=1.96

#Initializing tibble
wilson_df = tibble(
  n = numeric(),  
  p = numeric(),  
  wilson = numeric()
)

for(i in n){
  
  for (j in p){
    X = round(i*j) #Getting number of successes
    #wilson = ((z^2/i)+2*j)/(2*(1+(z^2/i))) #Calculating wilson estimate
    wilson_num = sqrt(z^4/4+z^2*X-(z^2*X^2)/i)
    wilson_denom = i+z^2
    wilson = wilson_num/wilson_denom
    #Adding new row to tibble
    wilson_df = bind_rows(wilson_df, tibble(n=i, p=j, wilson=wilson))
  }
}

wilson_df = wilson_df |>
  mutate(wilson = wilson*100) |>
  mutate(p = p*100)

#view(wilson_df)

wilson_plot = ggplot(wilson_df, aes(n,p)) +
  geom_raster(aes(fill = wilson))+
  scale_fill_viridis_c() +
  xlab("Sample Size") +
  ylab("Percentage Estimated")





