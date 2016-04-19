## under the null, the distribution of p-values is normal
library(ggplot2)
set.seed(3)

nSims <- 11000 #number of simulated experiments
pvalor_null <-numeric(nSims) #set up empty container for all simulated p-values

for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 100, mean = 100, sd = 20) #produce 100 simulated participants
  #with mean=100 and SD=20
  y<-rnorm(n = 100, mean = 100, sd = 20) #produce 100 simulated participants
  #with mean=100 and SD=20
  t<-t.test(x,y) #perform the t-test
  pvalor_null[i]<-t$p.value #get the p-value and store it
}

#now plot the histogram
hist(pvalor_null, main="Histogram of p-values under the null", xlab=("Observed p-value"))

## conclusion, under the null, there is 5% of chance that the p-value is less than .05!

## now, under alternative
pvalor_alternative <- numeric(nSims) #set up empty container for all simulated p-values

for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 1000, mean = 100, sd = 20) #produce  simulated participants
  y<-rnorm(n = 1000, mean = 103, sd = 20) #produce  simulated participants
  t <- t.test(x,y) #perform the t-test
  pvalor_alternative[i] <- t$p.value
}

## fig 1.
hist(pvalor_alternative , main="Histograma de p-valores sob a alternativa", xlab="p-valor", ylab="Frequência", breaks = 100)

## now with more power
pvalor_alternative1 <- numeric(nSims) #set up empty container for all simulated p-values

for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 500, mean = 100, sd = 20) #produce  simulated participants
  y<-rnorm(n = 500, mean = 103, sd = 20) #produce  simulated participants
  t <- t.test(x,y) #perform the t-test
  pvalor_alternative1[i] <- t$p.value
}

hist(pvalor_alternative1)

## so, under the null, p-value is uniform
## under the alternative, there's a peack close to zero. And the amount of concentration depend on the power and the effect size. The bigger both, the higher the peak.


## now, let's investigate publication bias alone (not p-hacking, not garden of forking paths)
# let's say that x% of null results are not published. Ans let's start assuming there are as many null results as alternative (too conservative?)

pvalor_all <- c(pvalor_null, pvalor_alternative)
true_world <- as.factor(rep(c("null", "alternative"), each=nSims))
pvalor_df <- data.frame(pvalor_all, true_world)

## hwat would be the distribution without publication bias
# now, let's say that 80% of null results are not reported.

index <- sample(1:nSims, .2*nSims) ## sampling 20% of all null results
pvalor_pub_bias <- c(pvalor_null[index], pvalor_alternative)

hist(pvalor_all)
hist(pvalor_pub_bias)

mean(pvalor_null)
mean(pvalor_alternative)
mean(pvalor_all)
mean(pvalor_pub_bias)

# publication bias make the data to looks more likly there's only true effects being studied.

## now, let's include p-hacking and include publication bias for a moment
## whenever a p value is above .05, some transformation of the data is included, or new data added, or some data excluded
## to simulate this behavior, we'll assume that, whenveer a p-value is bigger than .05, we simulate again, and again, up to 5 times, to see if it's at leas < .1.
## If after 5 times p-value is not significant, we report no pvalues. If it is, we report only the significant).
## So me mix p-hacking and publication bias



p_value_sig <- numeric(nSims)
p_value_nonsig <- numeric(nSims)
t_result <- numeric(nSims)
for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = 100, mean = 100, sd = 20) #produce  simulated participants
  y<-rnorm(n = 100, mean = 103, sd = 20) #produce  simulated participants
  t_result[i] <- t.test(x,y)$p.value #perform the t-test
}

## simulates p-hacking

## creating function that simulates p-hacking
my_phacking <- function(num_sims, n_sample=100, dif_media_perc = .03, prop_null_true = .5,
                        num_try_pahacking=5, bol_phacking = T, lower_alpha_hacking = .05, max_alpha_hacking = .1) {
  mu <- 100
  p_value_sig <- numeric(nSims)
  p_value_nonsig <- numeric(nSims)
  t_result <- numeric(nSims)
  vecSim <- c(nSims*prop_null_true, nSims)
  for(i in 1:vecSim[1]){ #for each simulated experiment with null hypothesis true
    x<-rnorm(n = n_sample, mean = mu, sd = 20) #produce  simulated participants
    y<-rnorm(n = n_sample, mean = mu, sd = 20) #produce  simulated participants
    t_result[i] <- t.test(x,y)$p.value #perform the t-test
  }

  for(i in (vecSim[1]+1):vecSim[2]){ #for each simulated experiment with null hypothesis false
    x<-rnorm(n = n_sample, mean = mu, sd = 20) #produce  simulated participants
    y<-rnorm(n = n_sample, mean = mu*(1+dif_media_perc), sd = 20) #produce  simulated participants
    t_result[i] <- t.test(x,y)$p.value #perform the t-test
  }


  i <- 1
  while (i <= num_sims){
    if (bol_phacking ) {
      if(t_result[i] <= lower_alpha_hacking) {
        # keep result
        p_value_sig[i] <- t_result[i]
        i <- i+1
      } else {
        n <- 1 # index for break
        repeat {
          n <- n+1
          i <- i+1
          if ( i > nSims) {
            break
          }
          if(t_result[i] <= max_alpha_hacking)  {
            p_value_sig[i] <- t_result[i]
            break
          }
          if (n == num_try_pahacking) {
            p_value_sig[i] <- t_result[i]
            break
          }

        }

      }
    } else {
      p_value_sig[i] <- t_result[i]
      i <- i+1
    }

  }

  p_value_sig1 <- p_value_sig[p_value_sig>0]
  df <- data.frame(p_value_sig1, prop_null_true = paste("prop_null_true é", prop_null_true),
                   dif_media = paste("dif média é de", dif_media_perc*100, "%"),
                   tamnho_amostra = paste("tamanho_amostra é", n_sample),
                   bol_phacking = paste("p-hacking é", bol_phacking))
  return(df)
}

 p_value_sig1 <- my_phacking (num_sims= nSims, prop_null_true = 1, n_sample= 100, num_try_pahacking = 5, bol_phacking = F)
# p_value_sig2 <- my_phacking (num_sims= nSims, prop_null_true = .5, n_sample= 300)
# test <- bind_rows(p_value_sig1, list_df)
hist(p_value_sig1$p_value_sig1)
library(dplyr)

list_df <- list()
prop_null_vec <- seq(0, 1, .2)
amostra_vec <- seq(100, 1000, 200)
bol_p_hacking <- c(T, F)
for ( i in 1:length(prop_null_vec)) {
  for (j in 1:length(amostra_vec)) {
    for (k in 1:length(my_phacking)) {
      aux <- my_phacking (num_sims= nSims, prop_null_true = prop_null_vec[i], n_sample=amostra_vec[j],
                          bol_phacking= bol_p_hacking[k])
      list_df <- bind_rows(aux, list_df)
    }
  }
}

dim(list_df)
summary(list_df)
unique(list_df$tamnho_amostra)
unique(list_df$prop_null_true)
unique(list_df$bol_phacking)
head(list_df)

ggplot(list_df, aes(p_value_sig1)) + geom_histogram() + facet_grid( bol_phacking ~ prop_null_true)

## making multiple plots

## sem p=hacking vs p=hackin
## e variando prop de hype null true

## Usar algum desses plots
## só tem que formatar pra ficar bonitinho.
list_df %>%
  filter(tamnho_amostra == "tamanho_amostra é 100") %>%
ggplot( aes(p_value_sig1)) + geom_histogram() + facet_grid(bol_phacking ~ prop_null_true)

list_df %>%
  filter(tamnho_amostra == "tamanho_amostra é 300") %>%
  ggplot( aes(p_value_sig1)) + geom_histogram() + facet_grid(bol_phacking ~ prop_null_true)

list_df %>%
  filter(tamnho_amostra == "tamanho_amostra é 500") %>%
  ggplot( aes(p_value_sig1)) + geom_histogram() + facet_grid(bol_phacking ~ prop_null_true)


list_df %>%
  filter(tamnho_amostra == "tamanho_amostra é 700") %>%
  ggplot( aes(p_value_sig1)) + geom_histogram() + facet_grid(bol_phacking ~ prop_null_true)

list_df %>%
  filter(tamnho_amostra == "tamanho_amostra é 900") %>%
  ggplot( aes(p_value_sig1)) + geom_histogram() + facet_grid(bol_phacking ~ prop_null_true)



list_df %>%
  group_by(prop_null_true) %>%
  summarise(perc_menor_05 = sum(p_value_sig1 <= .05)/n(),
            perc_entre_05_e_01 = sum(p_value_sig1 > .05 & p_value_sig1 <= .1)/n(),
            perc_maior_01 = sum(p_value_sig1 > .1)/n())

hist( t_result, 20) ## original p-value with no p-hacking

sum(t_result <= .05)/length(t_result)
sum(t_result > .05 & t_result <= .1)/length(t_result)
sum(t_result > .1)/length(t_result)

ggplot(my_df, )

## with this we derive two hipothesis

# 1. Results with p-value below .05 will comprise at least 50% of all results
# 2. Results with p-value between .05 and .1 will comprise at least 20% off all results

# our hipothesis will be falsified if results are close to no p-hacking than with p-hacking.
