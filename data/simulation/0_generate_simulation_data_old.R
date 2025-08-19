library(dplyr)
library(ggplot2)
library(tidyr)

## number of  students
N = 120
p = 10
## generate network from a stochastic block model; this is yield fixed across simulations
## groups are 3 and equally splitted
groups = c(rep(1,N/3),rep(2,N/3),rep(3,N/3))
set.seed(1234)
W = matrix(0,N,N)
for(i in 2:N)
{
 for(j in 1:(i-1))
 {
  if(groups[i] == groups[j])
  {
	 W[i,j] = rbinom(1,1, 0.9)
  }
  else
  {
	  W[i,j] = rbinom(1,1, 0.1)
  }
  W[j,i] = W[i,j]
 }
}

saveRDS(W,"sim_W.rds")


plotW = as_tibble(W) %>%
        	mutate( row = 1:N) %>%
         gather(col,value,-row) %>% 
         mutate(col = sapply(col, \(x) as.numeric(strsplit(x,"V")[[1]][2] )  )) %>% 
         mutate(row = factor(row, levels =  1:N), col = factor(col, levels =  N:1)) %>% 
	 ggplot() + 
	 geom_tile(aes(x = row, y = col, fill = factor(value)),col = 'grey50', show.legend = FALSE)+
	 scale_fill_manual(values= c('white','black'))+
	 xlab('')+
	 ylab('')+
	 theme_classic(14)+
	 theme( axis.text.x = element_blank(), axis.text.y = element_blank(), aspect.ratio=0.9/1 )
        #ggsave("plots/simulatedNetwork.png", height = 7, width = 7)


#++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++
#++ Generate Latent ability   +++++++++++++
#++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++

### NORMAL(0,1) ---S1

set.seed(123 + 1)
Scenario1 = lst()

Scenario1$ability  = rnorm(N,0,1)
##Scenario1$ability  = scale(Scenario1$ability)


plotAbilityS1 = tibble(x =1:N, ability = Scenario1$ability) |>
                ggplot() +
                geom_histogram(aes(x= ability), bins = 30, col = 'black', fill = 'grey80')+
                scale_y_continuous( expand = c(0, 0)) +
                ggtitle("Scenario 1")+
                theme_classic(14) +
                theme(plot.title = element_text(hjust = 0.5))

### CAR model with sigma2 = 1 and rho = 0.98
set.seed(123 + 2)
Scenario2 = lst()
Scenario2$rho = 0.98
Scenario2$sigma2 = 20
D_w = diag(colSums(W))
Scenario2$Sigma_W = solve((D_w - Scenario2$rho*W)/Scenario2$sigma2)
Scenario2$ability = c(mvtnorm::rmvnorm(1, sigma = Scenario2$Sigma_W))
## renormalize ability, i.e. dividing by their standard deviation

Scenario2$ability = Scenario2$ability/sqrt(diag(Scenario2$Sigma_W))


##Scenario2$ability  = scale(Scenario2$ability)

##tibble(x =1:N, ability = Scenario2$ability) |>
##ggplot() +
##geom_violin(aes(x="1", y = ability))+
##geom_jitter(aes(x="1", y = ability))




plotAbilityS2 = tibble(x =1:N, ability = Scenario2$ability) |>
                ggplot() +
                geom_histogram(aes(x= ability), bins = 30, col = 'black', fill = 'grey80')+
                scale_y_continuous( expand = c(0, 0)) +
                ggtitle("Scenario 2")+
                theme_classic(14) +
                theme(plot.title = element_text(hjust = 0.5))

### MIXTURE MODEL ABILITY ---S3

##sim = function(x)
##{
##  m = c(-0.5,0,.5) 
##  g =sample(1:3,size = 1)
##  rnorm(1,m[g],sqrt(5/6))
##}
##res = replicate(10000,sim()) 
##mean(res);var(res)

set.seed(123 + 3)
Scenario3 = lst()
Scenario3$trueAbilityMeans      = c(-0.5,0,0.5)
Scenario3$trueAbilityVars       = rep(5/6,3)
Scenario3$groups                = groups


set.seed(123)
for(i in 1:N)
{
  Scenario3$ability[i] = with(Scenario3,
				      rnorm(1, trueAbilityMeans[groups[i]],
					    sqrt(trueAbilityVars[groups[i]])))
}

## scale the ability to have mean 0 and variance 1
##Scenario3$ability = scale(Scenario3$ability)


plotAbilityS3 = tibble(x =1:N, ability = Scenario3$ability) |>
                ggplot() +
                geom_histogram(aes(x= ability), bins = 30, col = 'black', fill = 'grey80')+
                scale_y_continuous(limits = c(0,10), expand = c(0, 0)) +
                ggtitle("Scenario 3")+
                theme_classic(14) +
                theme(plot.title = element_text(hjust = 0.5))

dfAbility = tibble(x = 1:N, S1 = Scenario1$ability, S2 = Scenario2$ability, S3 = Scenario3$ability)

plotMarginalAbility = dfAbility |>
                      gather(key,value,-x) |>
                      ggplot(aes(x = key, y =value)) +
                      geom_violin(fill = 'black',alpha = 0.3) +
                      geom_dotplot(binaxis = "y",
                                   stackdir = "center",
                                   dotsize = 0.5) + 
                      xlab("Simulation scenario")+
                      ylab("Ability")+
                      theme_classic(14) 



###plotConditionalAbility =
###dfAbility |>
###gather(key,value,-x) |>
###ggplot(aes(x = x, y =value, shape = key)) +
###geom_point()+
###geom_line(aes(x = x, y =value, group = x), lty = 'dashed')+
###scale_shape_manual(values = c(0,1,2))+
###labs(x = "Student id", y = "Ability", shape = '')+
###theme_classic(14)+
###theme(legend.direction ='horizontal', legend.position = c(0.7,0.1), legend.spacing.x =unit(0.01,"cm"))
###




plotConditionalAbility = dfAbility |>
                         bind_cols(groups = as.factor(groups)) |>
                         gather(key,value,-x, -groups) |>
                         ggplot(aes(x = groups, y =value)) +
                         geom_violin(fill = 'black',alpha = 0.3)+
                         geom_dotplot(binaxis = "y",
                                      stackdir = "center",
                                      dotsize = 0.5) + 
                         labs(x = "Latent group", y = "Ability")+
                         facet_wrap(~key)+
                         theme_classic(14)

library(cowplot)
simPlot = plot_grid(plotW, plot_grid(plotMarginalAbility,plotConditionalAbility, nrow = 2, labels = c('(b)', '(c)')), labels = c("(a)",""))
save_plot("plots/simulationData.pdf",simPlot, base_width  =20, base_height = 10)


### generate difficulties parameters---common to the three scenarios
##betas = c(0,rnorm(3,0,2))
##a = 0.5
##c(1,exp(a * cumsum(betas[-1])))/(1 +  sum(exp(a * cumsum(betas[-1]))))

Betas = matrix(0,p,4)
for(i in 1:p)
{
 Betas[i,2:4] = rnorm(3,0,2)
}

Scenario1$difficulties = Betas
Scenario2$difficulties = Betas
Scenario3$difficulties = Betas

### generate discrimination parameters---common to the three scenarios
alphas = runif(p,0.5,1)

Scenario1$discrimination = alphas
Scenario2$discrimination = alphas
Scenario3$discrimination = alphas


## generate data in the 3 three scenarios



## Scenario1
Scenario1$data = matrix(0, N,p)


for(i in 1:N)
{
	for(j in 1:p)
	{
	  num =  exp(cumsum(Scenario1$discrimination[j] *( Scenario1$ability[i] - Scenario1$difficulties[j,])))
	  den =  sum(num)
	  prob = num/den
	  Scenario1$data[i,j] = sample(0:3,size = 1, prob = prob)
	}
}

saveRDS(Scenario1, "sim_scenario1.rds")

## Scenario2

Scenario2$data = matrix(0, N,p)


for(i in 1:N)
{
	for(j in 1:p)
	{
	  num =  exp(cumsum(Scenario1$discrimination[j] *( Scenario1$ability[i] - Scenario1$difficulties[j,])))
	  den =  sum(num)
	  prob = num/den
	  Scenario2$data[i,j] = sample(0:3,size = 1, prob = prob)
	}
}

saveRDS(Scenario2, "sim_scenario2.rds")


## Scenario3

Scenario3$data = matrix(0, N,p)


for(i in 1:N)
{
	for(j in 1:p)
	{
	  num =  exp(cumsum(Scenario1$discrimination[j] *( Scenario1$ability[i] - Scenario1$difficulties[j,])))
	  den =  sum(num)
	  prob = num/den
	  Scenario3$data[i,j] = sample(0:3,size = 1, prob = prob)
	}
}

saveRDS(Scenario3, "sim_scenario3.rds")
