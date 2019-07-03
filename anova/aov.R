source('~/projects/src/pkgs.R')
phe = read.csv('~/projects/nil/data/nil_phenotype.csv', na=".") %>% as_tibble()

y<- phe %>% filter(Genotype != 'Mo17') %>% mutate(envi = paste(Location, Density, sep = '_'))
y[8:c(ncol(y)-1)] <- lapply(y[8:c(ncol(y)-1)], as.numeric)
#extract names of columns and remove some unrelated column names
responseList <- names(y)[-c(1:7, ncol(y))]
modelList<- lapply(responseList, function(resp){
    mF <- formula(paste(resp, " ~ envi*Genotype"))
    aov(mF, data = y)                          
                })

mm<- lapply(modelList, summary)
mm
#or use tidy function from "broom" package to reorganize results
tdm <- lapply(modelList, tidy)
names(tdm) <- responseList
tdm
#tidy results for multiple traits
otp<- NULL
for (i in 1:length(tdm)){
	td1 = bind_rows(tdm[i], .id = "trait")
	otp = rbind(otp, td1)
}


otp1 = otp %>% group_by(trait) %>% mutate_at(vars(sumsq), funs(sumsq_all = sum)) %>% mutate(propsq = sumsq/sumsq_all*100) 
otp1 %>% select(trait, term, meansq) %>% spread(.,term, meansq) %>% mutate_if(is.numeric, round, 2) %>% data.frame 
otp1 %>% select(trait, term, propsq) %>% spread(.,term, propsq) %>% mutate_if(is.numeric, round, 2) %>% data.frame

























