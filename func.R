## Escrevendo a funcao
func <- function(input,key,periodo,cod){
    ret <- 0
    frac <- 0
    suc <- 0
    for(i in 1:length(input)){
        if(input[i] == key){
            j <- i
            lim <- j+periodo
            for(j in j:lim){
                if(j>length(input)){break}
                if(input[j] == cod){
                    ret <- ret + 1
                    suc <- 1
                    break
                }
            }
            if(suc == 0){
                frac <- frac + 1
            }
            suc <- 0
        }
    }
    return(c(ret,frac,ret+frac))
}

## Lendo os arquivos
ldf <- list()
listcsv <- dir(pattern = "*.csv")

for(i in 1:13){
    listcsv[i] <- paste0(as.character(i),".csv")
}

for(k in 1:length(listcsv)){
    ldf[[k]] <- read.table(file = listcsv[k],header = F,sep = ";",
                           stringsAsFactors = F)
}

func(input = teste, key = "ERB", periodo = 1, cod = "CRB2")
