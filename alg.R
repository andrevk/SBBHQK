#Fun��o 1: mensura a magnitude da diferen�a entre duas m�dias (Cohen, 1988)
# Uso: es(x = vetor1,y = vetor2)
es <- function(x,y) {
  a <- mean(x) - mean (y)
  b <- sqrt((sd(x)+sd(y))/2)
  return(a/b)
}
#Fun��o 2: compara k vezes duas popula��es de tamanho n
# alg(x = popula��o1,y = popula��o2,n = tamanho da amostra de cada popula��o,k = n�mero de compara��es)
alg <- function(x,y,n,k) {
  set.seed(123)
  i = 0
  t.correto <- 0
  t.errado <- 0
  wilcox.correto <- 0
  wilcox.errado <- 0
  while(i < k) {
  a <- sample(x,n)
  b <- sample(y,n)
  i = i+1
  c <- t.test(a,b)
  d <- wilcox.test(a,b)
  
  if ((mean(a) > mean(b)) & (c$p.value < 0.05)) { 
    t.correto <- t.correto + 1
    #print("t correto")
    #print(mean(a))
    #print(mean(b))
    #print(c$p.value)
    #print(t.correto)
  } else { 
    t.errado <- t.errado + 1
    #print("t errado")
    #print(mean(a))
    #print(mean(b))
    #print(c$p.value)
    #print(t.errado)
  }
  if ((mean(a) > mean(b)) & (d$p.value < 0.05)) { 
    wilcox.correto <- wilcox.correto + 1
    #print("W correto")
    #print(mean(a))
    #print(mean(b))
    #print(d$p.value)
    #print(wilcox.correto)
  } else { 
    wilcox.errado <- wilcox.errado + 1
    #print("W errado")
    #print(mean(a))
    #print(mean(b))
    #print(d$p.value)
    #print(wilcox.errado)
      }
  }
  #print("Total de compara��es:")
  #print(i)
  print("N�mero de teste t correto:")
  print(t.correto)
  #print("N�mero de teste t errado:")
  #print(t.errado)
  print("N�mero de teste Mann-Whitney correto:")
  print(wilcox.correto)
  #print("N�mero de teste mann-whitney errado:")
  #print(wilcox.errado)
  print("--------------------")
}

Exemplo de uso:
p1 <- cbind(rchisq(10000000, 2.47, ncp = 2)) # gera popula��o qui-quadrado 1 de tamanho 10 milh�es, 2.47 gl e 2 ncp
p2 <- cbind(rchisq(10000000, 1, ncp = 2)) # gera popula��o qui-quadrado 2 de tamanho 10 milh�es, 1 gl e 2 ncp
es(p1,p2) # compara as m�dias
alg(p1,p2,100,1000) # realiza 1000 compara��es entre amostras de tamanho 100