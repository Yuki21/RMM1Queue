simQueue <- function(lambda, mu, N, t, plotXt) {
  
  debug <- FALSE
  print('*****[INFO] Initialisation du serveur*****')
  t.end   <- t # duration of sim
  t.clock <- 0    # sim time

  q <- 0 # Queue
  nextJob <- rexp(1,lambda) # Tps avant le prochain job
  nextDep <- 0 # Prochain départ
  tours <- 0
  nbLaunch <- 0
  nbTerm <- 0
  nbCancelled <- 0
  sommeReq <- 0
  
  if(plotXt) {
    xax <- 0
    yax <- 0
    plot(xax, yax, xlab="time", ylab="Xt", type="s", main="Nombre de requetes dans le système")
  }

  #On va faire du fifo
  print('*****[INFO] Démarrage*****')
  while(t.clock < t.end) {
    if((nextJob < nextDep && q < 30) || q == 0) {
      if(debug)
        print('*****[DEBUG/INFO] Un job a été lancé !*****')
      t.clock = t.clock + nextJob
      if(q==0) {
        nextDep = nextDep - nextJob
      }
      nextJob = rexp(1,lambda)
      q = q + 1
      nbLaunch = nbLaunch + 1
    } else {
      if(debug)
        print('*****[DEBUG/INFO] Un job a été terminé !*****')
      if(nextJob < nextDep && debug)
        print('*****[DEBUG/WARNING] Un job a été annulé !*****')
      if(nextJob < nextDep)
        nbCancelled = nbCancelled +1
      t.clock = t.clock + nextDep
      nextJob = nextJob - nextDep
      nextDep = rexp(1,mu)
      q = q - 1
      nbTerm = nbTerm + 1
    }
    #print(q)
    tours = tours + 1
    sommeReq = sommeReq + q
    if(plotXt) {
      xax = c(xax,t.clock)
      yax = c(yax,q)
      plot(xax, yax, xlab="time", ylab="Xt", type="s", main="Nombre de requetes dans le système")
    }
  }
  print('*****[INFO] Nombre d\'éléments en queue :*****')
  print(q)
  print('*****[INFO] Nombre de jobs lancés :*****')
  print(nbLaunch)
  print('*****[INFO] Nombre de jobs terminés :*****')
  print(nbTerm)
  print('*****[INFO] Nombre de jobs annulés :*****')
  print(nbCancelled)
  print('*****[INFO] Nombre de tours :*****')
  print(tours)
  print('*****[INFO] Nombre moyen de requetes (simulé) :*****')
  print(sommeReq/tours)
  print('*****[INFO] Nombre moyen de requetes (théorique) :*****')
  ro <- lambda/mu
  Etx <- (((1-(ro^(N-1))) / (1-ro)) - N*(ro^(N))) * (ro / (1-(ro^(N+1))))
  print(Etx)
  print('*****[INFO] Simulation terminée*****')
}

