simQueue <- function() {
  
  debug <- FALSE
  print('*****[INFO] Initialisation du serveur*****')
  t.end   <- 10^4 # duration of sim
  t.clock <- 0    # sim time

  lambda <- 1 # Intensité
  mu <- 1 # Tps de traitement
  N <- 30 # Taille Queue
  q <- 0 # Queue
  nextJob <- rexp(1,lambda) # Tps avant le prochain job
  nextDep <- 0 # Prochain départ
  tours <- 0
  nbLaunch <- 0
  nbTerm <- 0
  nbCancelled <- 0

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
  print('*****[INFO] Simulation terminée*****')
}