simQueue <- function(lambda, mu, N, t, p1, p2, p3, debug, plotXt) {
  
  print('*****[INFO] Vérification des paramètres*****')
  stopifnot(p1+p2+p3==1) # On arrête si p1+p2+p3 != 1
  stopifnot(lambda!=mu) # On arrête si lambda = mu (on ne veut pas ro = 1 pour éviter une division par 0)
  
  print('*****[INFO] Initialisation du serveur*****')
  t.end   <- t # Durée max de la simulation
  t.clock <- 0    # Durée courante

  q <- 0 # Queue Générale
  q1 <- 0 # Part p1
  q2 <- 0 # Part p2
  q3 <- 0 # Part p3
  nextJob <- rexp(1,lambda) # Tps avant le prochain job
  nextDep <- 0 # Prochain départ
  tours <- 0 # Variable pour compter les tours (nombre de jobs total)
  nbLaunch <- 0 # Nombre de jobs lancés (général)
  nbLaunch1 <- 0 # Nombre de jobs lancés (p1)
  nbLaunch2 <- 0 # Nombre de jobs lancés (p2)
  nbLaunch3 <- 0 # Nombre de jobs lancés (p3)
  nbTerm <- 0 # Nombre de jobs terminés (général)
  nbTerm1 <- 0 # Nombre de jobs terminés (p1)
  nbTerm2 <- 0 # Nombre de jobs terminés (p2)
  nbTerm3 <- 0 # Nombre de jobs terminés (p3)
  nbCancelled <- 0 # Nombre de jobs annulés
  sommeReq <- 0 # Somme de la séquence des nombres de requetes en queue (pour la moyenne)
  
  # Si l'utilisateur veut plotter, on prépare le plot
  if(plotXt) {
    xax <- 0 # Axe du temps
    yax <- 0 # Axe du nombre de requetes à un instant t (total)
    yax1 <- 0 # Axe du nombre de requetes à un instant t (p1)
    yax2 <- 0 # Axe du nombre de requetes à un instant t (p2)
    yax3 <- 0 # Axe du nombre de requetes à un instant t (p3)
  }

  #On va faire du fifo
  print('*****[INFO] Démarrage*****')
  while(t.clock < t.end) { # Tant que c'est pas fini
    if((nextJob < nextDep && q < N) || q == 0) { # Si c'est le 1er tour ou qu'on doit ajouter un job
      if(debug) # On affiche les jobs lancés en debug
        print('*****[DEBUG/INFO] Un job a été lancé !*****')
      t.clock = t.clock + nextJob # On augmente le tps virtuel
      if(q==0) { # Si la queue est vide
        nextDep = nextDep - nextJob # On peut préparer le prochain départ
      }
      nextJob = rexp(1,lambda) # On génère le job suivant
      q = q + 1 # On le met dans la queue
      v <- runif(1) # On tire un nombre dans [0,1]
      if(v < p1) { # Si la requete est prioritaire
        q1 = q1 + 1
        nbLaunch1 = nbLaunch1 + 1 # On le compte
      } else if(v < (p1+p2)) { # Sinon si la requete est normale
        q2 = q2 + 1
        nbLaunch2 = nbLaunch2 + 1 # On le compte
      } else { # Sinon la requete est lente
        q3 = q3 + 1
        nbLaunch3 = nbLaunch3 + 1 # On le compte
      }
      nbLaunch = nbLaunch + 1 # On le compte
    } else { # Sinon, on va peut être annuler ou terminer un job
      if(debug) # En mode debug
        print('*****[DEBUG/INFO] Un job a été terminé !*****')
      if(nextJob < nextDep && debug) # En mode debug
        print('*****[DEBUG/WARNING] Un job a été annulé !*****')
      if(nextJob < nextDep) # Si on était censés recevoir un job, on l'annule car la queue est pleine
        nbCancelled = nbCancelled +1
      t.clock = t.clock + nextDep # On augmente le tps virtuel
      nextJob = nextJob - nextDep # On peu préparer le prochain job
      nextDep = rexp(1,mu) # On génère le prochain départ (tps de service)
      q = q - 1 # On enlève le job annulé de la queue
      if(q1 > 0) { # On prend en priorité la q1
        q1 = q1 -1
        nbTerm1 = nbTerm1 + 1 # On le compte 
      } else if(q2 > 0) { # Puis la q2
        q2 = q2 -1
        nbTerm2 = nbTerm2 + 1 # On le compte 
      } else { # Et enfin la q3
        q3 = q3 - 1
        nbTerm3 = nbTerm3 + 1 # On le compte 
      }
      nbTerm = nbTerm + 1 # On le compte 
    }
    tours = tours + 1 # On compte les tours
    sommeReq = sommeReq + q # On somme la moyenne des requetes
    if(plotXt) { # Si on veut plotter
      xax = c(xax,t.clock) # On combine les vecteurs
      yax = c(yax,q) # On combine les vecteurs (total)
      yax1 = c(yax1,q1) # On combine les vecteurs (p1)
      yax2 = c(yax2,q2) # On combine les vecteurs (p1)
      yax3 = c(yax3,q3) # On combine les vecteurs (p1)
    }
  }
  
  if(plotXt) { # Si on veut plotter
    #Légende du graph
    plot(xax, yax, xlab="time", ylab="Xt", type="s", main="Nombre de requetes dans le système") # On plote
    points(xax, yax1, col="red", pch=NA_integer_)
    lines(xax, yax1, col="red",type="s")
    points(xax, yax2, col="blue", pch=NA_integer_)
    lines(xax, yax2, col="blue",type="s")
    points(xax, yax3, col="green", pch=NA_integer_)
    lines(xax, yax3, col="green",type="s")
    legend("topleft",legend=c("q","q1","q2","q3"), col=c("black", "red","blue","green"),lty=c(1,1,1,1), ncol=1)
  }
  
  print('*****[INFO] Nombre d\'éléments en queue (total) :*****')
  print(q)
  print('*****[INFO] Nombre d\'éléments en queue (p1) :*****')
  print(q1)
  print('*****[INFO] Nombre d\'éléments en queue (p2) :*****')
  print(q2)
  print('*****[INFO] Nombre d\'éléments en queue (p3) :*****')
  print(q3)
  print('*****[INFO] Nombre de jobs lancés (total) :*****')
  print(nbLaunch)
  print('*****[INFO] Nombre de jobs lancés (p1) :*****')
  print(nbLaunch1)
  print('*****[INFO] Nombre de jobs lancés (p2) :*****')
  print(nbLaunch2)
  print('*****[INFO] Nombre de jobs lancés (p3) :*****')
  print(nbLaunch3)
  print('*****[INFO] Nombre de jobs terminés (total) :*****')
  print(nbTerm)
  print('*****[INFO] Nombre de jobs terminés (p1) :*****')
  print(nbTerm1)
  print('*****[INFO] Nombre de jobs terminés (p2) :*****')
  print(nbTerm2)
  print('*****[INFO] Nombre de jobs terminés (p3) :*****')
  print(nbTerm3)
  print('*****[INFO] Pourcentage de jobs terminés (total) :*****')
  print(nbTerm/nbLaunch*100)
  print('*****[INFO] Pourcentage de jobs terminés (p1) :*****')
  print(nbTerm1/nbLaunch1*100)
  print('*****[INFO] Pourcentage de jobs terminés (p2) :*****')
  print(nbTerm2/nbLaunch2*100)
  print('*****[INFO] Pourcentage de jobs terminés (p3) :*****')
  print(nbTerm3/nbLaunch3*100)
  print('*****[INFO] Nombre de jobs annulés :*****')
  print(nbCancelled)
  print('*****[INFO] Pourcentage de jobs annulés :*****')
  print(100*(nbCancelled/(nbLaunch+nbCancelled)))
  print('*****[INFO] Nombre de tours :*****')
  print(tours)
  print('*****[INFO] Nombre moyen de requetes (simulé) :*****')
  print(sommeReq/tours)
  print('*****[INFO] Nombre moyen de requetes (théorique) :*****')
  ro <- lambda/mu
  Etx <- (((1-(ro^(N-1))) / (1-ro)) - N*(ro^(N))) * (ro / (1-(ro^(N+1))))
  print(Etx)
  print('*****[INFO] Taux de perte (simulé) :*****')
  print(nbCancelled/(nbLaunch+nbCancelled))
  print('*****[INFO] Taux de perte (théorique) :*****')
  print(((1-ro)/(1-ro^(N+1)))*ro^(N))
  print('*****[INFO] Simulation terminée*****')
  
  return (100*(nbCancelled/(nbLaunch+nbCancelled))) # On renvoi le pourcentage de requêtes perdues
}

multiSim <- function() {
  lambda <- 1 # Départ à 1
  xax <- 0 # Axe de lambda
  yax <- 0 # Axe du pourcentage de requetes perdues
  while (lambda < 100) { # Tant que lambda < 20
    xax = c(xax,lambda) # Vecteur des valeurs de lambda
    lambda = lambda+0.4 # lambda += 0.4 (On veut éviter lambda == mu)
    yax = c(yax, simQueue(lambda ,4,10,1000,1/3,1/3,1/3,FALSE,FALSE)) # Simulation et vecteur du pourcentage de requetes perdues
  }
  plot(xax, yax, xlab="lambda", ylab="Pourcentage de requêtes perdues", main="Pourcentage de requêtes perdues fct de lambda") # On plote
}