simQueue <- function(lambda, mu, N, t, p1, p2, p3, debug, plotXt) {
  
  print('*****[INFO] Vérification des paramètres*****')
  stopifnot(p1+p2+p3==1) # On arrête si p1+p2+p3 != 1
  stopifnot(lambda!=mu) # On arrête si lambda = mu (on ne veut pas ro = 1 pour éviter une division par 0)
  
  print('*****[INFO] Initialisation du serveur*****')
  t.end   <- t # Durée max de la simulation
  t.clock <- 0    # Durée courante

  q <- 0 # Queue Générale
  q1 <- 0 # Queue p1
  q2 <- 0 # Queue p2
  q3 <- 0 # Queue p3
  nextJob <- rexp(1,lambda) # Tps avant le prochain job
  nextDep <- 0 # Prochain départ
  tours <- 0 # Variable pour compter les tours (nombre de jobs total)
  nbLaunch <- 0 # Nombre de jobs lancés
  nbTerm <- 0 # Nombre de jobs terminés
  nbCancelled <- 0 # Nombre de jobs annulés
  sommeReq <- 0 # Somme de la séquence des nombres de requetes en queue (pour la moyenne)
  
  # Si l'utilisateur veut plotter, on prépare le plot
  if(plotXt) {
    xax <- 0 # Axe du temps
    yax <- 0 # Axe du nombre de requetes à un instant t
    plot(xax, yax, xlab="time", ylab="Xt", type="s", main="Nombre de requetes dans le système")
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
      } else if(v < (p1+p2)) { # Sinon si la requete est normale
        q2 = q2 + 1
      } else { # Sinon la requete est lente
        q3 = q3 + 1
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
      } else if(q2 > 0) { # Puis la q2
        q2 = q2 -1
      } else { # Et enfin la q3
        q3 = q3 - 1
      }
      nbTerm = nbTerm + 1 # On le compte 
    }
    tours = tours + 1 # On compte les tours
    sommeReq = sommeReq + q # On somme la moyenne des requetes
    if(plotXt) { # Si on veut plotter
      xax = c(xax,t.clock) # On combine les vecteurs
      yax = c(yax,q) # On combine les vecteurs
      plot(xax, yax, xlab="time", ylab="Xt", type="s", main="Nombre de requetes dans le système") # On plote
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
  print('*****[INFO] Taux de perte (simulé) :*****')
  print(nbCancelled/tours)
  print('*****[INFO] Taux de perte (théorique) :*****')
  print(((1-ro)/(1-ro^(N+1)))*ro^(N))
  print('*****[INFO] Simulation terminée*****')
}

