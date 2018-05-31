# Projet R de Probas

## Introduction

Le projet a été écrit en langage R. Le code n'est pas forcément le plus optimisé possible mais se veut le plus lisible possible.

Voici les conventions adoptées :

- `lambda` représente l'intensité des requêtes.
- `mu` représente le temps de service.
- Ces deux paramètres sont utilisés par une loi exponentielle grace à la fonction `rexp`.
- La variable (structure) `t` contient les éléments `t.clock` et `t.end` qui représentent respectivement le temps écoulé et le temps maximum virtuel de simulation.
- La variable `q` représente le nombre d'éléments dans la queue ET en traitement par le serveur (on suppose qu'une requête quitte la queue lorsque son traitement est terminé).
- Les variables `q1`, `q2`, et `q3` représentent les parts de la queue q attribuées à chaque priorité. On a donc `q=q1+q2+q3`.
- Les variables `tours`, `nbLaunch/1/2/3`, `nbTerm/1/2/3`, `nbCancelled` et `sommeReq` servent à générer les indicateurs (compteurs, moyennes). Il est important de noter que les variables `nbLaunch/1/2/3` et `nbTerm/1/2/3` ne tiennent pas compte des requêtes annulées !
- La variable `N` représente le nombre maximum de requêtes dans le système, en comptant celle en cours de traitement. On a toujours `q <= N`.
- Les paramètres `p1`, `p2`, et `p3` représentent la proportion des requêtes prioritaires, normales et lentes, avec l'assertion de départ `p1+p2+p3=1` (ligne 4).
- La deuxième assertion de départ est `lambda != mu` : On ne pourrait pas calculer le nombre moyen de reqêtes à un instant t théorique s'ils étaient égaux car on aurait `rho = 1` et donc une division par zéro dans la formule de l'espérance de `Xt` (ligne 5). Cependant, il est tout à fait possible de ne pas calculer cette espérance théorique et d'avoir les deux paramètres égaux à condition de commenter l'assertion et le calcul en question.
- La paramètre `debug` est un boolean qui permet d'afficher toutes les informations sur les requêtes durant l'exécution (lorsqu'il vaut `TRUE`).
- Le paramètre `plotXt` est un boolean qui, lorsqu'il vaut `TRUE`, permet de calculer un graphique représentant `Xt` en fonction de `t.clock`. Attention, l'activation de ce paramètre ralentit énormément la simulation (il ne faut pas l'activer si le temps max est supérieur à 10000).
- On va traiter les éléments dans la queue selon le modèle fifo.
- Il est possible de n'utiliser qu'un seul type de requête avec le réglage suivant : `p1=1, p2=0, p3=0`.
- La simulation est dynamique : pas de structure de données complexes ou de génération statique au démarrage.
- La fonction `multiSim` a été conçue pour simuler plusieurs fois de suite en faisant varier uniquement la valeur de `lambda`. Cette fonction va ensuite générer un graphique du pourcentage de requêtes perdues en fonction de `lambda`.

## Exemple qui fonctionne bien :

On appelle la fonction `simQueue` de la façon suivante `simQueue(1.3333,1,30,100,1/3,1/3,1/3,FALSE,TRUE)`