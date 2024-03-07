#Global

rm(list=ls())
library(shiny)
library(tidyverse)
library(waiter)
library(shinycssloaders)
print("Global execution")
setwd("~/Loïc/Data Science/Statistiques/Stats_R/Projet Winamax/Dealer_game")
options(digits=15)

give_proba_3D<-function(collec_hauteur,collec_couleur,collec_paquet,extra=2,
                        nb_couleur=4,nb_hauteur=13,n_deck=10,cartes_restantes=54){
  
  #construction array 
  out_new_c_new_h<-(nb_couleur-collec_couleur)*(nb_hauteur-collec_hauteur)
  out_new_c_only<-(nb_couleur-collec_couleur)*(collec_hauteur)#augmenter sur toutes les dim
  out_new_h_only<-(nb_hauteur-collec_hauteur)*(collec_couleur)
  
  
  #1 seul ajout
  h0c1p0<-(nb_couleur-collec_couleur)*(collec_hauteur)*collec_paquet
  h1c0p0<-(nb_hauteur-collec_hauteur)*(collec_couleur)*collec_paquet
  h0c0p1<-(n_deck-collec_paquet)*(extra+collec_hauteur*collec_couleur)#piocher un joker cas 1/2
  #2 ajout
  h0c1p1<-(n_deck-collec_paquet)*collec_hauteur*(nb_couleur-collec_couleur)
  h1c0p1<-(n_deck-collec_paquet)*collec_couleur*(nb_hauteur-collec_hauteur)
  h1c1p0<-collec_paquet*(nb_couleur-collec_couleur)*(nb_hauteur-collec_hauteur)
  
  h1c1p1<-(nb_couleur-collec_couleur)*(nb_hauteur-collec_hauteur)*(n_deck-collec_paquet)
  h0c0p0<-cartes_restantes-(h1c0p0+h0c1p0+h1c1p0+h0c0p1+h1c0p1+h0c1p1+h1c1p1)#piocher un joker cas 2/2
  
  out<-array(c(h0c0p0,h1c0p0,h0c1p0,h1c1p0,h0c0p1,h1c0p1,h0c1p1,h1c1p1),
             dim=c(2,2,2),
             dimnames = list(paste("New_hauteur=",0:1,sep=""),paste("New_couleur=",0:1,sep=""),paste("New_paquet=",0:1,sep="")))
  return(out/cartes_restantes)
}



give_proba_transi<-function(proba_general,end_hauteur=F,end_couleur=F,end_paquet=F){#la cible a des NA sur les collections complétés 
  #il y a des différences de calculs entre les avecs NAS ou avec les O.
  proba_transi_reel<-proba_general
  #decaler une face de l'array
  if(end_hauteur){
    proba_transi_reel[1,,]<-apply(proba_transi_reel,c(2,3),sum)
    proba_transi_reel[2,,]<-0
  }
  #decaler une face de l'array
  if(end_couleur){
    proba_transi_reel[,1,]<-apply(proba_transi_reel,c(1,3),sum)
    proba_transi_reel[,2,]<-0
  }
  if(end_paquet){
    proba_transi_reel[,,1]<-apply(proba_transi_reel,c(1,2),sum)
    proba_transi_reel[,,2]<-0
  }
  return(proba_transi_reel)
}




calcul_exact<-function(nb_hauteur=13,nb_couleur = 4,n_deck = 2,extra=2,
                       nb_success_hauteur=13,nb_success_paquet=2,nb_success_couleur = 4){
  
  
  
  #fonction
  cartes<-(extra+nb_couleur*nb_hauteur)*n_deck
  
  #pire cas possible pour le nombre de tirage
  manque_couleur<-(nb_hauteur*(nb_success_couleur-1)+extra)*n_deck+1
  manque_hauteur<-(nb_couleur*(nb_success_hauteur-1)+extra)*n_deck+1
  manque_paquet<-(nb_success_paquet-1)*(nb_couleur*nb_hauteur+extra)+1
  possible_max<-c(manque_couleur,manque_hauteur,manque_paquet)
  nb_tire_max<-max(possible_max)
  
  
  #matrice des etats de la chaine de markov 
  m_proba<-array(0,dim=c(nb_success_hauteur+1,nb_success_couleur+1,nb_success_paquet+1,nb_tire_max), dimnames = list(paste("nb_collec_h=",0:nb_success_hauteur,sep=""),
                                                                                                   paste("nb_collec_c=",0:nb_success_couleur,sep=""),
                                                                                                   paste("nb_collec_p=",0:nb_success_paquet,sep=""),
                                                                                                   paste("nb_draw=",1:nb_tire_max,sep="")))#marche en 4 dimension
  
  ########################################################################################
  #init 
  #premiere carte joker
  p_joker<-extra*n_deck/cartes
  m_proba[1,1,2,1]<-p_joker
  #premiere carte hauteur
  next_collec_coulours<-min(1,nb_success_couleur)
  next_collec_hauteur<-min(1,nb_success_hauteur)
  m_proba[next_collec_hauteur+1,next_collec_coulours+1,2,1]<-1-p_joker
  #m_proba[2,2,2,1]<-1-p_joker
  
  
  #fin de linitialisation
  ###########################################################
  for (tirage in 1:(nb_tire_max-1)){
   # print("Parcours grille tirage N")
    #print(tirage)
    if(max(m_proba[,,,tirage])>1e-15){#condition sur les petites probas
      
      #si on a fini, on reste dans l'etat finin avec une proba de 1
      #m_proba[nb_success_hauteur+1,nb_success_couleur+1,nb_success_paquet+1,tirage+1]<-m_proba[nb_success_hauteur+1,nb_success_couleur+1,nb_success_paquet+1,tirage]
      
      for (collec_h in 0:(nb_success_hauteur)){
        for(collec_c in 0:nb_success_couleur){
          for(collec_p in 1:(nb_success_paquet)){#il y a toujours 1 paquet
            
            p_case_running<-m_proba[collec_h+1,collec_c+1,collec_p+1,tirage]
            cartes_restantes<-cartes-tirage
            
            if(p_case_running!=0){#on calcul les probas des cases a l'instant suivant uniquement si la case a l'instant t a une proba differentes de 0.
              
              
              
              
              #etats des collections
              end_hauteur<-collec_h>=nb_success_hauteur
              end_couleur<-collec_c>=nb_success_couleur
              end_paquet<-collec_p>=nb_success_paquet
              
              #creer les proba de transition
              proba_general<-give_proba_3D(collec_h,collec_c,collec_p,
                                           nb_hauteur=nb_hauteur,nb_couleur=nb_couleur,n_deck=n_deck,
                                           extra,cartes_restantes)
              
              proba_transi<-give_proba_transi(proba_general,end_hauteur,end_couleur,end_paquet)
              
              
              
              #creer la liste des cases 2^D cases cible à modifier au tirage n+1
              
              cible_hauteur<-1:2
              cible_couleur<-1:2
              cible_paquet<-data.frame(1:2)
              cases_cibles<-merge(cible_hauteur,cible_couleur)
              cases_cibles<-merge(cases_cibles,cible_paquet)
              id<-1:dim(cases_cibles)[1]
              cases_cibles$id<-id
              data_proba<-data.frame(id,as.vector(proba_transi))
              cases_cibles<-merge(cases_cibles,data_proba)
              names(cases_cibles)<-c("ID","hauteur","couleur","paquet","proba")
              
              cases_cibles<-cases_cibles[cases_cibles$proba!=0,]
              #parcours cette liste 
              #calcules la proba targeted ( # les ifs sont dans la function give proba targeted)
              # et modifier la bonne case 
              
              
              
              for (case in 1:length(cases_cibles$ID)){
                m_proba[collec_h+cases_cibles$hauteur[case],collec_c+cases_cibles$couleur[case],collec_p+cases_cibles$paquet[case],tirage+1]<-m_proba[collec_h+cases_cibles$hauteur[case],collec_c+cases_cibles$couleur[case],collec_p+cases_cibles$paquet[case],tirage+1]+
                  p_case_running*cases_cibles$proba[case]
              }
              
            }#fin de la verif de case non nul
            
          }
        }
      }#fin des triples boucles 
      
    }#fin de la condition sur les proba taille
  }#fin de la boucle tirage
  #################################################################################
  #fin de la boucle
  proba_s_all_dim<-m_proba[nb_success_hauteur+1,nb_success_couleur+1,nb_success_paquet+1,]
  proba_s_all_dim_first<-c(proba_s_all_dim[1],diff(proba_s_all_dim))
  mean<-weighted.mean(1:nb_tire_max,w = proba_s_all_dim_first)
  res<-list(m_proba,proba_s_all_dim_first,mean)
  names(res)<-c("proba_matrice","p","moyenne")
  return(res)
}

simulation<-function(x, 
                     nb_hauteur=13,nb_couleur=4,n_deck=1,extra=2,
                     nb_success_couleur=4,
                     nb_success_hauteur=13,
                     nb_success_paquet=1){
  #set.seed(225)

  cartes_per_deck<-(extra+nb_couleur*nb_hauteur)
  cartes<-cartes_per_deck*n_deck
  hauteur<-rep(c(rep(1:nb_hauteur,nb_couleur),rep(nb_hauteur+1,extra)),n_deck)
  paquet<-rep(1:n_deck,each=cartes_per_deck)
  couleur<-rep(c(rep(1:nb_couleur,each=nb_hauteur),rep(nb_couleur+1,extra)),n_deck)
  deck<-data.frame(hauteur,couleur,paquet)
  
  
  
  melange<-sample(1:length(hauteur))
  
  fin=FALSE
  i<-0
  collection_hauteur<-numeric()
  collection_couleur<-numeric()
  collection_paquet<-numeric()
  while (!fin){
    i<-i+1
    #print("Nouvelle carte")
    #print(i)
    
    #verifie que ce n'est pas un joker
    if(couleur[melange[i]]!=(nb_couleur+1)){
      collection_couleur<-unique(c(collection_couleur,couleur[melange[i]]))
      collection_hauteur<-unique(c(collection_hauteur,hauteur[melange[i]]))
    }
    collection_paquet<-unique(c(collection_paquet,paquet[melange[i]]))
    
    
    
    all_hauteur<-length(collection_hauteur)>=nb_success_hauteur
    all_couleur<-length(collection_couleur)>=nb_success_couleur
    all_paquet<-length(collection_paquet)>=nb_success_paquet
    fin<-all_hauteur&all_couleur&all_paquet
  }
  
  return(i)
}


n_rank_max=20
n_colours_max=10
n_decks_max=10

n_rank_max_collec=20
n_colours_max_collec=10
n_decks_max_collec=10

