#! /usr/bin/env gsi -:dR
;;; Fichier : tp2.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la première section.

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction "traiter" doit
;;; être définie, et vous pouvez ajouter des définitions de fonction
;;; afin de bien décomposer le traitement à faire en petites
;;; fonctions.  Il faut vous limiter au sous-ensemble *fonctionnel* de
;;; Scheme dans votre codage (donc n'utilisez pas set!, set-car!,
;;; begin, etc).

;;; La fonction traiter reçoit en paramètre une liste de caractères
;;; contenant la requête lue et le dictionnaire des variables sous
;;; forme d'une liste d'association.  La fonction retourne
;;; une paire contenant la liste de caractères qui sera imprimée comme
;;; résultat de l'expression entrée et le nouveau dictionnaire.  Vos
;;; fonctions ne doivent pas faire d'affichage car c'est la fonction
;;; "repl" qui se charge de cela.

(define traiter
  (lambda (expr dict)
    (let ((lecture (lire-expr expr dict)))
        (if (null? (car lecture)) 
            (cons '() (cdr lecture)) ;;Ne pas sauter de ligne si aucune entree
            (cons (append (car lecture) '(#\newline)) (cdr lecture))))))
            
(define lire-expr-aux
    (lambda (expr init_dict updated_dict tmp pile)
        
        (cond ((null? expr) ;;Fin de l'expression
                    (let ((new_pile (if (> (string-length tmp) 0) 
                                    (append (list (string->number tmp)) pile) ;;Si on était en train de lire un nombre, le mettre sur la pile
                                    pile))) 
                        (cond   ((null? new_pile) (cons '() init_dict)) ;;Ligne vide
                                ((> (length new_pile) 1) (cons (exception 'peu_operation expr) init_dict)) ;;Trop d'operandes - exception
                                (else (cons (string->list (number->string (car new_pile))) updated_dict))))) ;;Operation valide - Retourner la pile et le nouveau dictionnaire
               
              ((eq? (car expr) #\space)
                (lire-expr-aux (cdr expr) init_dict updated_dict
                                "" ;;Vider tmp pour récupérer le prochain nombre
                                (if (> (string-length tmp) 0) 
                                    (append (list (string->number tmp)) pile) ;;Si on a lu un nombre, le mettre sur la pile
                                    pile))) 
              
              ((eq? (car expr) #\+) ;; Addition
               (if (< (length pile) 2)
                   (cons (exception 'peu_operand expr) init_dict)
                   (lire-expr-aux (cdr expr) init_dict updated_dict
                                  ""
                                  (operation + pile))))
                                
              ((eq? (car expr) #\-) ;; Soustraction
               (if (< (length pile) 2)
                   (cons (exception 'peu_operand expr) init_dict)
                   (lire-expr-aux (cdr expr) init_dict updated_dict
                                  ""
                                  (operation - pile))))
                                
              ((eq? (car expr) #\*) ;; Multiplication
               (if (< (length pile) 2)
                   (cons (exception 'peu_operand expr) init_dict)             
                   (lire-expr-aux (cdr expr) init_dict updated_dict
                                  ""
                                  (operation * pile))))
           
              ((char-numeric? (car expr)) ;; Lecture d'un chiffre
                (lire-expr-aux (cdr expr) init_dict updated_dict
                                (string-append tmp (string (car expr))) ;; Ajout au nombre en cours de lecture
                                pile))

              ((char-alphabetic? (car expr)) ;; Lecture d'une variable
               (if (null? updated_dict)
                   (cons (exception 'dict_vide expr) init_dict)
                   (let ((variable (assoc (car expr) updated_dict)))
                     (if (pair? variable)
                      (lire-expr-aux (cdr expr) init_dict updated_dict
                                    ""
                                    (append (list (cdr variable)) pile)) ;; Ajoute la valeur de la variable lue sur la pile
                      (cons (exception 'paire_introuv expr) init_dict)))))

              ((eq? (car expr) #\=) ;; Affectation
               (cond ((null? pile)
                   (cons (exception 'affect_pile_vide expr) init_dict))
                     ((or (not (char-alphabetic? (cadr expr))) (null? (cdr expr)))
                      (cons (exception 'affect_sans_nom expr) init_dict))
                     (else (lire-expr-aux (cddr expr) init_dict (affectation expr updated_dict pile)
                               ""
                               pile))))
              
              ((eq? (car expr) #\,) ;; ,q: Quitter la calculatrice
                (if (or (null? (cdr expr)) (not (eq? (cadr expr) #\q))) (cons (exception 'invalide expr) init_dict) (exit))) 
              
              (else (cons (exception 'invalide expr) init_dict))))) ;; Toute autre entree est invalide

(define lire-expr
 (lambda (expr dict)
   (lire-expr-aux expr dict dict "" '()))) ;; Au debut de la lecture, pile vide et dictionnaires identiques
   
(define operation
    (lambda(op pile)  ;;Appel a la procédure op sur les deux premiers éléments de la pile     
        (append (list (op (cadr pile) (car pile))) (cddr pile)))) ;;Remplace les opérandes par le résultat

(define affectation ;; Ajoute la variable et sa valeur au dictionnaire dans les cas ou le dict est vide ou la variable n'existe pas dans le dict
  (lambda (expr dict pile)
    (if (not (pair? (assoc (cadr expr) dict)))
        (append (list (cons (cadr expr) (car pile))) dict)
        (creer-dict (cadr expr) (car pile) dict))))

(define creer-dict-aux ;; Pour rendre creer-dict iterative
  (lambda (cle val old_dict new_dict)
    (if (null? old_dict)
        (append (list (cons cle val)) new_dict)
        (if (eq? cle (caar old_dict))
            (creer-dict-aux cle val (cdr old_dict) new_dict)
            (creer-dict-aux cle val (cdr old_dict) (append (list (cons (caar old_dict) (cdar old_dict))) new_dict))))))

(define creer-dict ;; Creer un nouveau dictionnaire en remplacant la valeur pour une variable existante
  (lambda (cle val old_dict)
    (creer-dict-aux cle val old_dict '())))


(define exception ;;Gestion des erreurs
    (lambda (msg expr)
    (cond
    ((equal? msg 'peu_operation)
        (string->list "Erreur - Operateur manquant (pas assez d'operations pour les operandes fournis)"))
    ((equal? msg 'peu_operand)
        (string->list "Erreur - Operande manquant (operation necessite deux operandes) "))
    ((equal? msg 'dict_vide)
        (string->list (string-append "Erreur - " (string (car expr)) " n'a pas de valeur car le dictionnaire est vide")))
    ((equal? msg 'paire_introuv)
        (string->list (string-append "Erreur - " (string (car expr)) " n'existe pas dans le dictionnaire")))
    ((equal? msg 'affect_pile_vide)
        (string->list "Erreur - Aucune valeur a affecter (exemple d'utilisation: 100 =a)"))
    ((equal? msg 'affect_sans_nom)
        (string->list "Erreur - Espace superflu apres l'operateur d'assignation (exemple d'utilisation: 100 =a)"))
    ((equal? msg 'invalide)
        (string->list "Erreur - Expression invalide")))))

;;;----------------------------------------------------------------------------

;;; Ne pas modifier cette section.

(define repl
  (lambda (dict)
    (print "# ")
    (let ((ligne (read-line)))
      (if (string? ligne)
          (let ((r (traiter-ligne ligne dict)))
            (for-each write-char (car r))
            (repl (cdr r)))))))

(define traiter-ligne
  (lambda (ligne dict)
    (traiter (string->list ligne) dict)))

(define main
  (lambda ()
    (repl '()))) ;; dictionnaire initial est vide
    
;;;----------------------------------------------------------------------------


