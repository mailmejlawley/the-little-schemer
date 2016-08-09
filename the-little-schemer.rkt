#lang racket

#|
Chapters 1-10
James L. Lawley
|#

;is it an atom
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;is it a list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

;is the atom an element of the list of atoms
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? (car lat) a)
                (member? a (cdr lat)))))))

;remove the first occurence of an atom from a list
(define rember1
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember1 a (cdr lat)))))))

;describe the first typical element and cons onto the natural recursion
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

;insert a new atom to the right of the first occurance of old
(define insertR-a
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
            ((eq? (car lat) old)
            (cons old (cons new (cdr lat))))
          (else (cons (car lat)
                      (insertR-a new old  (cdr lat)))))))))

;insert a new atom to the left of the first occurance of old
(define insertL-a
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
              (cons new (cons old (cdr lat))))
            (else (cons (car lat)
                             (insertL-a new old (cdr lat)))))))))

;replace the first occurence of o1 or the first occurence of o2 by new
(define subst-a
 (lambda (new o1 o2 lat)
   (cond
     ((null? lat) (quote()))
     (else (cond
             ((or (eq? (car lat) o1) (eq? (car lat) o2))
              (cons new (cdr lat)))
             (else (cons (car lat)
                         (subst-a new o1 o2 (cdr lat)))))))))

;return the lat with all occurences of a removed 
(define multirember
 (lambda (a lat)
   (cond
     ((null? lat) (quote()))
     (else (cond
             ((equal? (car lat) a)
              (multirember a (cdr lat)))
             ;save (car lat) to be consed onto multiremember later
             (else (cons (car lat)
                         (multirember a (cdr lat)))))))))

;insert new atoms to the right of every old occurence
(define multiInsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons old
                     (cons new
                           (multiInsertR new old (cdr lat)))))
              (else (cons (car lat)
                     (multiInsertR new old (cdr lat)))))))))

;insert new atoms to the left of every old occurence
(define multiInsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new
                     (cons old
                           (multiInsertL new old (cdr lat)))))
              (else (cons (car lat)
                     (multiInsertL new old (cdr lat)))))))))

(define multiSubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new
                     (multiSubst new old (cdr lat))))
              (else (cons (car lat)
                          (multiSubst new old (cdr lat)))))))))

(define +
  (lambda (n m)
    (cond
      (
       (zero? m) n)
      (else (add1 (+ n (sub1 m)))))))

(define -
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

;build a number by totaling all the numbers in a tup
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

;build a number up by adding n up m times
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      ;much like cons, add n to the natural recursion of x
      (else (+ n (x n (sub1 m)))))))

;add the first/second/etc number of each tuple and build a tuple of the answers
(define tup+
  (lambda (tup1 tup2)
  (cond
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (+ (car tup1) (car tup2))
           (tup+ (cdr tup1) (cdr tup2)))))))

;ask if a number is greater than the other
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

;ask if a number is less than the other
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

;ask if two numbers are equivalent
(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

;find the exponent of a number
(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (expt n (sub1 m)))))))

;count many times the second arguement fits into the first one
(define quotient
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quotient (- n m) m))))))

;find the length of a list of atoms
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

;pick the nth atom from a list of atoms
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;pick an nth atom to remove from a list of atoms
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

;give a final value lat obtained by removing all the numbers from lat
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
            ((number? (car lat))
             (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))))

;extract a tup from a lat using all the numbers in the lat
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((number? (car lat))
               (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

;check if two arguments are equivalent
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
            (= a1 a2))
       ((or (number? a1) (number? a2))
            #f)
        (else (eq? a1 a2)))))

;count the number of times an atom appears in a list of atoms
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? (car lat) a)
               (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
   (= n 1)))

;remove all occurences of an atom from a list
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

;insert an atom new to the right of old regardless of where old occurs
(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;count all occurances of an atom
(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

;substitute all old atoms with new atoms
(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;insert an atom new to the left of old regardless of where old occurs
(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

;determine if an atom is a member of a list of atoms
(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
       (else (or (member* a (car l)) (member* a (cdr l)))))))

;find the leftmost atom in a non-empty list of S-expressions that does not contain the empty list
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

;determine if two lists are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
       (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

;determine whether two lists, vectors, etc. are equivalent
(define equal?
  (lambda (s1 s2)
    (cond 
      ((and (atom? s1) (atom? s2))
       ;determine if two arguments are equivalent
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

;remove the first occurence of a s-expression from a list of s-expressions
(define rember
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember s (cdr l)))))))

;determine whether an arthimetic expression contains only numbers
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

;determine the value of a numbered arithmetic expression
(define value-a
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) (quote +))
       (+ (value-a (1st-sub-exp nexp)) (value-a (2nd-sub-exp nexp))))
      ((eq? (operator nexp) (quote x))
       (x (value-a (1st-sub-exp nexp)) (value-a (2nd-sub-exp nexp))))
      (else
       (expt (value-a (1st-sub-exp nexp)) (value-a (2nd-sub-exp nexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

;determine if the list of atoms is a set
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

;
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

;
(define makeset2
  (lambda (lat)
    (cond
    ((null? lat) (quote()))
    (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

;determine if set1 is a subset of set2
(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
       (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

;determine if two sets are equivalent
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

;determine if an atom in set1 is in set2
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

;determine which atoms intersect in set1 and set2
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
       (else (intersect (cdr set1) set2)))))

;determine the union of set1 and set2
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

;determine the set difference of set1 and set2
(define diff
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote()))
      ((member? (car set1) set2) (diff (cdr set1) set2))
      (else (cons (car set1) (diff (cdr set1) set2))))))

;determine all atoms which intersect the l-set
(define intersectAll
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectAll (cdr l-set)))))))

;determine if a list has only two atoms
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #f)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

;describe the first typical element and determine if the relation is a finite function
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

;reverse the two components of a pair
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

;reverse the two components of every pair in the relation
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))


;describe the second typical element and cons onto the natural recursion
(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

;describe the second typical element and determine if the finite function is a full finite function
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

;describe the reversed relation and determine if the finite function is one-to-one
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

;compare the elements of a list with 'a.' the first one eq? to 'a' is removed
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

;compare the elements of a list with 'old.' insert a new atom to the left of the first occurance of old
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old) (cons new (cons old (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

;compare the elements of a list with 'old.' insert a new atom to the right of the first occurance of old
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((test? (car l) old) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

;takes three arguments and conses the first argument onto the result of consing the second argument onto the third argument
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

;takes three arguments and conses the second argument onto the result of consing the first argument onto the third argument
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

;takes seq and three arguments inserting at either the left or right
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote()))
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

;pass in insert-g and the function definition of seqL/R, inserting a new atom to the left of the first occurance of old 
(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

;pass in insert-g and the function definition of seqL/R, inserting a new atom to the right of the first occurance of old
(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

;takes three arguments and conses new onto the list
(define seqS
  (lambda (new old l)
    (cons new l)))

;pass in insert-g and the function definition of seqS, replacing the first occurence of old with new
(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define atom-to-function
  (lambda (atf)
    (cond
      ((eq? atf (quote +)) +)
      ((eq? atf (quote x)) x)
      (else expt))))

;returns the natural value of expressions
(define value-b
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value-b (1st-sub-exp nexp)) (value-b (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) (quote()))
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;insert new atoms to the left of every oldL and to the right of every oldR occurence
(define multiInsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote()))
      ((eq? (car lat) oldL) (cons new (cons oldL (multiInsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiInsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiInsertLR new oldL oldR (cdr lat)))))))

(define multiInsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote()) 0 0))
      ((eq? (car lat) oldL) (multiInsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col
                                                 (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiInsertLR&co new oldL oldR (cdr lat)
                                              (lambda (newlat L R)
                                                (col
                                                 (cons oldR (cons new newlat)) L (add1 R)))))
      (else (multiInsertLR&co new oldL oldR (cdr lat)
                              (lambda (newlat L R)
                                (col
                                 (cons (car lat) newlat) L R)))))))

(define col
  (lambda (lat L R)
    lat))

(define col2
  (lambda (lat L R)
    L))

(define col3
  (lambda (lat L R)
    R))

;removes all odd numbers from a list of nested lists
(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

;visit every number in the car of the list and collect the list without odd numbers, the product of the even numbers, and the sum of the odd numbers
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) (evens-only*&co (cdr l)
                                          (lambda (newl p s)
                                            (col (cons (car l) newl) (x (car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (lambda (newl p s)
                                 (col newl p (+ (car l) s)))))))
      (else (evens-only*&co (car l)
                            (lambda (al ap as)
                              (evens-only*&co (cdr l)
                                              (lambda (dl dp ds)
                                                (col (cons al dl)
                                                     (x ap dp)
                                                     (+ as ds))))))))))

;collector which returns the sum, product, and the list of evens consed together
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a)))))

;takes a pair whose first component is a pair by shifting the second part of the first component into the second component
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

;count the number of atoms in in align's arguments
(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (length* (first pora)) (length* (second pora)))))))

;count the number of atoms in align's arguments, multiplying the first pora by 2 
(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
       (+ (x (weight* (first pora)) 2) (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n) (A n (sub1 m)))))))

;looks in an entry to find the value by name
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

;lookup-in-entry uses lookup-in-entry-help as a helper function
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
       (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

;finds an entry in a table
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             ;action taken when the name is not found in the first entry
                             (lambda (name)
                               (lookup-in-table name (cdr table) table-f)))))))

;produce the correct action (or function) for each possible s-expression
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) (quote quote)) *quote)
         ((eq? (car e) (quote lambda)) *lambda)
         ((eq? (car e) (quote cond)) *cond)
         (else *application)))
      (else *application))))

;takes an expression and evaluates it
(define value
  (lambda (e)
    (meaning e (quote()))))

;translates an expression to its meaning
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)
(define extend-table cons)
(define new-entry build)

#|if the question on the left is false it looks at the rest of the lines. otherwise is proceeds to 
answer the right part. if it sees an else-line, it treats the cond-line as if its question part
were true |#

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

#|takes a list of (representations of) arguments and a table and returns a list composed of the
meaning of each argument |#
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote()))
      (else
       (cons (meaning (car args) table)
             (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?))
       (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
    ((atom? x) #t)
    ((null? x) #f)
    ((eq? (car x) (quote primitive)) #t)
    ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
               (table-of closure)))))