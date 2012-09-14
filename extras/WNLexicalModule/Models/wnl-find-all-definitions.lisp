(clear-all)

(define-model find-all-definition
  
  (sgp :wnl-chunks wnl)

  (chunk-type goal word-str state)
  
  (define-chunks 
   (find-word-definition isa chunk)
   (find-word-gloss isa chunk)
   (say-definition isa chunk)
   (g1 isa goal word-str "wordnet" state find-word-definition)
   (g2 isa goal word-str "act-r" state find-word-definition)
   (g3 isa goal word-str "dog" state find-word-definition)
   (g4 isa goal word-str "cat" state find-word-definition)
   )
  
  (p find-word-definition
     =goal> 
     isa goal
     state find-word-definition
     word-str =word
     
     ?wn-lexical>
     state free
     
     ==>
     
     +wn-lexical>
     isa wnl-request
     wn-operator s
     word =word
     context-criterion set-difference
     
     =goal>
     state find-word-gloss
     )
  
  (p find-word-gloss
     =goal> 
     isa goal
     state find-word-gloss
     word-str =word
     
     ?wn-lexical>
     state free
     
     ?wn-lexical>
     state success
     
     =wn-lexical>
     isa s
     word =word
     synset-id =synset-id
     
     ==>
     
     +wn-lexical>
     isa wnl-request
     wn-operator g
     synset-id =synset-id
     
     =goal>
     state say-definition
     )
  
  (p no-more-word-senses
     =goal> 
     isa goal
     state find-word-gloss
     word-str =word
     
     ?wn-lexical>
     state free
     
     ?wn-lexical>
     state error
     
     ==>
     
     !output! (no more word senses for =word)
     
     +wn-lexical>
     isa wnl-clear-context

     -goal>
     )
  
  (p say-definition
     =goal> 
     isa goal
     state say-definition
     word-str =word
     
     ?wn-lexical>
     state free
     
     ?wn-lexical>
     state success
     
     =wn-lexical>
     isa g
     gloss =gloss
     
     ==>
     
     !output! (A definition of =word is =gloss)
     
     =goal> 
     state find-word-definition
     )
  )

(goal-focus g1)
(run 10)

(goal-focus g2)
(run 10)

(goal-focus g3)
(run 10)

(goal-focus g4)
(run 10)

#| Sample run

NIL 
553001 WNL-Chunks loaded in module WNLexical.
FIND-ALL-DEFINITION 
G1 
     0.000   GOAL                   SET-BUFFER-CHUNK GOAL G1 REQUESTED NIL 
     0.000   PROCEDURAL             CONFLICT-RESOLUTION 
     0.050   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.050   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.050   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.050   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.050   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-106230152-1 
     0.050   PROCEDURAL             CONFLICT-RESOLUTION 
     0.100   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     0.100   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.100   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.100   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.100   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-106230152-1 
     0.100   PROCEDURAL             CONFLICT-RESOLUTION 
     0.150   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "wordnet" IS "(any of the machine-readable lexical databases modeled after the Princeton WordNet)" 
     0.150   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.150   PROCEDURAL             CONFLICT-RESOLUTION 
     0.200   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.200   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.200   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.200   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.200   PROCEDURAL             CONFLICT-RESOLUTION 
     0.250   PROCEDURAL             PRODUCTION-FIRED NO-MORE-WORD-SENSES 
NO MORE WORD SENSES FOR "wordnet" 
     0.250   PROCEDURAL             CLEAR-BUFFER GOAL 
     0.250   PROCEDURAL             CONFLICT-RESOLUTION 
     0.250   ------                 Stopped because no events left to process 

0.25 
58 
NIL 
G2 
     0.250   GOAL                   SET-BUFFER-CHUNK GOAL G2 REQUESTED NIL 
     0.250   PROCEDURAL             CONFLICT-RESOLUTION 
     0.300   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.300   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.300   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.300   PROCEDURAL             CONFLICT-RESOLUTION 
     0.350   PROCEDURAL             PRODUCTION-FIRED NO-MORE-WORD-SENSES 
NO MORE WORD SENSES FOR "act-r" 
     0.350   PROCEDURAL             CLEAR-BUFFER GOAL 
     0.350   PROCEDURAL             CONFLICT-RESOLUTION 
     0.350   ------                 Stopped because no events left to process 

0.099999994 
21 
NIL 
G3 
     0.350   GOAL                   SET-BUFFER-CHUNK GOAL G3 REQUESTED NIL 
     0.350   PROCEDURAL             CONFLICT-RESOLUTION 
     0.400   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.400   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.400   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.400   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     0.400   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-107205647-5 
     0.400   PROCEDURAL             CONFLICT-RESOLUTION 
     0.450   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     0.450   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.450   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.450   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.450   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-107205647-1 
     0.450   PROCEDURAL             CONFLICT-RESOLUTION 
     0.500   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(a smooth-textured sausage of minced beef or pork usually smoked; often served on a bread roll)" 
     0.500   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.500   PROCEDURAL             CONFLICT-RESOLUTION 
     0.550   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.550   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.550   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.550   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     0.550   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-201943890-7 
     0.550   PROCEDURAL             CONFLICT-RESOLUTION 
     0.600   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     0.600   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.600   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.600   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.600   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-201943890-1 
     0.600   PROCEDURAL             CONFLICT-RESOLUTION 
     0.650   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(go after with the intent to catch; \"The policeman chased the mugger down the alley\"; \"the dog chased the rabbit\")" 
     0.650   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.650   PROCEDURAL             CONFLICT-RESOLUTION 
     0.700   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.700   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.700   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.700   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     0.700   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-109465341-2 
     0.700   PROCEDURAL             CONFLICT-RESOLUTION 
     0.750   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     0.750   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.750   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.750   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.750   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-109465341-1 
     0.750   PROCEDURAL             CONFLICT-RESOLUTION 
     0.800   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(a dull unattractive unpleasant girl or woman; \"she got a reputation as a frump\"; \"she''s a real dog\")" 
     0.800   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.800   PROCEDURAL             CONFLICT-RESOLUTION 
     0.850   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     0.850   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.850   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.850   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     0.850   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-109382160-1 
     0.850   PROCEDURAL             CONFLICT-RESOLUTION 
     0.900   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     0.900   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.900   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     0.900   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     0.900   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-109382160-1 
     0.900   PROCEDURAL             CONFLICT-RESOLUTION 
     0.950   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(informal term for a man; \"you lucky dog\")" 
     0.950   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     0.950   PROCEDURAL             CONFLICT-RESOLUTION 
     1.000   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.000   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.000   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.000   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     1.000   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-102617005-3 
     1.000   PROCEDURAL             CONFLICT-RESOLUTION 
     1.050   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     1.050   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.050   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.050   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     1.050   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-102617005-1 
     1.050   PROCEDURAL             CONFLICT-RESOLUTION 
     1.100   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(metal supports for logs in a fireplace; \"the andirons were too hot to touch\")" 
     1.100   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.100   PROCEDURAL             CONFLICT-RESOLUTION 
     1.150   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.150   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.150   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.150   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     1.150   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-109256536-4 
     1.150   PROCEDURAL             CONFLICT-RESOLUTION 
     1.200   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     1.200   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.200   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.200   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     1.200   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-109256536-1 
     1.200   PROCEDURAL             CONFLICT-RESOLUTION 
     1.250   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(someone who is morally reprehensible; \"you dirty dog\")" 
     1.250   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.250   PROCEDURAL             CONFLICT-RESOLUTION 
     1.300   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.300   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.300   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.300   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     1.300   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-102001223-1 
     1.300   PROCEDURAL             CONFLICT-RESOLUTION 
     1.350   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     1.350   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.350   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.350   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     1.350   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-102001223-1 
     1.350   PROCEDURAL             CONFLICT-RESOLUTION 
     1.400   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(a member of the genus Canis (probably descended from the common wolf) that has been domesticated by man since prehistoric times; occurs in many breeds; \"the dog barked all night\")" 
     1.400   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.400   PROCEDURAL             CONFLICT-RESOLUTION 
     1.450   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.450   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.450   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.450   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     1.450   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-103754154-4 
     1.450   PROCEDURAL             CONFLICT-RESOLUTION 
     1.500   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     1.500   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.500   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.500   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     1.500   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-103754154-1 
     1.500   PROCEDURAL             CONFLICT-RESOLUTION 
     1.550   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "dog" IS "(a hinged catch that fits into a notch of a ratchet to move a wheel forward or prevent it from moving backward)" 
     1.550   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.550   PROCEDURAL             CONFLICT-RESOLUTION 
     1.600   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.600   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.600   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.600   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 8 
     1.600   PROCEDURAL             CONFLICT-RESOLUTION 
     1.650   PROCEDURAL             PRODUCTION-FIRED NO-MORE-WORD-SENSES 
NO MORE WORD SENSES FOR "dog" 
     1.650   PROCEDURAL             CLEAR-BUFFER GOAL 
     1.650   PROCEDURAL             CONFLICT-RESOLUTION 
     1.650   ------                 Stopped because no events left to process 

1.3 
310 
NIL 
G4 
     1.650   GOAL                   SET-BUFFER-CHUNK GOAL G4 REQUESTED NIL 
     1.650   PROCEDURAL             CONFLICT-RESOLUTION 
     1.700   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.700   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.700   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.700   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     1.700   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-102879203-2 
     1.700   PROCEDURAL             CONFLICT-RESOLUTION 
     1.750   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     1.750   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.750   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.750   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     1.750   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-102879203-1 
     1.750   PROCEDURAL             CONFLICT-RESOLUTION 
     1.800   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(a whip with nine knotted cords; \"British sailors feared the cat\")" 
     1.800   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.800   PROCEDURAL             CONFLICT-RESOLUTION 
     1.850   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     1.850   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.850   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.850   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     1.850   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-102877229-2 
     1.850   PROCEDURAL             CONFLICT-RESOLUTION 
     1.900   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     1.900   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.900   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     1.900   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     1.900   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-102877229-1 
     1.900   PROCEDURAL             CONFLICT-RESOLUTION 
     1.950   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(a large vehicle that is driven by caterpillar tracks; frequently used for moving earth in construction and farm work)" 
     1.950   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     1.950   PROCEDURAL             CONFLICT-RESOLUTION 
     2.000   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.000   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.000   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.000   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.000   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-201371307-1 
     2.000   PROCEDURAL             CONFLICT-RESOLUTION 
     2.050   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.050   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.050   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.050   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.050   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-201371307-1 
     2.050   PROCEDURAL             CONFLICT-RESOLUTION 
     2.100   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(beat with a cat-o''-nine-tails)" 
     2.100   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.100   PROCEDURAL             CONFLICT-RESOLUTION 
     2.150   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.150   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.150   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.150   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.150   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-109269334-1 
     2.150   PROCEDURAL             CONFLICT-RESOLUTION 
     2.200   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.200   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.200   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.200   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.200   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-109269334-1 
     2.200   PROCEDURAL             CONFLICT-RESOLUTION 
     2.250   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(a spiteful woman gossip; \"what a cat she is!\")" 
     2.250   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.250   PROCEDURAL             CONFLICT-RESOLUTION 
     2.300   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.300   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.300   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.300   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.300   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-102037721-1 
     2.300   PROCEDURAL             CONFLICT-RESOLUTION 
     2.350   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.350   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.350   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.350   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.350   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-102037721-1 
     2.350   PROCEDURAL             CONFLICT-RESOLUTION 
     2.400   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(feline mammal usually having thick soft fur and being unable to roar; domestic cats; wildcats)" 
     2.400   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.400   PROCEDURAL             CONFLICT-RESOLUTION 
     2.450   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.450   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.450   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.450   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.450   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-109500444-2 
     2.450   PROCEDURAL             CONFLICT-RESOLUTION 
     2.500   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.500   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.500   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.500   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.500   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-109500444-1 
     2.500   PROCEDURAL             CONFLICT-RESOLUTION 
     2.550   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(an informal term for a youth or man; \"a nice guy\"; \"the guy''s only doing it for some doll\")" 
     2.550   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.550   PROCEDURAL             CONFLICT-RESOLUTION 
     2.600   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.600   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.600   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.600   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.600   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-102043683-2 
     2.600   PROCEDURAL             CONFLICT-RESOLUTION 
     2.650   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.650   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.650   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.650   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.650   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-102043683-1 
     2.650   PROCEDURAL             CONFLICT-RESOLUTION 
     2.700   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(any of several large cats typically able to roar and living in the wild)" 
     2.700   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.700   PROCEDURAL             CONFLICT-RESOLUTION 
     2.750   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.750   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.750   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.750   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.750   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-103475580-5 
     2.750   PROCEDURAL             CONFLICT-RESOLUTION 
     2.800   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.800   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.800   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.800   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.800   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-103475580-1 
     2.800   PROCEDURAL             CONFLICT-RESOLUTION 
     2.850   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(the leaves of the shrub Catha edulis which are chewed like tobacco or used to make tea; has the effect of a euphoric stimulant; \"in Yemen kat is used daily by 85% of adults\")" 
     2.850   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.850   PROCEDURAL             CONFLICT-RESOLUTION 
     2.900   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     2.900   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.900   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.900   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     2.900   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL S-200074101-6 
     2.900   PROCEDURAL             CONFLICT-RESOLUTION 
     2.950   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-GLOSS 
     2.950   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     2.950   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     2.950   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 1 
     2.950   WN-LEXICAL             SET-BUFFER-CHUNK WN-LEXICAL G-200074101-1 
     2.950   PROCEDURAL             CONFLICT-RESOLUTION 
     3.000   PROCEDURAL             PRODUCTION-FIRED SAY-DEFINITION 
A DEFINITION OF "cat" IS "(eject the contents of the stomach through the mouth; \"After drinking too much, the students vomited\"; \"He purged continuously\"; \"The patient regurgitated the food we gave him last night\")" 
     3.000   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     3.000   PROCEDURAL             CONFLICT-RESOLUTION 
     3.050   PROCEDURAL             PRODUCTION-FIRED FIND-WORD-DEFINITION 
     3.050   PROCEDURAL             CLEAR-BUFFER WN-LEXICAL 
     3.050   WN-LEXICAL             RETRIEVE-WN-CHUNKS 
     3.050   WN-LEXICAL             SELECT-FROM-RETRIEVED-WN-CHUNKS SET OF SIZE 9 
     3.050   PROCEDURAL             CONFLICT-RESOLUTION 
     3.100   PROCEDURAL             PRODUCTION-FIRED NO-MORE-WORD-SENSES 
NO MORE WORD SENSES FOR "cat" 
     3.100   PROCEDURAL             CLEAR-BUFFER GOAL 
     3.100   PROCEDURAL             CONFLICT-RESOLUTION 
     3.100   ------                 Stopped because no events left to process 

1.4499999 
346 
NIL 
|#

