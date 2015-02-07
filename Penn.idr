module Penn

%default total

data Tree = ROOT Tree
          | S Tree
          | SBAR Tree
          | SBARQ Tree
          | SINV Tree
          | SQ Tree
          | ADJP Tree
          | ADVP Tree
          | CONJP Tree
          | FRAG Tree
          | INTJ Tree
          | LST Tree
          | NAC Tree
          | NP Tree
          | NX Tree
          | PP Tree
          | PRN Tree
          | PRT Tree
          | QP Tree
          | RRC Tree
          | UCP Tree
          | VP Tree
          | WHADJP Tree
          | WHAVP Tree
          | WHNP Tree
          | WHPP Tree
          | X Tree
          | CC String
          | CD String
          | DT String
          | EX String
          | FW String
          | IN String
          | JJ String
          | JJR String
          | JJS String
          | LS String
          | MD String
          | NN String
          | NNS String
          | NNP String
          | NNPS String
          | PDT String
          | POS String
          | PRP String
          | RB String
          | RBR String
          | RBS String
          | RP String
          | SYM String
          | TO String
          | UH String
          | VB String
          | VBD String
          | VBG String
          | VBN String
          | VBP String
          | VBZ String
          | WDT String
          | WP String
          | WRB String


-- See Penn Treebank II Constituent Tags
-- http://www.surdeanu.info/mihai/teaching/ista555-spring15/readings/PennTreebankConstituents.html
