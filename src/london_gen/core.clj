(ns london-gen.core
  (require [clojure.string :as str]))



(defn any
  "Rule which says that any of the given rules are legal here."
  [& args]
  (vec (cons :any args)))

(defn s
  "Rule for a sequence of rules."
  [& rules]
  (vec (cons :seq rules)))

(defn fmap
  "A rule which applies a function to transform the result of a rule."
  [f rule]
  [:map f rule])

(defn gen
  "Generates a random string by interpreting the given ruleset, starting with
  rule."
  [rule ruleset]
  (cond
    ; Literal
    (string? rule) rule

    ; Recursion
    (keyword? rule) (if-let [r (get ruleset rule)]
                      (recur (get ruleset rule) ruleset)
                      (throw (IllegalArgumentException.
                               (str "No rule " (pr-str rule)))))

    ; Composite rule
    (sequential? rule)
    (let [[rule-type & args] rule]
      (case rule-type
        :any (gen (rand-nth args) ruleset)
        :map ((first args) (gen (second args) ruleset))
        :seq (str/join (map (fn [r] (gen r ruleset)) args))))))

(def rules
  {:misc (any
           "dollis"
           "ealing"
           "harrow"
           "ken"
           "notting"
           "kent"
           "epp"
           "parsons"
           "kensing"
           "padding"
           "tooting"
           "ruis"
           "totteridge"
           "totten"
           "wimble"
           "wembley"
           "willes"
           "tufnell"
           )

   :thing (any
            "arch"
            "arsenal"
            "bank"
            "tower"
            "rod"
            "barb"
            "bark"
            "baron"
            "bay"
            "bond"
            "pike"
            "borough"
            "whetstone"
            "bush"
            "bound"
            "cottage"
            "can"
            "canada"
            "cannon"
            "canon"
            "chalk"
            "chance"
            "clap"
            "oval"
            "snare"
            "covent"
            "earl"
            "elm"
            "pin"
            "farring"
            "stead"
            "fin"
            "grange"
            "gunner"
            "hanger"
            "hat"
            "ham"
            "hammer"
            "heath"
            "hill"
            "moor"
            "step"
            "stock"
            "mile"
            "mill"
            "shepherd"
            "rick"
            "monument"
            "morning"
            "crescent"
            "horn"
            "king"
            "oak"
            "stone"
            "liver"
            "pool"
            "water"
            "weald"
            "hall"
            "toot"
            "wood"
            "wick"
            "sister"
            )

   :person (any
             "maryle"
             "brent"
             "colin"
             "collier"
             "harles"
             "pancras"
             "latimer"
             "laida"
             "victoria"
             "regent"
             "russel"
             "james"
             "john"
             "paul"
             "sloane"
             )

   :animal (any
              "cock"
              "angel"
              "elephant"
              "buck"
              "hen"
              "hawk"
              "horse"
              "raven"
              "finch"
              "ox"
              )

   :numbers (any "seven")

   :maker (any
             "smith"
             "friar"
             "caster")

   :title (any "st.")

   :adjective-prefix (any "black"
                          "burnt"
                          "white"
                          "rich"
                          "broad"
                          "fair"
                          "high"
                          "low"
                          "new"
                          "gold"
                          "great"
                          "green"
                          "red"
                          "queens"
                          "wan"
                          )

   :adjective (any
                 "queen's"
                 "swiss"
                 "black"
                 "burnt"
                 "caledonian"
                 "high"
                 "old"
                 "new"
                 "great"
                 "low"
                 "marble"
                 "white"
                 "broad"
                 "green"
                 "royal"
                 )

  :place (any
           "arch"
           "temple"
           "market"
           "castle"
           "junction"
           "circle"
           "circus"
           "central"
           "court"
           "corner"
           "common"
           "cross"
           "end"
           "farm"
           "gate"
           "garden"
           "green"
           "avenue"
           "grove"
           "valley"
           "kil"
           "hill"
           "brook"
           "lane"
           "manor"
           "mews"
           "park"
           "city"
           "plaza"
           "road"
           "street"
           "square"
           "town"
           "water"
           "wharf"
           "vale"
           )

  :place-suffix (any
                  "berg"
                  "borough"
                  "chapel"
                  "bury"
                  "burn"
                  "wark"
                  "bridge"
                  "church"
                  "court"
                  "hall"
                  "dale"
                  "den"
                  "don"
                  "dilly"
                  "font"
                  "ford"
                  "ham"
                  "row"
                  "sal"
                  "side"
                  "stead"
                  "ton"
                  "pool"
                  "town"
                  "tree"
                  "ville"
                  "wich"
                  "fields"
                  "water"
                  "way"
                  "well"
                  "wood"
                  "-on-the-hill"
                  )

   :simple-noun (any :thing :person :animal :place)

   :noun-noun   (s :simple-noun :simple-noun)

   :maker-noun  (s :simple-noun :maker)

   :adjectivized-noun (s :adjective-prefix (any :simple-noun
                                                :maker-noun
                                                :place))

   :place-noun (s (any :simple-noun
                       :maker-noun
                       :adjectivized-noun)
                   :place-suffix)

   :noun (any :simple-noun
              :maker-noun
              :noun-noun
              :adjectivized-noun
              :place-noun)

   :misc-prefix (any "ac"
                     "ald"
                     "turn"
                     "bromp"
                     "wat"
                     "brix"
                     "ux"
                     "bal"
                     "chal"
                     "vaux"
                     "bel"
                     "ber"
                     "bos"
                     "cam"
                     "beth"
                     "becon"
                     "picca"
                     "lan"
                     "char"
                     "ches"
                     "chis"
                     "chig"
                     "chor"
                     "crox"
                     "dagen"
                     "deb"
                     "edg"
                     "eus"
                     "em"
                     "ful"
                     "pres"
                     "hamp"
                     "hol"
                     "houn"
                     "ick"
                     "lad"
                     "lam"
                     "lei"
                     "ley"
                     "lon"
                     "lough"
                     "mor"
                     "stam"
                     "stan"
                     "strat"
                     "up"
                     "sud"
                     "put"
                     "oster"
                     "war"
                     "walt"
                     "wimble"
                     )

   :misc-suffix (any
                  "born"
                  "wick"
                  "beth"
                  "bone"
                  "well"
                  "broke"
                  "cote"
                  "more"
                  "bourne"
                     "cester"
                     "minster"
                  "den"
                  "don"
                  "dilly"
                  "ester"
                  "er"
                  "en"
                  "fosters"
                  "hurst"
                  "ican"
                  "ing"
                  "ley"
                  "loo"
                  "lop"
                  "ment"
                  "mond"
                  "mans"
                  "worth"
                  "lip"
                  "nal"
                  "ney"
                  "tow"
                  "sey"
                  "size"
                  "stone"
                  "ware"
                  "wick")

   :direction (any "north"
                   "east"
                   "south"
                   "west")

   :word (any :noun
             (s :misc-prefix :misc-suffix)
             (s :misc-prefix (any :noun :misc))
             (s (any :noun :misc) :misc-suffix))

   :adjective+word (s :adjective " " :word)

   :direction+word (any (s :word " " :direction)
                        (s :direction " " :word))

   :title+word (s :title " " :word)

   :word+place (s :word " " :place)

   :phrase (any :word
                :word+place
                :title+word
                :adjective+word
                :direction+word)

   :word+word (any (s :word "-by-" :word)
                    (s :phrase " & " :phrase))

   :name (any :phrase :word+word)})

(defn go
  []
  (->> rules
       (partial gen :name)
       (repeatedly)
       (take 12)))
