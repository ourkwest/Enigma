(ns uk.me.westmacott.clojure.enigma)

(def alpha (int \a))
(defn to-num [char] (- (int char) alpha))
(defn to-char [num] (char (+ num alpha)))

(comment                 a b c d e f g h i j k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z)
(comment                 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
(def reflector          [24 17 20 7 16 18 11 3 15 23 13 6 14 10 12 8 4 1 5 25 2 22 21 9 0 19])
(def rotor-1 (with-meta [4 10 12 5 11 6 3 16 21 25 13 19 14 22 24 7 23 20 18 15 0 8 1 17 2 9] {:nudge 16}))
(def rotor-2 (with-meta [0 9 3 10 18 8 17 20 23 1 11 7 22 19 12 2 16 6 25 13 15 24 5 21 14 4] {:nudge  4}))
(def rotor-3 (with-meta [1 3 5 7 9 11 2 15 17 19 23 21 25 13 24 4 8 22 6 0 10 12 20 18 16 14] {:nudge 21}))

(def rotate-num (comp #(mod % 26) dec))
(defn rotate-inputs [rotor] (conj (subvec rotor 1) (first rotor)))
(defn rotate-outputs [rotor] (vec (map rotate-num rotor)))
(defn rotate 
  ([rotor]
    (with-meta (rotate-outputs (rotate-inputs rotor)) {:nudge (rotate-num (:nudge (meta rotor)))}))
  ([rotor ref]
    (if (= 0 (:nudge (meta ref))) (rotate rotor) rotor)))
(defn rotate-all [[a b c]]
  [(rotate a b) (rotate b c) (rotate c)])
(defn rotations-of [a b c] 
  (drop 1 (iterate rotate-all [a b c])))
(defn pre-set [rotor starts-at]
  (nth (iterate rotate rotor) (to-num starts-at)))

(defn forward-pass [r n] (r n))
(defn reverse-pass [r n] (.indexOf r n))
(defn encode-char [char [r1 r2 r3]]
  ((comp 
     to-char 
     (partial reverse-pass r3) 
     (partial reverse-pass r2) 
     (partial reverse-pass r1) 
     (partial forward-pass reflector)
     (partial forward-pass r1)
     (partial forward-pass r2)
     (partial forward-pass r3)
     to-num) 
    char))

(defn build [r1 r2 r3 s1 s2 s3]
  #(apply str (map encode-char % (rotations-of (pre-set r1 s1) (pre-set r2 s2) (pre-set r3 s3)))))

(def my-machine (build rotor-1 rotor-2 rotor-3 \m \c \k))

(println (my-machine "qmjidomzwzjfjr"))
(println (my-machine "enigmarevealed"))