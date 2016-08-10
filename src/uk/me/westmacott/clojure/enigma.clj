; First we define a namespace
(ns uk.me.westmacott.clojure.enigma
  "This namespace contains the Clojure code.")

; The Enigma machine deals in letters, performing a letter-by-letter translation of a message into an encrypted message.
; However, each letter of the alphabet (a-z) can also be represented as a number (0-25). This can be useful.

(def alpha
  "The letter 'a' serves as a datum point for the other letters in the alphabet.
  Here we obtain the integer value of the character 'a'.
  This will help us to treat all letters as numbers with a common reference point."
  (int \a))

(defn to-num
  "A function to turn characters into numbers, relative to the letter 'a'.
  For example, 'a' becomes 0, 'b' becomes 1, 'c' becomes 2 etc."
  [char]
  (- (int char) alpha))

; And the reverse operation...
(defn to-char
  "A function to turn numbers into letters. 0 becomes 'a', 1 becomes 'b', etc."
  [num]
  (char (+ num alpha)))

; We could have defined a vector of letters ['a', 'b', 'c'...] and used the mapping between letter and position to
; perform these translations, but the mapping already exists in the integer values of the characters.


; Next we define the parts of the machine.
;
; The machine has three rotors and a reflector. Each of these parts serves to provide a 1-to-1 mapping between an input
; letter and an output letter.
;
; In a real Enigma machine, each rotor was a physical disc with internal wires connecting each of 26 electrical contacts
; on one side with one of the 26 contacts on the other.
; The wiring was such that the contacts on each side were mismatched.
; The reflector is a special case of a rotor. It differs in that it only had contacts on one side.
; Again the contacts were wired together in pairs to provide a 1-to-1 mapping.
;
; Each input letter to be encoded is passed through the three rotors in order, then passed through the reflector,
; and then back through the three rotors in reverse order to give an output letter.
;
; Each rotor can rotate between 26 positions.
; Every time a character is input the first rotor advances one position (One 26th of a 360 degree turn).
; Every time the first rotor completes a full rotation (every 26 input characters), the second rotor advances one
; position.
; Every time the second rotor completes a full rotation (every 26 x 26 input characters), the third rotor advances one
; position.
;
; Here in code, we define the wiring for the reflector and the three rotors in the machine.
;
; In pratice there would have been a box of rotors to choose from. A code book (secure out-of-band communication) would
; dictate which rotors to use each day, as well as the starting positions of the rotors.
;
; Here we use Clojure's vectors to store the data, relying on their ability to act as functions. When called as a
; function with a single numerical argument 'n' a vector will return the 'nth' item it contains.
; If we imagine each letter of the alphabet as a number from 0 to 25, then passing that number to one of these vectors
; will return a new number from 0 to 25. (The vectors only contain the numbers 0-25)
; Hence each vector provides the 1-to-1 mapping required to emulate a rotor.
;
; The ':nudge' metadata will tell us when the next rotor should be rotated.

;;                        a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
;;                        0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
(def reflector          [24 17 20  7 16 18 11  3 15 23 13  6 14 10 12  8  4  1  5 25  2 22 21  9  0 19])
(def rotor-1 (with-meta [ 4 10 12  5 11  6  3 16 21 25 13 19 14 22 24  7 23 20 18 15  0  8  1 17  2  9] {:nudge 16}))
(def rotor-2 (with-meta [ 0  9  3 10 18  8 17 20 23  1 11  7 22 19 12  2 16  6 25 13 15 24  5 21 14  4] {:nudge  4}))
(def rotor-3 (with-meta [ 1  3  5  7  9 11  2 15 17 19 23 21 25 13 24  4  8 22  6  0 10 12 20 18 16 14] {:nudge 21}))

; The inverse function will be useful when the signal is returning through the rotor from the other side.
(defn invert [r]
  "Provide the inverse mapping function for a rotor.
  E.g. if the provided rotor mapped 5 to 21, then the returned function will map 21 to 5."
  #(.indexOf r %))

; Here we use function composition to provide a modular decrement function.
(def rotate-num
  "Decrease a number by 1 modulo 26."
  (comp #(mod % 26) dec))

(defn rotate-inputs [rotor] (conj (vec (rest rotor)) (first rotor)))
(defn rotate-outputs [rotor] (vec (map rotate-num rotor)))
(defn rotate 
  ([rotor]
    (with-meta (rotate-outputs (rotate-inputs rotor)) {:nudge (rotate-num (:nudge (meta rotor)))}))
  ([rotor ref]
    (if (or (= 0 (:nudge (meta ref))) (= 0 (:nudge (meta rotor)))) (rotate rotor) rotor)))
(defn rotate-all [[a b c]]
  [(rotate a b) (rotate b c) (rotate c)])
(defn rotations-of [a b c] 
  (drop 1 (iterate rotate-all [a b c])))
(defn pre-set [rotor starts-at]
  (nth (iterate rotate rotor) (to-num starts-at)))

(defn encode-char [char [r1 r2 r3]]
  (let [i1 (invert r1) i2 (invert r2) i3 (invert r3)]
    ((comp to-char i3 i2 i1 reflector r1 r2 r3 to-num) char)))

(defn build [r1 r2 r3 s1 s2 s3]
  #(apply str (map encode-char % (rotations-of (pre-set r1 s1) (pre-set r2 s2) (pre-set r3 s3)))))

(def my-machine (build rotor-1 rotor-2 rotor-3 \m \c \k))

(println (my-machine "qmjidomzwzjfjr"))
(println (my-machine "apjoaxpkaoxffynefrdqyiyijbfzhenukofgslrxr"))