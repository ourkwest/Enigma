; First we define a namespace.
(ns uk.me.westmacott.clojure.enigma
  "This namespace contains the Clojure code.")

; The Enigma machine deals in letters, performing a letter-by-letter translation of a message into an encrypted message.
; However, each letter of the alphabet (a-z) can also be represented as a number (0-25).
; Modelling letters as numbers can be useful for performing arithmetic operations.

; The letter 'a' serves as a datum point for the other letters in the alphabet.
; Here we define a reference to the integer value of the character 'a'.
; This will help us to treat all letters as numbers with a common reference point.

(def alpha
  "The numerical value of the character 'a'."
  (int \a))

; And now some conversion functions...
(defn to-num
  "Turns characters into numbers, relative to the letter 'a'.
  For example, 'a' becomes 0, 'b' becomes 1, 'c' becomes 2 etc."
  [char]
  (- (int char) alpha))

; And the reverse operation...
(defn to-char
  "Turns numbers into characters. 0 becomes 'a', 1 becomes 'b', etc."
  [num]
  (char (+ num alpha)))

; We could have defined a vector of characters ['a', 'b', 'c'...] and used the mapping between character and position
; to perform these translations, but the mapping already exists in the integer values of the characters.


; Next we define the parts of the machine.
;
; The machine has three rotors and a reflector. Each of these parts serves to provide a 1-to-1 mapping between an input
; letter and an output letter.
;
; In a real Enigma machine, each rotor was a physical disk with internal wires connecting each of 26 electrical contacts
; on one side with one of the 26 contacts on the other.
; The wiring was such that the contacts on each side were mismatched.
; The reflector is a special case of a rotor. It differs in that it only had contacts on one side.
; Again the contacts were wired together in pairs to provide a 1-to-1 mapping.
; Also, the reflector did not rotate.
;
; Each input letter to be encoded was pressed on a keyboard. This generated an electrical signal in one of 26 different
; positions. The signal was passed through the three rotors in order, then passed through the reflector,
; and then back through the three rotors in reverse order to give a signal in a new position. This was mapped back to
; a light on a display board representing the output letter.
;
; You can read more about the Enigma machien here: https://en.wikipedia.org/wiki/Enigma_machine
;
; Each rotor can rotate between 26 positions.
; Every time a character is input the first rotor advances one position (one 26th of a 360 degree turn).
; Every time the first rotor completes a full rotation (every 26 input characters), the second rotor advances one
; position.
; Every time the second rotor completes a full rotation (every 26 x 26 input characters), the third rotor advances one
; position.
;
; Here in code, we define the wiring for the reflector and the three rotors in the machine.
;
; In pratice there would have been a box of rotors to choose from. A code book (out-of-band communication) would
; dictate which rotors to use each day, as well as the starting positions of the rotors.
;
; Here we use Clojure's vectors to store the data, relying on their ability to act as functions. When called as a
; function with a single numerical argument 'n' a vector will return the 'nth' item it contains.
; If we imagine each letter of the alphabet as a number from 0 to 25, then passing that number to one of these vectors
; will return a new number from 0 to 25. (The vectors only contain the numbers 0-25)
; Hence each vector provides the 1-to-1 mapping required to emulate a rotor.
;
; The ':notch' metadata will tell us when the next rotor should be rotated.

;;                        a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
;;                        0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
(def reflector          [24 17 20  7 16 18 11  3 15 23 13  6 14 10 12  8  4  1  5 25  2 22 21  9  0 19])
(def rotor-1 (with-meta [ 4 10 12  5 11  6  3 16 21 25 13 19 14 22 24  7 23 20 18 15  0  8  1 17  2  9] {:notch 16}))
(def rotor-2 (with-meta [ 0  9  3 10 18  8 17 20 23  1 11  7 22 19 12  2 16  6 25 13 15 24  5 21 14  4] {:notch  4}))
(def rotor-3 (with-meta [ 1  3  5  7  9 11  2 15 17 19 23 21 25 13 24  4  8 22  6  0 10 12 20 18 16 14] {:notch 21}))

; In the physical machine the signal passes through each disk once in each direction.
; The inverse function will be useful when the signal is returning through the rotor from the other side.
(defn invert [r]
  "Provide the inverse mapping function for a rotor.
  E.g. if the provided rotor mapped 5 to 21, then the returned function will map 21 to 5."
  #(.indexOf r %))

; In order to rotate a rotor in this model we need to do three things.
;
; Firstly we need to 'rotate' the contents of the vector; that is, to shuffle all the numbers in the vector along by
; one position. This has the effect of changing the index of every value by one.
; In the physical machine this is analagous to rotating the contacts on the input side of the rotor disk.
;
; Secondly, we need to 'rotate' the individual numbers; that is, to decrease every value by one. This has the effect of
; changing the value at every index.
; In the physical machine this is analagous to rotating the contacts on the output side of the rotor disk.
;
; Thirdly, we need to move the notch in the rotor that causes rotations to periodically cacsade to the next rotor.
;
; All three of these operations need to wrap around modulo 26.

; Here we use function composition to provide a modular decrement function.
(def rotate-num
  "Decrease a number by 1 modulo 26."
  (comp #(mod % 26) dec))

; Here we must be careful to transfer the metadata to the new vector.
(defn rotate-inputs
  "Appends the first value of a vector to the rest of the values in the vector.
  It has the effect of shuffling the contents along by one position, or 'rotating' the contents of the vector."
  [rotor]
  (with-meta (conj (vec (rest rotor)) (first rotor))
             (meta rotor)))

; Here we must also be careful to transfer the metadata to the new vector.
(defn rotate-outputs
  "Decrements every number in a vector modulo 26."
  [rotor]
  (with-meta (vec (map rotate-num rotor))
             (meta rotor)))

; Due to Clojure's immutable data-structures we do not update in place, but construct a copy with the new metadata.
(defn rotate-notch
  "Decrements the notch of a rotor modulo 26."
  [rotor]
  (vary-meta rotor update :notch rotate-num))

; Now we put these parts together to make our rotate function.
(defn rotate
  "Rotates a rotor by rotating both the input side and the output side and also the notch in the rotor.
  If a second rotor is provided then the first rotor will only be rotated if either of the provided rotor's notches are
  in the 0th position."
  ([rotor]
   (rotate-notch (rotate-outputs (rotate-inputs rotor))))
  ([rotor preceeding-rotor]
    (if (or (= 0 (:notch (meta preceeding-rotor)))
            (= 0 (:notch (meta rotor))))
      (rotate rotor)
      rotor)))

(defn rotate-all
  "Rotates three rotors, with regard to their relative positions in the Enigma machine."
  [[rotor-a rotor-b rotor-c]]
  [(rotate rotor-a rotor-b) (rotate rotor-b rotor-c) (rotate rotor-c)])

; We can define all future positions of the rotors.
; Notice that we drop the first position as the rotations happen before each key press, so the initial positions of the
; rotors are not used to encode any characters.
(defn rotations-of
  "Returns an infinite (but lazily-computed) sequence of all future positions of the three provided rotors."
  [rotor-a rotor-b rotor-c]
  (drop 1 (iterate rotate-all [rotor-a rotor-b rotor-c])))

; The rotors were pre-set in the Enigma machine to a position dictated by the day's code book. We need to be able to
; set the given rotors to arbitrary positions.
(defn pre-set
  "Rotates a rotor to a given position."
  [rotor starts-at]
  (nth (iterate rotate rotor) (to-num starts-at)))

; We can encode characters with the rotors. Here we use the threading macro ('->') to pass the input character
; through a pipeline of functions. Each function in the pipeline is called with the result of the last function.
; This makes clear the structure of the code, which here mimics the path of a signal through the Enigma machine.
(defn encode-char
  "Encode a character with the given rotors."
  [char [rotor-a rotor-b rotor-c]]
  (let [inverse-a (invert rotor-a)
        inverse-b (invert rotor-b)
        inverse-c (invert rotor-c)]
    (-> char
        to-num
        rotor-c rotor-b rotor-a
        reflector
        inverse-a inverse-b inverse-c
        to-char)))

; Finally, we can build an Enigma machine. The whole machine itself is a pure function (it has no side effects).
; It takes a sequence of characters (such as a string) and maps the encoding function over them and the sequence of
; upcoming rotor positions. It then applies 'str' to the sequence of characters returned to create a string output.
;
; Because it is stateless (having no side effects, it cannot even update its own internal state), if it is reused for
; a new message it appears to start from the original rotor positions.
(defn build
  "Returns a function that uses the given rotors and starting positions to encode characters."
  [rotor-a rotor-b rotor-c setting-a setting-b setting-c]
  (fn [message]
    (apply str (map encode-char
                    message
                    (rotations-of (pre-set rotor-a setting-a)
                                  (pre-set rotor-b setting-b)
                                  (pre-set rotor-c setting-c))))))

; Now we can build a sample Enigma machine, passing it today's selection of rotors and rotor positions.
(def my-machine
  (build rotor-1 rotor-2 rotor-3 \m \c \k))

; And we can test it with some sample data to ensure correctness!
(println (my-machine "qmjidomzwzjfjr"))
(println (my-machine "apjoaxpkaoxffynefrdqyiyijbfzhenukofgslrxr"))

; Thanks for reading!