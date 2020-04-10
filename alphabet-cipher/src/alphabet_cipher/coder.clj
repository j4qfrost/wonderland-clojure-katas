(ns alphabet-cipher.coder)

(require '[clojure.string :as string])

(def ^:const ascii-offset 97)
(def ^:const num-letters 26)

(defn str->ints [s offset]
  (map #(- (int %) offset) s))

(defn int->char-with-offset [i conversion-offset]
 (char (+ (mod i num-letters) conversion-offset)))

(defn process [keyword message op]
  (let [parts (partition-all (count keyword) (str->ints message ascii-offset))
        keynums (str->ints keyword ascii-offset)]
    (apply str (mapcat #(for [[m k] (map list % keynums)] (int->char-with-offset (op m k) ascii-offset)) parts))))

(defn encode [keyword message]
  (process keyword message +))

(defn decode [keyword message]
  (process keyword message -))

(defn all-equal [p0 parts]
  (if (or (empty? p0)) 
    false
    (every? #(= % p0) parts)))

(defn find-pattern [tape cut]
  (let [parts (reverse (partition-all cut tape))
        dups (rest parts)
        p0 (first dups)
        end (first parts)]
    (cond
      (and (=  (take (count end) p0) end) (all-equal p0 (rest dups))) cut
      (>= cut (count tape)) (count tape)
      :else (find-pattern tape (+ cut 1)))))
    

(defn decipher [cipher message]
  (let [cints (str->ints cipher ascii-offset)
        mints (str->ints message ascii-offset)
        tape (for [[c m] (map list cints mints)] (mod (+ (- c m) num-letters) num-letters))]
    (apply str (for [i (take (find-pattern tape 1) tape)] (int->char-with-offset i ascii-offset)))))

