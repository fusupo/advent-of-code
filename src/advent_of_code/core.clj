(ns advent-of-code.core
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DAY01 INVERSE-CAPTCHA

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn inverse-captcha [i]
  (let [dgts (digits i)]
    (reduce
     +
     0
     (loop [fst (first dgts)
            rst (rest dgts)
            l '()]
       (if (empty? rst)
         (if (= fst (first dgts))
           (conj l fst)
           l)
         (recur
          (first rst)
          (rest rst)
          (if (= fst (first rst))
            (conj l fst)
            l)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DAY-02 CORRUPTION CHECKSUM

(defn reduce-dgts-with-pred [dgts pred]
  (reduce
   (fn [m i]
     (if (pred m i) m i))
   (first dgts)
   dgts))

(defn find-diff [line]
  (let [dgts (map read-string (str/split line #"[ \t]"))
        lesser (reduce-dgts-with-pred dgts <)
        greater (reduce-dgts-with-pred dgts >)]
    (- greater lesser)))

(defn corruption-checksum [spreadsheet]
  (let [lines (str/split-lines spreadsheet)]
    (reduce
     (fn [m l]
       (+ m (find-diff l)))
     0
     lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DAY-04 HIGH-ENTROPY PASSPHRASES

(defn is-valid-passphrase [phrase]
  (let [words (str/split phrase #" ")
        words-set (set words)]
    (= (count words) (count words-set))))

(defn count-valid-passphrases [phrase-list]
  (let [lines (str/split-lines phrase-list)]
    (reduce
     (fn [m l]
       (if (is-valid-passphrase l)
         (+ m 1)
         m))
     0
     lines)))
