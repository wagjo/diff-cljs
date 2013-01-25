;; Copyright (C) 2011, Brenton Ashworth. All rights reserved.
;; Copyright (C) 2013, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns wagjo.diff.optimizations
  "String optimizations for diff algorithms.
  See http://neil.fraser.name/writing/diff/"
  (:require-macros [wagjo.data.string :as us])
  (:require [wagjo.data.string :as us]))

;;;; Implementation details

(defn- stripped-common-prefix
  "Strips common prefix.
  Returns [number-of-chars-stripped stripped-a stripped-b]."
  [^String a ^String b]
  (let [n (min (us/count a) (us/count b))
        i (loop [i 0]
            (if (and (< i n) (identical? (us/nth-unchecked a i)
                                         (us/nth-unchecked b i)))
              (recur (inc i))
              i))]
    [i (us/slice-from a i) (us/slice-from b i)]))

(defn- stripped-common-suffix
  "Stips common suffix.
  Returns [number-of-chars-stripped stripped-a stripped-b]."
  [^String a ^String b]
  (let [la (us/count a)
        lb (us/count b)
        inc-n (inc (min la lb))
        i (loop [i 1]
            (if (and (< i inc-n)
                     (identical? (us/nth-unchecked a (- la i))
                                 (us/nth-unchecked b (- lb i))))
              (recur (inc i))
              (dec i)))]
    [i (us/slice-to a (- la i)) (us/slice-to b (- lb i))]))

(defn- offset-script
  "Adjust indexes in the edit script with a specified offset.
  Returns adjusted edit script."
  [edit-script offset]
  [(vec (map #(assoc % 0 (+ offset (nth % 0))) (nth edit-script 0)))
   (vec (map #(+ offset %) (nth edit-script 1)))])

(defn- short-within-long
  "Return a diff edit script if the shorter sequence exists in the
  longer one. No need to use the expensive diff algorithm for this."
  [^String a ^String b]
  (let [ca (count a)
        cb (count b)
        switch? (> ca cb)
        short (if switch? b a)
        long (if switch? a b)
        i (us/index-of long short)]
    (when-not (== i -1)
      (if (identical? short a)
        [(remove nil?
                 [(when (> i 0)
                    (vec (cons 0 (seq (us/slice-to b i)))))
                  (when (< (+ i ca) cb)
                    (vec (cons (+ i ca)
                               (seq (us/slice-from b (+ i ca))))))])
         []]
        [[] (vec (concat (range 0 i) (range (+ i cb) ca)))]))))

(defn- find-half-match*
  "Searches for substring in sequences based on half match algorithm.
  Returns [common-sequence prefix-a suffix-a prefix-b suffix-b]
  or nil, if not found."
  [^String long ^String short ^Integer i]
  (let [target (us/slice long i (+ i (quot (count long) 4)))]
    (loop [j (us/index-of short target)
           result ["" nil nil nil nil]]
      (cond (not (neg? j))
            (let [[prefix-length] ^ArrayVector
                  (stripped-common-prefix (us/slice-from long i)
                                          (us/slice-from short j))
                  [suffix-length] ^ArrayVector
                  (stripped-common-suffix (us/slice-to long i)
                                          (us/slice-to short j))
                  new-result
                  (if (< (count (nth result 0))
                         (+ prefix-length suffix-length))
                    [(us/cat
                       (us/slice short (- j suffix-length) j)
                       (us/slice short j (+ j prefix-length)))
                      (us/slice-to short (- j suffix-length))
                      (us/slice-from short (+ j prefix-length))
                      (us/slice-to long (- i suffix-length))
                      (us/slice-from long (+ i prefix-length))]
                    result)]
              (recur (us/index-of-from short target (inc j)) new-result))
            (< (count (nth result 0)) (quot (count long) 2))
            nil
            :else
            result))))

(defn- find-half-match
  "Find a substring shared by both sequences which is at least half
  as long as the longer sequence. Returns [common-sequence
  prefix-a suffix-a prefix-b suffix-b] or nil, if not found."
  [^String a ^String b]
  (let [switch? (> (count a) (count b))
        short (if switch? b a)
        long (if switch? a b)
        short-count (count short)
        long-count (count long)]
    (when-not (or (< long-count 4) (< (* short-count 2) long-count))
      (let [hm-second-q
            (find-half-match* long short (quot (+ long-count 3) 4))
            hm-third-q
            (find-half-match* long short (quot (+ long-count 1) 2))
            half-match (if (and hm-second-q hm-third-q)
                         (if (> (count (nth hm-second-q 0))
                                (count (nth hm-third-q 0)))
                           hm-second-q
                           hm-third-q)
                         (or hm-second-q hm-third-q))]
        (cond (nil? half-match) nil
              (identical? b long) half-match
              :else
              (let [[common a-prefix a-suffix b-prefix b-suffix]
                    ^ArrayVector half-match]
                [common b-prefix b-suffix a-prefix a-suffix]))))))

(declare diff)

(defn- half-match-diff
  "Returns diff edit script when half match found."
  [half-match f]
  (log "half match!" (pr-str half-match))
  (let [[common a-prefix a-suffix b-prefix b-suffix] ^ArrayVector half-match
        [adds-a dels-a] ^ArrayVector (diff a-prefix b-prefix f)
        [adds-b dels-b] ^ArrayVector
        (-> (diff a-suffix b-suffix f)
            (offset-script (+ (count common)
                              (count a-prefix))))]
    [(concat adds-a adds-b) (concat dels-a dels-b)]))

(defn- diff*
  "Calculate the diff edit script using the function f only after
  ensuring that this algorithm is required. At this point we know
  that a and b are different at both ends. An edit script can be
  calculated manually if the length of a or b is 0 or if the smaller
  of the two sequences is contained within the longer."
  [^String a ^String b f]
  (or (and (empty? a) [[(vec (cons 0 (seq b)))] []])
      (and (empty? b) [[] (vec (range 0 (count a)))])
      (short-within-long a b)
      (when-let [half-match (find-half-match a b)]
        (half-match-diff half-match f))
      (f a b)))

(defn- prefixed-diff
  "Returns diff edit script.
  Strips prefix and suffix, computes diff and recalculates indexes."
  [a b f]
  (let [[prefix-count a b] ^ArrayVector (stripped-common-prefix a b)
        [_ a b] ^ArrayVector (stripped-common-suffix a b)
        result (diff* a b f)]
    (if (pos? prefix-count)
      (offset-script result prefix-count)
      result)))

;;;; Public API

(defn diff
  "Returns the diff edit script of a and b. Computes diff by
  performing various tricks. Falls back to f if diff optimizations
  cannot be applied. Checks for nil and equality.
  Tests for common prefix and suffix."
  [^String a ^String b f]
  (cond (or (nil? a) (nil? b)) (f a b)
        (identical? a b) [[] []]
        :else (prefixed-diff a b f)))
