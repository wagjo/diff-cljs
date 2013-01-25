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

(ns wagjo.diff.core
  "Diff, patch and related functions for Clojure sequences."
  (:require [wagjo.data.string :as us]
            [wagjo.diff.miller :as miller]))

;;;; Implementation details

(deftype Addition [elm-seq o])

(defn- merge-patch
  "Returns transformed sequence from sequence a according to edit
  script, replacing removed elements with delete symbol."
  [a edit-script delete-symbol]
  (let [[adds dels] ^ArrayVector edit-script
        a (vec a)
        l (count a)
        add-fn
        (fn [b-vec add] (let [index (first add)
                              elm-seq (rest add)]
                          (if (== index l)
                            (into b-vec elm-seq)
                            (assoc b-vec index
                                   (Addition. elm-seq
                                              (nth b-vec index))))))
        shallow-flatten-fn
        (fn [b-vec o] (if (instance? Addition o)
                        (conj (into b-vec (.-elm-seq o)) (.-o o))
                        (conj b-vec o)))]
    (as-> a b
          (reduce #(assoc %1 %2 delete-symbol) b dels)
          ;; inside adds, each index must be contained only once
          (reduce add-fn b adds)
          (reduce shallow-flatten-fn [] b))))

(defn- patch*
  "Returns transformed sequence s according to edit script."
  [a edit-script]
  (let [deletion (js-obj)]
    (remove #(identical? % deletion)
            (merge-patch a edit-script deletion))))

(defprotocol IPatch
  (-patch [a edit-script]
    "Returns transformed sequence from sequence a according to
    edit script."))

(extend-type default
  IPatch
  (-patch [a edit-script] (patch* a edit-script)))

(extend-type string
  IPatch
  (-patch [a edit-script] (us/cat-seq (patch* a edit-script))))

;;;; Public API

(defn diff
  "Create the edit script for transforming sequence a into sequence b.
  An edit script is a vector of [additions deletions].
  Additions are represented as a sequence of vectors.
  The first item in each vector is the index before which the rest of
  the items in the vector are to be inserted. For example [3 b c]
  means to insert b an c before whatever is in index 3. Deletions are
  represented as a sequence of indexes to delete.
  Returns [adds dels] representing edit script.

  For example: the diff of 'abcabba' and 'cbabac' would generate the
  edit script below.

      [[[3 b] [7 c]] [0 1 5]]

  An index of length of a may appear in additions and is a special
  case which means to add the elements at the end of the sequence."
  [a b]
  (miller/diff a b))

(defn patch
  "Use the instructions in the edit script to transform the sequence
  s into a new sequence. If the edit script was created by using diff
  on a and b then patch will use the edit script to transform a
  into b. Returns transformed sequence.

  (diff a b) -> x, (patch a x) -> b."  
  [a edit-script]
  (-patch a edit-script))

(defn edit-distance
  "Returns the edit distance between the two passed sequences. May
  also be passed an edit script. The edit distance is the minimum
  number of insertions and deletions required to transform one
  sequence into another."
  ([a b]
     (miller/edit-distance a b))
  ([edit-script]
     (let [[adds dels] ^ArrayVector edit-script]
       (apply + (count dels) (map (comp dec count) adds)))))

(defn levenshtein-distance
  "Returns the Levenshtein distance between two sequences. May either
  be passed the two sequences or a diff of the two sequences.

  From [Wikipedia](http://en.wikipedia.org/wiki/Levenshtein_distance):
  The Levenshtein distance between two strings is the minimum number
  of edits needed to transform one string into the other, with the
  allowable edit operations being insertion, deletion and substitution
  of a single character.

  This function works not only with strings but with any Clojure
  sequence.

  Warning! Technically this function is estimating the Levenshtein
  distance from a computed diff. Most of the time, it is the same as
  the real Levenshtein distance but in same cases it may be larger.
  The reason for this is that there may be multiple paths through an
  edit graph with the same edit distance but with differing
  Levenshtein distance. A future improvement to the diff algorithm
  whould be to find all paths and prefer the one with the
  minimum Levenshtein distance."
  ([a b]
     (levenshtein-distance (diff a b)))
  ([edit-script]
     (let [[adds dels] ^ArrayVector edit-script
           add-fn #(cons (first %) (map (constantly :a) (rest %)))
           adds (map add-fn adds)
           max-index (apply max 0 (concat (map first adds) dels))
           s (repeat max-index :e)]
       (->> (merge-patch s [adds dels] :d)
            (partition-by #(identical? % :e))
            (remove #(identical? (first %) :e))
            (map #(apply max (vals (frequencies %))))
            (reduce +)))))

(defn longest-common-subseq
  "Returns longest common subseq of a and b."
  [a b]
  (miller/longest-common-subseq a b))
