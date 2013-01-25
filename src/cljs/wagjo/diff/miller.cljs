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

(ns wagjo.diff.miller
  "Algorithm from 'An O(NP) Sequence Comparison Algorithm' by
   Sun Wu, Udi Manber, Gene Myers and Web Miller.

   Please refer to the above paper while reading this code."
  (:require [wagjo.data.array :as ua]
            [wagjo.data.string :as us]
            [wagjo.diff.optimizations :as opt]))

;;;; Implementation details

;;; FP

(defn- furthest-k-point
  "Returns furthest point in diagonal k for a given FP(p)."
  [fp k]
  (if (nil? fp)
    -1
    (let [[pos-fp neg-fp] ^ArrayVector fp]
      (if (neg? k)
        (ua/nth* neg-fp (- -1 k) -1)
        (ua/nth* pos-fp k -1)))))

(defn- set-furthest-k-point!
  "Returns furthest points with y set as a furthest point
  in diagonal k."
  [fp k y]
  (let [[pos-fp neg-fp] ^ArrayVector fp]
    (if (neg? k)
      (ua/assoc! neg-fp (- -1 k) y)
      (ua/assoc! pos-fp k y))
    fp))

(defn- furthest-points
  "Returns furthest points from fp array for a given distance p."
  [fp-arr p]
  (ua/nth* fp-arr p nil))

(defn- empty-furthest-points
  "Returns new empty furthest points for a band p."
  [delta p]
  (let [fill #(ua/mape (fn [_] -1) %)]
    [(fill (make-array p))
     (fill (make-array (+ delta p 1)))]))

(defn- add-furthest-points!
  "Adds furthest points into fp array. Mutates fp-arr. Returns nil."
  [fp-arr fp]
  (ua/conj! fp-arr fp)
  nil)

;;; Snake

(defn- snake
  "Returns y coordinate of the furthest point on diagonal k by
  performing a snake. Starting at the starting point on diagonal k,
  return the y value of the point at the end of the longest snake on
  this diagonal. A snake is a sequence of diagonal moves connecting
  match points on the edit graph."
  [a b m n fp-left fp-top k]
  (let [;; find starting point for snake
        y (max (inc (furthest-k-point fp-left (dec k)))
               (furthest-k-point fp-top (inc k)))
        m* (+ m k)]
    (loop [y (max y (dec k))]
      (let [y* (inc y)]
        ;; loop while elements on diagonal match
        (if (and (< y m*) (< y n)
                 (identical? (nth a (- y* k)) (nth b y*)))
          (recur y*)
          y)))))

(defn- search-p-band
  "Given a p value, search all diagonals in the p-band for the
  furthest reaching endpoints. Record the furthest reaching endpoint
  for each p value in the map fp. Returns an updated fp map for p. a
  and b are the two sequences and m and n are their lengths
  respectively. delta is the diagonal of the sink and is equal to
  n - m."
  [a b m n delta p prev-fp]
  (let [fp (empty-furthest-points delta p)
        fp (reduce
            #(set-furthest-k-point! %1 %2
                                    (snake a b m n %1 prev-fp %2))
            fp (range (* -1 p) delta))
        fp (reduce
            #(set-furthest-k-point! %1 %2
                                    (snake a b m n prev-fp %1 %2))
            fp (reverse (range (inc delta) (+ delta (inc p)))))]
    (set-furthest-k-point! fp delta (snake a b m n fp fp delta))))

(defn- compute-ses*
  "Find the size of the shortest edit script (ses). Returns a 3-tuple
  of the size of the ses, the delta value (which is the diagonal of
  the sink) and the fp array. The optimal path from source to sink can
  be constructed from this information."
  [a b]
  (let [m (dec (count a))
        n (dec (count b))
        delta (- n m)
        fp-arr (array)]
    (loop [p 0
           prev-fp nil]
      (if-not (< (furthest-k-point prev-fp delta) n)
        [(dec p) delta fp-arr]
        (let [fp (search-p-band a b m n delta p prev-fp)]
          (add-furthest-points! fp-arr fp)
          (recur (inc p) fp))))))

(defn- compute-ses
  "Sort sequences and returns [ses smaller-seq larger-seq]."
  [a b]
  (let [switch? (> (count a) (count b))
        a* (if switch? b a)
        b* (if switch? a b)]
    [(compute-ses* a* b*) a* b*]))

;;; Edit script

(deftype Edit [type x p k d])

(defn- edit-dist
  "Given a delta, p and k value, calculate the edit distance.
  Edit distance is computed from V = (D - k) / 2 and definition
  of a compressed distance P. Returns calculated edit distance."
  [delta p k]
  (if (> k delta)
    (+ (* 2 (- p (- k delta))) k)
    (+ (* 2 p) k)))

(defn- look-up
  "Get information about the vertex above the one at y on k. If this
  vertex is chosen, it will represent a deletion.
  Returns delete Edit or nil."
  [graph delta p y k]
  (when (> (- y k) 0)
    (let [up-k (inc k)
          up-p (if (> up-k delta) p (dec p))
          y* (furthest-k-point (furthest-points graph up-p) up-k)]
      (when (and (>= y* 0) (== y y*))
        (Edit. :delete (- y* up-k) up-p up-k
               (edit-dist delta up-p up-k))))))

(defn- look-left
  "Get information about the vertex to the left of the one at y on k.
  If this vertex is chosen, it will represent an insertion.
  Returns insert Edit or nil."
  [graph delta p y k]
  (when (> y 0)
    (let [left-k (dec k)
          left-p (if (< left-k delta) p (dec p))
          y* (furthest-k-point (furthest-points graph left-p) left-k)]
      (when (and (>= y* 0) (== (dec y) y*))
        (Edit. :insert (- y* left-k) left-p left-k
               (edit-dist delta left-p left-k))))))

(defn- backtrack-snake
  "Find the y value at the head of the longest snake ending at
  (x, y). Returns y value."
  [a b x y]
  (loop [x x
         y y]
    (if (or (and (zero? x) (zero? y))
            (not (== (nth a x) (nth b y))))
      y
      (recur (dec x) (dec y)))))

;; See the paper for an example of how there are multiple shortest
;; paths through an edit graph.

(defn- next-edit
  "Find the next move through the edit graph which will decrease the
  edit distance by 1. Returns next move."
  [a b graph delta p x k]
  (let [d (edit-dist delta p k)
        y (+ x k)
        head-y (backtrack-snake a b x y)]
    (loop [head-y head-y]
      (let [move (or (when-let [r (look-left graph delta p head-y k)]
                       (when (== (.-d r) (dec d)) r))
                     (when-let [r (look-up graph delta p head-y k)]
                       (when (== (.-d r) (dec d)) r)))]
        (if (and (nil? move) (< head-y y))
          (recur (inc head-y))
          move)))))

(defn- compute-edit-script
  "Calculate the sequence of edits from the map of farthest reaching
  end points. Returns seq of edits."
  [ses a b]
  (let [[p delta graph] ^ArrayVector ses]
    (loop [edits '()
           prev (Edit. nil (dec (count a)) p delta
                       (edit-dist delta p delta))]
      (if (== (.-d prev) 0)
        edits
        (let [next (next-edit a b graph delta
                              (.-p prev) (.-x prev) (.-k prev))]
          (recur (conj edits next) next))))))

(defn- transpose
  "If b is shorter than a, then the diff is calculated from b to a
  and this function is used to transpose the results into a diff
  from a to b. Returns transposed Edit."
  [edit]
  (let [type (if (identical? :insert (.-type edit)) :delete :insert)
        x (+ (.-x edit) (.-k edit))
        k (- (.-k edit))]
    (Edit. type x (.-p edit) k (.-d edit))))

;;; Script tuple

(defn- script-tuple
  "Convert a sequence of edits into an edit script tuple.
  Returns [adds dels] representing edit script."
  [b edits]
  (let [edit-fn
        (fn [[adds dels] ^ArrayVector edit]
          (let [type (.-type edit)
                x (.-x edit)
                k (.-k edit)]
            (if (identical? type :delete)
              [adds (conj dels x)]
              (let [last-index (dec (count adds))
                    last-add (when-not (empty? adds)
                               (nth adds last-index))
                    el (nth b (inc (+ x k)))
                    adds (if (== x (first last-add))
                           (assoc adds last-index (conj last-add el))
                           (conj adds [x el]))]
                [adds dels]))))]
    (reduce edit-fn [[] []] edits)))

;;; Diff

(defn- vectorize
  "Returns s as a vector prepended with nil."
  [s]
  (vec (cons nil s)))

(defn- seq-diff
  "Performs diff. Returns [adds dels].
  If the passed values of a and b need to be swapped then the
  resulting path with will transposed."
  [a b]
  (log "classic diff")
  (let [a (vectorize a)
        b (vectorize b)
        [ses a* b*] ^ArrayVector (compute-ses a b)
        edit-script (compute-edit-script ses a* b*)
        edit-script (if (identical? a* a)
                      edit-script
                      (map transpose edit-script))]
    (script-tuple b edit-script)))

(defn- seq-lcs
  "Returns longest common subsequence of two sequences."
  [a b]
  (let [dels (set (nth (seq-diff a b) 1))]
    (keep-indexed #(when-not (dels %1) %2) (seq a))))

;;; Type dispatch

(defprotocol IDiff
  (-diff [a b]
    "Returns edit script tuple for transforming sequence a
    into sequence b. See doc string for clj-diff.core/diff.")
  (-longest-common-subseq [a b]
    "Returns longest common subseq of a and b.")
  (-edit-distance [a b]
    "Returns edit distance between a and b."))

(extend-type default
  IDiff
  (-diff [a b] (seq-diff a b))
  (-longest-common-subseq [a b] (seq-lcs a b)))

(extend-type string
  IDiff
  (-diff [a b] (opt/diff a b seq-diff))
  (-longest-common-subseq [a b] (us/cat-seq (seq-lcs a b))))

;;;; Public API

(defn diff
  "Returns edit script tuple for transforming sequence a
  into sequence a. See doc string for clj-diff.core/diff."
  [a b]
  (-diff a b))

(defn longest-common-subseq
  "Returns the longest common subseq of a and b."
  [a b]
  (-longest-common-subseq a b))

(defn edit-distance
  "Returns the edit distance between a and b."
  [a b]
  (let [a (vectorize a)
        b (vectorize b)
        [px a* b*] ^ArrayVector (compute-ses a b)
        [p] ^ArrayVector px]
    (+ (* 2 p) (- (count b*) (count a*)))))
