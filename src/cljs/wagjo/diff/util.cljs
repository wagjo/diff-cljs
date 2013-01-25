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

(ns wagjo.diff.util
  "Diff utils.")

;;;; Public API

(defn shift-left
  "Returns deconstructed edit script with adds and dels shifted
  to the left whenever possible. Deconstructed edit script can have
  multiple adds pointing to the same position."
  [edit-script a]
  (let [a (if (string? a) a (vec a))
        [adds dels] ^ArrayVector edit-script
        deconstruct-fn
        (fn [add] (map #(vector (first add) %) (rest add)))
        adds (mapcat deconstruct-fn adds)
        compute-fn #(loop [i (dec %1)]
                      (if (and (not (neg? i))
                               (identical? (nth a i) %2))
                        (recur (dec i))
                        (inc i)))]
    [(map #(assoc % 0 (compute-fn (nth % 0) (nth % 1))) adds)
     (map #(compute-fn % (nth a %)) dels)]))
