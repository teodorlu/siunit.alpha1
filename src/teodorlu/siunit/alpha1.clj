(ns teodorlu.siunit.alpha1
  (:refer-clojure :exclude [+ * / -]))

;; Scope for now:
;;
;; 1. Provide a #siunit reader literal
;; 2. Define operations on a WithUnit type
;;
;; Out of scope for now:
;;
;; 1. Derived units
;; 2. Sicmutils integration
;; 3. Better equality

(defrecord WithUnit [quantity unit])

(def ^:private zero (WithUnit. 0 {}))
(def ^:private one (WithUnit. 1 {}))

(defn ^:private coerce [x]
  (cond (instance? WithUnit x) x
        (number? x) (WithUnit. x {})
        (keyword? x) (WithUnit. 1 {x 1})
        (map x) (WithUnit. 1 x)))

(defn ^:private canonicalize
  "Ensure unit maps can be compared for equality"
  [x]
  (WithUnit. (:quantity x)
             (into {} (for [[k v] (:unit x)
                            :when (not (zero? v))]
                        [k v]))))

(defn unit= [a b]
  (let [a (canonicalize a)
        b (canonicalize b)]
    (and (= (:quantity a)
            (:quantity b))
         (= (:unit a)
            (:unit b)))))

(defn * [& args]
  (reduce (fn [a b]
            (WithUnit. (clojure.core/* (:quantity a) (:quantity b))
                       (merge-with clojure.core/+ (:unit a) (:unit b))))
          one
          (map coerce args)))

(* 55 {:m 2})
(map coerce [55 {:m 2}])

(defn + [x & args]
  (reduce (fn [a b]
            (assert (= (:unit a) (:unit b))) ; not sure if we want to throw or return nil
            (WithUnit. (clojure.core/+ (:quantity a) (:quantity b))
                       (:unit a)))
          x
          (map (comp canonicalize coerce) args)))

(comment
  ;; 12 kNm
  (WithUnit. 12000 {:N 1 :m 1})

  (coerce 12)
  (coerce
   (WithUnit. 12000 {:m 1}))

  (*
   (WithUnit. 12000 {:m 1})
   (WithUnit. 12000 {:m 1}))
  ;; => {:quantity 144000000, :unit {:m 2}}

  (+
   (WithUnit. 12000 {:m 1})
   (WithUnit. 12000 {:m 1}))
  ;; => {:quantity 24000, :unit {:m 1}}

  )

(defn create-siunit [form]
  (cond (vector? form) (reduce * form)
        :else (coerce form))
  ;; I should perhaps crash here.
  )

(comment
  [#siunit [ 12 ]]

  (create-siunit [1 2 3])
  ;; => {:quantity 6, :unit {}}

  (create-siunit [55 {:m 2}])
  ;; => {:quantity 55, :unit {:m 2}}

  (create-siunit {:m 2})
  ;; => {:quantity 1, :unit {:m 2}}

  (let [x #siunit [33 :m]]
    (* x x))

  )

(defmethod print-method WithUnit [x ^java.io.Writer w]
  (.write w (str "#siunit [" (:quantity x) " " (:unit x) "]")))

(let [x #siunit [33 :m]]
  (* x x))
