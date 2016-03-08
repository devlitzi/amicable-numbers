;;;PPL Lab 2
;;Zach Litzinger & Jake Mitchel
(require '[clojure.core.reducers :as r])

(defn add-factors [n]
  (- (r/fold + (into (sorted-set)
    (r/mapcat (fn [x] [x (/ n x)])
      (r/filter #(zero? (rem n %)) (range 1 (inc (Math/sqrt n))))))) n))

(defn has-amicable-pair? [number]
  (let [pair (add-factors number)]
  (and
    (= (add-factors pair) number)
    (not= pair number)
    (> pair number))))


(defn find-amicable-numbers [n]
      (r/foldcat (r/take n (r/filter has-amicable-pair? (range)))))

;;(find-amicable-numbers 5)

(defn find-amicable-pairs [n]
  (let [x (find-amicable-numbers n)]
    (map vector x (map add-factors x))))

(time (find-amicable-pairs 20))
