(ns p-p-p-pokerface)

(def high-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn high-card? [hand]
  true)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get high-ranks r))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(1 2 2)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        range? (fn [r] (= r (range (first r) (+ (first r) 5))))]
    (if (range? ranks)
      true
      (if (= (apply max ranks))
        (let [ranks-low (sort (replace {14 1} ranks))]
          (range? ranks-low))
        false))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matched-hand (fn [checker] ((first checker) hand))]
    (apply max (map second (filter matched-hand checkers)))))
