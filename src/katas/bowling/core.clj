(ns katas.bowling.core)

;; - presumably, we could write a routine to go from one of these
;; back to a vector of rolls
(defn characterize-frames
  [rolls]
  (let [first-pass (reduce
                    (fn [{:keys [current frame frames in-frame-count]} roll]
                      (cond (= roll 10)
                            {:current        0
                             :frame          (inc frame)
                             :frames         (conj frames
                                                   {:roll-count  1
                                                    :rolls       [10]
                                                    :type        :strike
                                                    :frame-num   (inc frame)
                                                    :local-score 10})
                             :in-frame-count 0}

                            (= (+ roll current) 10)
                            {:current       0
                             :frame         (inc frame)
                             :frames        (conj frames
                                                  {:roll-count  2
                                                   :rolls       [current roll]
                                                   :type        :spare
                                                   :frame-num   (inc frame)
                                                   :local-score 10})
                             :in-frame-count 0}

                            (= in-frame-count 0)
                            {:current        roll
                             :frame          frame
                             :frames         frames
                             :in-frame-count 1}

                            :else
                            {:current        0
                             :frame          (inc frame)
                             :frames         (conj frames
                                                   {:roll-count  2
                                                    :type        :just-a-frame
                                                    :rolls       [current roll]
                                                    :frame-num   (inc frame)
                                                    :local-score
                                                    (+ roll current)})
                             :in-frame-count 0}))
                    {:current        0
                     :in-frame-count 0
                     :frame          0
                     :frames         []}
                    rolls)]
    (if (zero? (:current first-pass))
      (:frames first-pass)
      (conj (:frames first-pass)
            {:roll-count  1
             :rolls       [(:current first-pass)]
             :type        :bonus-roll
             :frame-num   (inc (:frame first-pass))
             :local-score (:current first-pass)}))))

(defn look-forward
  [rolls frame-num step]
  (let [target-idx (+ (dec frame-num) step)]
    (if (> target-idx (dec (count rolls)))
      {:type :not-there
       :local-score 0}
      (nth rolls target-idx))))

(defn score
  [rolls]
  (reduce
   (fn [{:keys [total-score]} {:keys [type frame-num local-score] :as roll}]
     {:total-score (cond (> frame-num 10)
                         total-score

                         (= type :spare)
                         (+ total-score
                            local-score
                            (get-in (look-forward rolls frame-num 1)
                                    [:rolls 0]))


                         (= type :bonus-roll)
                         total-score

                         (= type :strike)
                         (+ total-score
                            local-score
                            (get-in (look-forward rolls frame-num 1)
                                    [:rolls 0] 0)
                            (get-in (look-forward rolls frame-num 1)
                                    [:rolls 1] 0))

                         :else
                         (+ total-score local-score))})
   {:total-score 0}
   rolls))

(defn total-score
  [rolls]
  (-> rolls
      characterize-frames
      score
      :total-score))
