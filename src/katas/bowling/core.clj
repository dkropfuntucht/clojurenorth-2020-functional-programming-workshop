(ns katas.bowling.core)

(defn characterize-frames
  [rolls]
  (let [first-pass (reduce
                    (fn [{:keys [current frame frames in-frame-count]} roll]
                      (cond (= roll 10)
                            {:current        0
                             :frame          (inc frame)
                             :frames         (conj frames {:rolls       1
                                                           :type        :strike
                                                           :frame-num   (inc frame)
                                                           :local-score 10})
                             :in-frame-count 0}

                            (= (+ roll current) 10)
                            {:current       0
                             :frame         (inc frame)
                             :frames        (conj frames {:rolls       2
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
                             :frames         (conj frames {:rolls       2
                                                           :type        :whatever-you-call-this
                                                           :frame-num   (inc frame)
                                                           :local-score (+ roll current)})
                             :in-frame-count 0}))
                    {:current        0
                     :in-frame-count 0
                     :frame          0
                     :frames         []}
                    rolls)]
    (if (zero? (:current first-pass))
      (:frames first-pass)
      (conj (:frames first-pass)
            {:rolls       1
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
     {:total-score (condp = type

                     :spare
                     (+ total-score
                        local-score
                        (:local-score (look-forward rolls frame-num 1)))

                     :strike
                     (+ total-score
                        local-score
                        (:local-score (look-forward rolls frame-num 1))
                        (:local-score (look-forward rolls frame-num 2)))

                     (+ total-score local-score))})
   {:total-score 0}
   rolls))

(defn total-score
  [rolls]
  (-> rolls
      characterize-frames
      score
      :total-score))
