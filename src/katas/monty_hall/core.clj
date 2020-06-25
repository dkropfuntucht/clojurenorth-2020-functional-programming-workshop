(ns katas.monty-hall.core)

(defn make-doors []
  (shuffle [::goat ::goat ::car]))

(defn remove-door
  [doors idx]
  (vec (concat (subvec doors 0 idx)
               (subvec doors (inc idx)))))

(defn pick-one
  [doors]
  (let [pick-idx (rand-int (count doors))]
    {::pick  (get doors pick-idx)
     ::doors (remove-door doors pick-idx)}))

(defn pick-subsequent
  [result]
  (if (= (::pick result) ::car)
    result
    (pick-one (::doors result))))

(defn count-cars
  [result]
  (->> result
       (map ::pick)
       (filter #(= % ::car))
       count))

(defn simulate []
  (let [attempts       1000
        stay-results   (count-cars
                        (for [_ (range 0 attempts)]
                          (-> (make-doors)
                              pick-one)))
        switch-results (count-cars
                        (for [_ (range 0 attempts)]
                          (-> (make-doors)
                              pick-one
                              pick-subsequent)))]
    {:stay   {false switch-results
              true  stay-results}
     :switch {true  switch-results
              false stay-results} }))
