(ns katas.monty-hall.core)

(defn make-doors []
  {::doors
   (shuffle [::goat ::goat ::car])})

(defn remove-door
  [doors idx]
  (vec (concat (subvec doors 0 idx)
               (subvec doors (inc idx)))))

(defn pick-one
  [{:keys [::doors]}]
  (let [pick-idx (rand-int (count doors))]
    {::pick  (get doors pick-idx)
     ::doors (remove-door doors pick-idx)}))

(defn monty-shows-a-goat
  [{:keys [::doors] :as result}]
  (condp = (set doors)
    #{::goat}
    (assoc result
           ::doors [::goat])

    #{::car ::goat}
    (assoc result
           ::doors [::car])))

(defn count-cars
  [result]
  (->> result
       (map ::pick)
       (filter #(= % ::car))
       count))

(defn simulate []
  (let [attempts       1000
        ;; - if you stay, Monty doesn't need to show his goat
        stay-results   (count-cars
                        (for [_ (range 0 attempts)]
                          (-> (make-doors)
                              pick-one)))
        ;; - make your pick, Monty shows the goat, wisely switch
        switch-results (count-cars
                        (for [_ (range 0 attempts)]
                          (-> (make-doors)
                              pick-one
                              monty-shows-a-goat
                              pick-one)))]
    {:stay   {false switch-results
              true  stay-results}
     :switch {true  switch-results
              false stay-results} }))
