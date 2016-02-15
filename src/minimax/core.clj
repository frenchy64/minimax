(ns minimax.core
  (:use [clojure.test]))

;(defalias Move (U ':x ':o))

;(defalias State
;  (Vec (Vec (U Move ':empty))))

;(defalias Action
;  '{:x Int
;    :y Int
;    :move Move})

;(defalias Node
;  '{:state State
;    :player Move
;    :utility  Int})

; (Set Move)
(def moves #{:x :o})

; State -> (U nil Move ':draw)
(defn all-moves [s]
  {:pre [(set? s)]
   :post [((some-fn keyword? nil?) %)]}
  (when (or (= #{:x} s)
            (= #{:o} s))
    (first s)))

; Node -> (Set Action)
(defn actions [node]
  (let [state (:state node)
        player (:player node)]
    (set
      (for [i (range (count state))
            j (range (count (first state)))
            :when (#{:empty} ((state i) j))]
        {:x i
         :y j
         :move player}))))

; Node -> (U nil Move ':draw)
(defn terminal [node]
  {:post [((some-fn keyword? nil?) %)]}
  (let [rows (:state node)
        nrow (count rows)
        ncol (count (first rows))
        res (or ; check each row
                (some (fn [row-num]
                        (let [rs (into #{} (get rows row-num))]
                          (all-moves rs)))
                      (range (count rows)))
                ; check each column
                (some (fn [col-num]
                        (let [cs (into #{} 
                                       (for [i (range (count rows))]
                                         ((rows i) col-num)))]
                          (all-moves cs)))
                      (range (count (first rows))))
                ; check each diagonal
                (or (all-moves
                      (set [((rows 0) 0)
                            ((rows 1) 1)
                            ((rows 2) 2)]))
                    (all-moves
                      (set [((rows 0) 2)
                            ((rows 1) 1)
                            ((rows 2) 0)])))
                ; every position taken
                (when (every? (fn [row]
                                (every? (complement #{:empty}) row))
                              rows)
                  :draw))]
    res))

(deftest actions-test
  (is (= #{{:x 0 :y 0 :move :x}
           {:x 0 :y 1 :move :x}
           {:x 0 :y 2 :move :x}
           {:x 1 :y 0 :move :x}
           {:x 1 :y 1 :move :x}
           {:x 1 :y 2 :move :x}
           {:x 2 :y 0 :move :x}
           {:x 2 :y 1 :move :x}
           {:x 2 :y 2 :move :x}}
         (actions {:state [[:empty :empty :empty]
                           [:empty :empty :empty]
                           [:empty :empty :empty]]
                   :player :x})))
  (is (= #{{:x 0 :y 1 :move :o}
           {:x 0 :y 2 :move :o}
           {:x 1 :y 0 :move :o}
           {:x 1 :y 1 :move :o}
           {:x 1 :y 2 :move :o}
           {:x 2 :y 0 :move :o}
           {:x 2 :y 1 :move :o}
           {:x 2 :y 2 :move :o}}
         (actions {:state [[:x     :empty :empty]
                           [:empty :empty :empty]
                           [:empty :empty :empty]]
                   :player :o}))))

(deftest terminal?-test
  (is (= nil
         (terminal {:state [[:empty :empty :empty]
                            [:empty :empty :empty]
                            [:empty :empty :empty]]})))
  (is (= :draw
         (terminal {:state [[:o :o :x]
                            [:x :x :o]
                            [:o :o :x]]})))
  ; row
  (is (= :x
         (terminal {:state [[:x :x :x]
                             [:empty :empty :empty]
                             [:empty :empty :empty]]})))
  (is (= :o
         (terminal {:state [[:o :o :o]
                            [:empty :empty :empty]
                            [:empty :empty :empty]]})))
  (is (= :o
         (terminal {:state [[:o :empty :empty]
                            [:o :empty :empty]
                            [:o :empty :empty]]})))
  (is (= :x
         (terminal {:state [[:x :empty :empty]
                             [:x :empty :empty]
                             [:x :empty :empty]]})))
  (is (= :o
         (terminal {:state [[:o     :empty :empty]
                            [:empty :o     :empty]
                            [:empty :empty :o]]})))
  (is (= :x
         (terminal {:state [[:empty :empty :x]
                            [:empty :x     :empty]
                            [:x :empty     :empty]]}))))



(declare max-or-min-value)

; State -> Action
(defn minimax-decision 
  ([state] (minimax-decision state :o))
  ([state player]
   (max-or-min-value {:state state
                      :action nil
                      :player player
                      :utility 0})))

(def utilites {:x    1
               :draw 0
               :o    -1})

(defn utility [node]
  {:post [(integer? %)]}
  (let [i (utilites (terminal node))]
    (assert (integer? i) "Cannot get utility of non-terminal node")
    i))

(deftest utility-test
  (is (thrown? Error
         (utility {:state [[:empty :empty :empty]
                           [:empty :empty :empty]
                           [:empty :empty :empty]]})))
  (is (= 0
         (utility {:state [[:o :o :x]
                           [:x :x :o]
                           [:o :o :x]]})))
  ; row
  (is (= 1
         (utility {:state [[:x :x :x]
                           [:empty :empty :empty]
                           [:empty :empty :empty]]})))
  (is (= -1
         (utility {:state [[:o :o :o]
                           [:empty :empty :empty]
                           [:empty :empty :empty]]})))
  (is (= -1
         (utility {:state [[:o :empty :empty]
                           [:o :empty :empty]
                           [:o :empty :empty]]})))
  (is (= 1
         (utility {:state [[:x :empty :empty]
                           [:x :empty :empty]
                           [:x :empty :empty]]})))
  (is (= -1
         (utility {:state [[:o     :empty :empty]
                           [:empty :o     :empty]
                           [:empty :empty :o]]})))
  (is (= 1
         (utility {:state [[:empty :empty :x]
                           [:empty :x     :empty]
                           [:x :empty     :empty]]}))))


(defn apply-action [node action]
  (-> node
      (assoc :action action)
      (update :player {:x :o
                       :o :x})
      (update-in [:state (:x action) (:y action)] (:move action))))

; Node -> '[Int (U nil Action)]
(defn max-or-min-value [node]
  (let [[infinity choice] ({:x [Long/MAX_VALUE max]
                            :o [Long/MIN_VALUE min]}
                           (:player node))]
    (cond (terminal node) [(utility node) nil]
          :else (loop [[v best] [infinity nil]
                       a (actions (:state node))]
                  (if (empty? a)
                    [v best]
                    (recur (let [action (first a)
                                 n (max-or-min-value (apply-action node action))
                                 c (choice v n)]
                             [c
                              ({v best
                                n action}
                               c)])
                           (next a)))))))

(deftest minimax-test
  (is (minimax-decision [[:empty :empty :empty]
                         [:empty :empty :empty]
                         [:empty :empty :empty]])))
