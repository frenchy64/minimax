(ns minimax.core
  (:use [clojure.test])
  (:require [clojure.pprint :refer [pprint]]))

;(defalias Move (U ':x ':o))

;(defalias State
;  (Vec (Vec (U Move ':empty))))

;(defalias Action
;  '{:x Int     ;; should be :row
;    :y Int     ;; should be :column
;    :move Move})

;(defalias Node
;  '{:state State
;    :player Move
;    :utility  Int
;    :alpha Int
;    :beta Int})

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
        _ (assert (vector? state) node)
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
                      :utility 0
                      :alpha Long/MIN_VALUE
                      :beta Long/MAX_VALUE})))

; (Map (U Move ':draw) Integer)
(def utilites {:x    1
               :draw 0
               :o    -1})

; Node -> Integer
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

; Node Action -> Node
(defn apply-action [node action]
  (-> node
      (assoc :action action)
      (update :player {:x :o, :o :x})
      (assoc-in [:state (:x action) (:y action)] (:move action))))

; Node -> '[Int (U nil Action)]
(defn max-or-min-value [node]
  {:post [(vector? %)]}
  (let [[infinity choice compare-choice best-move-key best-opponent-move-key]
        ({:x [Long/MIN_VALUE max >= :alpha :beta]
          :o [Long/MAX_VALUE min <= :beta  :alpha]}
         (:player node))]
    (assert (integer? infinity))
    (assert (ifn? choice))
    ;(prn node)
    (cond (terminal node) [(utility node) nil]
          :else (loop [;; - v      best utility so far for current player (defaults to the worst utility)
                       ;; - best   the next move that goes towards the best utility for the current player,
                       ;;          defaults to nil
                       [v best] [infinity nil]
                       ;; set of possible legal actions for the current player
                       ;; :- (U nil (Coll Action))
                       a (actions node)
                       ;; the best utility gained so far for the current player
                       ;; :- Integer
                       best-self-move (best-move-key node)] 
                  ;(pprint (:state node))
                  ;(prn "testing action" (first a))
                  ;(prn "best-self-move" best-self-move)
                  ;(prn "best-opponent-move-key" (best-opponent-move-key node))
                  ;(prn "player" (:player node))
                  (prn "moved" (some->> a first (apply-action node) :state))
                  (if (empty? a)
                    [v best]
                    ;; if we have found a move that is great for us, but means
                    ;; the opponent will have to choose a unoptimal move, then we
                    ;; can prune this choice.
                    (if (compare-choice v (best-opponent-move-key node))
                      [v best]
                      (recur (let [action (first a)
                                   _ (prn action)
                                   [n _] (max-or-min-value 
                                           (assoc (apply-action node action)
                                                  ;; the other stays the same
                                                  best-move-key best-self-move))
                                   c (choice v n)]
                               [c
                                ;; choose the action corresponding to the best choice.
                                ;; favour the new choice, as `best` could be nil.
                                ((into {} [[v best], [n action]])
                                 c)])
                             (next a)
                             (choice best-self-move v))))))))

(deftest minimax-test
  (is (= [1 nil]
         (minimax-decision [[:x :x :x]
                            [:x :x :x]
                            [:x :x :x]])))
  (is (= [-1 nil]
         (minimax-decision [[:o :x :o]
                            [:x :o :o]
                            [:x :o :o]])))
  (is (= [1 {:x 1
             :y 1
             :move :x}]
         (minimax-decision [[:o :x     :x]
                            [:x :empty :o]
                            [:x :o     :o]]
                           :x)))
  (is (= [-1 {:x 1
              :y 1
              :move :o}]
         (minimax-decision [[:o :x     :x]
                            [:x :empty :o]
                            [:x :o     :o]]
                           :o)))
  (is (= [1 {:x 0
             :y 2
             :move :x}]
         (minimax-decision [[:o :x     :empty]
                            [:x :x :o]
                            [:x :o     :o]]
                           :x)))
  (is (minimax-decision [[:x :empty :empty]
                         [:o :empty :o]
                         [:x :empty :empty]]))
  #_(is (minimax-decision [[:empty :empty :empty]
                         [:empty :empty :empty]
                         [:empty :empty :empty]])))

#_#_(terminal {:state [[:empty :empty :empty]
                   [:empty :empty :empty]
                   [:empty :empty :empty]]})

(actions {:state [[:empty :empty :empty]
                  [:empty :empty :empty]
                  [:empty :empty :empty]]
          :player :o})
