(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [b p n]
                       (cond
                         (zero? p) 1
                         (== p 1) b
                         :else (recur (* b n) (dec p) n)))]
    (power-helper base exp base)))

(defn last-element [a-seq]
  (if(empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (not= (count seq1) (count seq2)) false
    :else (every? true? (map = seq1 seq2))))

(defn find-first-index [pred a-seq]
  (let [helper (fn[n p c]
                 (cond
                   (empty? c) nil
                   (p (first c)) n
                   :else (recur (inc n) p (rest c))))]
    (helper 0 pred a-seq)))

(defn avg [a-seq]
  (let [avg-helper (fn[s n c]
                     (if(empty? c)
                     (/ s n)
                     (recur (+ s (first c)) (inc n) (rest c))))]
    (avg-helper 0 0 a-seq)))

(defn parity [a-seq]
  (into #{} (map (fn[[x n]] x)(filter (fn[[x n]] (odd? n)) (frequencies a-seq)))))

(defn fast-fibo [n]
  (loop [n1 0 n2 1 iter 0]
    (if(= n iter)
      n1
      (recur n2 (+ n1 n2) (inc iter)))))

(defn cut-at-repetition [a-seq]
  (loop[[head & remaining :as tail] a-seq, coll #{}]
    (if (not (boolean head))
      (seq coll)
      (if (coll head)
        (seq coll)
        (recur (rest tail) (conj coll head))))))
