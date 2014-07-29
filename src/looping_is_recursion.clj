(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (= n 0)
                   acc
                   (recur (* base acc) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [h-seq]
                   (if (empty? (rest h-seq))
                     (first h-seq)
                     (recur (rest h-seq))))]
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [h-seq1 h-seq2] (cond
                                    (and (empty? h-seq1) (empty? h-seq2)) true
                                    (or (empty? h-seq1) (empty? h-seq2)) false
                                    (not (= (first h-seq1) (first h-seq2))) false
                                    :else (recur (rest h-seq1) (rest h-seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         h-seq a-seq]
    (cond (empty? h-seq) nil
          (pred (first h-seq)) index
          :else (recur (inc index) (rest h-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (let [[sum n] (loop [h-seq a-seq
                         l-sum 0
                         l-n 0]
                    (if (empty? h-seq)
                      [l-sum l-n]
                      (recur (rest h-seq) (+ l-sum (first h-seq)) (inc l-n))))]
      (/ sum n))))

(defn parity [a-seq]
  (let [freq-a-sec (frequencies a-seq)
        val-odd? (fn [x] (odd? (second x)))]
    (keys (filter val-odd? freq-a-sec))))

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [f1 0
           f2 1
           nth-fib 1]
      (if (= nth-fib n)
        f2
        (recur f2 (+ f1 f2) (inc nth-fib))))))


(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    []
    (loop [seen #{}
           acc []
           h-seq a-seq]
      (cond
       (empty? h-seq) acc
       (seen (first h-seq)) acc
       :else (recur (conj seen (first h-seq))
                    (conj acc (first h-seq))
                    (rest h-seq))))))


