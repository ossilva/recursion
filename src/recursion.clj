(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [max-rest (max-element (rest a-seq))
          first-e (first a-seq)]
      (if (singleton? a-seq)
        (first a-seq)
        (max max-rest first-e)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    (lazy-seq)
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (== (first a-seq) elem) (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [first-seq (first a-seq)]
    (if (or (empty? a-seq) (not (pred? first-seq)))
      (lazy-seq)
      (cons first-seq (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    (lazy-seq)
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) (lazy-seq)
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (product (repeat k n)))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    (lazy-seq)
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    (lazy-seq)
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons (lazy-seq) (lazy-seq))
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons (lazy-seq) (lazy-seq))
    (drop-last (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [updated-freqs (if (contains? (into #{} (keys freqs)) (first a-seq))
                          (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                          (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper updated-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [return-seq a-seq]
  (if (empty? a-seq)
    return-seq
    (let [first-entry (first a-seq)]
      (let [updated-return-seq
            (concat return-seq (repeat (second first-entry) (first first-entry)))]
        (un-frequencies-helper updated-return-seq (rest a-seq))))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    (lazy-seq)
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (Math/floor (/ (count a-seq) 2))]
    (lazy-seq [(my-take n a-seq) (my-drop n a-seq)])))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
      (nil? first-a) b-seq
      (nil? first-b) a-seq
      (>= first-a first-b) (cons first-b (seq-merge a-seq (rest b-seq)))
      (> first-b first-a) (cons first-a (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [count-seq (count a-seq)]
      (if (contains? #{0 1} count-seq)
        a-seq
        (conj (apply seq-merge (map merge-sort (halve a-seq))))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

