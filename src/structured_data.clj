(ns structured-data)

(defn do-a-thing [x]
  (let
    [sum (+ x x)]
    (Math/pow sum sum)))

(defn spiff [v]
  (let [val1 (get v 0)
        val2 (get v 2)]
    (+ val1 val2))
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[val1 x val2] v]
    (+ val1 val2)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))
;; Exercice 6
(defn square? [rectangle]
  (let [size1 (width rectangle)
        size2 (height rectangle)]
    (if (= size1 size2)
      true
      false)))
;; Exercice 7
(defn area [rectangle]
  (let [x (width rectangle)
        y (height rectangle)]
    (* x y)))
;; Exercice 8
(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [coordx coordy] point]
    (if (and (>= x2 coordx x1) (>= y2 coordy y1))
      true
      false)))
;; Exercice 9
(defn contains-rectangle? [outer inner]
  (let [[pointx pointy]  inner]
    (if (and (contains-point? outer pointx)
             (contains-point? outer pointy))
      true
      false)))
;; Exercice 10
(defn title-length [book]
  (count (get book :title)))
;; Exercice 11
(defn author-count [book]
  (count (get book :authors)))
;; Exercice 12
(defn multiple-authors? [book]
  (let [result (author-count book)]
    (if (> result 1)
      true
      false)))
;; Exercice 13
(defn add-author [book new-author]
  (let [old (get book :authors)
        new (conj old new-author)]
  (assoc book :authors new)))
;; Exercice 14
(defn alive? [author]
  (let [result (get author :death-year)]
  (if (nil? result)
    true
    false)))

;; Exercice 15
(defn element-lengths [collection]
  (let [countElements (fn [x] (count x))]
    (map countElements collection)))

;; Exercice 16
(defn second-elements [collection]
  (let [element (fn [x] (second x))]
    (map element collection)))

;; Exercice 17
(defn titles [books]
  (map :title books))

;; Exercice 18
(defn monotonic? [a-seq]
  (let [increase (apply <= a-seq)
        decrease (apply >= a-seq)]
    (if increase
      increase
      decrease)))

;; Exercice 18
(defn stars [n]
  (apply str (repeat n "*")))

;; Exercice 19
(defn mononic?
  [a-seq]
  (apply <= a-seq))

;; Exercice 20
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

;; Exercice 21
(defn contains-duplicates? [a-seq]
  (let [setSize (count(set a-seq))
        seqSize (count a-seq)]
    (not (= setSize seqSize))))

;; Exercice 22
(defn old-book->new-book
  [book]
  (let [authors (set (book :authors))]
    (assoc book :authors authors)))

(defn has-author? [book author]
  (contains? (book :authors) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

;;(defn all-author-names [books]
;;  (let [author-names
;;        (fn [book] (map :name (:authors book)))]
;;    (set (apply concat (map author-names books)))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (author :name)
        year (str " (" (author :birth-year) " - " (author :death-year) ")")]
    (str name (when (author :birth-year)
                  year))))

;; Exercice 27
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

;; Exercice 28
(defn book->string [book]
  (let [name (book :title)
        author (authors->string (book :authors))]
    (str name ", written by " author)))

;; Exercice 29
(defn books->string [books]
  (let [total (count books)
        strbook (str total " book" (when (> total 1) "s"))
        stringbooks (apply str (interpose ". "  (map book->string books)))]
    (if (= 0 total)
      "No books."
      (str (apply str (interpose ". " [strbook stringbooks])) "."))))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

;; Exercice 31
(defn author-by-name [name authors]
  (first (filter (fn [author] (= (author :name) name)) authors)))

;; Exercice 32
(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

;; Exercice 33
(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))

;; Exercice 34
(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))

; %________%
