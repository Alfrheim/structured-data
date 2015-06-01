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

(defn square? [rectangle]
  (let [size1 (width rectangle)
        size2 (height rectangle)]
    (if (= size1 size2)
      true
      false)))

(defn area [rectangle]
  (let [x (width rectangle)
        y (height rectangle)]
    (* x y)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [coordx coordy] point]
    (if (and (>= x2 coordx x1) (>= y2 coordy y1))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[pointx pointy]  inner]
    (if (and (contains-point? outer pointx)
             (contains-point? outer pointy))
      true
      false)))

(defn title-length [book]
  (count (get book :title)))

(defn author-count [book]
  (count (get book :authors)))

(defn multiple-authors? [book]
  (let [result (author-count book)]
    (if (> result 1)
      true
      false)))

(defn add-author [book new-author]
  (let [old (get book :authors)
        new (conj old new-author)]
  (assoc book :authors new)))

(defn alive? [author]
  (let [result (get author :death-year)]
  (if (nil? result)
    true
    false)))

(defn element-lengths [collection]
  :-)

(defn second-elements [collection]
  :-)

(defn titles [books]
  :-)

(defn monotonic? [a-seq]
  :-)

(defn stars [n]
  :-)

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
