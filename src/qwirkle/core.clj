;; Clojure implementation of the table-top tile-laying game [Qwirkle][1].
;;
;; Because Qwirkle is awesome.
;;
;; [1]:http://www.mindware.com/qwirkle-a2-32016.fltr
(ns qwirkle.core
  (:require [clojure.set :as set]))

(def all-colors
  (set [:blue :green :orange :purple :red :yellow]))

(def all-shapes
  (set [:circle :clover :crisscross :diamond :square :starburst]))

(defn tile
  [color shape]
  (if (and (all-colors color)
           (all-shapes shape))
    {:color color
     :shape shape}))

(defn color [{c :color}] c)

(defn shape [{s :shape}] s)

;; TODO: doesn't really need to be a set; a seq is fine
(defn tile-permutation
  "Given a selection of `colors` and `shapes`, create all permutations
  of tiles with those characteristics"
  [colors shapes]
  (set (for [c colors s shapes] (tile c s))))

(def all-tiles
  (tile-permutation all-colors all-shapes))

;; Need a true bag semantic; order is not relevant, and there should
;; be multiple copies of tiles
;; Invariant: 108 tiles total, 3 of each color / shape combo
(defn bag
  "Create a full bag of tiles. There are 3 copies of each color/shape
  combination."
  []
  (shuffle (apply concat (repeat 3 all-tiles))))

(defn deal-hand
  "Withdraw a hand of `count` tiles (1--6) from `bag`. Returns a vector
  of `[new-hand new-bag]`, where `new-bag` is the original `bag` with
  the contents of `new-hand` removed."
  [bag count]
  (split-at count bag))

;; Invariant: new tiles + new bag == original tiles + original bag
;; (proper bag semantics)
(defn replace-tiles
  ""
  [bag old-tiles]
  (let [[new-tiles new-bag] (deal-hand bag (count old-tiles))]
    [new-tiles (shuffle (concat new-bag old-tiles))]))

(defn- common-axis?
  [attr-fn tiles]
  (let [vals (set (map attr-fn tiles))]
    (= (count vals) 1)))

(def common-color?
  (partial common-axis? color))

(def common-shape?
  (partial common-axis? shape))

(defn- ensure-set [x]
  (set (if (sequential? x) x [x])))

(defn other-colors [color-or-colors]
  (set/difference all-colors (ensure-set color-or-colors)))

(defn other-shapes [shape-or-shapes]
  (set/difference all-shapes (ensure-set shape-or-shapes)))

(defn allowed
  [tiles]
  (let [common-color (common-color? tiles)
        common-shape (common-shape? tiles)
        colors (map color tiles)
        shapes (map shape tiles)
        common-color-tiles (tile-permutation colors (other-shapes shapes))
        common-shape-tiles (tile-permutation (other-colors colors) shapes)]
    (cond
     (and common-color common-shape) (set/union common-color-tiles common-shape-tiles)
     common-color common-color-tiles
     common-shape common-shape-tiles
     :else #{})))

(defn north [[x y]] [x (inc y)])

(defn east  [[x y]] [(inc x) y])

(defn south [[x y]] [x (dec y)])

(defn west  [[x y]] [(dec x) y])

(defn board []
  {:open-positions {[0 0] all-tiles}
   :taken-positions {}})

;; TODO: Hmm... might not truly be needed?
(defn open-positions  [{open  :open-positions}] open)

(defn taken-positions [{taken :taken-positions}] taken)

(defn tile-at
  "If there's a tile at `position`, return it; otherwise `nil`."
  [board position]
  (get (taken-positions board) position))

(defn tile-allowed-at?
  "Returns true if `tile` is allowed to be placed at `position` on `board`"
  [board tile position]
  (not (nil? (get-in (open-positions board) [position tile]))))

(defn close-position
  [board position]
  (update-in board [:open-positions] #(dissoc % position)))

(defn update-allowed
  [board position placed-tiles]
  (let [new-allowed (allowed placed-tiles)]
    (update-in board
               [:open-positions position]
               #(if (nil? %)
                  new-allowed
                  (set/intersection % new-allowed)))))

(defn take-position
  "Set the tile at `position` to `tile`, unless a tile already exists there.
  Returns a possibly-updated `board`"
  [board tile position]
  (update-in board
             [:taken-positions position]
             (fnil identity tile)))

(defn- nearest-open
  "Finds the nearest open position from `position` traversing placed tiles along
  `direction`.
  Assumes that `position` is a placed tile."
  [board position direction]
  (let [new-position (direction position)]
    (if (tile-at board new-position)
      (nearest-open board new-position direction)
      new-position)))

(defn traverse-tiles
  [board position direction]
  (loop [p position
         acc #{}]
    (let [new-position (direction p)]
      (if-let [tile (tile-at board new-position)]
        (recur new-position (conj acc tile))
        acc))))

(defn- tiles-in-line
  [board position dir1 dir2]
  (set/union (traverse-tiles board position dir1)
             #{(tile-at board position)}
             (traverse-tiles board position dir2)))

(defn- update
  [board position dir1 dir2]
  (let [placed-tiles (tiles-in-line board position dir1 dir2)
        start-open (nearest-open board position dir1)
        end-open (nearest-open board position dir2)]
    (-> board
        (update-allowed start-open placed-tiles)
        (update-allowed end-open placed-tiles))))

(defn- update-board
  [board position]
  (-> board
      (update position north south)
      (update position east west)))

(defn place-tile
  [board tile position]
  (if (tile-allowed-at? board tile position)
    (-> board
        (take-position tile position)
        (close-position position)
        (update-board position))
    board))
