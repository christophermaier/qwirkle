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
  (shuffle
   (apply concat
          (repeat 3 all-tiles))))

;; Invariant: new hand + new bag == original bag  (proper bag semantics)
;; If a bag starts as shuffled, we (strictly speaking) don't need to
;; shuffle the bag again. Can't really hurt though. Probably depends on
;; the underlying implementation of the bag, though. HOWEVER, if we only
;; shuffle when we create the bag, then we can replay games, which could
;; be good for testing.
(defn deal-hand
  "Withdraw a hand of `count` tiles (1--6) from `bag`. Returns a vector
  of `[new-hand new-bag]`, where `new-bag` is the original `bag` with
  the contents of `new-hand` removed."
  [bag count]
  (split-at count
            (shuffle bag)))

;; Invariant: new tiles + new bag == original tiles + original bag
;; (proper bag semantics)
(defn replace-tiles
  ""
  [bag old-tiles]
  (let [[new-tiles new-bag] (deal-hand bag (count old-tiles))]
    [new-tiles (shuffle (concat new-bag old-tiles))]))

;; ;; TODO: This falls apart if tiles has only one tile!
;; ;; Might not even really apply in the case of one tile... do we really
;; ;; care which thing is common, or do we care about what's allowed?
;; (defn common-trait
;;   "Given a collection of `tiles`, determine which trait (either `:color`
;;   or `:shape`) they share in common. If they share nothing in common,
;;   return `nil` (TODO: exception?)"
;;   [tiles]
;;   (let [colors (map color tiles)
;;         shapes (map shape tiles)]
;;     (case ((= (count colors) 1)
;;            [:color (first colors)])
;;           ((= (count shapes) 1)
;;            [:shape (first shapes)])
;;           nil)))

;; ;; Should work for no adjacent tiles, 1 adjacent tiles, etc.
;; (defn allowed
;;   [adjacent-tiles]
;;   (let [[common common-value] (common-trait adjacent-tiles)
;;         [diff different-values] (if (= common :color)
;;                                    [:shapes (map shape adjacent-tiles)]
;;                                    [:color (map color adjacent-tiles)])
;;         allowed-colors (if (= common :color)
;;                           (seq common-value)
;;                           (different-values))
;;         allowed-shapes (if (= common :shape)
;;                           (seq common-value)
;;                           (different-values))]
;;         (for [c allowed-colors
;;               s allowed-shapes]
;;           (tile c s))))

;; When there is only one tile, we don't have a "locked" tile axis; if we have
;; a green-circle, then we can place a green-anything-but-circle OR an anything-but-green-circle
;; next to it. As soon as we have 2 tiles, though, they share something in common, and
;; so that necessarily restricts us. A green-circle -> green-square pair reqires the color
;; to be green and the shape to be "anything but circle or square".

;; TODO: don't really need to return the color, just true or false
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

;; require to be sets
;; (defn all-allowed
;;   [allowed-tiles]
;;   (apply set/intersection allowed-tiles))

;; Next step: figure out a board implementation
;; place first tile - this is 0,0
;; second tile is an adjoining one
;; Board spaces are things? They know what's adjacent to them, what's current in them, what can go in them.
;;(defrecord board-space [coordinates tile])

;; (defn board-space2
;;   [coordinates tile]
;;   {:coordinates coordinates
;;    :tile tile})

;; (defn coordinates [{pos :coordinates}] pos)

;; (defn tile-at-space [{tile :tile}] tile)

;; TODO should be adjacent-spaces

(defn north [[x y]] [x (inc y)])
(defn east  [[x y]] [(inc x) y])
(defn south [[x y]] [x (dec y)])
(defn west  [[x y]] [(dec x) y])

;; (def opposite-direction
;;   {north south
;;    south north
;;    east west
;;    west east})

;; (defn adjacent-positions [position]
;;   (map #(% position) [north east south west]))

;; since the board-space's tile is nil, of course the position is open. And since the
;; board maps a position to a space, there's nothing (yet) for the space to "do"
(defn board []
  {:open-positions {[0 0] all-tiles}
   :taken-positions {}})

(defn open-positions  [{open  :open-positions}] open)
(defn taken-positions [{taken :taken-positions}] taken)

(defn tile-at-position
  "If there's a tile at `position`, return it; otherwise `nil`."
  [board position]
  (get (taken-positions board) position))

;; (defn position-free?
;;   [board position]
;;   (get (open-positions board) position))

(defn tile-allowed-at-position?
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

;; to compute new open positions from the tile we just laid down, we need to first figure
;; out what the adjacent positions are. Any of those that are already taken are removed from
;; our consideration for creating open spaces. For each remaining space, we need to determine
;; what the allowed tiles at that space are. In order to do THAT, we need to traverse in each
;; of the cardinal directions, accumulating tiles as we go. Stop when we run out of taken positions.
;; Take the intersection of all tiles.
;;
;; I think that once you put a tile down, though, it can affect the allowed tiles at the opposite
;; end of a tile traversal. I'll tackle that later.
;;
;; Actually, it might be easier... Probably just need to grab all the tiles in the east-west /
;; north-south axes.
;;
;; Actually actually, when we add a tile to the board, it has the potential to change the allowed
;; tiles at all adjacent positions, where adjacent positions here means "at the end of a run of tiles"
;;
;;    O
;;    X X X
;;  O @ O
;;    O
;;
;;
;; By placing the "@" tile down, we update the allowed tiles at each of the "O" positions
;;
;; OK, so when we place a tile, we need to grab all the tiles on the north-south column. We then
;; need the open positions at the end of this north-south column. At each end, if it's not already
;; an "open position", we create an open position with the "other" tiles as allowed values. If it
;; already is an open position, then we intersect the existing allowed tiles with what comes from the
;; tiles in the column.
;;
;; The spaces at the ends could be existing open positions, or they could be completely new.
;;
;; TODO: If position isn't an occupied position....
;; TODO: any way to extract this traversal logic?

(defn- nearest-open
  "Finds the nearest open position from `position` traversing placed tiles along
  `direction`.
  Assumes that `position` is a placed tile."
  [board position direction]
  (let [new-position (direction position)]
    (if (tile-at-position board new-position)
      (nearest-open board new-position direction)
      new-position)))

;; self-recursion should be fine here, since we'll never go beyond 5 iterations.
(defn traverse-tiles
  ([board position direction]
   (traverse-tiles board position direction #{}))
  ([board position direction acc]
   (let [new-position (direction position)]
     (if-let [tile (tile-at-position board new-position)]
       (traverse-tiles board new-position direction (conj acc tile))
       acc))))

(defn- tiles-in-line
  [board position dir1 dir2]
  (set/union (traverse-tiles board position dir1)
             #{(tile-at-position board position)}
             (traverse-tiles board position dir2)))

;; (defn- north-south-tiles
;;   [board position]
;;   (tiles-in-line board position north south))

;; (defn- east-west-tiles
;;   [board position]
;;   (tiles-in-line board position east west))

;; (defn- update-north-south
;;   [board position]
;;   (let [placed-tiles (north-south-tiles board position)
;;         north-terminus (nearest-open board position north)
;;         south-terminus (nearest-open board position south)]
;;     (-> board
;;         (update-allowed north-terminus placed-tiles)
;;         (update-allowed south-terminus placed-tiles))))

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
  (if (tile-allowed-at-position? board tile position)
    (-> board
        (take-position tile position)
        (close-position position)
        (update-board position))
    board))
