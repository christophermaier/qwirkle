(ns qwirkle.core-test
  (:require [clojure.test :refer :all]
            [qwirkle.core :refer :all]))

(deftest tiles
  (testing "creation"
    (is (= {:color :red :shape :circle}
           (tile :red :circle))
        "Tiles are implemented as hashes")
    (is (nil? (tile :mauve :circle))
        "Returns `nil` if using an invalid color")
    (is (nil? (tile :red :octagon))
        "Returns `nil` if using an invalid shape"))
  (testing "basic tile functionality"
    (is (color (tile :red :circle)) :red)
    (is (shape (tile :red :circle)) :circle))
  (testing "permutations"
    (is (tile-permutation [:red] [:circle])
        (set [(tile :red :circle)])))
    (is (tile-permutation [:red] [:circle :square])
        (set [(tile :red :circle)
              (tile :red :square)]))
    (is (= (count (tile-permutation all-colors all-shapes))
           36))
  (testing "all-tiles"
    (is (= #{(tile :blue :circle) (tile :blue :clover) (tile :blue :crisscross)
             (tile :blue :diamond) (tile :blue :square) (tile :blue :starburst)
             (tile :green :circle) (tile :green :clover) (tile :green :crisscross)
             (tile :green :diamond) (tile :green :square) (tile :green :starburst)
             (tile :orange :circle) (tile :orange :clover) (tile :orange :crisscross)
             (tile :orange :diamond) (tile :orange :square) (tile :orange :starburst)
             (tile :purple :circle) (tile :purple :clover) (tile :purple :crisscross)
             (tile :purple :diamond) (tile :purple :square) (tile :purple :starburst)
             (tile :red :circle) (tile :red :clover) (tile :red :crisscross)
             (tile :red :diamond) (tile :red :square) (tile :red :starburst)
             (tile :yellow :circle) (tile :yellow :clover) (tile :yellow :crisscross)
             (tile :yellow :diamond) (tile :yellow :square) (tile :yellow :starburst)}
           all-tiles)
        "Actually generate all possible tiles"))
  (testing "bag"
    (is (= (count (bag))
           108)))
    (is (= (count (filter #(= % (tile :red :circle)) (bag)))
           3))
;;     (let [[hand new-bag] (deal-hand (bag) 6)]
;;       (is (= (concat new-bag hand))
;;              []))
  )
(deftest set-operations
  (testing "other-colors"
    (is (= #{:blue :green :orange :purple :yellow}
           (other-colors :red))
        "takes a single color"))
    (is (= #{:blue :green :orange :purple}
           (other-colors [:red :yellow]))
        "takes multiple colors")
  (testing "other-shapes"
    (is (= #{:clover :crisscross :diamond :square :starburst}
           (other-shapes :circle))
        "takes a single shape"))
    (is (= #{:crisscross :diamond :square :starburst}
           (other-shapes [:circle :clover]))
        "takes multiple shapes"))

(deftest common-features
  (testing "common-color?"
    (is (common-color? [(tile :red :circle)
                        (tile :red :square)
                        (tile :red :diamond)])
        "Returns true when a common color exists")
    (is (not (common-color? [(tile :red :circle)
                             (tile :red :square)
                             (tile :green :diamond)]))
        "Returns false when there is no common color"))
  (testing "common-shape?"
    (is (common-shape? [(tile :red :circle)
                        (tile :green :circle)
                        (tile :blue :circle)])
        "Returns true when a common shape exists")
    (is (not (common-shape? [(tile :red :circle)
                             (tile :green :circle)
                             (tile :blue :diamond)]))
        "Returns false when there is no common shape")))

(deftest directions
  (testing "north"
    (is (= [0 1]
           (north [0 0]))))
  (testing "south"
    (is (= [0 -1]
           (south [0 0]))))
  (testing "east"
    (is (= [1 0]
           (east [0 0]))))
  (testing "west"
    (is (= [-1 0]
           (west [0 0])))))

(deftest board-features
  (testing "initialization"
    (is (= [[0 0]]
           (keys (open-positions (board))))
        "On a fresh board, only the 'origin' position is available")
    (is (= all-tiles
           (get (open-positions (board)) [0 0]))
        "All tiles are allowed at the first position")
    (is (empty? (taken-positions (board)))
        "No positions are taken on a fresh board"))
  (testing "board introspection functions"
    (is (nil? nil))))

(def ^:private board-with-one-tile
  {:open-positions
   {[0  1]
    #{(tile :red :clover)(tile :red :crisscross) (tile :red :diamond) (tile :red :square) (tile :red :starburst)
      (tile :blue :circle)(tile :green :circle) (tile :orange :circle) (tile :purple :circle) (tile :yellow :circle)}
    [0 -1]
    #{(tile :red :clover)(tile :red :crisscross) (tile :red :diamond) (tile :red :square) (tile :red :starburst)
      (tile :blue :circle)(tile :green :circle) (tile :orange :circle) (tile :purple :circle) (tile :yellow :circle)}
    [1  0]
    #{(tile :red :clover)(tile :red :crisscross) (tile :red :diamond) (tile :red :square) (tile :red :starburst)
      (tile :blue :circle)(tile :green :circle) (tile :orange :circle) (tile :purple :circle) (tile :yellow :circle)}
    [-1 0]
    #{(tile :red :clover)(tile :red :crisscross) (tile :red :diamond) (tile :red :square) (tile :red :starburst)
      (tile :blue :circle)(tile :green :circle) (tile :orange :circle) (tile :purple :circle) (tile :yellow :circle)}}
   :taken-positions
   {[0 0] (tile :red :circle)}})

(deftest board-manipulation
  (testing "adding a tile"
    (is (= board-with-one-tile
           (place-tile (board)
                       (tile :red :circle)
                       [0 0]))
        "Adding one tile appropriately updates a fresh board"))
  (testing "tile-at"
    (is (= (tile :red :circle)
           (tile-at board-with-one-tile [0 0]))
        "Returns a tile at the position, when it exists")
    (is (nil? (tile-at (board) [0 0]))
        "Returns nil when no tile ie present at the given position"))
  (testing "tile-allowed-at-position"
    (is (every? #(tile-allowed-at? (board) % [0 0])
                all-tiles)
        "All tiles are allowed at the first position")
    (is (tile-allowed-at? board-with-one-tile (tile :red :square) [0 1])
        "returns 'true' for a tile that is allowed")
    (is (not (tile-allowed-at? board-with-one-tile (tile :blue :square) [0 1]))
        "returns 'false' for a tile that is not allowed"))
  (testing "close-position"
    (is (= {:open-positions {} :taken-positions {}}
           (close-position (board) [0 0]))
        "removes an open position entry (when that position is open)")
    (is (= (board)
           (close-position (board) [0 1]))
        "doesn't change the board when the position isn't open to begin with"))
  (testing "update-allowed"
    (is (= {:open-positions {[0 0] all-tiles
                             [0 1] #{(tile :red :clover)(tile :red :crisscross) (tile :red :diamond)
                                     (tile :red :square) (tile :red :starburst)
                                     (tile :blue :circle)(tile :green :circle) (tile :orange :circle)
                                     (tile :purple :circle) (tile :yellow :circle)}}
            :taken-positions {}}
         (update-allowed (board) [0 1] #{(tile :red :circle)}))
        "updates an open position")
    (let [mock-board {:open-positions {[0 1] #{(tile :red :clover)(tile :red :crisscross) (tile :red :diamond)
                                               (tile :red :square) (tile :red :starburst)
                                               (tile :blue :circle)(tile :green :circle) (tile :orange :circle)
                                               (tile :purple :circle) (tile :yellow :circle)}}}]
      (is (= {:open-positions {[0 1] #{(tile :red :clover)
                                       (tile :red :crisscross)
                                       (tile :red :diamond)
                                       (tile :red :starburst)}}}
             (update-allowed mock-board [0 1] #{(tile :red :square)}))
          "Updates from one adjacent tile to two")))
  (testing "take-position"
    (is (= {:taken-positions
             {[0 0] (tile :red :circle)
              [0 1] (tile :red :square)}}
            (take-position {:taken-positions {[0 0] (tile :red :circle)}} ;; mock board
                           (tile :red :square)
                           [0 1]))
        "Claims a position on the board (note that this should be part of a larger operation)")
    (is (= {:taken-positions {[0 0] (tile :red :circle)}}
           (take-position {:taken-positions {[0 0] (tile :red :circle)}}
                          (tile :blue :square)
                          [0 0]))
        "Cannot claim previously claimed spot")))

(deftest allowed-tiles
  (testing "allowed"
    (is (= #{(tile :red :clover)
             (tile :red :crisscross)
             (tile :red :diamond)
             (tile :red :square)
             (tile :red :starburst)

             (tile :blue :circle)
             (tile :green :circle)
             (tile :orange :circle)
             (tile :purple :circle)
             (tile :yellow :circle)}
           (allowed [(tile :red :circle)]))
        "works with a single tile")
    (is (= #{(tile :red :crisscross)
             (tile :red :diamond)
             (tile :red :square)
             (tile :red :starburst)}
           (allowed [(tile :red :circle)
                     (tile :red :clover)]))
         "works with multiple same-color tiles")
    (is (= #{}
           (allowed [(tile :red :circle)
                     (tile :red :clover)
                     (tile :red :crisscross)
                     (tile :red :diamond)
                     (tile :red :square)
                     (tile :red :starburst)]))
         "nothing is allowed if all same-color tiles are used")
    (is (= #{(tile :orange :circle)
             (tile :purple :circle)
             (tile :red :circle)
             (tile :yellow :circle)}
           (allowed [(tile :blue :circle)
                     (tile :green :circle)]))
         "works with multiple same-shape tiles")
    (is (= #{}
           (allowed [(tile :blue :circle)
                     (tile :green :circle)
                     (tile :orange :circle)
                     (tile :purple :circle)
                     (tile :red :circle)
                     (tile :yellow :circle)]))
         "nothing is allowed if all same-shape tiles are used")

    ;; TODO: Might want to make this an exception instead?
    (is (= #{}
           (allowed [(tile :blue :circle)
                     (tile :green :square)]))
       "nothing happens if tiles don't have anything in common")))

(deftest mini-game
  (testing "placing 3 tiles"
    (is (= {:open-positions {[-1 2] #{{:color :red, :shape :circle}
                                      {:color :red, :shape :crisscross}
                                      {:color :red, :shape :diamond}
                                      {:color :red, :shape :square}
                                      {:color :red, :shape :starburst}
                                      {:color :blue, :shape :clover}
                                      {:color :green, :shape :clover}
                                      {:color :orange, :shape :clover}
                                      {:color :purple, :shape :clover}
                                      {:color :yellow, :shape :clover}}
                             [1 2] #{{:color :red, :shape :circle}
                                     {:color :red, :shape :crisscross}
                                     {:color :red, :shape :diamond}
                                     {:color :red, :shape :square}
                                     {:color :red, :shape :starburst}
                                     {:color :blue, :shape :clover}
                                     {:color :green, :shape :clover}
                                     {:color :orange, :shape :clover}
                                     {:color :purple, :shape :clover}
                                     {:color :yellow, :shape :clover}}
                             [0 3] #{{:color :red, :shape :crisscross}
                                     {:color :red, :shape :diamond}
                                     {:color :red, :shape :starburst}}
                             [-1 1] #{{:color :red, :shape :circle}
                                      {:color :red, :shape :clover}
                                      {:color :red, :shape :crisscross}
                                      {:color :red, :shape :diamond}
                                      {:color :red, :shape :starburst}
                                      {:color :blue, :shape :square}
                                      {:color :green, :shape :square}
                                      {:color :orange, :shape :square}
                                      {:color :purple, :shape :square}
                                      {:color :yellow, :shape :square}}
                             [1 1] #{{:color :red, :shape :circle}
                                     {:color :red, :shape :clover}
                                     {:color :red, :shape :crisscross}
                                     {:color :red, :shape :diamond}
                                     {:color :red, :shape :starburst}
                                     {:color :blue, :shape :square}
                                     {:color :green, :shape :square}
                                     {:color :orange, :shape :square}
                                     {:color :purple, :shape :square}
                                     {:color :yellow, :shape :square}}
                             [-1 0] #{{:color :red, :shape :clover}
                                      {:color :red, :shape :crisscross}
                                      {:color :red, :shape :diamond}
                                      {:color :red, :shape :square}
                                      {:color :red, :shape :starburst}
                                      {:color :blue, :shape :circle}
                                      {:color :green, :shape :circle}
                                      {:color :orange, :shape :circle}
                                      {:color :purple, :shape :circle}
                                      {:color :yellow, :shape :circle}}
                             [1 0] #{{:color :red, :shape :clover}
                                     {:color :red, :shape :crisscross}
                                     {:color :red, :shape :diamond}
                                     {:color :red, :shape :square}
                                     {:color :red, :shape :starburst}
                                     {:color :blue, :shape :circle}
                                     {:color :green, :shape :circle}
                                     {:color :orange, :shape :circle}
                                     {:color :purple, :shape :circle}
                                     {:color :yellow, :shape :circle}}
                             [0 -1] #{{:color :red, :shape :crisscross}
                                      {:color :red, :shape :diamond}
                                      {:color :red, :shape :starburst}}},
            :taken-positions {[0 2] {:color :red, :shape :clover},
                              [0 1] {:color :red, :shape :square},
                              [0 0] {:color :red, :shape :circle}}}
           (-> (board)
               (place-tile (tile :red :circle) [0 0])
               (place-tile (tile :red :square) [0 1])
               (place-tile (tile :red :clover) [0 2]))
           )
        "Placing 3 tiles actually works!")))
