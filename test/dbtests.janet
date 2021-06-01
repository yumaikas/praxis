(import ../src/praxis/sqlite :as db)
(import ../src/praxis/schema :as s)

(use testament)

(s/defschema
  Player
  (s/field :rowid :integer)
  (s/field :name :string)
  (s/field :avatar-url :string))

(s/defschema 
  Message 
  (s/field :rowid :integer) # Treated as primary key
  (s/field :content :string)
  (s/field :from-id :integer)
  (s/field :to-id :integer)
  (s/field :updated :timestamp))

(s/defschema
  BattleshipShot
  (s/field :rowid :integer) # Treated as primary key
  (s/field :x :integer) 
  (s/field :y :integer)
  (s/field :hit :bool))

(s/defschema 
  BattleshipBoat
  (s/field :rowid :integer) # Treated as primary key
  (s/field :x :integer)
  (s/field :y :integer)
  (s/field :length :integer) # (2-5)
  # Dirction is one of "UP" "DOWN" "RIGHT" "LEFT"
  (s/field :direction :string))

(def file "test/battleship.db")

(exercise! 
  [] 
  (when (os/stat file) (os/rm file))
  (db/tx file
         (db/init [Player Message BattleshipShot BattleshipBoat]))

  (deftest can-insert
    (def p1 (s/cast :to Player
                    :from {:name "Alice" 
                           :avatar-url "https://example.com/alice/avatar"} 
                    :fields [:name :avatar-url]))
    (def p2 (s/cast :to Player
                    :from {:name "Bob"
                           :avatar-url "https://example.com/bob/avatar"}
                    :fields [:name :avatar-url]))

    (def ids (db/tx file
                    [(db/insert Player p1)
                     (db/insert Player p2)]))
    (assert-expr (all number? ids) "An insert failed!")
    (def p1db (db/tx file
           (db/fetch Player (ids 0))))
    (assert-expr (= 'Player (get-in p1db [:schema :name])))
    (put-in p1db [:vals :name] "Amanda")
    (put-in p1db [:vals :avatar-url] "https://example.com/amanda/avatar")
    (db/tx file
           (db/update Player p1db))
    (def p1db-after 
      (db/tx file
             (db/fetch Player (ids 0))))
    (assert-equal "Amanda" (get-in p1db-after [:vals :name]))
    (assert-equal "https://example.com/amanda/avatar" (get-in p1db-after [:vals :avatar-url]))
  ))


