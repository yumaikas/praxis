(import ../../src/praxis/sqlite :as db)
(import ../../src/praxis/schema :as s)

(use testament)
(defn s. [& args] (string ;args))

(s/defschema
  Player
  (s/field :rowid :integer)
  (s/field :name :string :flags [:unique])
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

(defn file [tname] (s. "test/battleship" tname ".db"))

(exercise! 
  [] 
  (each f [(file "T1") (file "T2") (file "T3")]
    (when (os/stat f) (os/rm f)))

  (deftest can-insert
    (db/tx (file "T1")
           (db/init [Player Message BattleshipShot BattleshipBoat]))
    (def p1 (s/cast :to Player
                    :from {:name "Alice" 
                           :avatar-url "https://example.com/alice/avatar"} 
                    :fields [:name :avatar-url]))
    (def p2 (s/cast :to Player
                    :from {:name "Bob"
                           :avatar-url "https://example.com/bob/avatar"}
                    :fields [:name :avatar-url]))

    (def ids (db/tx (file "T1")
                    [(db/insert Player p1)
                     (db/insert Player p2)]))
    (assert-expr (all number? ids) "An insert failed!")
    (def p1db (db/tx (file "T1")
           (db/fetch Player (ids 0))))
    (assert-expr (= 'Player (get-in p1db [:schema :name])))
    (put-in p1db [:vals :name] "Amanda")
    (put-in p1db [:vals :avatar-url] "https://example.com/amanda/avatar")
    (db/tx (file "T1")
           (db/update Player p1db))
    (def p1db-after 
      (db/tx (file "T1")
             (db/fetch Player (ids 0))))
    (assert-equal "Amanda" (get-in p1db-after [:vals :name]))
    (assert-equal "https://example.com/amanda/avatar" (get-in p1db-after [:vals :avatar-url])))

  (deftest can-delete
    (def p1 (s/cast :to Player
                    :from {:name "Alice" 
                           :avatar-url "https://example.com/alice/avatar"} 
                    :fields [:name :avatar-url]))
    (def p2 (s/cast :to Player
                    :from {:name "Bob"
                           :avatar-url "https://example.com/bob/avatar"}
                    :fields [:name :avatar-url]))
    (db/tx (file "T2")
           (db/init [Player Message BattleshipShot BattleshipBoat]))

    (def ids (db/tx (file "T2")
                    [(db/insert Player p1)
                     (db/insert Player p2)]))
    (def ids (db/tx (file "T2") (db/eval "SELECT rowid from Player" )))
    (db/tx (file "T2")
      (each {:rowid id} ids
        (db/delete Player id)))

    (assert-equal 0 (length (db/tx 
                              (file "T2")
                              (db/eval "SELECT rowid from Player;")))))

  (deftest can-drop-tables
    (db/tx (file "T3")
           (db/init [Player Message BattleshipShot BattleshipBoat]))
    (assert-expr 
      (db/migrate
        (file "T3")
        (db/drop-tables Player Message BattleshipShot BattleshipBoat)
        true)))
  )

