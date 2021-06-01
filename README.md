# Praxis

The idea behind praxis is to take some good ideas from Elixir's ecto, and turn them into a library useful for validating forms, automatically rendering them as HTML (with a few default options, as well as ways to override rendering entirely), and to make it easy to scaffold out web servers interacting with Schema'd data.

# Status

Praxis is in v0.3. It has a decent amount of tests, . I welcome other people looking for data schemas or auto-rendered forms to take it for a spin, or test it. The API is still subject to change and rearrangement, however.

## Example

```
(use ../praxis)
(import ../praxis/sqlite :as db)
(use testament)

(defschema Contact
    (field :rowid :integer :hidden)
    (field :name :string :title "Name")
    (field :term-of-address :string :title "How we should address you")
    (field :phone-number :string :title "Phone #"))

(db/tx "contacts.db"
    (db/init [Contact]))


    


(defn- phone-len-match [& args] 
  (def digits (string ;args))
  (if (= 10 (length digits)) digits nil))

(def- phone-number-patt
  (peg/compile ~{ 
                 :capture-digs (* (+ (<- :d+) (some :D)))
                 :main (cmt (some :capture-digs) ,phone-len-match)}))

(defn new-contact [kvargs] 
  (as-> (cast :to Contact :from kvargs :fields [:name :term-of-address :phone-number]) it
        (validate-required it :name "Name is required for contact")
        (validate-required it :term-of-address)
        (validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")))

(def alice-id (db/tx "contacts.db"
    (db/insert Contact (new-conact {
        :name "Alice"
        :term-of-address "Alice"
        :phone-number "555-555-5555"
    }))

(db/tx "contacts.db"
    (db/fetch Contact alice-id))

(defn updateable-contact [kvargs]
  (as-> (cast :to Contact :from kvargs :fields [:id :name :term-of-addres :phone-number]) it
        (validate-required it :name "Name is required for contact")
        (validate-required it :rowid "Cannot save contact without rowid!")
        (validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")))

```
