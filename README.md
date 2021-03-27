# Praxis

The idea behind praxis is to take some good ideas form Elixir's [ecto](https://hexdocs.pm/ecto/getting-started.html), and turn them into a library useful for validating forms, automatically rendering them as HTML (with a few default options, as well as ways to override rendering entirely), and to make it easy to scaffold out web servers interacting with Schema'd data.

# Status

Praxis is in v0.2. It has a decent amount of tests, and is about to be used for anno-server. I welcome other people looking for data schemas or auto-rendered forms to take it for a spin, or test it. The API is still subject to change and rearrangement, however.

## Example

```
(use ../praxis)
(use testament)

(defschema Contact
    (field :id :integer :hidden)
    (field :name :string :title "Name")
    (field :term-of-address :string :title "How we should address you")
    (field :phone-number :string :title "Phone #"))

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

(defn updateable-contact [kvargs]
  (as-> (cast :to Contact :from kvargs :fields [:id :name :term-of-addres :phone-number]) it
        (validate-required it :name "Name is required for contact")
        (validate-required it :id "Cannot save contact without id!")
        (validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")))

```
