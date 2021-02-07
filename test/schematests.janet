(use ../praxis)
(use testament)

(s/defschema Contact
    (s/field :id :integer :hidden)
    (s/field :name :string :title "Name")
    (s/field :term-of-address :string :title "How we should address you")
    (s/field :phone-number :string :title "Phone #"))

(defn- phone-len-match [& args] 
  (def digits (string ;args))
  (if (= 10 (length digits)) digits nil))

(def- phone-number-patt
  (peg/compile ~{ 
                 :capture-digs (* (+ (<- :d+) (some :D)))
                 :main (cmt (some :capture-digs) ,phone-len-match)}))

(defn new-contact [kvargs] 
  (as-> (s/cast :to Contact :from kvargs :fields [:name :term-of-address :phone-number]) it
        (s/validate-required it :name "Name is required for contact")
        (s/validate-required it :term-of-address)
        (s/validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")))

(defn updateable-contact [kvargs]
  (as-> (s/cast :to Contact :from kvargs :fields [:id :name :term-of-addres :phone-number]) it
        (s/validate-required it :name "Name is required for contact")
        (s/validate-required it :id "Cannot save contact without id!")
        (s/validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")))

(defsuite! 
  (deftest cast-works
    (assert-matches
      @{ 
        :schema Contact
        :vals @{ :name "Andrew Owen" :phone-number "555-555-5555" } 
        :errs @{}
        :field-order [:name :term-of-address :phone-number] }
      (s/cast :to Contact 
            :from { "name" "Andrew Owen" "phone-number" "555-555-5555" } 
            :fields [:term-of-address :name :phone-number])))

  (deftest new-contact
    (assert-matches 
      {:errs (e (and (dictionary? e) (= (length (pairs e)) 0))) } 
      (new-contact { 
                    "name" "Andrew Owen" 
                    "phone-number" "555-555-5555" 
                    "term-of-address" "Andrew"
                    }))))

