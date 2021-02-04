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
        (validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")
  ))

(defn updateable-contact [kvargs]
  (as-> (cast :to Contact :from kvargs :fields [:id :name :term-of-addres :phone-number]) it
        (validate-required it :name "Name is required for contact")
        (validate-required it :id "Cannot save contact without id!")
        (validate-peg it :phone-number phone-number-patt "Please enter a 10-digit phone number")))

(defsuite! 
  (deftest cast-works
    (assert-matches
      @{ 
        :schema Contact
        :vals @{ :name "Andrew Owen" :phone-number "555-555-5555" } 
        :errs @{}
        :field-order [:name :term-of-address :phone-number] }
      (cast :to Contact 
            :from { "name" "Andrew Owen" "phone-number" "555-555-5555" } 
            :fields [:term-of-address :name :phone-number])))

  (deftest new-contact
    (assert-matches 
      {:errs (e (and (dictionary? e) (= (length (pairs e)) 0))) } 
      (new-contact { 
                    "name" "Andrew Owen" 
                    "phone-number" "555-555-5555" 
                    "term-of-address" "Andrew"
                    })))
)

(comment ```
         So, I have a use case like so

         (defschema Contact
           (field :name :string :title "Name")
           (field :term-of-address :string :title "Term of Address")
           (field :phone-number :string :title "Phone #")
           (field :date-met :date :title "Date met"))

         (defn cast-contact [kvargs]
           (as-> (cast Contact kvargs [:name :phone-number :date]) it
            (validate-peg it :phone-number {:main } "")
            it))

         (defschema Expense
          (field :description :string :title "Description")
          (field :amount :number :title "Amount")
          (field :date :date :title "Spent on"))

        (defn cast-expense [kvargs]
          (as-> 
            (cast Expense kvargs [:description :amount :date]) it
            (validate-range it :amount [-1000.0 1000.0])
          )
         ```)

