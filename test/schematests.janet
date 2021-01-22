(use ../praxis)

(defschema appointment
    (field :date :integer)
    (field :location :string)
    (field :description :string)
)

(defn changeset [self args] 
  (as-> 
    (cast self args [:date :location :description]) it
  )
)

(def changes (changeset @{} {:date (os/time) :location "here"}))


