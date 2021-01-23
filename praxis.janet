(import globals)
(import err)

(defn field [name type &keys { :default default }]
    (def schema (dyn :praxis/schema))
    (put-in schema [:fields name] @{
        :type type
    })
    nil
)

(defn- cast-key [key] 
    (match (type key)
        :buffer (keyword key)
        :string (keyword key)
        :keyword key
        _ (err/str "Key of type: " (type key) " cannot be used for schema!")
    )
)

(comment 
    What is the list of types I'd want to support out of the box, and how do I want to exend that list?
    :datetime
    :timestamp
    :string
    :number
    :integer
    :boolean
    
    # Later
    (:array :)
    (:dictionary :)
)

(defn- try-convert-value [value target-type] 
    (match [(type value) target-type] 
        [:string :string] value
        [:string :number] (scan-number value)
        [:string :integer] (int/s64 value)
        [:string :boolean] (match value 
            "true" true 
            "false" false 
            _ (err/str "Cannot convert " value " to boolean!"))
        [:string :timestamp] (scan-number value)
        _ (err/str "A conversion from " (type value) " to " target-type " is not known by Praxis.")
    )
)

(defn cast [self kvargs allowed-fields]
    (assert (indexed? allowed-fields) "Allowed fields should be an indexed type!")
    (assert (dictionary? kvargs) "Fields that we are merging should be a dict!")
    (put self :fields @{})
    
    (loop [(k v) :in (pairs kvargs)
            :when (find |(= k $) allowed-fields)]
        # TODO: So, here we need to figure out how to cast 
        (def key (cast-key k))
        (put (self :fields) (cast-key k) v) 
    )
)

(defmacro defschema [name & body]
    (with-syms [$name] 
        ~(upscope 
            (def $name @{ :fields @{} :type :praxis/schema })
            (with-dyns [:praxis/schema $name] 
                ,(splice body)
            )
            (def ,name $name)
        )
    )
) 
