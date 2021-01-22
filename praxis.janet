(import globals)
(defn field [name type &keys { :default default }]
    (def schema (dyn :praxis/schema))
    (put-in schema [:fields name] @{
        :type type
    })
    nil
)

(defn- cast-key [key] 
    (match (type key)
        :string (keyword key)
        :keyword key
        _ (error (string "Key of type: " (type key) " cannot be used for schema!"))
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
