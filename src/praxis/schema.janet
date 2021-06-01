(import err)
(import stringx :as "str")
(import datex :as "dt")

(comment ```
         Praxis is a system (currently) for defining objects that interact with HTML forms and 
         databases without a ton of extra fuss. Right now that means defining a schema to be used
         for validation and rendering.
         ```)

(defn- get-schema [name] 
  (or (dyn :praxis/schema) (err/str name " field can only be called inside defschema")))

(defn field 
  ```
  Expects a name and a type. The rest of the args are as follows:

  :default <val> - A value to be used if this field isn't populated after a cast
  :title <val>  - A user-friendly description for this field, typically used for field lables
  :hidden <true|false> - Indicates that this field isn't for display.
  ```
  [name my/type &keys kwargs]
  (match (kwargs :hidden) 
    true nil false nil nil nil
    _ (err/str "Cannot set :hidden to " (kwargs :hidden)))
  (match (kwargs :title)
    (t (bytes? t)) nil nil nil
    v (err/str "Cannot set title to non-bytes value: " v))

  (def field-spec 
    @{
      :name name
      :type my/type
      :default (kwargs :default)
      :title (kwargs :title)
      :hidden (kwargs :hidden)
      })
  (put-in (get-schema name) [:fields name] field-spec)
  (array/concat (get (get-schema name) :field-order) name))


(comment ```
    What is the list of types I'd want to support out of the box, and how do I want to exend that list?
    
    # Later
    (:array :)
    (:dictionary :) ```)

(defn- convert-error-msg [strval type-target] 
  [:err (string "\""strval"\" isn't a valid "type-target)])

(defn- convert-number 
  [strval] 
  (def ret (scan-number strval))
  (if ret [:ok ret] (convert-error-msg strval "number")))

(defn- convert-timestamp 
  [strval]
  (def ret (scan-number strval))
  (if ret [:ok ret] (convert-error-msg strval "timestamp")))

(defn- convert-s64
  [strval] 
  (match (protect (int/s64 strval))
    [:ok val] [:ok val]
    [:err _] (convert-error-msg strval "non-fractional number")))

(defn- convert-date 
  [strval] 
  (match (dt/parse-ymdstr strval) 
    nil (convert-error-msg strval "date")
    val [:ok val]))

(defn- convert-bool
  [name strval]
  (match strval
    name [:ok true]
    "true" [:ok true]
    "false" [:ok false]
    nil [:ok false]
    _ (convert-error-msg strval "boolean")))

(defn- try-convert-value [name value target-type] 
    (match [(type value) target-type] 
        [same/type same/type] [:ok value]
        [:number :integer] [:ok value]
        [:string :text]   [:ok value]
        [:string :string] [:ok value]
        [:string :number] (convert-number value)
        [:string :integer] (convert-s64 value)
        [:string :bool] (convert-bool name value)
        [:string :date] (convert-date value)
        [:string :timestamp] (convert-timestamp value)
        _ (err/str "A conversion from " (type value) " to " target-type " is not known by Praxis.")))

(defn- field-desc [obj field-key] 
  (assert (obj :schema) "obj must have a schema!")
  (def finfo (get-in obj [:schema :fields field-key]))
  (or (finfo :title) (string field-key)))

(defn validate-fn [obj field-key f message]
  (when (get-in obj [:errs field-key]) 
    (break obj))
  (unless (f (get-in obj [:vals field-key]))
    (put-in obj [:errs field-key] message))
  obj)

(defn validate-required [obj field-key &opt message]
  (assert (obj :schema) "Object must have a schema")
  (default message 
    (string (field-desc obj field-key) " is a required field. "))
  (when ((obj :errs) field-key)
    (break obj))
  (unless ((obj :vals) field-key)
    (put-in obj [:errs field-key] message))
  obj)

(defn validate-range [obj fkey [low high] &opt message]
  (when ((obj :errs) fkey) 
    (break obj))
  (def field ((obj :vals) fkey))
  (default message 
    (string (field-desc obj fkey) " should be between " low " and " high))
  (unless (<= low field high)
    (put-in obj [:errs field ] message))
  obj)

(defn validate-peg [obj fkey patt message]
  (assert (obj :schema) "Object must have a schema")
  (when ((obj :errs) fkey)
    (break obj))
  (def val (get-in obj [:vals fkey]))
  (unless (and val (bytes? val))
    (err/str "Invalid field " fkey))
  (unless (peg/match patt val)
    (put-in obj [:errs fkey] message))
  obj)


(defn- cast-kv [[key value]] 
    (unless (bytes? key)
        (err/str "Key of type: " (type key) " cannot cast to schema key"))
    [(keyword key) value])

(defn schema? [maybe-schema] 
  (match maybe-schema 
    { :name (n (symbol? n))
     :fields (f (dictionary? f))
     :type :praxis/schema
     :field-order (a (indexed? a))}
    true
    _ false))

(defn empty-of [schema] {
                          :field-order (schema :field-order)
                          :errs @{}
                          :vals @{}
                          :schema schema
                          })

(defn cast [&keys {:to schema :from kvargs :fields allowed-fields}]
    (assert (indexed? allowed-fields) "Allowed fields should be array|tuple!")
    (assert (dictionary? kvargs) "Casting should be from a struct|table")

    (def field-names (seq 
                         [fname :in (schema :field-order)
                           :when (find |(= fname $) allowed-fields) ]
                       (keyword fname)))

    (assert (= (length field-names) (length allowed-fields)) "There was a mismatch between the requested fields and the fields on the schema. This likely means a type on :fields argument")
    
    (def errs @{})
    (def schema-name (get schema :name))

    (defn add-err [k msg] 
      # (def msg (str/replace-pairs ["%FNAME" k "%SCHEMA" schema-name] msg))
      (match (errs k)
        nil (put errs k @[msg])
        (x (indexed? x)) (array/concat x msg)
        _ (err/str
            "add-err failed in an unxpected way!?!"
           " File a bug-report at https://github.com/yumaikas/Praxis.git")))

    (def vals @{})
    (def obj 
      @{
        :field-order field-names
        :errs errs
        :vals vals
        :schema schema
        })
    (defn add-val [k val] (put vals k val))
    (loop [(k v) :in (map cast-kv (pairs kvargs))
           :when (find |(= k $) field-names)
           :before (def field-spec (get-in schema [:fields k]))]
      (match (try-convert-value (field-spec :name) v (field-spec :type)) 
        # We need the unconverted value in case of 
        [:err msg] (do (add-val k v) (add-err k msg))
        [:ok val] (add-val k val)))
    obj)

(defn has-errors? [obj] 
  (> (length (keys (get obj :errs))) 0))

(defn has-schema? [obj] 
  (match 
    obj {
         :field-order (fo (indexed? fo))
         :errs (e (dictionary? e))
         :vals (v (dictionary? v))
         :schema (s (schema? s))}
    true
    _ false))

(defmacro defschema [name & body]
    (with-syms [$name] 
        ~(upscope 
            (def $name @{ :name (symbol ',name) :fields @{} :field-order @[] :type :praxis/schema })
            (with-dyns [:praxis/schema $name] 
                ,(splice body))
            (def ,name $name)))) 
