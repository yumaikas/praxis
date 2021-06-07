(import ./schema :as s)
(import sqlite3 :as sql)
(defn- s. [& args] (string ;args))


(defmacro tx [file & body] 
  ~(with-dyns 
     [:praxis/db (or (dyn :praxis/db) (,sql/open ,file))]
     (defer (,sql/close (dyn :praxis/db))
       (try
         (do 
           (,sql/eval (dyn :praxis/db) "PRAGMA journal_mode=WAL")
           (,sql/eval (dyn :praxis/db) "BEGIN TRANSACTION")
           (def retval (do ,;body))
           (,sql/eval (dyn :praxis/db) "COMMIT TRANSACTION")
           retval)
         ([err fib] (do 
                      (,sql/eval (dyn :praxis/db) "ROLLBACK TRANSACTION")
                      (propagate err fib)))))))

(defn- fields-from-schema [schema] 
  (def order (schema :field-order))
  (seq [f :in order] 
    (get-in schema [:fields f])))

(defn- clean-name [name] (string/replace "-" "_" name))

(defn init [schemata] 
  # Iteratate over the schemas involved
  (defn fields-to-cols [fields] 
    (seq [f :in fields] 

      (def uniq (and (index-of :unique (or (f :flags) [])) "UNIQUE"))
      (defn from-type [_type] 
        (match [uniq _type]
          ["UNIQUE" "INTEGER PRIMARY KEY"] 
          (error "Cannot apply unique to a primary key!")
          _ (string/trim (s. _type " " uniq))))

      {:name (clean-name (f :name)) 
       :type (match [(f :name) (f :type)]
               [:rowid :integer] (from-type "INTEGER PRIMARY KEY")
               [_ :integer] (from-type "INTEGER")
               _ (from-type "TEXT"))}))

  (defn schema->table [schema]
      {:name (clean-name (get schema :name))
       :columns (fields-to-cols (fields-from-schema schema))})

  (def statements (seq [t :in (map schema->table schemata)]
    (s.
      "CREATE TABLE IF NOT EXISTS " (clean-name (t :name)) " (\r\n"
      (string/join (map |(s. "\t" ($ :name) " " ($ :type)) (t :columns )) ",\r\n")
      ");")))
  (each stmt statements 
    (sql/eval (dyn :praxis/db) stmt)))

  # Then execute a number of queries from them.

(defn- clean-vals [vals] 
  (def out @{})
  (eachp [k v] vals
    (put out (keyword (clean-name k)) v))
  out)

(defn- memoize [cache func]
  (fn [& args]
    (if (cache args)
      (cache args)
      (do
        (def val (func ;args))
        (put cache args val)
        (cache args)))))


(defn _update-query-for [schema] 
 (def {:fields fields} schema)
 (def field-names (map |(clean-name (get-in fields [$ :name])) (get schema :field-order)))
 (s. 
   "UPDATE " (s. (get schema :name)) "\n"
   "SET\n"
   (string/join (map |(s. "\t" $ " = :" $) field-names) ",\n")
   "\nwhere rowid = :rowid"))

(def- update-query-for (memoize @{} _update-query-for))

(def- _update update) 

(defn update [schema obj] 
  (unless (s/schema? schema) (error "Cannot emit update code for a non-praxis schema!"))
  (def query (update-query-for schema))
    (unless (= schema (get obj :schema)) (error (s. "Object isn't a " (get schema :name) "!" )))
    (when (s/has-errors? obj) (error "Cannot save updates when object has errors!"))
    (sql/eval (dyn :praxis/db) query (clean-vals (get obj :vals))))


(defn- _insert-query-for [schema] 
  (def fields (get schema :fields))
  # Don't try to insert :rowid columns
  (def field-names (map |(clean-name (get-in fields [$ :name])) 
                        (filter |(not= $ :rowid) (get schema :field-order))))
  (s.
    "INSERT INTO " (s. (get schema :name)) "\n"
    "(" (string/join field-names ", ") ")\n"
    "values (" (string/join (map |(s. ":" $) field-names) ",") ");"))

(def- insert-query-for (memoize @{} _insert-query-for))

(defn insert [schema obj]
  (def name (string (get schema :name)))
  (unless (s/schema? schema) (error "Cannot emit update code for a non-praxis schema!"))
  (unless (= schema (get obj :schema)) (error (s. "Object isn't a " name "!") ))
  (when (s/has-errors? obj) (error "Cannot save updates when object has errors!"))

  (def query (insert-query-for schema))
  (sql/eval (dyn :praxis/db) query (clean-vals (get obj :vals)))
  (sql/last-insert-rowid (dyn :praxis/db)))


(defn- _fetch-query-for [schema]
  (def fields (get schema :fields))
  # Don't try to insert :rowid columns
  (def field-names (map |{:from (clean-name (get-in fields [$ :name]))
                          :to (get-in fields [$ :name]) }
                        (get schema :field-order)))
  (s. 
    "SELECT "
    (string/join (map |(s. ($ :from) " as [" ($ :to) "]") field-names) ",\n")
    "\nFROM " (s. (get schema :name)) "\n"
    "WHERE rowid = :rowid"))
(def- fetch-query-for (memoize @{} _fetch-query-for))

(defn fetch [schema id]
  (def name (string (get schema :name)))
  (unless (s/schema? schema) (error "Cannot emit fetch code for non-praxis schema!"))
  (def query (fetch-query-for schema))
  (def [row] (sql/eval (dyn :praxis/db) query { :rowid id }))
  (s/cast :to schema :from row :fields (get schema :field-order)))


(defn- _delete-query-for [schema]
  (s. 
    "DELETE \n"
    "FROM " (s. (get schema :name)) "\n"
    "WHERE rowid = :rowid"))

(def- delete-query-for (memoize @{} _delete-query-for))

(defn delete [schema id]
  (def name (string (get schema :name)))
  (unless (s/schema? schema) (error "Cannot emit delete code for non-praxis schema!"))

  (def query (delete-query-for schema))
  (sql/eval (dyn :praxis/db) query {:rowid id})
  nil)


(defn eval [query &opt params]
  (sql/eval (dyn :praxis/db) query (or params {})))



