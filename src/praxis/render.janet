(import ./schema)
(import datex :as dt)
(import err)
(use assertx)

(def render-form-fieldtypes @{})

(defmacro add-form-fieldtype [my/type & body] 
  ~(put render-form-fieldtypes ,my/type (fn [title name value my/err field] 
                                          (def name (keyword name))
                                          ,;body)))

(defn- ok-or-nil [val] (match val [:ok val] val _ nil))

(defn- bool-render [val] (if val "true" "false"))

(add-form-fieldtype 
  :text
  [:div 
   [:label { :for name }
    [:div (string title ":")]
    [:textarea { :name name } value ]]
   (when my/err [:div {:class "form-error"} my/err])])

(add-form-fieldtype
  :string 
  [:div 
   [:label { :for name } 
    [:div (string title ":")]
    [:input { :name name :type "text" :value value }]
    (when my/err [:div {:class "form-error"} my/err])]])

(add-form-fieldtype
  :number
  [:div
   [:label { :for name }
    [:div (string title ":")]
    [:input { :name name :type "number" :value value }]
    (when my/err [:div {:class "form-error" } my/err])]])

(add-form-fieldtype
  :integer
  [:div
  [:label { :for name }
    [:div (string title ":")]
   [:input { :name name :type "number" :value  value }]]
  (when my/err [:div {:class "form-error" } my/err])])

(add-form-fieldtype
  :bool
  [:div
   [:label { :for name }
    [:div (string title ":")]
    [:input { :name name :type "checkbox" :value name :checked (bool-render value) }]
    (when my/err [:div {:class "form-error" } my/err])]])

(add-form-fieldtype
  :picklist
  (def vals (get-in field [:meta :values]))
  [:div
   [:label {:for name}
    [:div (string title ":")]
    [:select {:name name} 
     (seq [v :in vals
           :when (v :active)]
       [:option {:value (v :value) 
                 :selected (when (= (v :value) value) true) }  
        (v :title)]) ]]])

(add-form-fieldtype
  :date
  (defn to-ymdstr [value]
    (as?-> value it
          (os/date it)
          (dt/to-ymdstr it)))
  [:div
   [:label { :for name }
    [:div (string title ":")]
    [:input { :name name :type "date" :value (to-ymdstr value) :checked (bool-render value) }]
    (when my/err [:div {:class "form-error" } my/err])]])

(add-form-fieldtype
  :timestamp
  [:div
   [:label { :for name }
    [:div (string title ":")]
    [:input { :name name :type "datetime-local" :value value }]
    (when my/err [:div {:class "form-error" } my/err])]])

(defn hidden-field [fkey value] )

(defn form-fields [obj] 
  (unless (schema/has-schema? obj)
    (err/str "Cannot render an object that wasn't built with a praxis schema"))
  (defn get-type [fkey] 
    (get-in obj [:schema :fields fkey :type]))

  (defn get-title [fkey] (get-in obj [:schema :fields fkey :title] fkey))
  (defn is-hidden? [fkey] (get-in obj [:schema :fields fkey :hidden]))
  (defn get-err [fkey] (get-in obj [:errs fkey]))
  (defn get-val [fkey] (get-in obj [:vals fkey]))
  
  (seq [fkey :in (obj :field-order)
        :when (not (is-hidden? fkey))]
      (def ftype (get-type fkey))
      (def value (get-val fkey))
      (def title (get-title fkey))
      (def my/err (get-err fkey))
      (def render/fn (render-form-fieldtypes ftype))
      (unless render/fn 
        (err/str "Cannot render type " ftype "!"))

      (render/fn title fkey value my/err (get-in obj [:schema :fields fkey]))))
        
#@task[Add test for hidden fields. And/or give hidden fields more values
# I'm thinking "embed" "hide" and something else? Or a way to easily overload that on a per-form basis? Something to noodle on later]
(defn form [obj &keys { :method method :action action :submit-txt submit-txt } ] 
  (default method "POST")
  (default submit-txt "Submit")
  [:form {:action action :method method } 
   (form-fields obj)
   [:input {:type "submit" :value submit-txt }]])

(defn- header-cell [schema opts fkey] 
  [:th {} (string (or 
            (get-in opts [:computed fkey :title])
            (get-in schema [:fields fkey :title])
            (get-in schema [:fields fkey :name])))])

(defn table-options? [opts] 
  (match opts 
    {
     :ord (o (indexed? o) (all keyword? o))
     :computed (c (dictionary? c))
     } true
    _ false))


(def- _table table)

(defn table [schema rows &keys opts]
  # We only have columns that are either on the core schema or computed
  (defn get-col [field-key] 
    (or 
      (get-in opts [:computed field-key])
      (get-in schema [:fields field-key])))

  (def is-col? get-col)
  (assert-all schema/has-schema? rows "Cannot render the non-praxis object %q!")
  (assert-all is-col? (opts :ord) "%q is not present on the schema or the computed columns!") 

  (def header [:tr (map (partial header-cell schema opts) (opts :ord))])

  (defn col-val [r fkey]
    (defn row-val [fk] (get-in r [:vals fk]))
    [:td (or 
           (as?-> (get-in opts [:computed fkey :fn]) fun
                  (fun row-val))
           (row-val fkey))])

  (def body (seq [r :in rows] 
              [:tr (map (partial col-val r) (opts :ord))]))

  [:table {} header ;body])
