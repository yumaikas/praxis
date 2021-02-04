(import ./praxis)
(import err)

(def render-form-fieldtypes @{})

(defmacro add-form-fieldtype [my/type & body] 
  ~(put render-form-fieldtypes ,my/type (fn [title name value my/err] ,;body)))

(defn- ok-or-nil [val] (match val [:ok val] val _ nil))


(add-form-fieldtype
  :string 
  [:label { :for name } 
   title
   [:input { :name name :type "text" :value (ok-or-nil value) }]] 
   (when my/err [:div {:class "form-error"} my/err]))

(add-form-fieldtype 
  :text
  [:label { :for name }
   [:textarea { :name name } (ok-or-nil value) ]]
  (when my/err [:div {:class "form-error"} my/err]))

(add-form-fieldtype
  :integer
  [:label { :for name }
   [:input {:type "number" :value (ok-or-nil value) }]]
  (when my/err [:div {:class "form-error" } my/err]))

(add-form-fieldtype
  :number
  [:label { :for name }
   [:input {:type "number" :value (ok-or-nil value) }]
    (when my/err [:div {:class "form-error" } my/err])])

(defn form-fields [obj] 
  (unless (praxis/has-schema? obj)
    (err/str "Cannot render an object that wasn't built with a praxis schema"))
  (defn get-type [fkey] 
    (get-in obj [:schema :fields fkey :type]))
  (defn get-title [fkey]
    (get-in obj [:schema :fields fkey :title] fkey))
  (defn get-err [fkey]
    (get-in obj [:errs fkey]))
  (defn get-val [fkey] 
    (match [(get-in obj [:vals fkey]) (get-in obj [:errs fkey])]
      [val nil] [:ok val]
      [nil err] [:err err]
      [val err] [:err err val]))
  
  (pp obj)
  (seq [fkey :in (obj :field-order)
        :when (get-in obj [:vals fkey])]
      (def ftype (get-type fkey))
      (def value (get-val fkey))
      (def title (get-title fkey))
      (def my/err (get-err fkey))
      # (fn [title name value my/err] ,;body)))
      # TODO: Figure out how to fail out if a type is unsupported
      ((render-form-fieldtypes ftype) title fkey value my/err)))
