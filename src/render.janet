(import ./praxis)
(import datex :as dt)
(import err)

(def render-form-fieldtypes @{})

(defmacro add-form-fieldtype [my/type & body] 
  ~(put render-form-fieldtypes ,my/type (fn [title name value my/err] 
                                          (def name (keyword name))
                                          ,;body)))

(defn- ok-or-nil [val] (match val [:ok val] val _ nil))

(defn- bool-render [val] (if val "true" "false"))

(add-form-fieldtype 
  :text
  [:div 
   [:label { :for name }
    [:textarea { :name name } value ]]
   (when my/err [:div {:class "form-error"} my/err])])

(add-form-fieldtype
  :string 
  [:div 
   [:label { :for name } 
    title
    [:input { :name name :type "text" :value value }]
    (when my/err [:div {:class "form-error"} my/err])]])

(add-form-fieldtype
  :number
  [:div
   [:label { :for name }
    [:input { :name name :type "number" :value value }]
    (when my/err [:div {:class "form-error" } my/err])]])

(add-form-fieldtype
  :integer
  [:div
  [:label { :for name }
   [:input { :name name :type "number" :value  value }]]
  (when my/err [:div {:class "form-error" } my/err])])

(add-form-fieldtype
  :bool
  [:div
   [:label { :for name }
    [:input { :name name :type "checkbox" :value name :checked (bool-render value) }]
    (when my/err [:div {:class "form-error" } my/err])]])

(add-form-fieldtype
  :date
  (defn to-ymdstr [value]
    (as?-> value it
          (os/date it)
          (dt/to-ymdstr it)))
  [:div
   [:label { :for name }
    [:input { :name name :type "date" :value (to-ymdstr value) :checked (bool-render value) }]
    (when my/err [:div {:class "form-error" } my/err])]])

(add-form-fieldtype
  :timestamp
  [:div
   [:label { :for name }
    [:input { :name name :type "datetime-local" :value value }]
    (when my/err [:div {:class "form-error" } my/err])]])


(defn form-fields [obj] 
  (unless (praxis/has-schema? obj)
    (err/str "Cannot render an object that wasn't built with a praxis schema"))
  (defn get-type [fkey] 
    (get-in obj [:schema :fields fkey :type]))
  (defn get-title [fkey] (get-in obj [:schema :fields fkey :title] fkey))
  (defn get-err [fkey] (get-in obj [:errs fkey]))
  (defn get-val [fkey] (get-in obj [:vals fkey]))
  
  (seq [fkey :in (obj :field-order)]
      (def ftype (get-type fkey))
      (def value (get-val fkey))
      (def title (get-title fkey))
      (def my/err (get-err fkey))
      (def render/fn (render-form-fieldtypes ftype))
      (unless render/fn 
        (err/str "Cannot render type " ftype "!"))
      (def output (render/fn title fkey value my/err))
      # (fn [title name value my/err] ,;body)))
      # TODO: Figure out how to fail out if a type is unsupported
      # (tracev output)
      # (tracev (get-in output [0 2]))
      output))

