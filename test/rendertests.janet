(use ../praxis)
(use testament)
(import ../render :as r)
(import joy/html :as html)
(import golden-master :as gold)

(defschema Point
  (field :x :number)
  (field :y :number))

(gold/set-dir "test" "recording")

(defn ensure-clean-encoding [html] 
  (and 
    (not (string/find "<tuple 0x" html))))

(defsuite!

  (deftest render-is-clean-and-not-regressive
    (def p (cast :to Point :from {"x" "1" "y" "2"} :fields [:x :y]))
    (def output (html/encode (r/form-fields p)))
    (assert-expr (ensure-clean-encoding output) "Output contained a non-understood Janet type!")
    (gold/compare :text "PointRender" "CleanPoint.html" output))

  (deftest render-shows-error
    (def y-missing-err "Cannot define a point without a y coordinate")
    (def p (as-> (cast 
                   :to Point :from {"x" "1" "y" nil} 
                   :fields [:x :y]) it
                 (validate-required it :y y-missing-err)))
    (assert-matches  {:errs {:y y-missing-err }} p)
    (def output (html/encode (r/form-fields p)))
    (assert-expr (not (string/find y-missing-err output)) "Output should contain Y missing error!")
    (assert-expr (ensure-clean-encoding output) "Output contained a non-understood Janet type!")
    (gold/compare :text "PointRender" "ErrorPoint.html" output))

 )

(defschema Message
  (field :id :integer)
  (field :from :string)
  (field :to :string)
  (field :body :text)
  (field :updated :timestamp))
    
(defn new-message [&keys kwargs]
  (as-> (cast :to Message :from kwargs  :fields [:id :from :to :body :updated]) it
        (tracev (reduce |(do (validate-required $0 $1) $0) it [:to :from :body :updated]))
        (tracev it)
        ))

(defsuite!
  # Test is currently failing, this is on purpose
  (deftest msg-no-err
    (def msg (new-message :to "me" :from "tests" :body `This is a story all about how, I became a message, updated about now.` :updated "Now!"))
    (assert-matches  {:errs (e (= 0 (length (keys e))))} msg)))


