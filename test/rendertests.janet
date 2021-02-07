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
    (not (string/find "<tuple 0x" html))
    (not (string/find "<struct 0x" html))
    (not (string/find "<table 0x" html))
    (not (string/find "<function 0x" html))
    (not (string/find "<array 0x" html))))


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
    (assert-expr (string/find y-missing-err output) "Output should contain Y missing error!")
    (assert-expr (ensure-clean-encoding output) "Output contained a non-understood Janet type!")
    (gold/compare :text "ErrorPoint" "ErrorPoint.html" output)))

(defschema Message
  (field :id :integer)
  (field :from :string)
  (field :to :string)
  (field :body :text)
  (field :updated :timestamp))
    
(defn new-message [&keys kwargs]
  (as-> (cast :to Message :from kwargs  :fields [:id :from :to :body :updated]) it
        (reduce |(do (validate-required $0 $1) $0) it [:to :from :body :updated])))

(defn- dict-empty? [dict] 
  (and (dictionary? dict)
       (= 0 (length (keys dict)))))

(defn- not-dict-empty? [dict] (not (dict-empty? dict)))

(defsuite!
  # Test is currently failing, this is on purpose
  (deftest msg-no-err
    (def msg (new-message 
               :to "me" 
               :from "tests" 
               :body `This is a story all about how, I became a message, updated about now.` 
               :updated "Now!"))
    (assert-matches {:errs (e (not-dict-empty? e))} msg)))



(defschema GameFile
  # An entirely fictional and likely impractical schema, acts as a catch-all for 
  (field :name :string :title "Name")
  (field :desc :text :title "Decription")
  (field :rating :number :title "Rating")
  (field :promotion-start-date :date :title "The date the download promo starts")
  (field :is-featured :bool :title "Will this download be featured on our front page?")
  (field :allowed-downloads :integer :title "Number allowed downloads per user")
  (field :allow-downloads-after :timestamp :title "The UTC timestamp for when the download is avaialble"))

(setdyn :pretty-format "%n")
(defsuite! 
  (def gf (as-> (cast
                  :to GameFile
                  :from { "name" "Raiders of Bythnia"
                         "desc" "A small top-down adventure where you save Bythnia from the Slimes!"
                         "rating" "3.7"
                         "promotion-start-date" "2021-3-1"
                         "is-featured" "false"
                         "allowed-downloads" "40"
                         "allow-downloads-after" "1612642033" }
                  :fields [
                           :name 
                           :desc 
                           :rating
                           :promotion-start-date 
                           :is-featured
                           :allowed-downloads 
                           :allow-downloads-after
                           ]
                 ) it
                #(validate-required it :name)
                it
                ))
  (deftest no-errors 
    (assert-matches { :errs (e (dict-empty? e)) } gf)
    # (gold/compare :text "PointRender" "ErrorPoint.html" output)
    (def hic (r/form-fields gf))
    (def output (html/encode hic))
    (assert-expr (ensure-clean-encoding output) "Output isn't clean!")
    (assert-equal true (gold/compare :text "GameFileRender" "GameFileRender.html" output)) ))

(defsuite!
  (def gf (empty-of GameFile))
  (deftest can-render-empty-obj-with-schema 
    # (pp (r/form-fields gf))
    (def output (html/encode (r/form-fields gf)))
    (assert-expr (ensure-clean-encoding output))
    (assert-equal true (gold/compare :text "GameFileRenderEmpty" "GameFileRenderEmpty.html" output))
    (assert-expr (not= nil output))))

