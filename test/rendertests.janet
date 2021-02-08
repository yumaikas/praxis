(use ../src/praxis)
(use testament)
(import janet-html :as html)
(import golden-master :as gold)

(gold/set-dir "test" "recording")

(s/defschema Point
             (s/field :x :number)
             (s/field :y :number))

(s/defschema Message
             (s/field :id :integer)
             (s/field :from :string)
             (s/field :to :string)
             (s/field :body :text)
             (s/field :updated :timestamp))

(defn- new-message [&keys kwargs]
  (as-> (s/cast :to Message :from kwargs  :fields [:id :from :to :body :updated]) it
        (reduce |(do (s/validate-required $0 $1) $0) it [:to :from :body :updated])))

(defn- dict-empty? [dict] 
  (and (dictionary? dict)
       (= 0 (length (keys dict)))))

(defn- not-dict-empty? [dict] (not (dict-empty? dict)))

(defn ensure-clean-encoding [html] 
  (and 
    (not (string/find "<tuple 0x" html))
    (not (string/find "<struct 0x" html))
    (not (string/find "<table 0x" html))
    (not (string/find "<function 0x" html))
    (not (string/find "<array 0x" html))))

(s/defschema GameFile
             # An entirely fictional and likely impractical schema, acts as a catch-all for 
             (s/field :name :string :title "Name")
             (s/field :desc :text :title "Decription")
             (s/field :rating :number :title "Rating")
             (s/field :promotion-start-date :date 
                      :title "The date the download promo starts")
             (s/field :is-featured :bool 
                      :title "Will this download be featured on our front page?")
             (s/field :allowed-downloads :integer 
                      :title "Number allowed downloads per user")
             (s/field :allow-downloads-after :timestamp 
                      :title "The UTC timestamp for when the download is avaialble"))
(exercise! 
  []
  (deftest can-render-table
    (def rows (map |(s/cast :to Point :from $ :fields [:x :y]) [
               {"x" 1 "y" 2}
               {"x" 4 "y" 8}
               {"x" -3 "y" 2} ]))
    (def tbl-hic 
      (r/table 
        Point rows :ord [:x :y :mag]
        :computed 
        { :mag
         {:title "Vector Magnitude"
          :fn (fn [r] 
                (math/sqrt 
                  (+ (math/pow (r :x) 2) (math/pow (r :y) 2))))}}))

    (def output (html/encode tbl-hic))
    (assert-expr (ensure-clean-encoding output) 
                 "Output contained a non-understood Janet type!")
    (gold/compare :text "PointTable" "PointTable.html" output))

  (deftest render-is-clean-and-not-regressive
    (def p (s/cast :to Point :from {"x" "1" "y" "2"} :fields [:x :y]))
    (def output (html/encode (r/form-fields p)))
    (assert-expr (ensure-clean-encoding output) 
                 "Output contained a non-understood Janet type!")
    (gold/compare :text "PointRender" "CleanPoint.html" output))

  (deftest render-shows-error
    (def y-missing-err "Cannot define a point without a y coordinate")
    (def p (as-> (s/cast 
                   :to Point :from {"x" "1" "y" nil} 
                   :fields [:x :y]) it
                 (s/validate-required it :y y-missing-err)))
    (assert-matches  {:errs {:y y-missing-err }} p)
    (def output (html/encode (r/form-fields p)))
    (assert-expr (string/find y-missing-err output) 
                 "Output should contain Y missing error!")
    (assert-expr (ensure-clean-encoding output) 
                 "Output contained a non-understood Janet type!")
    (gold/compare :text "ErrorPoint" "ErrorPoint.html" output))


  # Test is currently failing, this is on purpose
  (deftest msg-no-err
    (def msg (new-message 
               :to "me" 
               :from "tests" 
               :body `This is a story all about how, I became a message, updated about now.` 
               :updated "Now!"))

    (assert-matches {:errs (e (not-dict-empty? e))} msg))


  (setdyn :pretty-format "%n")
  (def gf (as-> (s/cast
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
    (assert-equal true (gold/compare :text "GameFileRender" "GameFileRender.html" output)))

  (deftest can-render-empty-obj-with-schema 
    (def gf (s/empty-of GameFile))
    # (pp (r/form-fields gf))
    (def submit-txt "")
    (def output (html/encode (r/form gf :action "/CreateGameFile" :submit-txt "Create Game Submission!" )))
    (assert-expr (ensure-clean-encoding output))
    (assert-equal true (gold/compare :text "GameFileRenderEmpty" "GameFileRenderEmpty.html" output))
    (assert-expr (not= nil output)))
)

