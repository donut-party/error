(ns donut.error
  (:require
   [malli.core :as m]
   [malli.dev.pretty :as pretty]
   [malli.dev.virhe :as v]
   [malli.error :as me])
  #?(:cljs
     (:require-macros [donut.error]))
  (:refer-clojure :exclude [format]))

(defn -block [text body printer]
  [:group (v/-text (str text ":") printer)
   :break
   (into [:align 2]
         (map (fn [x] (if (string? x) (v/-text x printer) x))
              (if (sequential? body) body (vector body))))])

(defn schema-explain-body
  [explanation printer]
  [(-block "Value" (v/-visit (me/error-value explanation printer) printer) printer)
   (-block "Errors" (v/-visit (me/humanize (me/with-spell-checking explanation)) printer) printer)
   (-block "Schema" (v/-visit (:schema explanation) printer) printer)])

(defn donut-footer
  [{:keys [::url]} printer]
  (cond-> []
    url (conj (-block "More Information" (v/-visit url printer) printer))))

(defn build-group
  [& blocks]
  (->> blocks
       (filter identity)
       (apply concat)
       (interpose [:group :break :break])
       (into [:group])))

(defmethod v/-format ::schema-validation-error [_ {:keys [explanation] :as data} printer]
  {:title "Schema Validation Error"
   :body (build-group
          (schema-explain-body explanation printer)
          (donut-footer data printer))})

(defn url
  [error-id]
  (str "https://donut.party/errors/#" error-id))

(defmacro validate!
  ([schema x]
   `(validate! ~schema ~x nil nil))
  ([schema x more-ex-data]
   `(validate! ~schema ~x nil ~more-ex-data))
  ([schema x error-msg more-ex-data]
   `(let [more-ex-data# ~more-ex-data
          explanation# (m/explain ~schema ~x)]
      (when explanation#
        (let [ei# (ex-info (or ~error-msg
                               (some-> more-ex-data# ::id str)
                               (str ::schema-validation-error))
                           (merge
                            {::id               ::schema-validation-error
                             ::url              (url ::schema-validation-error)
                             :explanation-human (me/humanize (me/with-spell-checking explanation#))
                             :explanation       explanation#}
                            more-ex-data#))]
          (tap> ei#)
          (throw ei#))))))

(defn format [e printer]
  (let [data (ex-data e)]
    (v/-format (ex-info (::id data) {:type (::id data)}) data printer)))

(defn exception-document [e printer]
  (let [{:keys [title body] :or {title (:title printer)}} (format e printer)
        location #?(:clj (v/-location e (:throwing-fn-top-level-ns-names printer)), :cljs nil)]
    (v/-section title location body printer)))

(defn -reporter
  ([] (-reporter (pretty/-printer)))
  ([printer]
   (fn [ex]
     (-> ex
         (exception-document printer)
         (v/-print-doc printer)
         #?(:cljs (-> with-out-str println))))))

(def reporter (delay (-reporter)))

(defn error-tap
  [v]
  (when (-> v ex-data ::id)
    (@reporter v)))
