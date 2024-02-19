(ns donut.error
  (:require
   [malli.core :as m]
   [malli.dev.pretty :as pretty]
   [malli.dev.virhe :as v]
   [malli.error :as me]))

(defn -block [text body printer]
  [:group (v/-text (str text ":") printer)
   :break
   (into [:align 2]
         (map (fn [x] (if (string? x) (v/-text x printer) x))
              (if (sequential? body) body (vector body))))])

(defn- schema-explain-body
  [explanation printer]
  [(-block "Value" (v/-visit (me/error-value explanation printer) printer) printer)
   (-block "Errors" (v/-visit (me/humanize (me/with-spell-checking explanation)) printer) printer)
   (-block "Schema" (v/-visit (:schema explanation) printer) printer)])

(defn- donut-footer
  [{:keys [::url]} printer]
  (cond-> []
    url (conj (-block "More Information" (v/-visit url printer) printer))))

(defn- build-group
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

(defn validate-ex-info
  [schema x ex-data]
  (when-let [explanation (m/explain schema x)]
    (ex-info (str ::schema-validation-error)
             (merge
              {::id               ::schema-validation-error
               ::url              "https://donut.party/errors/#schema-validation-error"
               :explanation-human (me/humanize (me/with-spell-checking explanation))
               :explanation       explanation}
              ex-data))))

(defmacro validate!
  [schema x & [ex-opts]]
  `(when-let [ei# (validate-ex-info ~schema ~x ~ex-opts)]
     (tap> ei#)
     (throw ei#)))

(def reporter (delay (pretty/reporter)))

(defn error-tap
  [v]
  (when-let [{:keys [::id] :as data} (ex-data v)]
    (@reporter id data)))

#_(validate! int? "a")
