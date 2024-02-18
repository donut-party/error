(ns donut.whoops
  (:require
   [malli.core :as m]
   [malli.dev.virhe :as v]
   [malli.error :as me]))

(defn -block [text body printer]
  [:group (v/-text text printer)
   :break
   #_:break
   (into [:align 2]
         (map (fn [x] (if (string? x) (v/-text x printer) x))
              (if (sequential? body) body (vector body))))])

(defn- schema-explain-body
  [explanation printer]
  [(-block "Value:" (v/-visit (me/error-value explanation printer) printer) printer)
   (-block "Errors:" (v/-visit (me/humanize (me/with-spell-checking explanation)) printer) printer)
   (-block "Schema:" (v/-visit (:schema explanation) printer) printer)])

(defn- donut-footer
  [{:keys [docs-url]} printer]
  (when docs-url
    [(-block "Docs URL" (v/-visit docs-url printer) printer)]))

(defn- build-group
  [& blocks]
  (->> blocks
       (filter identity)
       (apply concat)
       (interpose [:group :break :break "---" :break :break])
       (into [:group])))

(defmethod v/-format ::invalid-schema [_ {:keys [explanation] :as data} printer]
  {:title "Validation Error"
   :body (build-group
          (schema-explain-body explanation printer)
          (donut-footer data printer))})

(defn validate-ex-info
  [schema x ex-opts]
  (when-let [explanation (m/explain schema x)]
    (ex-info (str ::invalid-schema)
             {:type              ::invalid-schema
              :explanation-human (me/humanize (me/with-spell-checking explanation))
              :explanation       explanation})))

(defn validate!
  [schema x ex-opts]
  (some-> (validate-ex-info schema x ex-opts)
          throw))
