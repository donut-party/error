(ns donut.error
  (:require
   [malli.core :as m]
   [malli.error :as me])
  #?(:cljs (:require-macros [donut.error :refer [validate!]])))

(defn url
  [error-id]
  (str "https://donut.party/errors/#" error-id))

#?(:clj
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
             (throw ei#)))))))
