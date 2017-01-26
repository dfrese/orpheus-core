(ns orpheus.html
  (:require [orpheus.core :as core]
            #?(:cljs [orpheus.impl.util :refer-macros (deftag)])
            #?(:clj [orpheus.impl.util :refer (deftag)]))
  (:refer-clojure :exclude [time]))

#?(:cljs
   (defn target-value "Return the `target.value` of an event"
     [^js/Event e]
     (.-value (.-target e))))

#?(:cljs
   (defn target-checked "Return the `target.checked` of an event." [^js/Event e]
     (.-checked (.-target e))))

;; TODO: keys, mouse positions/clicks?

(def ^:no-doc html-ns "http://www.w3.org/1999/xhtml")

(deftag h1 html-ns)
(deftag h2 html-ns)
(deftag h3 html-ns)
(deftag h4 html-ns)
(deftag h5 html-ns)
(deftag h6 html-ns)
(deftag div html-ns)
(deftag p html-ns)
(deftag hr html-ns)
(deftag pre html-ns)
(deftag blockquote html-ns)
(deftag span html-ns)
(deftag a html-ns)
(deftag code html-ns)
(deftag em html-ns)
(deftag strong html-ns)
(deftag i html-ns)
(deftag b html-ns)
(deftag u html-ns)
(deftag sub html-ns)
(deftag sup html-ns)
(deftag br html-ns)
(deftag ol html-ns)
(deftag ul html-ns)
(deftag li html-ns)
(deftag dl html-ns)
(deftag dt html-ns)
(deftag dd html-ns)
(deftag img html-ns)
(deftag iframe html-ns)
(deftag canvas html-ns)
(deftag form html-ns)
(deftag input html-ns)
(deftag textarea html-ns)
(deftag button html-ns)
(deftag select html-ns)
(deftag option html-ns)
(deftag section html-ns)
(deftag nav html-ns)
(deftag article html-ns)
(deftag aside html-ns)
(deftag header html-ns)
(deftag footer html-ns)
(deftag address html-ns)
(deftag main html-ns)
(deftag body html-ns)
(deftag figure html-ns)
(deftag figcaption html-ns)
(deftag table html-ns)
(deftag caption html-ns)
(deftag colgroup html-ns)
(deftag col html-ns)
(deftag tbody html-ns)
(deftag thead html-ns)
(deftag tfoot html-ns)
(deftag tr html-ns)
(deftag td html-ns)
(deftag th html-ns)
(deftag fieldset html-ns)
(deftag legend html-ns)
(deftag label html-ns)
(deftag datalist html-ns)
(deftag optgroup html-ns)
(deftag keygen html-ns)
(deftag output html-ns)
(deftag progress html-ns)
(deftag meter html-ns)
(deftag audio html-ns)
(deftag video html-ns)
(deftag source html-ns)
(deftag track html-ns)
(deftag embed html-ns)
(deftag object html-ns)
(deftag param html-ns)
(deftag ins html-ns)
(deftag del html-ns)
(deftag small html-ns)
(deftag cite html-ns)
(deftag dfn html-ns)
(deftag abbr html-ns)
(deftag time html-ns)
(deftag var html-ns)
(deftag samp html-ns)
(deftag kbd html-ns)
(deftag s html-ns)
(deftag q html-ns)
(deftag mark html-ns)
(deftag ruby html-ns)
(deftag rt html-ns)
(deftag rp html-ns)
(deftag bdi html-ns)
(deftag bdo html-ns)
(deftag wbr html-ns)
(deftag details html-ns)
(deftag summary html-ns)
(deftag menuitem html-ns)
(deftag menu html-ns)
