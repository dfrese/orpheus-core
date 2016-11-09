(ns orpheus.html
  (:require [orpheus.transformer :as t]
            [orpheus.dom :as dom])
  (:refer-clojure :exclude [time]))

#?(:cljs
   (def target-value ^{:doc "A handler, that returns the target.value"}
     (dom/event-handler (fn [e]
                          (.-value (.-target e))))))

#?(:cljs
   (def target-checked ^{:doc "A handler, that returns the `target.checked`."}
     (dom/event-handler (fn [e]
                          (.-checked (.-target e))))))

;; TODO: keys, mouse positions/clicks?

(def html-ns "http://www.w3.org/1999/xhtml")

(defn- v [name]
  (let [type (dom/element-type html-ns name nil)]
    (partial dom/h type)))

(def h1 (v "h1"))
(def h2 (v "h2"))
(def h3 (v "h3"))
(def h4 (v "h4"))
(def h5 (v "h5"))
(def h6 (v "h6"))
(def div (v "div"))
(def p (v "p"))
(def hr (v "hr"))
(def pre (v "pre"))
(def blockquote (v "blockquote"))
(def span (v "span"))
(def a (v "a"))
(def code (v "code"))
(def em (v "em"))
(def strong (v "strong"))
(def i (v "i"))
(def b (v "b"))
(def u (v "u"))
(def sub (v "sub"))
(def sup (v "sup"))
(def br (v "br"))
(def ol (v "ol"))
(def ul (v "ul"))
(def li (v "li"))
(def dl (v "dl"))
(def dt (v "dt"))
(def dd (v "dd"))
(def img (v "img"))
(def iframe (v "iframe"))
(def canvas (v "canvas"))
(def form (v "form"))
(def input (v "input"))
(def textarea (v "textarea"))
(def button (v "button"))
(def select (v "select"))
(def option (v "option"))
(def section (v "section"))
(def nav (v "nav"))
(def article (v "article"))
(def aside (v "aside"))
(def header (v "header"))
(def footer (v "footer"))
(def address (v "address"))
(def main (v "main"))
(def body (v "body"))
(def figure (v "figure"))
(def figcaption (v "figcaption"))
(def table (v "table"))
(def caption (v "caption"))
(def colgroup (v "colgroup"))
(def col (v "col"))
(def tbody (v "tbody"))
(def thead (v "thead"))
(def tfoot (v "tfoot"))
(def tr (v "tr"))
(def td (v "td"))
(def th (v "th"))
(def fieldset (v "fieldset"))
(def legend (v "legend"))
(def label (v "label"))
(def datalist (v "datalist"))
(def optgroup (v "optgroup"))
(def keygen (v "keygen"))
(def output (v "output"))
(def progress (v "progress"))
(def meter (v "meter"))
(def audio (v "audio"))
(def video (v "video"))
(def source (v "source"))
(def track (v "track"))
(def embed (v "embed"))
(def object (v "object"))
(def param (v "param"))
(def ins (v "ins"))
(def del (v "del"))
(def small (v "small"))
(def cite (v "cite"))
(def dfn (v "dfn"))
(def abbr (v "abbr"))
(def time (v "time"))
(def var (v "var"))
(def samp (v "samp"))
(def kbd (v "kbd"))
(def s (v "s"))
(def q (v "q"))
(def mark (v "mark"))
(def ruby (v "ruby"))
(def rt (v "rt"))
(def rp (v "rp"))
(def bdi (v "bdi"))
(def bdo (v "bdo"))
(def wbr (v "wbr"))
(def details (v "details"))
(def summary (v "summary"))
(def menuitem (v "menuitem"))
(def menu (v "menu"))
