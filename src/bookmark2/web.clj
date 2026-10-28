(ns bookmark2.web
  "Page titles and opening links, with the JDK's own HTTP client and Desktop."
  (:require [clojure.string :as str])
  (:import (java.awt Desktop Desktop$Action)
           (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers)
           (java.time Duration)))

(def title-timeout (Duration/ofSeconds 8))
(def user-agent "Mozilla/5.0 (bookmark2)")

(def ^:private entities
  {"amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'" "nbsp" " "
   "mdash" "—" "ndash" "–" "middot" "·" "hellip" "…"
   "lsquo" "‘" "rsquo" "’" "ldquo" "“" "rdquo" "”"})

(defn unescape-html
  "&amp; -> &, &#39; -> ', &#x2014; -> the dash. Unknown names stay as written."
  [s]
  (str/replace s #"&(#[xX][0-9a-fA-F]+|#\d+|[a-zA-Z]+);"
               (fn [[whole code]]
                 (cond
                   (re-matches #"#[xX].*" code) (String. (Character/toChars (Integer/parseInt (subs code 2) 16)))
                   (str/starts-with? code "#") (String. (Character/toChars (Integer/parseInt (subs code 1))))
                   :else (get entities code whole)))))

(defn fallback-title
  "The url's last path segment: https://x.com/a/b-c/ -> b-c"
  [url]
  (let [tail (last (str/split (str/replace url #"/+$" "") #"/"))]
    (if (str/blank? tail) url tail)))

(defn fetch-title
  "The page's <title>, or the fallback when the fetch gives nothing."
  [url]
  (try
    (let [client (-> (HttpClient/newBuilder)
                     (.followRedirects HttpClient$Redirect/NORMAL)
                     (.connectTimeout title-timeout)
                     .build)
          req (-> (HttpRequest/newBuilder (URI/create url))
                  (.timeout title-timeout)
                  (.header "User-Agent" user-agent)
                  .GET
                  .build)
          body (.body (.send client req (HttpResponse$BodyHandlers/ofString)))
          title (some->> body (re-find #"(?is)<title[^>]*>(.*?)</title>") second
                         (#(str/replace % #"\s+" " ")) unescape-html str/trim)]
      (if (str/blank? title) (fallback-title url) title))
    (catch Exception _ (fallback-title url))))

(defn open-url!
  "Open in the default browser. Desktop where it can browse, else xdg-open."
  [url]
  (if (and (Desktop/isDesktopSupported)
           (.isSupported (Desktop/getDesktop) Desktop$Action/BROWSE))
    (.browse (Desktop/getDesktop) (URI. url))
    (.exec (Runtime/getRuntime) ^"[Ljava.lang.String;" (into-array String ["xdg-open" url]))))
