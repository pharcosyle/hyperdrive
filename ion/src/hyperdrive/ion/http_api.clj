(ns hyperdrive.ion.http-api
  (:require [clojure.data.json :as json]
            [hyperdrive.queries :as q]
            [hyperdrive.ion.shopify-integration :as shopify])
  (:import [java.util Base64]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(defn- hmac [secret s]
  (let [hmac-sha1 "HmacSHA256"
        mac (Mac/getInstance hmac-sha1)
        secret-key (SecretKeySpec. (.getBytes secret) hmac-sha1)]
    (as-> (doto mac (.init secret-key)) $
      (.doFinal $ (.getBytes s))
      (.encodeToString (Base64/getEncoder) $))))

(defn- secure-eq?
  "Test whether two sequences of characters or bytes are equal in a way that protects against timing attacks. Note that this does not prevent an attacker from discovering the *length* of the data being compared."
  [a b]
  (let [a (map int a)
        b (map int b)]
    (if (and a b (= (count a) (count b)))
      (zero? (reduce bit-or (map bit-xor a b)))
      false)))

(defn shopify-webhooks [req]
  (if-let [secret (when (q/get-setting :settings/shopify-enabled?)
                    (q/get-setting :settings/shopify-shared-secret))]
    (if (secure-eq? (get-in req [:headers "x-shopify-hmac-sha256"]) (hmac secret (:body req)))
      (do
        (shopify/handle-webhook! (get-in req [:headers "x-shopify-topic"])
                                 (json/read-str (:body req) :key-fn keyword))
        {:status 200})
      {:status 401})
    {:status 200}))
