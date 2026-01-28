(ns futon3.ssl
  "TLS/SSL utilities for httpkit. Loads Let's Encrypt PEM files into SSLContext."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.io FileInputStream BufferedReader StringReader]
           [java.security KeyStore KeyFactory]
           [java.security.cert CertificateFactory]
           [java.security.spec PKCS8EncodedKeySpec]
           [javax.net.ssl KeyManagerFactory SSLContext]
           [java.util Base64]))

(defn- read-pem-content
  "Extract base64 content from PEM file, stripping headers/footers."
  [pem-path]
  (let [content (slurp pem-path)
        lines (str/split-lines content)]
    (->> lines
         (remove #(or (.startsWith % "-----BEGIN")
                      (.startsWith % "-----END")))
         (str/join ""))))

(defn- load-certificate
  "Load X.509 certificate from PEM file."
  [cert-path]
  (let [cf (CertificateFactory/getInstance "X.509")]
    (with-open [is (FileInputStream. cert-path)]
      (.generateCertificate cf is))))

(defn- load-certificates
  "Load certificate chain from PEM file (may contain multiple certs)."
  [cert-path]
  (let [cf (CertificateFactory/getInstance "X.509")]
    (with-open [is (FileInputStream. cert-path)]
      (vec (.generateCertificates cf is)))))

(defn- convert-pkcs1-to-pkcs8
  "Convert PKCS#1 PEM key to PKCS#8 format using openssl."
  [key-path]
  (let [result (shell/sh "openssl" "pkcs8" "-topk8" "-nocrypt"
                                       "-in" key-path)]
    (if (zero? (:exit result))
      (:out result)
      (throw (ex-info "Failed to convert private key"
                      {:exit (:exit result) :err (:err result)})))))

(defn- read-pkcs8-pem
  "Extract base64 content from PKCS#8 PEM string."
  [pem-string]
  (->> (str/split-lines pem-string)
       (remove #(or (.startsWith % "-----BEGIN")
                    (.startsWith % "-----END")))
       (str/join "")))

(defn- load-private-key
  "Load private key from PEM file. Handles RSA and EC keys in PKCS#1 or PKCS#8 format."
  [key-path]
  (let [content (slurp key-path)]
    (cond
      ;; PKCS#1 RSA format - convert to PKCS#8 using openssl
      (.contains content "BEGIN RSA PRIVATE KEY")
      (let [pkcs8-pem (convert-pkcs1-to-pkcs8 key-path)
            pem-content (read-pkcs8-pem pkcs8-pem)
            key-bytes (.decode (Base64/getDecoder) pem-content)
            kf (KeyFactory/getInstance "RSA")]
        (.generatePrivate kf (PKCS8EncodedKeySpec. key-bytes)))

      ;; EC key format - convert to PKCS#8 using openssl
      (.contains content "BEGIN EC PRIVATE KEY")
      (let [pkcs8-pem (convert-pkcs1-to-pkcs8 key-path)
            pem-content (read-pkcs8-pem pkcs8-pem)
            key-bytes (.decode (Base64/getDecoder) pem-content)
            kf (KeyFactory/getInstance "EC")]
        (.generatePrivate kf (PKCS8EncodedKeySpec. key-bytes)))

      ;; PKCS#8 format - detect algorithm from key bytes
      (.contains content "BEGIN PRIVATE KEY")
      (let [pem-content (read-pem-content key-path)
            key-bytes (.decode (Base64/getDecoder) pem-content)]
        ;; Try EC first (more common with Let's Encrypt now), fall back to RSA
        (try
          (.generatePrivate (KeyFactory/getInstance "EC")
                            (PKCS8EncodedKeySpec. key-bytes))
          (catch Exception _
            (.generatePrivate (KeyFactory/getInstance "RSA")
                              (PKCS8EncodedKeySpec. key-bytes)))))

      :else
      (throw (ex-info "Unsupported private key format" {:path key-path})))))

(defn make-ssl-context
  "Create SSLContext from Let's Encrypt PEM files.

   cert-path: path to fullchain.pem (or cert.pem)
   key-path:  path to privkey.pem

   Example:
     (make-ssl-context \"/etc/letsencrypt/live/domain/fullchain.pem\"
                       \"/etc/letsencrypt/live/domain/privkey.pem\")"
  [cert-path key-path]
  (let [certs (load-certificates cert-path)
        private-key (load-private-key key-path)
        ks (KeyStore/getInstance "PKCS12")
        _ (.load ks nil nil)
        _ (.setKeyEntry ks "server"
                        private-key
                        (char-array "")
                        (into-array java.security.cert.Certificate certs))
        kmf (KeyManagerFactory/getInstance (KeyManagerFactory/getDefaultAlgorithm))
        _ (.init kmf ks (char-array ""))
        ctx (SSLContext/getInstance "TLS")]
    ;; For a server, we don't need client cert verification, so pass nil for trust managers
    (.init ctx (.getKeyManagers kmf) nil nil)
    ctx))

(defn ssl-context-from-letsencrypt
  "Convenience fn for Let's Encrypt cert paths.

   domain: the domain name as it appears in /etc/letsencrypt/live/

   Example:
     (ssl-context-from-letsencrypt \"172-236-28-208.ip.linodeusercontent.com\")"
  [domain]
  (let [base (str "/etc/letsencrypt/live/" domain)]
    (make-ssl-context (str base "/fullchain.pem")
                      (str base "/privkey.pem"))))
