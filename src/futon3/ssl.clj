(ns futon3.ssl
  "TLS/SSL utilities for httpkit. Loads Let's Encrypt PEM files into SSLContext."
  (:import [java.io FileInputStream BufferedReader StringReader]
           [java.security KeyStore KeyFactory]
           [java.security.cert CertificateFactory]
           [java.security.spec PKCS8EncodedKeySpec]
           [javax.net.ssl KeyManagerFactory TrustManagerFactory SSLContext]
           [java.util Base64]))

(defn- read-pem-content
  "Extract base64 content from PEM file, stripping headers/footers."
  [pem-path]
  (let [content (slurp pem-path)
        lines (clojure.string/split-lines content)]
    (->> lines
         (remove #(or (.startsWith % "-----BEGIN")
                      (.startsWith % "-----END")))
         (clojure.string/join ""))))

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

(defn- load-private-key
  "Load RSA private key from PEM file."
  [key-path]
  (let [pem-content (read-pem-content key-path)
        key-bytes (.decode (Base64/getDecoder) pem-content)
        key-spec (PKCS8EncodedKeySpec. key-bytes)
        kf (KeyFactory/getInstance "RSA")]
    (.generatePrivate kf key-spec)))

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
        kmf (KeyManagerFactory/getInstance "SunX509")
        _ (.init kmf ks (char-array ""))
        tmf (TrustManagerFactory/getInstance "SunX509")
        null-ks (cast KeyStore nil)
        _ (.init tmf null-ks)
        ctx (SSLContext/getInstance "TLS")]
    (.init ctx (.getKeyManagers kmf) (.getTrustManagers tmf) nil)
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
