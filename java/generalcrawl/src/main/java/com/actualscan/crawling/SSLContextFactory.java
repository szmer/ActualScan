package com.actualscan.crawling;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.SecureRandom;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;

class SSLContextFactory {
   String keystorePath;
   String keystorePassword;

   public SSLContextFactory(String keystorePath, String keystorePassword) {
      this.keystorePath = keystorePath;
      this.keystorePassword = keystorePassword;
   }

   public SSLContext get()
         throws FileNotFoundException, GeneralSecurityException, IOException  {
      KeyStore keystore = KeyStore.getInstance("PKCS12");
      keystore.load(new FileInputStream(keystorePath), keystorePassword.toCharArray());
      KeyManagerFactory keyManagerFactory =
         KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
      keyManagerFactory.init(keystore, keystorePassword.toCharArray());
      TrustManagerFactory trustManagerFactory =
         TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
      trustManagerFactory.init(keystore);
      SSLContext context = SSLContext.getInstance("TLS");
      context.init(
            keyManagerFactory.getKeyManagers(),
            trustManagerFactory.getTrustManagers(),
            new SecureRandom());
      return context;
   }
}
