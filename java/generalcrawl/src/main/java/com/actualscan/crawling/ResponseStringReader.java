package com.actualscan.crawling;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;

import com.digitalpebble.stormcrawler.Metadata;
import com.digitalpebble.stormcrawler.protocol.HttpHeaders;
import com.digitalpebble.stormcrawler.util.CharsetIdentification;
import org.apache.commons.lang.StringUtils;
import org.apache.tika.config.TikaConfig;
import org.apache.tika.detect.Detector;
import org.apache.tika.mime.MediaType;

public class ResponseStringReader {
   private Detector detector = TikaConfig.getDefaultConfig().getDetector();

   /**
    *      * Length of content to use for detecting the charset. Set to -1 to use the
    *           * full content (will make the parser slow), 0 to deactivate the detection
    *                * altogether, or any other value (at least a few hundred bytes).
    *                     **/
   private int maxLengthCharsetDetection = 2048;


   public String guessMimeType(String URL, String httpCT, byte[] content) {

      org.apache.tika.metadata.Metadata metadata = new org.apache.tika.metadata.Metadata();

      if (StringUtils.isNotBlank(httpCT)) {
         // pass content type from server as a clue
         metadata.set(org.apache.tika.metadata.Metadata.CONTENT_TYPE, httpCT);
      }

      // use full URL as a clue
      metadata.set(org.apache.tika.metadata.Metadata.RESOURCE_NAME_KEY, URL);

      metadata.set(org.apache.tika.metadata.Metadata.CONTENT_LENGTH,
            Integer.toString(content.length));

      try (InputStream stream = new ByteArrayInputStream(content)) {
         MediaType mt = detector.detect(stream, metadata);
         return mt.toString();
      } catch (IOException e) {
         throw new IllegalStateException("Unexpected IOException", e);
      }
   }

   public String responseText(byte[] content, String url, Metadata metadata) throws IOException {
      // This is adapted from Stormcrawler's JSoupParserBolt.
      // check that its content type is HTML
      // look at value found in HTTP headers
      boolean CT_OK = false;

      // originally the default here was this.protocolMDprefix
      String mimeType = metadata.getFirstValue(HttpHeaders.CONTENT_TYPE, "");

      try {
         mimeType = guessMimeType(url, mimeType, content);
      } catch (Exception e) {
         String errorMessage = "Exception while guessing mimetype on "
            + url + ": " + e;
         throw new IOException(errorMessage);
      }
      // store identified type in md
      metadata.setValue("parse.Content-Type", mimeType);

      if (StringUtils.isNotBlank(mimeType)) {
         if (mimeType.toLowerCase().contains("html")) {
            CT_OK = true;
         }
      }
      // go ahead even if no mimetype is available
      else {
         CT_OK = true;
      }

      String charset = CharsetIdentification.getCharset(metadata, content,
                            maxLengthCharsetDetection);
      String html = Charset.forName(charset)
                             .decode(ByteBuffer.wrap(content)).toString();
      return html;
   }
}
