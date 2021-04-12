/*
SPDX-License-Identifier: AGPL-3.0-or-later
Copyright (c) 2021 Szymon Rutkowski.
 */
package com.actualscan.crawling;

import com.digitalpebble.stormcrawler.Metadata;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

@ExtendWith(MockitoExtension.class)
public class ResponseStringReaderTest {
    String testString = "SL'Assemblée nationale voulant établir la Constitution française sur les principes qu'elle"
            +"vient de reconnaître et de déclarer, abolit irrévocablement les institutions qui blessaient la liberté"
            +"et l'égalité des droits. ";
    String testUrl = "http://example.com";
    Metadata testMetadata = new Metadata();

    @Test
    public void testSimpleUTFBytesToString() throws IOException {
        byte[] bytes = testString.getBytes(StandardCharsets.UTF_8);
        ResponseStringReader respReader = new ResponseStringReader();
        assertEquals(respReader.responseText(bytes, testUrl, testMetadata), testString);
    }

    @Test
    public void testDeclaredISOBytesToString() throws IOException {
        String testPage = "<html><head><meta charset='ISO-8859-1'></head>"+testString+"</html>";
        byte[] bytes = testPage.getBytes(StandardCharsets.ISO_8859_1);
        ResponseStringReader respReader = new ResponseStringReader();
        assertEquals(respReader.responseText(bytes, testUrl, testMetadata), testPage);
    }
}