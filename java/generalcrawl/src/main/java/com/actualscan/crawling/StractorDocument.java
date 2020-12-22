package com.actualscan.crawling;

import java.util.ArrayList;

public class StractorDocument {
   // the fields that we may/shold receive from Speechtractor:
   public String author;
   public String text;
   public String datePost;
   public String url;
   public boolean isSearch;
   // these are meant to be filled by the client:
   public String dateRetr;
   public String siteName;
   public String realDoc;
   public String[] tags;
}
