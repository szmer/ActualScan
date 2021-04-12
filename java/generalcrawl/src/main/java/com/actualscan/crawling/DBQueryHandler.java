/*
SPDX-License-Identifier: AGPL-3.0-or-later
Copyright (c) 2021 Szymon Rutkowski.
 */
package com.actualscan.crawling;

import java.sql.*;
import java.util.Properties;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

public class DBQueryHandler {
   protected String dbHost;
   protected String dbPort;
   protected String dbUsername;
   protected String dbPassword;

   private static final Logger logger = LogManager.getLogger(ScrapeRequestsSpout.class);

   public DBQueryHandler(String host, String port, String username, String password) {
      //System.exit(0);
      if (host != null) {
         this.dbHost = host;
      }
      else {
         this.dbHost = "localhost";
      }
      if (port != null) {
         this.dbPort = port;
      }
      else {
         this.dbPort = "5432";
      }
      if (username != null) {
         this.dbUsername = username;
      }
      else {
         this.dbUsername = "";
      }
      if (password != null) {
         this.dbPassword = password;
      }
      else {
         this.dbPassword = "";
      }
   }

   public void executeUpdate(String query) throws SQLException {
      String dbUrl = "jdbc:postgresql://" + dbHost + ":" + dbPort + "/";
      Properties props = new Properties();
      props.setProperty("user", dbUsername);
      props.setProperty("password", dbPassword);
      // TODO SSL
      Connection conn = DriverManager.getConnection(dbUrl, props);
      Statement st = conn.createStatement();
      st.executeUpdate(query);
      conn.close();
   }

   public Connection getConnection() throws SQLException {
      String dbUrl = "jdbc:postgresql://" + dbHost + ":" + dbPort + "/";
      Properties props = new Properties();
      props.setProperty("user", dbUsername);
      props.setProperty("password", dbPassword);
      // TODO SSL
      Connection conn = DriverManager.getConnection(dbUrl, props);
      return conn;
   }
}
