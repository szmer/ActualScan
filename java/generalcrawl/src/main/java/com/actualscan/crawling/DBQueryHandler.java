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
      this.dbHost = host;
      this.dbPort = port;
      this.dbUsername = username;
      this.dbPassword = password;
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
