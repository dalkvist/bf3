(defproject bf3 "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [cheshire "2.0.6"]
                 [clj-http "0.2.7"]
                 [noir "1.2.2"]
                 [congomongo "0.1.7"]
                 [org.clojure/core.memoize "0.5.1"]
                 [org.clojure/core.cache "0.5.0"]
                 [paddleguru/enlive "1.2.0-alpha1"]
                 [clj-time "0.3.7"]
                 [net.sourceforge.htmlunit/htmlunit "2.9"
                            :exclusions [javax.mail/mail
                                         javax.jms/jms
                                         com.sun.jdmk/jmxtools
                                         com.sun.jmx/jmxri
                                         org.seleniumhq.selenium/selenium-firefox-driver
                                         org.seleniumhq.selenium/selenium-htmlunit-driver
                                         org.seleniumhq.selenium/selenium-ie-driver
                                         org.slf4j/slf4j-api
                                         org.slf4j/slf4j-log4j12
                                         log4j
                                         junit
                                         jfree
                                         gsbase
                                         commons-fileupload
                                         ]]
                 [clj-stacktrace "0.2.4"]]
  :repositories {"HtmlUnitSnapshots"
                 "http://htmlunit.sourceforge.net/m2-repo-snapshots"}
  :main bf3.core
  :jvm-opts ["-Xms350m" "-Xmx350m" "-server" "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSIncrementalMode" "-XX:+UseCompressedOops"
             "-XX:+DoEscapeAnalysis" "-XX:+UseBiasedLocking"
             "-XX:PermSize=64M" "-XX:MaxPermSize=85M"])
