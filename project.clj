(defproject com.wagjo/diff-cljs "0.2.1-SNAPSHOT"
  :description "Diff for ClojureScript sequences."
  :url "https://github.com/wagjo/diff-cljs"
  :scm {:name "git" :url "https://github.com/wagjo/diff-cljs"}
  :signing {:gpg-key "AFA4E115"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :source-paths ["src/cljs"]
  :dependencies [[com.wagjo/data-cljs "0.2.0"]]
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as ClojureScript"})
