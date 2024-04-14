* Usage

#+begin_src shell
npm start
#+end_src

* TODOs

** Remove Electron IPC and use [[https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API][WebSockets API]].

** Write a function for each of the following steps.

#+begin_quote
1. [ ] Emacs State -> Peekable (Propertied) Buffer String and Overlays
2. [ ]             -> Json String
3. [ ]             -> Json String [via WebSocket]
4. [ ]             -> String, Properties, Overlays (and print out onto the end of the webpage)
5. [ ]             -> Presentable Characters
6. [ ]             -> Document Operations
7. [ ]             -> Effects
#+end_quote

** Page Starts: Start Listening to Emacs WebSocket Server.

Show the live state of connection in the webpage.

** Refresh State Upon Button Clicks

** Keypress Effects

1. Print it on a div in the webpage with timestamp.
2. Print it in emacs \*Message\* buffer with timestamp.
