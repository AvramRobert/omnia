(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting)
  (require [lanterna.screen :as s]))


;; The idea is to avoid reprocessing past seekers and just redisplay them in an already processed version.
;; Or is it possible to refresh only part of the screen? That would be a good solution.
;; There's a delta refresh type. Perhaps I can use that? I must however implement it.
;; And add it to my fork of laterna and rebuild. Fun times.

;; The last resort would be to write a screen of my own that actually controls the redrawing process.
;; I have to however figure out how it might be able to to this.

;; First however try to see if the DELTA refresh type would be appropriate.
;; Otherwise think about adding this additional level of indirection through my own `screen`.
;; The simplest solution is to simply store maintain a history of all the response seekers and always redraw them. => inefficient.

;; If I avoid using `clear!` and define an invisible box binds the input to it, then `redraw` will do exactly what I want.
;; No it won't.
