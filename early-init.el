;;; early-init.el -*- lexical-binding: t; -*-

;; Turn GC off during startup; gcmh takes over in init.el
(setq gc-cons-threshold most-positive-fixnum)

;; libgccjit (bundled, 14.3.0) emits -mmacosx-version-min=18.0, which this
;; machine's clang rejects.  Set the driver options explicitly: the macOS
;; default ("-Wl,-w") plus a valid min-version.  (Can't append here because
;; the variable is defined later, in comp.el.)
(setq native-comp-driver-options '("-Wl,-w" "-mmacosx-version-min=27.0"))
