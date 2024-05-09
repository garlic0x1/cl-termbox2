(defpackage :termbox2
  (:use :cl :cffi)
  (:export :tb-event*
           :tb-event
           :tb-event-type
           :tb-event-mod
           :tb-event-key
           :tb-event-ch
           :tb-event-w
           :tb-event-h
           :tb-event-x
           :tb-event-y
           :tb-init
           :tb-shutdown
           :tb-width
           :tb-height
           :tb-clear
           :tb-set-clear-attrs
           :tb-present
           :tb-invalidate
           :tb-set-cursor
           :tb-hide-cursor
           :tb-set-cell
           :tb-set-cell-ex
           :tb-extend-cell
           :tb-peek-event*
           :tb-poll-event*
           :tb-peek-event
           :tb-poll-event
           :tb-get-fds
           :tb-set-input-mode
           :tb-set-output-mode
           :tb-print
           :tb-print-ex
           :tb-printf
           :tb-send
           :tb-set-func
           :tb-utf8-char-length))
(in-package :termbox2)

(use-foreign-library "libtermbox2.so")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;; Structure bindings ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcstruct tb-event*
  (type :uint8)
  (mod  :uint8)
  (key  :uint16)
  (ch   :uint32)
  (w    :uint32)
  (h    :uint32)
  (x    :uint32)
  (y    :uint32))

(defstruct tb-event
  type
  mod
  key
  ch
  w
  h
  x
  y)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ;;;
;;; Function bindings ;;;
;;;                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("tb_init" tb-init) :int
  "Initializes the termbox library. This function should be called before any
other functions. tb_init() is equivalent to tb_init_file(\"/dev/tty\"). After
successful initialization, the library must be finalized using the
tb_shutdown() function.")
(defcfun ("tb_shutdown" tb-shutdown) :int)

(defcfun ("tb_width" tb-width) :int
  "Returns the width of the internal back buffer (which is the same as
terminal's window size in columns). The internal buffer can be resized after
tb_clear() or tb_present() function calls. Both dimensions have an
unspecified negative value when called before tb_init() or after
tb_shutdown().")
(defcfun ("tb_height" tb-height) :int
  "Returns the height of the internal back buffer (which is the same as
terminal's window size in rows). The internal buffer can be resized after
tb_clear() or tb_present() function calls. Both dimensions have an
unspecified negative value when called before tb_init() or after
tb_shutdown().")

(defcfun ("tb_clear" tb-clear) :int
  "Clears the internal back buffer using TB_DEFAULT color or the
color/attributes set by tb_set_clear_attrs() function.")
(defcfun ("tb_set_clear_attrs" tb-set-clear-attrs) :int
  (fg :uint64)
  (bg :uint64))
(defcfun ("tb_present" tb-present) :int
  "Synchronizes the internal back buffer with the terminal by writing to tty.")
(defcfun ("tb_invalidate") :int
  "Clears the internal front buffer effectively forcing a complete re-render
of the back buffer to the tty. It is not necessary to call this under normal
circumstances.")

(defcfun ("tb_set_cursor" tb-set-cursor) :int
  "Sets the position of the cursor. Upper-left character is (0, 0)."
  (cx :int)
  (cy :int))
(defcfun ("tb-hide-cursor" tb-hide-cursor) :int)

(defcfun ("tb_set_cell" tb-set-cell) :int
  "Set cell contents in the internal back buffer at the specified position.

Use tb_set_cell_ex() for rendering grapheme clusters (e.g., combining
diacritical marks).

Function tb_set_cell(x, y, ch, fg, bg) is equivalent to
tb_set_cell_ex(x, y, &ch, 1, fg, bg).

Function tb_extend_cell() is a shortcut for appending 1 codepoint to
cell->ech."
  (x :int)
  (y :int)
  (ch :uint32)
  (fg :uint64)
  (bg :uint64))
(defcfun ("tb_set_cell_ex" tb-set-cell-ex) :int
  (x :int)
  (y :int)
  (ch (:pointer :uint32))
  (nch :uint32)
  (fg :uint64)
  (bg :uint64))
(defcfun ("tb_extend_cell" tb-extend-cell) :int
  (x :int)
  (y :int)
  (ch :uint32))

(defcfun ("tb_peek_event" tb-peek-event*) :int
  (event :pointer)
  (timeout :int))
(defcfun ("tb_poll_event" tb-poll-event*) :int
  (event :pointer))
(defcfun ("tb_get_fds" tb-get-fds) :int
  "Internal termbox FDs that can be used with poll() / select(). Must call
tb_poll_event() / tb_peek_event() if activity is detected."
  (ttyfd (:pointer :int))
  (resizefd (:pointer :int)))

(defcfun ("tb_set_input_mode" tb-set-input-mode) :int)
(defcfun ("tb_set_output_mode" tb-set-output-mode) :int)

(defcfun ("tb_print" tb-print) :int
  "Incomplete trailing UTF-8 byte sequences are replaced with U+FFFD.
For finer control, use tb_set_cell()."
  (x :int)
  (y :int)
  (fg :uint64)
  (bg :uint64)
  (str :string))
(defcfun ("tb_print_ex" tb-print-ex) :int
  "Same as tb_print() except with out_w to determine width of printed."
  (x :int)
  (y :int)
  (fg :uint64)
  (bg :uint64)
  (out-w :pointer)
  (str :string))

(defcfun ("tb_send" tb-send) :int
  "Send raw bytes to terminal"
  (buf :string)
  (nbuf :uint32))

(defcfun ("tb_set_func" tb-set-func) :int
  "Set custom functions. fn_type is one of TB_FUNC_* constants, fn is a
compatible function pointer, or NULL to clear.

TB_FUNC_EXTRACT_PRE:
  If specified, invoke this function BEFORE termbox tries to extract any
  escape sequences from the input buffer.

TB_FUNC_EXTRACT_POST:
  If specified, invoke this function AFTER termbox tries (and fails) to
  extract any escape sequences from the input buffer."
  (fn-type :int)
  (fn :pointer))

(defcfun ("tb_utf8_char_length" tb-utf8-char-length) :int
  "Return byte length of codepoint given first byte of UTF-8 sequence (1-6)."
  (c :char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      ;;;
;;; Convenience wrappers ;;;
;;;                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-tb-event (ev)
  (with-foreign-slots ((type mod key ch w h x y) ev (:struct tb-event*))
    (make-tb-event :type type :mod mod :key key :ch ch :w w :h h :x x :y y)))

(defun tb-poll-event ()
  "Same as tb_peek_event except no timeout."
  (with-foreign-object (ev '(:struct tb-event*))
    (tb-poll-event* ev)
    (translate-tb-event ev)))

(defun tb-peek-event (timeout)
  "Wait for an event up to timeout_ms milliseconds and fill the event
structure with it. If no event is available within the timeout period,
TB_ERR_NO_EVENT is returned. On a resize event, the underlying select(2)
call may be interrupted, yielding a return code of TB_ERR_POLL.
In this case, you may check errno via tb_last_errno(). If it's EINTR,
you can safely ignore that and call tb_peek_event() again."
  (with-foreign-object (ev '(:struct tb-event*))
    (tb-peek-event* ev timeout)
    (translate-tb-event ev)))

(defun tb-printf (x y fg bg control &rest rest)
  "Same as tb_print() except using a Lisp format string.
Note: This is not the C function, it does not use C formatting."
  (tb-print x y fg bg (apply #'format (append (list nil control) rest))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;; Constant values ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro export-constants (&body consts)
  `(progn
     ,@(mapcar (lambda (const)
                 `(progn (defconstant ,(first const) ,(second const) ,(third const))
                         (export (quote ,(first const)))))
               consts)))

(defmacro export-vars (&body vars)
  `(progn
     ,@(mapcar (lambda (var)
                 `(progn (defvar ,(first var) ,(second var) ,(third var))
                         (export (quote ,(first var)))))
               vars)))

; ASCII key constants (tb_event.key)
(export-constants
  (+TB-KEY-CTRL-TILDE+       #x00)
  (+TB-KEY-CTRL-2+           #x00 "clash with 'CTRL_TILDE'")
  (+TB-KEY-CTRL-A+           #x01)
  (+TB-KEY-CTRL-B+           #x02)
  (+TB-KEY-CTRL-C+           #x03)
  (+TB-KEY-CTRL-D+           #x04)
  (+TB-KEY-CTRL-E+           #x05)
  (+TB-KEY-CTRL-F+           #x06)
  (+TB-KEY-CTRL-G+           #x07)
  (+TB-KEY-BACKSPACE+        #x08)
  (+TB-KEY-CTRL-H+           #x08 "clash with 'CTRL_BACKSPACE'")
  (+TB-KEY-TAB+              #x09)
  (+TB-KEY-CTRL-I+           #x09 "clash with 'TAB'")
  (+TB-KEY-CTRL-J+           #x0a)
  (+TB-KEY-CTRL-K+           #x0b)
  (+TB-KEY-CTRL-L+           #x0c)
  (+TB-KEY-ENTER+            #x0d)
  (+TB-KEY-CTRL-M+           #x0d "clash with 'ENTER'")
  (+TB-KEY-CTRL-N+           #x0e)
  (+TB-KEY-CTRL-O+           #x0f)
  (+TB-KEY-CTRL-P+           #x10)
  (+TB-KEY-CTRL-Q+           #x11)
  (+TB-KEY-CTRL-R+           #x12)
  (+TB-KEY-CTRL-S+           #x13)
  (+TB-KEY-CTRL-T+           #x14)
  (+TB-KEY-CTRL-U+           #x15)
  (+TB-KEY-CTRL-V+           #x16)
  (+TB-KEY-CTRL-W+           #x17)
  (+TB-KEY-CTRL-X+           #x18)
  (+TB-KEY-CTRL-Y+           #x19)
  (+TB-KEY-CTRL-Z+           #x1a)
  (+TB-KEY-ESC+              #x1b)
  (+TB-KEY-CTRL-LSQ-BRACKET+ #x1b "clash with 'ESC'")
  (+TB-KEY-CTRL-3+           #x1b "clash with 'ESC'")
  (+TB-KEY-CTRL-4+           #x1c)
  (+TB-KEY-CTRL-BACKSLASH+   #x1c "clash with 'CTRL_4'")
  (+TB-KEY-CTRL-5+           #x1d)
  (+TB-KEY-CTRL-RSQ-BRACKET+ #x1d "clash with 'CTRL_5'")
  (+TB-KEY-CTRL-6+           #x1e)
  (+TB-KEY-CTRL-7+           #x1f)
  (+TB-KEY-CTRL-SLASH+       #x1f "clash with 'CTRL_7'")
  (+TB-KEY-CTRL-UNDERSCORE+  #x1f "clash with 'CTRL_7'")
  (+TB-KEY-SPACE+            #x20)
  (+TB-KEY-BACKSPACE2+       #x7f)
  (+TB-KEY-CTRL-8+           #x7f "clash with 'BACKSPACE2'"))

;; Terminal-dependent key constants (tb_event.key) and terminfo capabilities
(export-constants
  (+TB-KEY-F1+               (- #xffff 0))
  (+TB-KEY-F2+               (- #xffff 1))
  (+TB-KEY-F3+               (- #xffff 2))
  (+TB-KEY-F4+               (- #xffff 3))
  (+TB-KEY-F5+               (- #xffff 4))
  (+TB-KEY-F6+               (- #xffff 5))
  (+TB-KEY-F7+               (- #xffff 6))
  (+TB-KEY-F8+               (- #xffff 7))
  (+TB-KEY-F9+               (- #xffff 8))
  (+TB-KEY-F10+              (- #xffff 9))
  (+TB-KEY-F11+              (- #xffff 10))
  (+TB-KEY-F12+              (- #xffff 11))
  (+TB-KEY-INSERT+           (- #xffff 12))
  (+TB-KEY-DELETE+           (- #xffff 13))
  (+TB-KEY-HOME+             (- #xffff 14))
  (+TB-KEY-END+              (- #xffff 15))
  (+TB-KEY-PGUP+             (- #xffff 16))
  (+TB-KEY-PGDN+             (- #xffff 17))
  (+TB-KEY-ARROW-UP+         (- #xffff 18))
  (+TB-KEY-ARROW-DOWN+       (- #xffff 19))
  (+TB-KEY-ARROW-LEFT+       (- #xffff 20))
  (+TB-KEY-ARROW-RIGHT+      (- #xffff 21))
  (+TB-KEY-BACK-TAB+         (- #xffff 22))
  (+TB-KEY-MOUSE-LEFT+       (- #xffff 23))
  (+TB-KEY-MOUSE-RIGHT+      (- #xffff 24))
  (+TB-KEY-MOUSE-MIDDLE+     (- #xffff 25))
  (+TB-KEY-MOUSE-RELEASE+    (- #xffff 26))
  (+TB-KEY-MOUSE-WHEEL-UP+   (- #xffff 27))
  (+TB-KEY-MOUSE-WHEEL-DOWN+ (- #xffff 28)))

(export-constants
  (+TB-CAP-F1+               0)
  (+TB-CAP-F2+               1)
  (+TB-CAP-F3+               2)
  (+TB-CAP-F4+               3)
  (+TB-CAP-F5+               4)
  (+TB-CAP-F6+               5)
  (+TB-CAP-F7+               6)
  (+TB-CAP-F8+               7)
  (+TB-CAP-F9+               8)
  (+TB-CAP-F10+              9)
  (+TB-CAP-F11+              10)
  (+TB-CAP-F12+              11)
  (+TB-CAP-INSERT+           12)
  (+TB-CAP-DELETE+           13)
  (+TB-CAP-HOME+             14)
  (+TB-CAP-END+              15)
  (+TB-CAP-PGUP+             16)
  (+TB-CAP-PGDN+             17)
  (+TB-CAP-ARROW-UP+         18)
  (+TB-CAP-ARROW-DOWN+       19)
  (+TB-CAP-ARROW-LEFT+       20)
  (+TB-CAP-ARROW-RIGHT+      21)
  (+TB-CAP-BACK-TAB+         22)
  (+TB-CAP--COUNT-KEYS+      23)
  (+TB-CAP-ENTER-CA+         23)
  (+TB-CAP-EXIT-CA+          24)
  (+TB-CAP-SHOW-CURSOR+      25)
  (+TB-CAP-HIDE-CURSOR+      26)
  (+TB-CAP-CLEAR-SCREEN+     27)
  (+TB-CAP-SGR0+             28)
  (+TB-CAP-UNDERLINE+        29)
  (+TB-CAP-BOLD+             30)
  (+TB-CAP-BLINK+            31)
  (+TB-CAP-ITALIC+           32)
  (+TB-CAP-REVERSE+          33)
  (+TB-CAP-ENTER-KEYPAD+     34)
  (+TB-CAP-EXIT-KEYPAD+      35)
  (+TB-CAP-DIM+              36)
  (+TB-CAP-INVISIBLE+        37)
  (+TB-CAP--COUNT+           38))

(export-vars
  (+TB-HARDCAP-ENTER-MOUSE+  "\x1b[?1000h\x1b[?1002h\x1b[?1015h\x1b[?1006h")
  (+TB-HARDCAP-EXIT-MOUSE+   "\x1b[?1006l\x1b[?1015l\x1b[?1002l\x1b[?1000l")
  (+TB-HARDCAP-STRIKEOUT+    "\x1b[9m")
  (+TB-HARDCAP-UNDERLINE-2+  "\x1b[21m")
  (+TB-HARDCAP-OVERLINE+     "\x1b[53m"))

;; Colors (numeric) and attributes (bitwise) (tb_cell.fg, tb_cell.bg)
(export-constants
  (+TB-DEFAULT+              #x0000)
  (+TB-BLACK+                #x0001)
  (+TB-RED+                  #x0002)
  (+TB-GREEN+                #x0003)
  (+TB-YELLOW+               #x0004)
  (+TB-BLUE+                 #x0005)
  (+TB-MAGENTA+              #x0006)
  (+TB-CYAN+                 #x0007)
  (+TB-WHITE+                #x0008))

;; Event types (tb_event.type)
(export-constants
  (+TB-EVENT-KEY+        1)
  (+TB-EVENT-RESIZE+     2)
  (+TB-EVENT-MOUSE+      3))

;; Key modifiers (bitwise) (tb_event.mod)
(export-constants
  (+TB-MOD-ALT+          1)
  (+TB-MOD-CTRL+         2)
  (+TB-MOD-SHIFT+        4)
  (+TB-MOD-MOTION+       8))

;; Input modes (bitwise) (tb_set_input_mode)
(export-constants
  (+TB-INPUT-CURRENT+    0)
  (+TB-INPUT-ESC+        1)
  (+TB-INPUT-ALT+        2)
  (+TB-INPUT-MOUSE+      4))

;; Output modes (tb_set_output_mode)
(export-constants
  (+TB-OUTPUT-CURRENT+   0)
  (+TB-OUTPUT-NORMAL+    1)
  (+TB-OUTPUT-256+       2)
  (+TB-OUTPUT-216+       3)
  (+TB-OUTPUT-GRAYSCALE+ 4)
  (+TB-OUTPUT-TRUECOLOR+ 5))

;;  Common function return values unless otherwise noted.
;;
;; Library behavior is undefined after receiving TB_ERR_MEM. Callers may
;; attempt reinitializing by freeing memory, invoking tb_shutdown, then
;; tb_init.
(export-constants
  (+TB-OK+                   0)
  (+TB-ERR+                  -1)
  (+TB-ERR-NEED-MORE+        -2)
  (+TB-ERR-INIT-ALREADY+     -3)
  (+TB-ERR-INIT-OPEN+        -4)
  (+TB-ERR-MEM+              -5)
  (+TB-ERR-NO-EVENT+         -6)
  (+TB-ERR-NO-TERM+          -7)
  (+TB-ERR-NOT-INIT+         -8)
  (+TB-ERR-OUT-OF-BOUNDS+    -9)
  (+TB-ERR-READ+             -10)
  (+TB-ERR-RESIZE-IOCTL+     -11)
  (+TB-ERR-RESIZE-PIPE+      -12)
  (+TB-ERR-RESIZE-SIGACTION+ -13)
  (+TB-ERR-POLL+             -14)
  (+TB-ERR-TCGETATTR+        -15)
  (+TB-ERR-TCSETATTR+        -16)
  (+TB-ERR-UNSUPPORTED-TERM+ -17)
  (+TB-ERR-RESIZE-WRITE+     -18)
  (+TB-ERR-RESIZE-POLL+      -19)
  (+TB-ERR-RESIZE-READ+      -20)
  (+TB-ERR-RESIZE-SSCANF+    -21)
  (+TB-ERR-CAP-COLLISION+    -22))
