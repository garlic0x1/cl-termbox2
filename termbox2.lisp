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
           :tb-send
           :tb-set-func
           :tb-utf8-char-length))
(in-package :termbox2)

(use-foreign-library "libtermbox2.so")

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
  (fg :unsigned-int)
  (bg :unsigned-int))
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
  (ch :unsigned-int)
  (fg :unsigned-int)
  (bg :unsigned-int))
(defcfun ("tb_set_cell_ex" tb-set-cell-ex) :int
  (x :int)
  (y :int)
  (ch (:pointer :unsigned-int))
  (nch :unsigned-int)
  (fg :unsigned-int)
  (bg :unsigned-int))
(defcfun ("tb_extend_cell" tb-extend-cell) :int
  (x :int)
  (y :int)
  (ch :unsigned-int))

(defcfun ("tb_peek_event" tb-peek-event*) :int
  "Wait for an event up to timeout_ms milliseconds and fill the event
structure with it. If no event is available within the timeout period,
TB_ERR_NO_EVENT is returned. On a resize event, the underlying select(2)
call may be interrupted, yielding a return code of TB_ERR_POLL.
In this case, you may check errno via tb_last_errno(). If it's EINTR,
you can safely ignore that and call tb_peek_event() again."
  (event :pointer)
  (timeout :int))
(defcfun ("tb_poll_event" tb-poll-event*) :int
  "Same as tb_peek_event except no timeout."
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
  (fg :unsigned-int)
  (bg :unsigned-int)
  (str :string))
(defcfun ("tb_print_ex" tb-print-ex) :int
  "Same as tb_print() except with out_w to determine width of printed."
  (x :int)
  (y :int)
  (fg :unsigned-int)
  (bg :unsigned-int)
  (out-w :unsigned-int)
  (str :string))

(defcfun ("tb_send" tb-send) :int
  "Send raw bytes to terminal"
  (buf :string)
  (nbuf :unsigned-int))

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

(defun translate-tb-event (ev)
  (with-foreign-slots ((type mod key ch w h x y) ev (:struct tb-event*))
    (make-tb-event :type type :mod mod :key key :ch ch :w w :h h :x x :y y)))

(defun tb-poll-event ()
  (with-foreign-object (ev '(:struct tb-event*))
    (tb-poll-event* ev)
    (translate-tb-event ev)))

(defun tb-peek-event (timeout)
  (with-foreign-object (ev '(:struct tb-event*))
    (tb-peek-event* ev timeout)
    (translate-tb-event ev)))

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
  (+TB_KEY_CTRL_TILDE+       #x00)
  (+TB_KEY_CTRL_2+           #x00 "clash with 'CTRL_TILDE'")
  (+TB_KEY_CTRL_A+           #x01)
  (+TB_KEY_CTRL_B+           #x02)
  (+TB_KEY_CTRL_C+           #x03)
  (+TB_KEY_CTRL_D+           #x04)
  (+TB_KEY_CTRL_E+           #x05)
  (+TB_KEY_CTRL_F+           #x06)
  (+TB_KEY_CTRL_G+           #x07)
  (+TB_KEY_BACKSPACE+        #x08)
  (+TB_KEY_CTRL_H+           #x08 "clash with 'CTRL_BACKSPACE'")
  (+TB_KEY_TAB+              #x09)
  (+TB_KEY_CTRL_I+           #x09 "clash with 'TAB'")
  (+TB_KEY_CTRL_J+           #x0a)
  (+TB_KEY_CTRL_K+           #x0b)
  (+TB_KEY_CTRL_L+           #x0c)
  (+TB_KEY_ENTER+            #x0d)
  (+TB_KEY_CTRL_M+           #x0d "clash with 'ENTER'")
  (+TB_KEY_CTRL_N+           #x0e)
  (+TB_KEY_CTRL_O+           #x0f)
  (+TB_KEY_CTRL_P+           #x10)
  (+TB_KEY_CTRL_Q+           #x11)
  (+TB_KEY_CTRL_R+           #x12)
  (+TB_KEY_CTRL_S+           #x13)
  (+TB_KEY_CTRL_T+           #x14)
  (+TB_KEY_CTRL_U+           #x15)
  (+TB_KEY_CTRL_V+           #x16)
  (+TB_KEY_CTRL_W+           #x17)
  (+TB_KEY_CTRL_X+           #x18)
  (+TB_KEY_CTRL_Y+           #x19)
  (+TB_KEY_CTRL_Z+           #x1a)
  (+TB_KEY_ESC+              #x1b)
  (+TB_KEY_CTRL_LSQ_BRACKET+ #x1b "clash with 'ESC'")
  (+TB_KEY_CTRL_3+           #x1b "clash with 'ESC'")
  (+TB_KEY_CTRL_4+           #x1c)
  (+TB_KEY_CTRL_BACKSLASH+   #x1c "clash with 'CTRL_4'")
  (+TB_KEY_CTRL_5+           #x1d)
  (+TB_KEY_CTRL_RSQ_BRACKET+ #x1d "clash with 'CTRL_5'")
  (+TB_KEY_CTRL_6+           #x1e)
  (+TB_KEY_CTRL_7+           #x1f)
  (+TB_KEY_CTRL_SLASH+       #x1f "clash with 'CTRL_7'")
  (+TB_KEY_CTRL_UNDERSCORE+  #x1f "clash with 'CTRL_7'")
  (+TB_KEY_SPACE+            #x20)
  (+TB_KEY_BACKSPACE2+       #x7f)
  (+TB_KEY_CTRL_8+           #x7f "clash with 'BACKSPACE2'"))

;; Terminal-dependent key constants (tb_event.key) and terminfo capabilities
(export-constants
  (+TB_KEY_F1+               (- #xffff 0))
  (+TB_KEY_F2+               (- #xffff 1))
  (+TB_KEY_F3+               (- #xffff 2))
  (+TB_KEY_F4+               (- #xffff 3))
  (+TB_KEY_F5+               (- #xffff 4))
  (+TB_KEY_F6+               (- #xffff 5))
  (+TB_KEY_F7+               (- #xffff 6))
  (+TB_KEY_F8+               (- #xffff 7))
  (+TB_KEY_F9+               (- #xffff 8))
  (+TB_KEY_F10+              (- #xffff 9))
  (+TB_KEY_F11+              (- #xffff 10))
  (+TB_KEY_F12+              (- #xffff 11))
  (+TB_KEY_INSERT+           (- #xffff 12))
  (+TB_KEY_DELETE+           (- #xffff 13))
  (+TB_KEY_HOME+             (- #xffff 14))
  (+TB_KEY_END+              (- #xffff 15))
  (+TB_KEY_PGUP+             (- #xffff 16))
  (+TB_KEY_PGDN+             (- #xffff 17))
  (+TB_KEY_ARROW_UP+         (- #xffff 18))
  (+TB_KEY_ARROW_DOWN+       (- #xffff 19))
  (+TB_KEY_ARROW_LEFT+       (- #xffff 20))
  (+TB_KEY_ARROW_RIGHT+      (- #xffff 21))
  (+TB_KEY_BACK_TAB+         (- #xffff 22))
  (+TB_KEY_MOUSE_LEFT+       (- #xffff 23))
  (+TB_KEY_MOUSE_RIGHT+      (- #xffff 24))
  (+TB_KEY_MOUSE_MIDDLE+     (- #xffff 25))
  (+TB_KEY_MOUSE_RELEASE+    (- #xffff 26))
  (+TB_KEY_MOUSE_WHEEL_UP+   (- #xffff 27))
  (+TB_KEY_MOUSE_WHEEL_DOWN+ (- #xffff 28)))

(export-constants
  (+TB_CAP_F1+               0)
  (+TB_CAP_F2+               1)
  (+TB_CAP_F3+               2)
  (+TB_CAP_F4+               3)
  (+TB_CAP_F5+               4)
  (+TB_CAP_F6+               5)
  (+TB_CAP_F7+               6)
  (+TB_CAP_F8+               7)
  (+TB_CAP_F9+               8)
  (+TB_CAP_F10+              9)
  (+TB_CAP_F11+              10)
  (+TB_CAP_F12+              11)
  (+TB_CAP_INSERT+           12)
  (+TB_CAP_DELETE+           13)
  (+TB_CAP_HOME+             14)
  (+TB_CAP_END+              15)
  (+TB_CAP_PGUP+             16)
  (+TB_CAP_PGDN+             17)
  (+TB_CAP_ARROW_UP+         18)
  (+TB_CAP_ARROW_DOWN+       19)
  (+TB_CAP_ARROW_LEFT+       20)
  (+TB_CAP_ARROW_RIGHT+      21)
  (+TB_CAP_BACK_TAB+         22)
  (+TB_CAP__COUNT_KEYS+      23)
  (+TB_CAP_ENTER_CA+         23)
  (+TB_CAP_EXIT_CA+          24)
  (+TB_CAP_SHOW_CURSOR+      25)
  (+TB_CAP_HIDE_CURSOR+      26)
  (+TB_CAP_CLEAR_SCREEN+     27)
  (+TB_CAP_SGR0+             28)
  (+TB_CAP_UNDERLINE+        29)
  (+TB_CAP_BOLD+             30)
  (+TB_CAP_BLINK+            31)
  (+TB_CAP_ITALIC+           32)
  (+TB_CAP_REVERSE+          33)
  (+TB_CAP_ENTER_KEYPAD+     34)
  (+TB_CAP_EXIT_KEYPAD+      35)
  (+TB_CAP_DIM+              36)
  (+TB_CAP_INVISIBLE+        37)
  (+TB_CAP__COUNT+           38))

(export-vars
  (+TB_HARDCAP_ENTER_MOUSE+  "\x1b[?1000h\x1b[?1002h\x1b[?1015h\x1b[?1006h")
  (+TB_HARDCAP_EXIT_MOUSE+   "\x1b[?1006l\x1b[?1015l\x1b[?1002l\x1b[?1000l")
  (+TB_HARDCAP_STRIKEOUT+    "\x1b[9m")
  (+TB_HARDCAP_UNDERLINE_2+  "\x1b[21m")
  (+TB_HARDCAP_OVERLINE+     "\x1b[53m"))

;; Colors (numeric) and attributes (bitwise) (tb_cell.fg, tb_cell.bg)
(export-constants
  (+TB_DEFAULT+              #x0000)
  (+TB_BLACK+                #x0001)
  (+TB_RED+                  #x0002)
  (+TB_GREEN+                #x0003)
  (+TB_YELLOW+               #x0004)
  (+TB_BLUE+                 #x0005)
  (+TB_MAGENTA+              #x0006)
  (+TB_CYAN+                 #x0007)
  (+TB_WHITE+                #x0008))

;; Event types (tb_event.type)
(export-constants
  (+TB_EVENT_KEY+        1)
  (+TB_EVENT_RESIZE+     2)
  (+TB_EVENT_MOUSE+      3))

;; Key modifiers (bitwise) (tb_event.mod)
(export-constants
  (+TB_MOD_ALT+          1)
  (+TB_MOD_CTRL+         2)
  (+TB_MOD_SHIFT+        4)
  (+TB_MOD_MOTION+       8))

;; Input modes (bitwise) (tb_set_input_mode)
(export-constants
  (+TB_INPUT_CURRENT+    0)
  (+TB_INPUT_ESC+        1)
  (+TB_INPUT_ALT+        2)
  (+TB_INPUT_MOUSE+      4))

;; Output modes (tb_set_output_mode)
(export-constants
  (+TB_OUTPUT_CURRENT+   0)
  (+TB_OUTPUT_NORMAL+    1)
  (+TB_OUTPUT_256+       2)
  (+TB_OUTPUT_216+       3)
  (+TB_OUTPUT_GRAYSCALE+ 4)
  (+TB_OUTPUT_TRUECOLOR+ 5))

;;  Common function return values unless otherwise noted.
;;
;; Library behavior is undefined after receiving TB_ERR_MEM. Callers may
;; attempt reinitializing by freeing memory, invoking tb_shutdown, then
;; tb_init.
(export-constants
  (+TB_OK+                   0)
  (+TB_ERR+                  -1)
  (+TB_ERR_NEED_MORE+        -2)
  (+TB_ERR_INIT_ALREADY+     -3)
  (+TB_ERR_INIT_OPEN+        -4)
  (+TB_ERR_MEM+              -5)
  (+TB_ERR_NO_EVENT+         -6)
  (+TB_ERR_NO_TERM+          -7)
  (+TB_ERR_NOT_INIT+         -8)
  (+TB_ERR_OUT_OF_BOUNDS+    -9)
  (+TB_ERR_READ+             -10)
  (+TB_ERR_RESIZE_IOCTL+     -11)
  (+TB_ERR_RESIZE_PIPE+      -12)
  (+TB_ERR_RESIZE_SIGACTION+ -13)
  (+TB_ERR_POLL+             -14)
  (+TB_ERR_TCGETATTR+        -15)
  (+TB_ERR_TCSETATTR+        -16)
  (+TB_ERR_UNSUPPORTED_TERM+ -17)
  (+TB_ERR_RESIZE_WRITE+     -18)
  (+TB_ERR_RESIZE_POLL+      -19)
  (+TB_ERR_RESIZE_READ+      -20)
  (+TB_ERR_RESIZE_SSCANF+    -21)
  (+TB_ERR_CAP_COLLISION+    -22))
