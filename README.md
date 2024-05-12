# cl-termbox2

Lisp bindings to [termbox2](https://github.com/termbox/termbox2).

```lisp
(defun main ()
  (tb-init)
  (tb-print 0 0 +tb-cyan+ +tb-black+ "Hello, Termbox!")
  (tb-print 0 1 +tb-blue+ +tb-black+ "Press any key...")
  (tb-present)
  (tb-clear)
  (tb-printf 0 0 +tb-cyan+ +tb-black+ "~a" (tb-poll-event))
  (tb-present)
  (tb-peek-event 5000)
  (tb-shutdown))
```

See the snake demo:

```bash
make snake
./snake-exe
```

# Installation

Install termbox2 so that `libtermbox2.so` is accessible

```bash
git clone https://github.com/termbox/termbox2
cd termbox2
make install_lib
```
