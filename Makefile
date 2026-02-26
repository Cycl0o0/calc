CC       = cc
CFLAGS   = -Wall -Wextra -O2
PREFIX   = $(HOME)/.local
DESTDIR  =

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
  # macOS: ncurses ships as -lncurses (wide-char enabled by default)
  # Homebrew ncurses (if installed) lives under /opt/homebrew or /usr/local
  BREW_PREFIX := $(shell brew --prefix ncurses 2>/dev/null)
  ifneq ($(BREW_PREFIX),)
    CFLAGS  += -I$(BREW_PREFIX)/include
    LDFLAGS  = -L$(BREW_PREFIX)/lib -lncurses -lm
  else
    LDFLAGS  = -lncurses -lm
  endif
else
  # Linux and others: use wide-char ncurses
  LDFLAGS  = -lncursesw -lm
endif

calculator: calculator.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

install: calculator
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 calculator $(DESTDIR)$(PREFIX)/bin/calculator

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/calculator

# User install (no sudo):   make install
# System-wide install:      sudo make install PREFIX=/usr/local

clean:
	rm -f calculator

.PHONY: clean install uninstall
