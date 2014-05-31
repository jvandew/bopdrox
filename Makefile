.PHONY: all clean util server client test

all: util server client test

clean:
	@-$(MAKE) clean -C util -s
	@-$(MAKE) clean -C server -s
	@-$(MAKE) clean -C client -s
	@-$(MAKE) clean -C test -s

util:
	@$(MAKE) util -C util --no-print-directory

server: util
	@$(MAKE) server -C server --no-print-directory

client: util
	@$(MAKE) client -C client --no-print-directory

test: util server client
	@$(MAKE) test -C test --no-print-directory
