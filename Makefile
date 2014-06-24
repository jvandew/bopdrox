.PHONY: all clean util server client tests

all: util server client tests

clean:
	@-$(MAKE) clean -C util -s
	@-$(MAKE) clean -C server -s
	@-$(MAKE) clean -C client -s
	@-$(MAKE) clean -C tests -s

util:
	@$(MAKE) util -C util --no-print-directory

server: util
	@$(MAKE) server -C server --no-print-directory

client: util
	@$(MAKE) client -C client --no-print-directory

tests: util server client
	@$(MAKE) tests -C tests --no-print-directory
