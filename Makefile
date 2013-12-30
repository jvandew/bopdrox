.PHONY: all clean msg util server client test

all: msg util server client test

clean:
	@-$(MAKE) clean -C msg -s
	@-$(MAKE) clean -C util -s
	@-$(MAKE) clean -C server -s
	@-$(MAKE) clean -C client -s
	@-$(MAKE) clean -C test -s
	
msg:
	@$(MAKE) msg -C msg --no-print-directory

util: msg
	@$(MAKE) util -C util --no-print-directory

server: msg util
	@$(MAKE) server -C server --no-print-directory

client: msg util
	@$(MAKE) client -C client --no-print-directory

test: server client
	@$(MAKE) test -C test --no-print-directory
