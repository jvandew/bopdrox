rpath = .:..
cpaths = -cp ../.. -d ../..
copts = $(cpaths) -Xlint -Xfatal-warnings -deprecation -optimise
ropts = -cp $(rpath) -J-Xmx3G
scalac = fsc $(copts)

export scalac

.PHONY: all clean client run-client run-server server test tests util

all: util server client tests

clean:
	@-$(MAKE) clean -C util -s
	@-$(MAKE) clean -C server -s
	@-$(MAKE) clean -C client -s
	@-$(MAKE) clean -C tests -s

client: util
	@$(MAKE) client -C client --no-print-directory

# to pass args do 'make run-client args="arg0 arg1 ..."'
run-client:
	scala $(ropts) bopdrox.client.Client $(args)

# to pass args do 'make run-client args="arg0 arg1 ..."'
run-server:
	scala $(ropts) bopdrox.server.Server $(args)

server: util
	@$(MAKE) server -C server --no-print-directory

test:
	scala $(ropts) bopdrox.tests.Test

tests: util server client
	@$(MAKE) tests -C tests --no-print-directory

util:
	@$(MAKE) util -C util --no-print-directory
