include config.mk

all: packages/sandbox/done
	@make --no-print-directory -C source $@
ifneq ($(LLC),)
	@make --no-print-directory -C llvm-reader $@
endif
ifneq ($(GSL_PREFIX),)
	@make --no-print-directory -C gsl_caller $@
endif

clean:
#	@cd packages && sh build.sh clean
	@make --no-print-directory -C source clean
ifneq ($(LLC),)
	@make --no-print-directory -C llvm-reader $@
endif
ifneq ($(GSL_PREFIX),)
	@make --no-print-directory -C gsl_caller $@
endif

dist-clean: clean
	@rm -f config.mk

packages/sandbox/done:
	@cd packages && sh build.sh

config.mk:
	@./configure

.PHONY: all clean dist-clean install
