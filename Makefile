

include config.mk

all: config.mk $(TOOL_NAME)

$(TOOL_NAME):
	cd source && make
	mv source/Driver.byte $(TOOL_NAME)

clean:
	cd source && make clean

config.mk:
	@./configure

.PHONY: all clean install
