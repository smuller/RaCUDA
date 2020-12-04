include ../config.mk

BIN = gsl_caller

CXXFLAGS += \
    -I $(GSL_PREFIX)/include/gsl

LDFLAGS += -L $(GSL_PREFIX)/lib/x86_64-linux-gnu -lgsl -lgslcblas -lm

all: ${BIN}

${BIN}: ${BIN}.o
	${CXX} $^ ${LDFLAGS} -o $@

%.o: %.cpp
	${CXX} ${CXXFLAGS} -c $^

clean:
	rm *.o ${BIN}

.PHONY: all clean
