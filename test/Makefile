SMLNJ_BIN_PATH=$(SMLNJ_PATH)/bin/
ML_BUILD=$(SMLNJ_BIN_PATH)/ml-build
SML=$(SMLNJ_BIN_PATH)/sml
ML_BUILD_DEBUG_FLAGS=-Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm

SML_SOURCES=$(wildcard *.sml) $(wildcard ../src/*.sml)
CM_SOURCES=$(wildcard *.cm) $(wildcard ../src/*.cm)

run_tests_smlnj: test_smlnj
	$(SML) @SMLload=test 20002

test_smlnj: $(SML_SOURCES) $(CM_SOURCES)
	$(ML_BUILD) $(ML_BUILD_DEBUG_FLAGS) sources.cm SMLNJTest.main test

clean:
	rm -rf .cm
