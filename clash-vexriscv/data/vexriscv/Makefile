# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: CC0-1.0

CLASH_VEXRISCV_DATA_DIR = $(dir $(MAKEFILE_LIST))
SCALA_CONFIG_DIR = SCALA_CONFIG_DIR-you-have-to-set-this-as-an-argument
CPU_NAME = CPU_NAME-you-have-to-set-this-as-an-argument

VERILATOR_DIR = verilator
VERILATOR_FLAGS = -CFLAGS '-O3 -fPIC' -Wno-fatal +1364-2001ext+v --trace

N := $(patsubst -j%,%,$(filter -j%,$(MAKEFLAGS)))

VERILATOR_CFLAGS = $(shell pkg-config --cflags verilator)
FFI_CPPFLAGS = $(VERILATOR_CFLAGS) -fPIC -O3 -I$(VERILATOR_DIR)

# XXX: Frustratingly, REPL only works with dynamic libraries, but Cabal prefers
#      static ones. On the flip side, normal *runs* only work with static
#      libraries. We therefore put them into separate directories so that we
#      can easily pick the right one depending on the use-case.
all: ../static/lib$(CPU_NAME)VexRiscvFFI.a ../dynamic/lib$(CPU_NAME)VexRiscvFFI.so

clean:
	rm -rf *.o *.v *.a *.so *.sbt ffi lib project src $(VERILATOR_DIR) target

build.sbt: $(CLASH_VEXRISCV_DATA_DIR)/build.sbt $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	cp $(CLASH_VEXRISCV_DATA_DIR)/build.sbt build.sbt

project/%: $(CLASH_VEXRISCV_DATA_DIR)/project/% $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	mkdir -p project
	cp $< $@

lib/vexriscv.jar: $(wildcard $(CLASH_VEXRISCV_DATA_DIR)/lib/vexriscv_*.jar) $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	mkdir -p lib
	cp $< $@

ffi/%: $(CLASH_VEXRISCV_DATA_DIR)/ffi/% $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	mkdir -p ffi
	cp $< $@
	sed -i 's/__CPU_NAME__/$(CPU_NAME)/g' $@

src/main/scala/cpu/%: $(SCALA_CONFIG_DIR)/% $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	mkdir -p src/main/scala/cpu
	cp $< $@

$(CPU_NAME)VexRiscv.v: src/main/scala/cpu/$(CPU_NAME).scala lib/vexriscv.jar build.sbt project/build.properties project/Dependencies.scala $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	flock $(HOME)/.sbt-lock sbt "runMain cpu.$(CPU_NAME)"
	sed -i -E '/\/\/ Git hash  :.*$$/d' $(CPU_NAME)VexRiscv.v

$(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv.mk $(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv.h: $(CPU_NAME)VexRiscv.v $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	verilator $(VERILATOR_FLAGS) --cc -Mdir $(VERILATOR_DIR) $(CPU_NAME)VexRiscv.v

$(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv__ALL.a: $(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv.mk $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	cd $(VERILATOR_DIR); make -f V$(CPU_NAME)VexRiscv.mk -j $(N)

impl.o: ffi/impl.cpp ffi/interface.h $(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv.h $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	$(CXX) $(FFI_CPPFLAGS) -c ffi/impl.cpp -o impl.o

vimpl.o: ffi/vimpl.cpp ffi/interface.h $(shell pkg-config --variable=includedir verilator)/verilated.h $(shell pkg-config --variable=includedir verilator)/verilated_vcd_c.h $(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv.h $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	$(CXX) $(FFI_CPPFLAGS) -c ffi/vimpl.cpp -o vimpl.o

verilated_vcd_c.o: $(shell pkg-config --variable=includedir verilator)/verilated_vcd_c.cpp $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	$(CXX) $(FFI_CPPFLAGS) -c $(shell pkg-config --variable=includedir verilator)/verilated_vcd_c.cpp -o verilated_vcd_c.o

verilated.o: $(shell pkg-config --variable=includedir verilator)/verilated.cpp $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	$(CXX) $(FFI_CPPFLAGS) -c $(shell pkg-config --variable=includedir verilator)/verilated.cpp -o verilated.o

verilated_threads.o: $(shell pkg-config --variable=includedir verilator)/verilated_threads.cpp $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	$(CXX) $(FFI_CPPFLAGS) -c $(shell pkg-config --variable=includedir verilator)/verilated_threads.cpp -o verilated_threads.o

V$(CPU_NAME)VexRiscv__ALL.a: $(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv__ALL.a $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	cp $(VERILATOR_DIR)/V$(CPU_NAME)VexRiscv__ALL.a V$(CPU_NAME)VexRiscv__ALL.a

lib$(CPU_NAME)VexRiscvFFI.a: V$(CPU_NAME)VexRiscv__ALL.a vimpl.o impl.o verilated.o verilated_threads.o verilated_vcd_c.o $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	rm -f libVexRiscvFFI.a
	cp V$(CPU_NAME)VexRiscv__ALL.a lib$(CPU_NAME)VexRiscvFFI.a
	ar r \
		lib$(CPU_NAME)VexRiscvFFI.a \
		vimpl.o \
		impl.o \
		verilated.o \
		verilated_threads.o \
		verilated_vcd_c.o

lib$(CPU_NAME)VexRiscvFFI.so: lib$(CPU_NAME)VexRiscvFFI.a $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	rm -f lib$(CPU_NAME)VexRiscvFFI.so
	$(CXX) -shared -o lib$(CPU_NAME)VexRiscvFFI.so -Wl,--whole-archive lib$(CPU_NAME)VexRiscvFFI.a -Wl,--no-whole-archive

../static/lib$(CPU_NAME)VexRiscvFFI.a: lib$(CPU_NAME)VexRiscvFFI.a $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	mkdir -p ../static
	cp lib$(CPU_NAME)VexRiscvFFI.a ../static

../dynamic/lib$(CPU_NAME)VexRiscvFFI.so: lib$(CPU_NAME)VexRiscvFFI.so $(CLASH_VEXRISCV_DATA_DIR)/Makefile
	mkdir -p ../dynamic
	cp lib$(CPU_NAME)VexRiscvFFI.so ../dynamic
