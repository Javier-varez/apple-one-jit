ifeq ($(LLVM_MOS),)
$(error Please set the LLVM_MOS environment variable to point to the LLVM mos build)
endif

BUILD_DIR := build

all: $(BUILD_DIR)/woz_monitor.bin

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean

$(BUILD_DIR)/woz_monitor: $(BUILD_DIR)/woz_monitor.o woz_monitor.ld
	@mkdir -p $(dir $@)
	$(LLVM_MOS)/bin/ld.lld -T woz_monitor.ld $< -o $@ --entry=RESET

$(BUILD_DIR)/woz_monitor.o: woz_monitor.s
	@mkdir -p $(dir $@)
	$(LLVM_MOS)/bin/llvm-mc -arch=mos --assemble --filetype=obj -o $@ $<

$(BUILD_DIR)/woz_monitor.bin: $(BUILD_DIR)/woz_monitor
	@mkdir -p $(dir $@)
	$(LLVM_MOS)/bin/llvm-objcopy -O binary $< $@
