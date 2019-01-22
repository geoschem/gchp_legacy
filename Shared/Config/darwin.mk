  ################################
  #                              #
  #    Arch Specific Defaults    #
  #                              #
  ################################

  # --------------
  # Darwin (macOS)
  # --------------

  ifeq ($(ARCH),Darwin)
     MKL_OS = mac

     MAC_VER := $(subst ., ,$(word 3,$(shell sw_vers -productVersion)))
     MAC_MAJOR := $(word 1,$(MAC_VER))
     MAC_MINOR := $(word 2,$(MAC_VER))

     # MAT INC_SYS doesn't seem used in GEOS-5. But this was added...commenting out for now
     #     pending further testing
     #INC_SYS := $(shell xcodebuild -version -sdk macosx$(MAC_MAJOR).$(MAC_MINOR) Path)/usr/include

     RANLIB_FLAGS = -c -no_warning_for_no_symbols

     # Tom Clune asked for this flag as it helped suppress many
     # warnings
     LDFLAGS += -Wl,-no_compact_unwind

     DLLEXT = dylib
  endif
