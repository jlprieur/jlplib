# =========================================================================
#     This makefile was generated by
#     Bakefile 0.2.13 (http://www.bakefile.org)
#     Do not modify, all changes will be overwritten!
# =========================================================================




prefix = /usr/local
exec_prefix = ${prefix}
datarootdir = ${prefix}/share
INSTALL = /usr/bin/install -c
EXEEXT = 
WINDRES = 
STRIP = strip
NM = nm
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DIR = mkdir -p
BK_DEPS = /home/jprieur/jlp/src/jlplib/wxwidgets/wxWidgets-3.2.4/bk-deps
srcdir = .
top_srcdir = ../../..
bindir = ${exec_prefix}/bin
LIBS = -lz -lm
LDFLAGS_GUI = 
CXX = g++
CXXFLAGS = 
CPPFLAGS = 
LDFLAGS = 
USE_DPI_AWARE_MANIFEST = 2
WX_LIB_FLAVOUR = 
TOOLKIT = GTK
TOOLKIT_LOWERCASE = gtk
TOOLKIT_VERSION = 3
TOOLCHAIN_FULLNAME = gtk3-unicode-3.2
EXTRALIBS = -pthread   -Wl,--version-script,$(wx_top_builddir)/version-script -lz -lm -lpcre2-32  
EXTRALIBS_XML =  -lexpat
EXTRALIBS_HTML = 
EXTRALIBS_GUI = -lgtk-3 -lgdk-3 -lz -lpangocairo-1.0 -lpango-1.0 -lharfbuzz -latk-1.0 -lcairo-gobject -lcairo -lgdk_pixbuf-2.0 -lgio-2.0 -lgobject-2.0 -lgthread-2.0 -pthread -lglib-2.0 -lX11 -lSM -lxkbcommon -lgtk-3 -lgdk-3 -lz -lpangocairo-1.0 -lpango-1.0 -lharfbuzz -latk-1.0 -lcairo-gobject -lcairo -lgdk_pixbuf-2.0 -lgio-2.0 -lgobject-2.0 -lglib-2.0 -lXtst -lpangoft2-1.0 -lpango-1.0 -lgobject-2.0 -lglib-2.0 -lharfbuzz -lfontconfig -lfreetype -lpng -lz -ljpeg -ltiff -ljbig
WX_CPPFLAGS = -I${wx_top_builddir}/lib/wx/include/gtk3-unicode-3.2 -I${top_srcdir}/include -D_FILE_OFFSET_BITS=64 -I/usr/include/gtk-3.0 -I/usr/include/pango-1.0 -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/harfbuzz -I/usr/include/freetype2 -I/usr/include/libpng16 -I/usr/include/libmount -I/usr/include/blkid -I/usr/include/fribidi -I/usr/include/cairo -I/usr/include/pixman-1 -I/usr/include/gdk-pixbuf-2.0 -I/usr/include/x86_64-linux-gnu -I/usr/include/gio-unix-2.0 -I/usr/include/atk-1.0 -I/usr/include/at-spi2-atk/2.0 -I/usr/include/at-spi-2.0 -I/usr/include/dbus-1.0 -I/usr/lib/x86_64-linux-gnu/dbus-1.0/include -pthread 
WX_CXXFLAGS = -pthread -Wall -Wundef -Wunused-parameter -Wno-ctor-dtor-privacy -Woverloaded-virtual -Wno-deprecated-declarations -O2 -I/usr/include/freetype2 -I/usr/include/libpng16 -I/usr/include/pango-1.0 -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/harfbuzz -I/usr/include/libmount -I/usr/include/blkid -I/usr/include/fribidi -I/usr/include/cairo -I/usr/include/pixman-1 -I/usr/include/gtk-3.0/unix-print -I/usr/include/gtk-3.0 -I/usr/include/gdk-pixbuf-2.0 -I/usr/include/x86_64-linux-gnu -I/usr/include/gio-unix-2.0 -I/usr/include/atk-1.0 -I/usr/include/at-spi2-atk/2.0 -I/usr/include/at-spi-2.0 -I/usr/include/dbus-1.0 -I/usr/lib/x86_64-linux-gnu/dbus-1.0/include  -fvisibility=hidden -fvisibility-inlines-hidden
WX_LDFLAGS = -pthread 
HOST_SUFFIX = 
SAMPLES_RPATH_FLAG = -Wl,-rpath,$(wx_top_builddir)/lib
HEADER_PAD_OPTION = 
wx_top_builddir = /home/jprieur/jlp/src/jlplib/wxwidgets/wxWidgets-3.2.4

### Variables: ###

DESTDIR = 
WX_RELEASE = 3.2
WX_VERSION = $(WX_RELEASE).4
LIBDIRNAME = $(wx_top_builddir)/lib
SCREENSHOTGEN_CXXFLAGS = $(WX_CPPFLAGS) -D__WX$(TOOLKIT)__ \
	$(__WXUNIV_DEFINE_p) $(__DEBUG_DEFINE_p) $(__EXCEPTIONS_DEFINE_p) \
	$(__RTTI_DEFINE_p) $(__THREAD_DEFINE_p) -I$(srcdir) $(__DLLFLAG_p) \
	-I$(srcdir)/../../../samples $(WX_CXXFLAGS) $(CPPFLAGS) $(CXXFLAGS)
SCREENSHOTGEN_OBJECTS =  \
	screenshotgen_screenshot_app.o \
	screenshotgen_screenshot_main.o \
	screenshotgen_guiframe.o \
	screenshotgen_customcombo.o \
	screenshotgen_autocapture.o \
	$(__screenshotgen___win32rc)

### Conditionally set variables: ###

#CXXC = $(CXX)
CXXC = $(BK_DEPS) $(CXX)
#PORTNAME = base
PORTNAME = $(TOOLKIT_LOWERCASE)$(TOOLKIT_VERSION)
#WXBASEPORT = _carbon
#WXDEBUGFLAG = d
WXUNICODEFLAG = u
#WXUNIVNAME = univ
EXTRALIBS_FOR_BASE = $(EXTRALIBS)
#EXTRALIBS_FOR_BASE = $(EXTRALIBS) \
#	$(EXTRALIBS_XML) $(EXTRALIBS_GUI)
EXTRALIBS_FOR_GUI = $(EXTRALIBS_GUI)
#EXTRALIBS_FOR_GUI = 
#__WXUNIV_DEFINE_p = -D__WXUNIVERSAL__
#__WXUNIV_DEFINE_p_1 = --define __WXUNIVERSAL__
#__DEBUG_DEFINE_p = -DwxDEBUG_LEVEL=0
#__DEBUG_DEFINE_p_1 = --define wxDEBUG_LEVEL=0
#__EXCEPTIONS_DEFINE_p = -DwxNO_EXCEPTIONS
#__EXCEPTIONS_DEFINE_p_1 = --define wxNO_EXCEPTIONS
#__RTTI_DEFINE_p = -DwxNO_RTTI
#__RTTI_DEFINE_p_1 = --define wxNO_RTTI
#__THREAD_DEFINE_p = -DwxNO_THREADS
#__THREAD_DEFINE_p_1 = --define wxNO_THREADS
__DLLFLAG_p = -DWXUSINGDLL
__DLLFLAG_p_1 = --define WXUSINGDLL
#__WIN32_DPI_MANIFEST_p = \
#	--define \
#	wxUSE_DPI_AWARE_MANIFEST=$(USE_DPI_AWARE_MANIFEST)
COND_PLATFORM_OS2_1___screenshotgen___os2_emxbindcmd = $(NM) \
	screenshotgen$(EXEEXT) | if grep -q pmwin.763 ; then emxbind -ep \
	screenshotgen$(EXEEXT) ; fi
#__screenshotgen___os2_emxbindcmd = $(COND_PLATFORM_OS2_1___screenshotgen___os2_emxbindcmd)
#__RCDEFDIR_p = --include-dir \
#	$(LIBDIRNAME)/wx/include/$(TOOLCHAIN_FULLNAME)
#__screenshotgen_app_Contents_PkgInfo___depname \
#	= screenshotgen.app/Contents/PkgInfo
#__screenshotgen_bundle___depname \
#	= screenshotgen_bundle
#____screenshotgen_BUNDLE_TGT_REF_DEP = \
#	$(__screenshotgen_app_Contents_PkgInfo___depname)
#____screenshotgen_BUNDLE_TGT_REF_DEP \
#	= \
#	$(__screenshotgen_app_Contents_PkgInfo___depname)
#____screenshotgen_BUNDLE_TGT_REF_DEP \
#	= \
#	$(__screenshotgen_app_Contents_PkgInfo___depname)
#____screenshotgen_BUNDLE_TGT_REF_DEP \
#	= \
#	$(__screenshotgen_app_Contents_PkgInfo___depname)
#____screenshotgen_BUNDLE_TGT_REF_DEP \
#	= $(__screenshotgen_app_Contents_PkgInfo___depname)
COND_MONOLITHIC_0___WXLIB_AUI_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)_aui-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_AUI_p = $(COND_MONOLITHIC_0___WXLIB_AUI_p)
COND_MONOLITHIC_0___WXLIB_RICHTEXT_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)_richtext-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_RICHTEXT_p = $(COND_MONOLITHIC_0___WXLIB_RICHTEXT_p)
COND_MONOLITHIC_0___WXLIB_HTML_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)_html-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_HTML_p = $(COND_MONOLITHIC_0___WXLIB_HTML_p)
COND_MONOLITHIC_0___WXLIB_XML_p = \
	-lwx_base$(WXBASEPORT)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)_xml-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_XML_p = $(COND_MONOLITHIC_0___WXLIB_XML_p)
COND_MONOLITHIC_0___WXLIB_CORE_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)_core-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_CORE_p = $(COND_MONOLITHIC_0___WXLIB_CORE_p)
COND_MONOLITHIC_0___WXLIB_BASE_p = \
	-lwx_base$(WXBASEPORT)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
__WXLIB_BASE_p = $(COND_MONOLITHIC_0___WXLIB_BASE_p)
#__screenshotgen___win32rc = \
#	screenshotgen_screenshotgen_rc.o
COND_MONOLITHIC_1___WXLIB_MONO_p = \
	-lwx_$(PORTNAME)$(WXUNIVNAME)$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
#__WXLIB_MONO_p = $(COND_MONOLITHIC_1___WXLIB_MONO_p)
#__LIB_SCINTILLA_IF_MONO_p \
#	= \
#	-lwxscintilla$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
#__LIB_TIFF_p \
#	= \
#	-lwxtiff$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
#__LIB_JPEG_p \
#	= \
#	-lwxjpeg$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
#__LIB_PNG_p \
#	= \
#	-lwxpng$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
#__LIB_ZLIB_p = \
#	-lwxzlib$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
COND_wxUSE_REGEX_builtin___LIB_REGEX_p = \
	-lwxregex$(WXUNICODEFLAG)$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)
#__LIB_REGEX_p = $(COND_wxUSE_REGEX_builtin___LIB_REGEX_p)
#__LIB_EXPAT_p = \
#	-lwxexpat$(WXDEBUGFLAG)$(WX_LIB_FLAVOUR)-$(WX_RELEASE)$(HOST_SUFFIX)

### Targets: ###

all: screenshotgen$(EXEEXT) $(__screenshotgen_bundle___depname) bitmaps xml

install: install_screenshotgen

uninstall: uninstall_screenshotgen

install-strip: install
	$(STRIP) $(DESTDIR)$(bindir)/screenshotgen$(EXEEXT)

clean: 
	rm -rf ./.deps ./.pch
	rm -f ./*.o
	rm -f screenshotgen$(EXEEXT)
	rm -rf screenshotgen.app

distclean: clean
	rm -f config.cache config.log config.status bk-deps bk-make-pch Makefile

screenshotgen$(EXEEXT): $(SCREENSHOTGEN_OBJECTS) $(__screenshotgen___win32rc)
	$(CXX) -o $@ $(SCREENSHOTGEN_OBJECTS)    -L$(LIBDIRNAME)  $(LDFLAGS_GUI) $(HEADER_PAD_OPTION) $(SAMPLES_RPATH_FLAG) $(LDFLAGS)  $(WX_LDFLAGS) $(__WXLIB_AUI_p)  $(__WXLIB_RICHTEXT_p)  $(__WXLIB_HTML_p) $(EXTRALIBS_HTML) $(__WXLIB_XML_p) $(EXTRALIBS_XML) $(__WXLIB_CORE_p)  $(__WXLIB_BASE_p)  $(__WXLIB_MONO_p) $(__LIB_SCINTILLA_IF_MONO_p) $(__LIB_TIFF_p) $(__LIB_JPEG_p) $(__LIB_PNG_p)  $(EXTRALIBS_FOR_GUI) $(__LIB_ZLIB_p) $(__LIB_REGEX_p) $(__LIB_EXPAT_p) $(EXTRALIBS_FOR_BASE) $(LIBS)
	$(__screenshotgen___os2_emxbindcmd)

#screenshotgen.app/Contents/PkgInfo: screenshotgen$(EXEEXT) $(top_srcdir)/src/osx/carbon/Info.plist.in $(top_srcdir)/src/osx/carbon/wxmac.icns
#	mkdir -p screenshotgen.app/Contents
#	mkdir -p screenshotgen.app/Contents/MacOS
#	mkdir -p screenshotgen.app/Contents/Resources
#	
#	
#	sed -e "s/\$${MACOSX_BUNDLE_GUI_IDENTIFIER}/org.wxwidgets.screenshotgen/" \
#	-e "s/\$${MACOSX_BUNDLE_EXECUTABLE_NAME}/screenshotgen/" \
#	-e "s/\$${MACOSX_BUNDLE_BUNDLE_NAME}/screenshotgen/" \
#	-e "s/\$${MACOSX_BUNDLE_COPYRIGHT}/Copyright 2002-2023 wxWidgets/" \
#	-e "s/\$${MACOSX_BUNDLE_BUNDLE_VERSION}/$(WX_VERSION)/" \
#	-e "s/\$${MACOSX_BUNDLE_INFO_STRING}/screenshotgen version $(WX_VERSION), (c) 2002-2023 wxWidgets/" \
#	-e "s/\$${MACOSX_BUNDLE_LONG_VERSION_STRING}/$(WX_VERSION), (c) 2002-2023 wxWidgets/" \
#	-e "s/\$${MACOSX_BUNDLE_SHORT_VERSION_STRING}/$(WX_RELEASE)/" \
#	$(top_srcdir)/src/osx/carbon/Info.plist.in >screenshotgen.app/Contents/Info.plist
#	
#	
#	/bin/echo "APPL????" >screenshotgen.app/Contents/PkgInfo
#	
#	
#	ln -f screenshotgen$(EXEEXT) screenshotgen.app/Contents/MacOS/screenshotgen
#	
#	
#	cp -f $(top_srcdir)/src/osx/carbon/wxmac.icns screenshotgen.app/Contents/Resources/wxmac.icns

#screenshotgen_bundle: $(____screenshotgen_BUNDLE_TGT_REF_DEP)

install_screenshotgen: screenshotgen$(EXEEXT)
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) screenshotgen$(EXEEXT) $(DESTDIR)$(bindir)

uninstall_screenshotgen: 
	rm -f $(DESTDIR)$(bindir)/screenshotgen$(EXEEXT)

bitmaps: 
	@mkdir -p ./bitmaps
	@for f in wxwin32x32.png bell.png sound.png dropbuth.png dropbutn.png dropbutp.png throbber.gif; do \
	if test ! -f ./bitmaps/$$f -a ! -d ./bitmaps/$$f ; \
	then x=yep ; \
	else x=`find $(srcdir)/bitmaps/$$f -newer ./bitmaps/$$f -print` ; \
	fi; \
	case "$$x" in ?*) \
	cp -pRf $(srcdir)/bitmaps/$$f ./bitmaps ;; \
	esac; \
	done

xml: 
	@mkdir -p .
	@for f in richtext.xml; do \
	if test ! -f ./$$f -a ! -d ./$$f ; \
	then x=yep ; \
	else x=`find $(srcdir)/$$f -newer ./$$f -print` ; \
	fi; \
	case "$$x" in ?*) \
	cp -pRf $(srcdir)/$$f . ;; \
	esac; \
	done

screenshotgen_screenshot_app.o: $(srcdir)/screenshot_app.cpp
	$(CXXC) -c -o $@ $(SCREENSHOTGEN_CXXFLAGS) $(srcdir)/screenshot_app.cpp

screenshotgen_screenshot_main.o: $(srcdir)/screenshot_main.cpp
	$(CXXC) -c -o $@ $(SCREENSHOTGEN_CXXFLAGS) $(srcdir)/screenshot_main.cpp

screenshotgen_guiframe.o: $(srcdir)/guiframe.cpp
	$(CXXC) -c -o $@ $(SCREENSHOTGEN_CXXFLAGS) $(srcdir)/guiframe.cpp

screenshotgen_customcombo.o: $(srcdir)/customcombo.cpp
	$(CXXC) -c -o $@ $(SCREENSHOTGEN_CXXFLAGS) $(srcdir)/customcombo.cpp

screenshotgen_autocapture.o: $(srcdir)/autocapture.cpp
	$(CXXC) -c -o $@ $(SCREENSHOTGEN_CXXFLAGS) $(srcdir)/autocapture.cpp

screenshotgen_screenshotgen_rc.o: $(srcdir)/screenshotgen.rc
	$(WINDRES) -i$< -o$@    --define __WX$(TOOLKIT)__ $(__WXUNIV_DEFINE_p_1) $(__DEBUG_DEFINE_p_1)  $(__EXCEPTIONS_DEFINE_p_1) $(__RTTI_DEFINE_p_1) $(__THREAD_DEFINE_p_1)  --include-dir $(srcdir) $(__DLLFLAG_p_1) $(__WIN32_DPI_MANIFEST_p) --include-dir $(srcdir)/../../../samples $(__RCDEFDIR_p) --include-dir $(top_srcdir)/include


# Include dependency info, if present:
-include ./.deps/*.d

.PHONY: all install uninstall clean distclean screenshotgen_bundle \
	install_screenshotgen uninstall_screenshotgen bitmaps xml
