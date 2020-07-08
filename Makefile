# $Id$

# COPYRIGHT
# 
# Copyright © 2020 Douglas Lewan, d.lewan2000@gmail.com
# All rights reserved.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

################################################################
#
# What this is
# 

PACKAGE			=	affiliated-buffers
PACKAGE_TARNAME		=	affiliated-buffers-${PACKAGE_VERSION}.tar	
PACKAGE_VERSION		=	0.02
PACKAGE_BUGREPORT	=	d.lewan2000@gmail.com	
PACKAGE_URL		=	

################################################################
#
# Components
# 
CFILES		=
OFILES		=	${CFILES:.c=.o}
BINFILES	=	${PACKAGE}
SHELLFILES	=	
EXECUTABLES	=	${BINFILES} ${SHELLFILES}
ELFILES		=	affiliated-buffers.el
ELCFILES	=	${ELFILES:.el=.elc}
TESTFILES	=	affiliated-buffers-test.el
SRCFILES	=	${ELFILES} ${TESTFILES} Makefile COPYING

################################################################
#
# How we build
#
SHELL	=	/bin/sh

CC	=	gcc
CFLGAGS	=	
LIBS	=	

INSTALL	=	install

################################################################
#
# Installation directories
# 
prefix			=	${HOME}/local
exec_prefix		=	${prefix}
bindir			=	${prefix}/bin
sbindir			=	${prefix}/sbin
libexecdir		=	${libdir}
datarootdir		=	
datadir			=	${datarootdir}
sysconfdir		=	${prefix}/etc/${PACKAGE}/${PACKAGE_VERSION}
sharedstatedir		=	
localstatedir		=	
includedir		=	${prefix}/include
oldincludedir		=	@oldincludedir@
docdir			=	${prefix}/share/doc/${PACKAGE}/${PACKAGE_VERSION}
infodir			=	${docdir}/info
htmldir			=	${docdir}/html
dvidir			=	${docdir}
pdfdir			=	${docdir}
psdir			=	${docdir}
libdir			=	${prefix}/lib/${PACKAGE}/${PACKAGE_VERSION}
lispdir			=	${prefix}/lisp/${PACKAGE}/${PACKAGE_VERSION}
localedir		=	
mandir			=	${prefix}/man
man1dir			=	${mandir}/man1
man2dir			=	${mandir}/man2
man3dir			=	${mandir}/man3
man4dir			=	${mandir}/man4
man5dir			=	${mandir}/man5
man6dir			=	${mandir}/man6
man7dir			=	${mandir}/man7
man8dir			=	${mandir}/man8
manext			=	.1
man1ext			=	.1
man2ext			=	.2
man3ext			=	.3
man4ext			=	.4
man5ext			=	.5
man6ext			=	.6
man7ext			=	.7
man8ext			=	.8
abssrcdir		=	${shell pwd}
srcdir			=	${shell pwd}

.SUFFIXES:
.SUFFIXES: .c .o

################################################################
#
# Building
# 
.PHONY: default all check TAGS tags

.c.o:
	${CC} -c ${CFLAGS} ${<}

%.elc: %.el
	emacs --batch --quick --eval "(byte-compile-file \"${<}\")"

default:
	@echo "This Makefile has no default target."
	@echo "Try one of the following:"
	@echo "    all"
	@echo "    check"
	@echo "    clean"
	@echo "    dist"
	@echo "    doc"
	@echo "    install"

all: ${ELCFILES}

check: clean all
	emacs -Q -batch					\
		--load affiliated-buffers-test.el	\
		--load ert				\
		--funcall ert-run-tests-batch-and-exit

TAGS:
	etags ${ELFILES} affiliated-buffers-test.el

tags: TAGS

################################################################
#
# Packaging
# 

ship:	check clean
	packageDir=${PACKAGE}-${PACKAGE_VERSION} ; 	\
	if [ -e $${packageDir} ] ; then			\
		rm -rf $${packageDir} ;			\
	fi ;						\
	mkdir $${packageDir} ;				\
	cp ${SRCFILES} $${packageDir} ;			\
	tar cf ${PACKAGE_TARNAME} ./$${packageDir}


################################################################
#
# Installation
# 
.PHONY: install install check installdirs install-doc install-html install-dvi install-pdf install-ps uninstall install-strip
install: check all installcheck installdirs install-doc
	${INSTALL} ${ELCFILES} ${lispdir}
installcheck:
installdirs:
	${INSTALL} -d ${bindir} ${sbindir} ${libdir} ${mandir} ${man1dir} ${lispdir}
install-doc:
install-html:
install-dvi:
install-pdf:
install-ps:
uninstall:
	for f in ${ELCFILES} ; do		\
		rm -f ${lispdir}/$${f} ; 	\
	done
install-strip:
################################################################
#
# Hygiene
# 
.PHONY: clean distclean mostlyclean maintainer-clean
clean:
	-rm -f *.elc
distclean: clean
mostlyclean: clean
maintainer-clean: distclean
################################################################
#
# Documentation
# 
.PHONY: doc info dvi html pdf ps
doc: info dvi html pdf ps
info:
dvi:
html:
pdf:
ps:

################################################################
#
# Distribution
# 
.PHONY: dist
dist:
