### ---------------------------------------------------------------------------- ###
###  LISCPL - Lisp Player Client - makefile                                      ###
###  Copyright (C) 2006, Armin Mueller                                           ###
###                                                                              ###
###  This program is free software; you can redistribute it and/or modify        ###
###  it under the terms of the GNU General Public License as published by        ###
###  the Free Software Foundation; either version 2 of the License, or           ###
###  (at your option) any later version.                                         ###
###                                                                              ###
###  This program is distributed in the hope that it will be useful,             ###
###  but WITHOUT ANY WARRANTY; without even the implied warranty of              ###
###  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               ###
###  GNU General Public License for more details.                                ###
###                                                                              ###
###  You should have received a copy of the GNU General Public License           ###
###  along with this program; if not, write to the Free Software                 ###
###  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   ###
### ---------------------------------------------------------------------------- ###

HEADERS_PATH := $(shell pkg-config --cflags playercore | xargs echo | sed 's/-I//')/libplayercore
LISP_FILES := constants-player.lisp xdr-player.lisp proxies-player.lisp
CONST_GEN := ../scripts/player-constants-gen
XDR_GEN := ../scripts/player-xdr-gen.py
PROXY_GEN := ../scripts/player-proxy-gen.py

.PHONY: default
default: $(LISP_FILES)

.PHONY: clean
clean:
	rm -f $(LISP_FILES)

constants-player.lisp: $(HEADERS_PATH)/player.h
	$(CONST_GEN) $< $@

xdr-player.lisp: $(HEADERS_PATH)/player.h
	$(XDR_GEN) $< $@

proxies-player.lisp: $(HEADERS_PATH)/player.h
	$(PROXY_GEN) $< $@

