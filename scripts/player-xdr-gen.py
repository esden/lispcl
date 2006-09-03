#!/usr/bin/env python

### ---------------------------------------------------------------------------- ###
###  LISCPL - Lisp Player Client - player-xdr-gen.py                             ###
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

import re
import string
import os.path
import sys

# constants
USAGE = 'USAGE: player-xdr-gen.y <interface-spec.h> <outfile.lisp>'
SIZE_GENERIC = 'get-size'
READ_GENERIC = 'xdr-read'
WRITE_GENERIC = 'xdr-write'

# auxilliary functions
def lispTypeName ( var ):
  return (var.endswith('_t') and var[:-2] or var).replace('_', '-')

def lispVarName ( var ):
  return var.replace('_', '-')

def lispConstant ( var ):
  return var.isdigit() and var or '*%s*' % var.lower().replace('_', '-')

# main
if __name__ == '__main__':

  if len(sys.argv) < 3:
    print USAGE
    sys.exit(-1)

  infilename = sys.argv[1]
  lispfilename = sys.argv[2]

  # Read in the entire file
  infile = open(infilename, 'r')
  instream = infile.read()
  infile.close()

  lispfile = open(lispfilename, 'w+')

  # strip C++-style comments
  pattern = re.compile('//.*')
  instream = pattern.sub('', instream)

  # strip C-style comments
  pattern = re.compile('/\*.*?\*/', re.MULTILINE | re.DOTALL)
  instream = pattern.sub('', instream)

  # strip blank lines
  pattern = re.compile('^\s*?\n', re.MULTILINE)
  instream = pattern.sub('', instream)

  # find structs
  pattern = re.compile('typedef\s+struct\s+player_\w+[^}]+\}[^;]+', re.MULTILINE)
  structs = pattern.findall(instream)

  print 'Found ' + `len(structs)` + ' struct(s)'

  # write gpl header
  gpl_header = [''';;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - ''', lispfilename, '''                               ;;;
;;;  Copyright (C) 2006, Armin Mueller                                           ;;;
;;;                                                                              ;;;
;;;  This program is free software; you can redistribute it and/or modify        ;;;
;;;  it under the terms of the GNU General Public License as published by        ;;;
;;;  the Free Software Foundation; either version 2 of the License, or           ;;;
;;;  (at your option) any later version.                                         ;;;
;;;                                                                              ;;;
;;;  This program is distributed in the hope that it will be useful,             ;;;
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of              ;;;
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               ;;;
;;;  GNU General Public License for more details.                                ;;;
;;;                                                                              ;;;
;;;  You should have received a copy of the GNU General Public License           ;;;
;;;  along with this program; if not, write to the Free Software                 ;;;
;;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   ;;;
;;; ---------------------------------------------------------------------------- ;;;

''']
  lispfile.write(''.join(gpl_header))

  # write package
  lispfile.write('(in-package "LISPCL")\n\n')

  # define regular expression patterns
  contentspattern = re.compile('.*\{\s*(.*?)\s*\}', re.MULTILINE | re.DOTALL)
  declpattern = re.compile('\s*([^;]*?;)', re.MULTILINE)
  typepattern = re.compile('\s*\S+')
  variablepattern = re.compile('\s*([^,;]+?)\s*[,;]')
  arraypattern = re.compile('\[(.*?)\]')

  # init exports
  exportClasses = []
  exportAccessors = {}

  for s in structs:
    # extract structure name
    structname = lispTypeName(string.split(s)[-1])
    exportClasses.append(structname)

    # pick out the contents of the struct
    varpart = contentspattern.findall(s)
    if len(varpart) != 1:
      print 'skipping nested / empty struct ' + structname
      continue

    # init strings
    comment = ';; %s\n' % structname
    defclass = '; class\n(defclass %s (player-standard-object)\n  (' % structname
    classSpace = ' '
    getsize = '; get size\n(defmethod %s ( (data %s) )\n  (+' % (SIZE_GENERIC, structname)
    getsizeSpace = ' '
    xdrread = '; xdr read\n(defmethod %s ( (data %s) (xdrs xdr-stream) )' % (READ_GENERIC, structname)
    xdrwrite = '; xdr write\n(defmethod %s ( (data %s) (xdrs xdr-stream) )' % (WRITE_GENERIC, structname)

    # init struct variables
    varlist = []

    # separate the variable declarations
    decls = declpattern.findall(varpart[0])
    for dstring in decls:
      # find the type and variable names in this declaration
      type = lispTypeName(typepattern.findall(dstring)[0])
      dstring = typepattern.sub('', dstring, 1)
      vars = variablepattern.findall(dstring)

      # Do some name mangling for common types
      if type == 'int64':
        lispType = 'integer'
        initVal = '0'
        readFun = 'xdr-read-hyper'
        writeFun = 'xdr-write-hyper'
        size = '8'
      elif type == 'uint64':
        lispType = 'integer'
        initVal = '0'
        readFun = 'xdr-read-uhyper'
        writeFun = 'xdr-write-uhyper'
        size = '8'
      elif type in ('int32', 'int16', 'int8'):
        lispType = 'integer'
        initVal = '0'
        readFun = 'xdr-read-int'
        writeFun = 'xdr-write-int'
        size = '4'
      elif type in ('uint32', 'uint16', 'uint8'):
        lispType = 'integer'
        initVal = '0'
        readFun = 'xdr-read-uint'
        writeFun = 'xdr-write-uint'
        size = '4'
      elif type == 'float':
        lispType = 'float'
        initVal = '0.0';
        readFun = 'xdr-read-float'
        writeFun = 'xdr-write-float'
        size = '4'
      elif type == 'double':
        lispType = 'float'
        initVal = '0.0';
        readFun = 'xdr-read-double'
        writeFun = 'xdr-write-double'
        size = '8'
      elif type == 'bool':
        lispType = 'boolean'
        initVal = 'nil'
        readFun = 'xdr-read-bool'
        writeFun = 'xdr-write-bool'
        size = '4'
      elif type == 'char':
        lispType = 'character'
        initVal = '#\\0'
        readFun = 'xdr-read-char'
        writeFun = 'xdr-write-char'
        size = '4'
      else:
        lispType = type
        initVal = '(make-instance \'%s)' % type
        readFun = READ_GENERIC
        writeFun = WRITE_GENERIC
        size = SIZE_GENERIC

      # iterate through each variable
      nextGetsizeSpace = ''
      for varname in vars:
        readFunVar = readFun
        writeFunVar = writeFun
        sizeVar = size
        # is it an array or a scalar?
        arraysize = arraypattern.findall(varname)
        if len(arraysize) > 0:
          arraysize = lispConstant(arraysize[0])
          varname = lispVarName(arraypattern.sub('', varname))
          accessorName = 'player-%s' % varname
          lispType = '(vector %s)' % lispType
          countvar = varname + '-count'
          countvarAccessor = 'player-%s' % countvar
          countvarP = countvar in varlist

          # special player xdr encoding
          if ( type in ('uint8', 'int8', 'char') ):
            sizeVar = 1
            padBytes = ' :pad-bytes T'
            extraSize = ' (mod (- 4 (mod %s 4)) 4)' \
                          % (countvarP and '(%s data)' % countvarAccessor or arraysize)
            if type == 'int8':
              readFunVar = 'xdr-read-player-int8'
              writeFunVar = 'xdr-write-player-int8'
            if type == 'uint8':
              readFunVar = 'xdr-read-player-uint8'
              writeFunVar = 'xdr-write-player-uint8'
            else:
              readFunVar = 'xdr-read-player-char'
              writeFunVar = 'xdr-write-player-char'
          else:
            extraSize = ''
            padBytes = ''

          # init value for defclass
          if ( initVal.startswith('(') ):
            initVal  = '\n    (map \'%s\n' % lispType
            initVal +=   '         #\'(lambda (x)\n'
            initVal +=   '             (declare (ignore x))\n'
            initVal +=   '             (make-instance \'%s))\n' % type
            initVal +=   '         (make-sequence \'%s %s))\n   ' % (lispType, arraysize)
          else:
            initVal  = '(make-sequence \'%s %s :initial-element %s)' \
                         % (lispType, arraysize, initVal)

          # size
          if sizeVar == SIZE_GENERIC:
            sizeVar = '(reduce #\'+ (map \'(vector integer) #\'%s %s))' \
                      % ( SIZE_GENERIC,
                          countvarP \
                            and '(subseq (%s data) 0 (%s data))' % (accessorName, countvarAccessor) \
                            or '(%s data)' % accessorName )
          else:
            sizeVar = '(* %s %s)' % (sizeVar,
                                     countvarP and '(%s data)' % countvarAccessor or arraysize)
          sizeVar = '%s%s%s' % (countvarP and '4 ' or '', sizeVar, extraSize)

          # read
          readFunVar = readFunVar == READ_GENERIC \
                         and readFunVar \
                         or '(lambda (data xdrs) (declare (ignore data)) (%s xdrs))' % readFunVar
          if countvarP:
            readFunVar = '(xdr-read-var-array (%s data) #\'%s xdrs%s)' \
                           % (accessorName, readFunVar, padBytes)
          else:
            readFunVar = '(xdr-read-fixed-array (%s data) #\'%s %s xdrs%s)' \
                           % (accessorName, readFunVar, arraysize, padBytes)

          # write
          writeFunVar = '(xdr-write-%s-array (%s data) #\'%s %s xdrs%s)' \
                          % (countvarP and 'var' or 'fixed',
                             accessorName,
                             writeFunVar,
                             countvarP and '(%s data)' % countvarAccessor or arraysize,
                             padBytes)

        # non array case
        else:
          varname = lispVarName(varname)
          accessorName = 'player-%s' % varname
          sizeVar = sizeVar == SIZE_GENERIC and '(%s (%s data))' % (sizeVar, accessorName) or sizeVar
          readFunVar = readFunVar == READ_GENERIC \
                         and '(%s (%s data) xdrs)' % (readFunVar, accessorName) \
                         or '(setf (%s data) (%s xdrs))' % (accessorName, readFunVar)
          writeFunVar = '(%s (%s data) xdrs)' % (writeFunVar, accessorName)

        # add varname to varlist
        varlist.append(varname)
        # append to export lists
        exportAccessors[accessorName] = 1
        # append to strings
        defclass += '%s(%s :initarg :%s :accessor %s :initform %s :type %s)' \
                      % (classSpace, varname, varname, accessorName, initVal, lispType)
        classSpace = '\n    '
        getsize += '%s%s' % (getsizeSpace, sizeVar)
        getsizeSpace = '\n     '
        xdrread += '\n  %s' % readFunVar
        xdrwrite += '\n  %s' % writeFunVar

    # finish string
    defclass += ' ))\n\n'
    getsize += '))\n\n'
    xdrread += '\n  data)\n\n'
    xdrwrite += ')\n\n'

    # write strings
    lispfile.write(comment)
    lispfile.write(defclass)
    lispfile.write(getsize)
    lispfile.write(xdrread)
    lispfile.write(xdrwrite)

  # write export declaration
  exportAccessors = exportAccessors.keys();
  exportAccessors.sort()
  lispfile.write('(eval-when (:compile-toplevel :load-toplevel :execute)\n')
  lispfile.write('  (export \'(; classes\n')
  lispfile.write('            %s\n' % '\n            '.join(exportClasses))
  lispfile.write('            ; accessors\n')
  lispfile.write('            %s)))' % '\n            '.join(exportAccessors))


  lispfile.close()

