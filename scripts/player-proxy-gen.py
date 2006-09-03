#!/usr/bin/env python

### ---------------------------------------------------------------------------- ###
###  LISCPL - Lisp Player Client - player-proxy-gen.py                           ###
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
USAGE = 'USAGE: player-proxy-gen.y <interface-spec.h> <outfile.lisp>'

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

  # find interfaces
  pattern = re.compile('PLAYER_\w+_CODE');
  interfaces = [ lispConstant(interf) for interf in pattern.findall(instream)
                                      if not interf in ['PLAYER_AUDIO_CODE',
                                                        'PLAYER_ENERGY_CODE',
                                                        'PLAYER_NOMAD_CODE',
                                                        'PLAYER_NULL_CODE',
                                                        'PLAYER_PLAYER_CODE',
                                                        'PLAYER_TRUTH_CODE'] ]
  interfaces.sort()

  # interfaces data
  interfData = {'*player-actarray-code*':{'*player-actarray-data-state*':'player-actarray-data'},
                '*player-aio-code*':{'*player-aio-data-state*':'player-aio-data'},
                '*player-audiodsp-code*':{'*player-audiodsp-data-tones*':'player-audiodsp-data'},
                '*player-audiomixer-code*':False,
                '*player-blinkenlight-code*':{'*player-blinkenlight-data-state*':'player-blinkenlight-data'},
                '*player-blobfinder-code*':{'*player-blobfinder-data-blobs*':'player-blobfinder-data'},
                '*player-bumper-code*':{'*player-bumper-data-state*':'player-bumper-data',
                                        '*player-bumper-data-geom*':'player_bumper_geom'},
                '*player-camera-code*':{'*player-camera-data-state*':'player-camera-data'},
                '*player-dio-code*':{'*player-dio-data-values*':'player-dio-data'},
                '*player-fiducial-code*':{'*player-fiducial-data-scan*':'player-fiducial-data'},
                '*player-gps-code*':{'*player-gps-data-state*':'player-gps-data'},
                '*player-graphics2d-code*':False,
                '*player-graphics3d-code*':False,
                '*player-gripper-code*':{'*player-gripper-data-state*':'player-gripper-data'},
                '*player-ir-code*':{'*player-ir-data-ranges*':'player-ir-data'},
                '*player-joystick-code*':{'*player-joystick-data-state*':'player-joystick-data'},
                '*player-laser-code*':{'*player-laser-data-scan*':'player-laser-data',
                                       '*player-laser-data-scanpose*':'player-laser-data-scanpose'},
                '*player-limb-code*':{'*player-limb-data*':'player-limb-data'},
                '*player-localize-code*':{'*player-localize-data-hypoths*':'player-localize-data'},
                '*player-log-code*':False,
                '*player-map-code*':{'*player-map-data-info*':'player-map-info'},
                '*player-mcom-code*':False,
                '*player-opaque-code*':{'*player-opaque-data-state*':'player-opaque-data'},
                '*player-planner-code*':{'*player-planner-data-state*':'player-planner-data'},
                '*player-position1d-code*':{'*player-position1d-data-state*':'player-position1d-data',
                                            '*player-position1d-data-geom*':'player-position1d-geom'},
                '*player-position2d-code*':{'*player-position2d-data-state*':'player-position2d-data',
                                            '*player-position2d-data-geom*':'player-position2d-geom'},
                '*player-position3d-code*':{'*player-position3d-data-state*':'player-position3d-data',
                                            '*player-position3d-data-geometry*':'player-position3d-geom'},
                '*player-power-code*':{'*player-power-data-state*':'player-power-data'},
                '*player-ptz-code*':{'*player-ptz-data-state*':'player-ptz-data',
                                     '*player-ptz-data-geom*':'player-ptz-geom'},
                '*player-rfid-code*':{'*player-rfid-data*':'player-rfid-data'},
                '*player-simulation-code*':False,
                '*player-sonar-code*':{'*player-sonar-data-ranges*':'player-sonar-data',
                                       '*player-sonar-data-geom*':'player-sonar-geom'},
                '*player-sound-code*':False,
                '*player-speech-code*':False,
                '*player-speech-recognition-code*':{'*player-speech-recognition-data-string*':'player-speech-recognition-data'},
                '*player-waveform-code*':{'*player-waveform-data-sample*':'player-waveform-data'},
                '*player-wifi-code*':{'*player-wifi-data-state*':'player-wifi-data'},
                '*player-wsn-code*':{'*player-wsn-data*':'player-wsn-data'}}

  # write gpl header
  gpl_header = [''';;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - ''', lispfilename, '''                           ;;;
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

  # write proxies
  exportProxies = []
  for interface in interfaces:
    proxyname = '%s-proxy' % re.compile('player-([\w-]+)-code').search(interface).group(1)
    exportProxies.append(proxyname)
    output = [';; ', proxyname, '''
(defclass ''', proxyname, ''' (std-proxy)
  ()
  (:default-initargs
    :devaddr (make-instance 'player-devaddr :interf ''', interface, ''')''',
    interfData[interface] and ''.join(['''
    :data (add-hash-entries (make-hash-table)
''', "\n".join(["            %s (make-instance '%s)" % (k,v) for k,v in interfData[interface].items()]), ''')'''])
      or '', ''')
  (:documentation "''', proxyname, '''"))

''']
    lispfile.write(''.join(output))

  # write export declaration
  lispfile.write('(eval-when (:compile-toplevel :load-toplevel :execute)\n')
  lispfile.write('  (export \'(; proxies\n')
  lispfile.write('            %s)))' % '\n            '.join(exportProxies))

  lispfile.close()

