# -*- coding:    utf-8 -*-
#===============================================================================
# This file is part of ACTR6_JNI.
# Copyright (C) 2012-2013 Ryan Hope <rmh3093@gmail.com>
#
# ACTR6_JNI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ACTR6_JNI is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ACTR6_JNI.  If not, see <http://www.gnu.org/licenses/>.
#===============================================================================

class VisualChunk(object):

    def __init__( self, name, isa, screenx, screeny, width = None, height = None, color = None, size = None, value = None, **slots ):
        self.name = name
        self.isa = isa
        self.screenx = screenx
        self.screeny = screeny
        self.width = width
        self.height = height
        self.color = color
        self.size = size
        self.value = value
        self.slots = slots

    def get_visual_object( self ):
        chunk = {"isa": self.isa,
                 "slots": {}}
        if self.width:
            chunk["slots"]["width"] = self.width
        if self.height:
            chunk["slots"]["height"] = self.height
        if self.color:
            chunk["slots"]["color"] = self.color
        if self.value:
            chunk["slots"]["value"] =  self.value
        for s, v in self.slots.iteritems():
            chunk["slots"][s] = v
        return chunk

    def get_visual_location( self ):
        chunk = {"isa": "visual-location",
                 "slots": {"kind": ":%s" % self.isa,
                           "screen-x": self.screenx,
                           "screen-y": self.screeny,
                           }}
        if self.width:
            chunk["slots"]["width"] = self.width
        if self.height:
            chunk["slots"]["height"] = self.height
        if self.color:
            chunk["slots"]["color"] = self.color
        if self.size:
            chunk["slots"]["size"] = self.size
        if self.value:
            chunk["slots"]["value"] =  self.value
        return chunk

class PAAVChunk(VisualChunk):
    
    def get_visual_location( self ):
        chunk = super(PAAVChunk, self).get_visual_location()
        for s, v in self.slots.iteritems():
            if s in ["fcolor", "fshape", "fsize", "fshading", "forient"]:
                chunk["slots"][s] = v
        return chunk
    