# -*- coding:    utf-8 -*-
#===============================================================================
# This file is part of ACTR6_JNI.
# Copyright (C) 2012 Ryan Hope <rmh3093@gmail.com>
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
        chunk = []#[self.name]
        chunk.append( "isa %s" % self.isa )
        if self.width:
            chunk.append( "width %d" % self.width )
        if self.height:
            chunk.append( "height %d" % self.height )
        if self.color:
            chunk.append( "color %s" % self.color )
        if self.value:
            chunk.append( "value %s" % self.value )
        for s, v in self.slots.iteritems():
            chunk.append( "%s %s" % ( s, str( v ) ) )
        return "(%s)" % " ".join( chunk )

    def get_visual_location( self ):
        chunk = []#[self.name]
        chunk.append( "isa visual-location-ext" )
        chunk.append( "kind %s" % self.isa )
        chunk.append( "screen-x %d" % self.screenx )
        chunk.append( "screen-y %d" % self.screeny )
        if self.width:
            chunk.append( "width %d" % self.width )
        if self.height:
            chunk.append( "height %d" % self.height )
        if self.color:
            chunk.append( "color %s" % self.color )
        if self.size:
            chunk.append( "size %d" % self.size )
        if self.value:
            chunk.append( "value %s" % self.value )
        for s, v in self.slots.iteritems():
            chunk.append( "%s %s" % ( s, str( v ) ) )
        return "(%s)" % " ".join( chunk )
