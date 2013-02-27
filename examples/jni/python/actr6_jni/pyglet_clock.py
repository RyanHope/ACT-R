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

import pyglet

class Pyglet_MPClock(pyglet.clock.Clock):
           
    def __init__(self, fps_limit=None, time_function=pyglet.clock._default_time_function):
        super(Pyglet_MPClock, self).__init__(fps_limit=fps_limit, time_function=time_function)
        self.actr_clock_on = False
        self.original_clock = self.time
        self.actr_time = 0
        self.actr_start_time = 0
        
    def use_actr_clock(self, state):
        if self.actr_clock_on != state:
            self.actr_clock_on = state
            if self.actr_clock_on:
                self.actr_start_time = self.time()
                self.actr_time = self.actr_start_time
                self.time = self.actr_clock
            else:
                self.time = pyglet.clock._default_time_function
                
    def actr_clock(self):
        print "ACT-R Clock: ",
        print self.actr_time
        return self.actr_time

    def setTime(self, newTime):
        print "ACT-R Set Time: ",
        print newTime
        self.actr_time = self.actr_start_time + newTime
