#!/usr/bin/env python

# For general environment
from twisted.internet import reactor
from twisted.internet.task import LoopingCall, Cooperator

import pygame
import pygame.font

from random import sample, randint, choice
import string

ACTR6 = True
try:
    from actr6_jni import Dispatcher, JNI_Server, VisualChunk
except ImportError:
    ACTR6 = False

class Letter(object):

    colors = {'blue':(0, 0, 255), 'red':(255, 0, 0)}

    def __init__(self, letter, quad, start, colors, max_font_size):
        self.letter = letter
        self.start = start
        self.colorname = colors[self.start]
        self.color = self.colors[self.colorname]
        self.quad = quad
        self.size = randint(18, max_font_size)
        self.font = pygame.font.Font(pygame.font.match_font("Monospace", True), self.size)
        self.surf = self.font.render(self.letter, True, self.color)
        self.rect = self.surf.get_rect()
        self.bounding_rect = self.surf.get_bounding_rect()

    def toChunk(self):
        return VisualChunk(None, "letterobj", self.rect.centerx, self.rect.centery,
                           width=self.bounding_rect.width, color=self.colorname,
                           value=self.letter, quad=self.quad)

class Environment(object):
    
    if ACTR6:
        d = Dispatcher()

    STATE_WAIT = -2
    STATE_INTRO = -1
    STATE_RESET = 0
    STATE_FIXATION = 1
    STATE_UPDATE = 2
    STATE_SEARCH = 3
    STATE_DONE = 4

    colors = {'white':(255, 255, 255), 'black':(0, 0, 0)}

    def __init__(self, actr=False):

        self.screen_rect = pygame.Rect(0, 0, 350, 350)
        self.screen = pygame.display.set_mode((self.screen_rect.width, self.screen_rect.height), 0)

        self.grid_color = (128, 128, 128)
        self.max_font_size = int(min([self.screen_rect.width, self.screen_rect.height]) / 4)

        self.font = pygame.font.Font(pygame.font.match_font("Monospace", True), self.max_font_size / 5)
        self.spinner = ['|', '|', '/', '/', '-', '-', '\\', '\\']
        self.spinner_index = 0
        
        self.intro_t = self.font.render("Click the red 'X' to start!", True, (0, 0, 0))
        self.intro_ts = self.intro_t.get_rect()
        self.intro_ts.center = self.screen_rect.center
        self.intro_x = self.font.render("X", True, (255, 0, 0))
        self.intro_xs = self.intro_x.get_rect()
        self.intro_xs.center = (randint(0, self.screen_rect.width), randint(0, self.screen_rect.height))
        while not self.screen_rect.contains(self.intro_xs) or self.intro_ts.colliderect(self.intro_xs):
            self.intro_xs.center = (randint(0, self.screen_rect.width), randint(0, self.screen_rect.height))

        self.snd_correct = pygame.mixer.Sound("beep-3.wav")
        self.snd_incorrect = pygame.mixer.Sound("beep-5.wav")

        self.trial = 0
        self.fake_cursor = self.screen_rect.center

        self.state = self.STATE_INTRO
        self.actr = actr
        self.actr_running = False
        if ACTR6 and self.actr:
            self.state = self.STATE_WAIT
            self.actr = JNI_Server(self)
            self.actr.addDispatcher(self.d)
            reactor.listenTCP(5555, self.actr)

        self.lc1 = LoopingCall(self.update_env)
        self.lc1.start(1.0 / 30)

        self.coop = Cooperator()
        self.coop.coiterate(self.process_event())

    def reset(self):
        self.trial += 1
        self.intro_xs.center = (randint(0, self.screen_rect.width), randint(0, self.screen_rect.height))
        while not self.screen_rect.contains(self.intro_xs) or self.intro_ts.colliderect(self.intro_xs):
            self.intro_xs.center = (randint(0, self.screen_rect.width), randint(0, self.screen_rect.height))
        self.start = sample([1, 0, 0, 0], 4)
        colors = sample(["red", "blue"], 2)
        self.letters = sample(string.ascii_uppercase, 4)
        self.objects = [Letter(self.letters[i], i + 1, self.start[i], colors, self.max_font_size) for i in range(0, len(self.letters))]
        self.clockwise = choice([True, False])
        if self.clockwise:
            self.bgcolorname = 'black'
        else:
            self.bgcolorname = 'white'
        self.bgcolor = self.colors[self.bgcolorname]
        start = self.start.index(True)
        if self.clockwise:
            self.answer = [self.letters[(start + i) % 4] for i in range(0, 4)]
        else:
            self.answer = [self.letters[(start - i) % 4]  for i in range(0, 4)]
        self.response = []

    def validate(self):
        if self.answer == self.response:
            self.snd_correct.play()
            if self.actr and self.actr_running:
                self.actr.tone_sound(1320, .25)
        else:
            self.snd_incorrect.play()
            if self.actr and self.actr_running:
                self.actr.tone_sound(440, .25)
        self.state = self.STATE_DONE

    def update_objects(self):
        for i in range(0, len(self.objects)):
            if self.objects[i].quad == 1 or self.objects[i].quad == 2:
                basey = self.screen_rect.height / 4
            else:
                basey = self.screen_rect.height / 4 * 3
            if self.objects[i].quad == 1 or self.objects[i].quad == 4:
                basex = self.screen_rect.width / 4
            else:
                basex = self.screen_rect.width / 4 * 3
            self.objects[i].rect.centerx = randint(basex - self.screen_rect.width / 8, basex + self.screen_rect.width / 8)
            self.objects[i].rect.centery = randint(basey - self.screen_rect.height / 8, basey + self.screen_rect.height / 8)

    def draw_intro(self):
        self.screen.fill((128, 128, 128))
        self.screen.blit(self.intro_t, self.intro_ts)
        self.screen.blit(self.intro_x, self.intro_xs)
        pygame.display.flip()

    def draw_actr_wait(self):
        self.screen.fill((128, 128, 128))
        f = self.font.render("Waiting for ACT-R", True, (0, 0, 0))
        fs = f.get_rect()
        fs.midbottom = self.screen_rect.center
        self.screen.blit(f, fs)
        f = self.font.render(self.spinner[self.spinner_index], True, (0, 0, 0))
        fs = f.get_rect()
        fs.midtop = self.screen_rect.center
        self.screen.blit(f, fs)
        self.spinner_index = (self.spinner_index + 1) % 8
        pygame.display.flip()

    def draw_fixation(self):
        self.screen.fill(self.bgcolor)
        pygame.draw.line(self.screen, self.grid_color, (self.screen_rect.centerx - 10, self.screen_rect.centery), (self.screen_rect.centerx + 10, self.screen_rect.centery), 1)
        pygame.draw.line(self.screen, self.grid_color, (self.screen_rect.centerx, self.screen_rect.centery - 10), (self.screen_rect.centerx, self.screen_rect.centery + 10), 1)
        pygame.display.flip()

    def draw_search(self):
        self.screen.fill(self.bgcolor)
        pygame.draw.line(self.screen, self.grid_color, self.screen_rect.midtop, self.screen_rect.midbottom, 1)
        pygame.draw.line(self.screen, self.grid_color, self.screen_rect.midleft, self.screen_rect.midright, 1)
        for o in self.objects:
            self.screen.blit(o.surf, o.rect)
        pygame.display.flip()

    def update_env(self):
        if self.state == self.STATE_WAIT:
            self.draw_actr_wait()
        if self.state == self.STATE_INTRO:
            self.draw_intro()
        if self.state == self.STATE_RESET:
            self.reset()
            self.state = self.STATE_FIXATION
            if self.actr and self.actr_running:
                fix = VisualChunk("f%d" % self.trial, "fixation-cross", self.screen_rect.centerx, self.screen_rect.centery)
                self.actr.update_display([fix], clear=True)
        if self.state == self.STATE_UPDATE:
            self.update_objects()
            self.state = self.STATE_SEARCH
            if self.actr and self.actr_running:
                chunks = [obj.toChunk() for obj in self.objects]
                chunks.append(VisualChunk(None, "background", self.screen_rect.centerx, self.screen_rect.centery, self.screen_rect.width, self.screen_rect.height, self.bgcolorname))
                self.actr.update_display(chunks, clear=True)
        if self.state == self.STATE_FIXATION:
            self.draw_fixation()
        elif self.state == self.STATE_SEARCH:
            self.draw_search()

    def process_event(self):
        while True:
            for e in pygame.event.get():
                if e.type == pygame.KEYDOWN:
                    if e.key == pygame.K_ESCAPE:
                        reactor.stop()
                    elif e.key == pygame.K_SPACE:
                        if self.state == self.STATE_FIXATION:
                            self.state = self.STATE_UPDATE
                        elif self.state == self.STATE_DONE:
                            self.state = self.STATE_RESET
                    elif e.key >= pygame.K_a and e.key <= pygame.K_z:
                        if self.state == self.STATE_SEARCH:
                            self.response.append(str.upper(str(e.unicode)))
                            if len(self.response) == 4:
                                self.validate()
                elif e.type == pygame.MOUSEBUTTONDOWN:
                    if self.state == self.STATE_INTRO:
                        if self.intro_xs.collidepoint(e.pos):
                            self.state = self.STATE_RESET
            yield

    def setDefaultClock(self):
        self.lc1.stop()
        self.lc1.clock = reactor
        self.lc1.start(1.0 / 30)

    if ACTR6:

        @d.listen('connectionMade')
        def ACTR6_JNI_Event(self, model, params):
            self.state = self.STATE_INTRO
            X = VisualChunk(None, "letterobj", self.intro_xs.centerx, self.intro_xs.centery, color="red")
            self.actr.update_display([X], clear=True)

        @d.listen('connectionLost')
        def ACTR6_JNI_Event(self, model, params):
            self.actr_running = False
            self.setDefaultClock()
            self.state = self.STATE_WAIT

        @d.listen('reset')
        def ACTR6_JNI_Event(self, model, params):
            self.actr_running = False
            self.setDefaultClock()
            self.state = self.STATE_WAIT

        @d.listen('model-run')
        def ACTR6_JNI_Event(self, model, params):
            self.actr_running = True
            self.lc1.stop()
            self.lc1.clock = self.actr.clock
            self.lc1.start(1.0 / 30)
            self.actr.ready()

        @d.listen('model-stop')
        def ACTR6_JNI_Event(self, model, params):
            self.actr_running = False

        @d.listen('keypress')
        def ACTR6_JNI_Event(self, model, params):
            pygame.event.post(pygame.event.Event(pygame.KEYDOWN, unicode=chr(params[0]), key=params[0], mod=None))

        @d.listen('mousemotion')
        def ACTR6_JNI_Event(self, model, params):
            # Store "ACT-R" cursor in variable since we are 
            # not going to move the real mouse
            self.fake_cursor = params[0]

        @d.listen('mouseclick')
        def ACTR6_JNI_Event(self, model, params):
            # Simulate a button press using the "ACT-R" cursor loc
            pygame.event.post(pygame.event.Event(pygame.MOUSEBUTTONDOWN, button=1, pos=self.fake_cursor))

if __name__ == '__main__':

    pygame.display.init()
    pygame.font.init()
    pygame.mixer.init()

    env = Environment(actr=True)
    reactor.run()
