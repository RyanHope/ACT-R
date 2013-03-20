#!/usr/bin/env python

"""Farback"""
print __doc__

import sys, time, random, os
import pygame
import numpy
import argparse

from pycogworks.logging import get_time, writeHistoryFile
try:
    from pycogworks.gui import getSubjectInfo
    Subj_Support = True
except ImportError:
    Subj_Support = False

#from pycogworks.crypto import rin2id
from twisted.internet import reactor
from twisted.internet.task import LoopingCall, Cooperator

ACTR6 = True
try:
    from actr6_jni import Dispatcher, JNI_Server, VisualChunk, Twisted_MPClock
    BB_Support = False
    ET_Support = False
except ImportError:
    ACTR6 = False


if not ACTR6:
    try:
        import Button_box as bb
        BB_Support = True
    except ImportError:
        BB_Support = False
    try:
        from pyviewx.client import iViewXClient, Dispatcher
        from pyviewx.pygame import Calibrator
        ET_Support = True
    except ImportError:
        ET_Support = False


### Generic Parameter, Subject and Logging Classes ###
class Parameters ():

    colors = {
            'Black':(0, 0, 0),
            'White':(255, 255, 255),
            'Red':(255, 0, 0),
            'Green':(0, 255, 0),
            'Blue':(0, 0, 255),
            'Yellow':(255, 255, 0),
            'Purple':(148, 0, 211),
            'Grey':(211, 211, 211),
            'Aqua':(0, 255, 255),
            'Snow1':(238, 233, 233),
            'Snow2':(205, 201, 201)
            }
    choices = '1357'
    key_mapping = {pygame.K_UP : 5, pygame.K_DOWN : 1,
                        pygame.K_LEFT : 3, pygame.K_RIGHT : 7}
    bb_mapping = {0 : 1, 1 : 3, 2 : 5, 3 : 7}
    logging = True
    eyetracking= False

    def __init__( self ):

        tm = time.localtime()
        self.filename = 'FB_%4i%02i%02i_%02i%02i%02i' % (tm.tm_year,tm.tm_mon,tm.tm_mday,
                                             tm.tm_hour,tm.tm_min,tm.tm_sec)
        return
    def get_color(self, c):
        return(self.colors[c])

    def get_key(self,v):
        for key,value in self.colors.items():
            if v == value:
                return(key)

    def get_choices(self):
        return(self.choices)

    def logging_enabled_p(self):
        return(self.logging)

    def eyetracking_enabled_p(self):
        return(self.eyetracking and ET_Support)

    def file_name (self):
        return(self.filename)


class Subject ():
    def __init__( self,dbg  = 1):
        if not dbg and Subj_Support:
            self.subj_info = getSubjectInfo()
            writeHistoryFile(p.file_name(),self.subj_info)
            logger.LogInfo('subject', 'init', self.subj_info['encrypted_rin'])
        return

class Logger ():
    def __init__( self,args):
        if p.logging_enabled_p():
            try:
                os.mkdir(args.logfile)
            except OSError:
                pass
            self.log_file = file(args.logfile + p.file_name() + ".txt",'w')
        return

    def LogInfo (self, *args):
        if p.logging_enabled_p():

            ln = [repr(get_time())]
            ln.extend(map(lambda x: str(x),args))
            self.log_file.write('\t'.join(ln))
            self.log_file.write('\n')
        else:
            print (get_time(), args)
        return

    def LogHeader(self):
        if p.logging_enabled_p():
            self.log_file.write('\t'.join(['Time','Event','v3','v4','v5','v6','v7','v8','v9']))
            self.log_file.write('\n')

    def CloseLog( self ):
        if p.logging_enabled_p():
            self.log_file.close()
        return


### Experiment Task Classes ###

# Farback definitions:
# item - presentation of stimulus or test rectangle
# sequence - a n-up or down sequnece of items
# trial - a complete series of 1-up to 1-down sequences Note: trial 2 begins with 2-up
# block - a series of trials

class ItemPerf():
    def __init__(self, response, response_correct,response_time):
        self.response = response
        self.response_correct = response_correct
        self.response_time = response_time

class PerfAccum():
    def __init__(self,num_correct,num_incorrect, num_noresponse,corr_tms,incorr_tms,no_response_tms):
        self.num_correct = num_correct
        self.num_incorrect = num_incorrect
        self.num_noresponse = num_noresponse
        self.corr_tms = corr_tms
        self.incorr_tms = incorr_tms
        self.no_response_tms = no_response_tms
        return
    
class FarBackSeq ( ):
    '''One windup or wind down sequence. Parameters are up or down, sequence length, and
       number back'''
    def __init__(self,dir, num, num_bk, seq):
        self.dir = dir
        self.num = num
        self.num_bk = num_bk
        self.seq = seq
        self.indx = 0
        self.item_results = []
        logger.LogInfo('farback', 'seq_init', dir, num_bk, self.seq)
        return

class FarBackTask( ):
    '''The nback task with variable number back'''

    seq_list = {('u',1): 'p1p1p1p1p1',('u',2): 'pp2p2p2p2p2', ('u',3): 'pp3p3p3p3p3',
                         ('u',4): 'pp4p4p4p4p4', ('d',3): '3p3p3p3p3', ('d',2): '2p2p2p2p2',
                         ('d',1): '1p1p1p1p1'} #List of trial sequences.
                                               #One trial represents wind up and wind down.
    def __init__(self, seq_len, num):
        # Num is the number of trials (complete up-down seqs) in a block
        self.trial_sequence = [FarBackSeq('u',seq_len,1, self.seq_list[('u',1)])]
        up = range(2, seq_len)
        dn = range (seq_len - 2, 0, -1)
        n_bk = up + dn
        k = seq_len + 1
        for i in range(num * k):
            j = i if i <= seq_len else i - k
            u_d = ('u'if j < len(up) else 'd')
            self.trial_sequence += [FarBackSeq(u_d,seq_len,n_bk[j],self.seq_list[(u_d,n_bk[j])])] #length will be length of seq_list
        self.seq_num = 0
        self.trial_num = 1
        self.item = []
        self.nback = None
        self.current_trial = self.trial_sequence[self.seq_num]
        self.block_response = []  #block summary
        self.resp_required = None
        self.actual_response = []
        self.num_resp_required = 0
        self.Display_Time = 2.0
        self.Clear_Time = 3.0
        self.resp = None
        self.display_blank_time = None
        self.trial_results = []
        self.block_results = []
        return

    def sequence_summary(self, seq, blknum):
        num_correct, num_incorrect, num_noresponse = 0,0,0
        corr_tms, incorr_tms, no_resp_tms = 0,0,0
        for item in seq.item_results:
            if not item.response:
                num_noresponse += 1
                no_resp_tms += item.response_time
            elif item.response and item.response_correct:
                num_correct += 1
                corr_tms += item.response_time
            else:
                num_incorrect += 1
                incorr_tms += item.response_time              
        logger.LogInfo('farback', 'sequence_summary',
                       'blk_num', blknum + 1,
                       'trial_num', self.trial_num,
                       'seq_num', self.seq_num + 1,
                       'seq_dir', seq.dir,
                       'seq_num_bk', seq.num_bk,
                       'seq_len', seq.num,
                       'accuracy', num_correct, num_incorrect, num_noresponse,
                       'response_times',corr_tms, incorr_tms, no_resp_tms)
        self.trial_results.append(PerfAccum(num_correct,num_incorrect,num_noresponse,
                                            corr_tms,incorr_tms,no_resp_tms))
        if seq.dir == 'd' and seq.num_bk == 1:
            self.trial_summary(blknum)
        return

    def trial_summary(self, blknum):
        num_correct, num_incorrect, num_noresponse = 0,0,0
        corr_tms, incorr_tms, no_resp_tms = 0,0,0
        for obj in self.trial_results:
            num_correct += obj.num_correct
            num_incorrect += obj.num_incorrect
            num_noresponse += obj.num_noresponse
            corr_tms += obj.corr_tms
            incorr_tms += obj.incorr_tms
            no_resp_tms += obj.no_response_tms
        logger.LogInfo('farback', 'trial_summary', 'blk_num', blknum + 1,'trial_num', self.trial_num,
                       'trial_accuracy', num_correct, num_incorrect, num_noresponse,
                       'trial_response_times' ,corr_tms, incorr_tms, no_resp_tms)
        self.block_results.append(PerfAccum(num_correct,num_incorrect,num_noresponse,
                                            corr_tms,incorr_tms,no_resp_tms))
        self.trial_results = []
        self.trial_num +=1
        return

    def block_summary(self, blknum):
        num_correct, num_incorrect, num_noresponse = 0,0,0
        corr_tms, incorr_tms, no_resp_tms = 0,0,0
        for obj in self.block_results:
            num_correct += obj.num_correct
            num_incorrect += obj.num_incorrect
            num_noresponse += obj.num_noresponse
            corr_tms += obj.corr_tms
            incorr_tms += obj.incorr_tms
            no_resp_tms += obj.no_response_tms
        logger.LogInfo('farback', 'block_summary', 'blk_num', blknum,
                       'block_accuracy', num_correct, num_incorrect, num_noresponse,
                       'block_response_times' ,corr_tms, incorr_tms, no_resp_tms)
        self.block_results =  [] 
        self.trial_num = 0
        return
                       
                       

    def process_response(self, inp , device):
        '''Subject entered response from keyboard or button box'''
        #print(inp, device,self.resp_required)
 
        if self.resp_required:
            if device == 'key':
                self.resp = p.key_mapping[inp]
            elif device == 'bb':
                self.resp = p.bb_mapping[inp]
            else:
                self.resp = None
            if self.nback and self.resp == int(self.item[self.nback*-1]):
                result = True
            elif self.nback:
                result = False
            else:
                print ('********This should not happen**********')
                return
            response_tm = get_time() - self.display_blank_time
            self.actual_response.append(result)
            self.current_trial.item_results.append(ItemPerf(True, result, response_tm))
            logger.LogInfo('farback', 'response', self.resp, int(self.item[self.nback*-1]), self.nback,
                           device, self.actual_response[-1], response_tm)
            self.resp_required = False
            task_env.sound_happy.play(maxtime = 600) if self.actual_response[-1] else task_env.sound_sad.play(maxtime = 600)
        else:
            logger.LogInfo('farback', 'extra response')
        return

    def clear_item(self):
        self.nback = None
        if self.resp_required:
            self.resp = 'None'
            self.current_trial.item_results.append(ItemPerf(False, None, 2.0))
            self.resp_required = False
            logger.LogInfo('farback','no_response')
            task_env.sound_sad.play(maxtime =500)
        return

class JitterTask ():
    '''Task to track a moving arrow'''
    road_width = 50
    jitter_calc_interval = 20

    def __init__(self, arrow, args):
        self.jitter_only_time = args.jitter_only
        self.burn_in_time = self.jitter_only_time // 4
        self.arrow = arrow
        self.jitter_seq = []
        self.jitter_idx = 0
        self.jitter_fb_tm  = None
        self.calibration_mean_deviation = None
        self.calibration_std_dev =0
        self.block_diff = []
        logger.LogInfo('jitter_init', self.burn_in_time, self.jitter_only_time,
                                      self.road_width, self.jitter_calc_interval)
        return

    def jitter_only_feedback(self):
        msg1 = "Calibrated Deviation None"
        dev = None
        if self.calibration_mean_deviation:
            dev =round(self.calibration_mean_deviation)
            msg1 = "Calibrated Deviation %i" % dev
        msg_lines = ["End of Jitter Only task",msg1]
        if self.block_diff and dev:
            bd = round(numpy.mean(self.block_diff))
            msg_lines.extend(['Deviation Score %i' % (dev + bd)])
            logger.LogInfo('jitteronlyfeedback', dev, (dev + bd))
        else:
            bd = 'empty'
            logger.LogInfo('jitteronlyfeedback', dev, bd)
        msg_lines.extend(["N-back Task only next"])

        #print('jitter_feedback', dev,bd)
        return (msg_lines)

    def jitter_feedback(self):
        if not self.jitter_fb_tm:
            self.jitter_fb_tm = get_time()
        cur_tm = get_time()
        if cur_tm > self.jitter_fb_tm + 5:
            seq = self.jitter_seq[self.jitter_idx:]
            l = len(seq)

            if l > self.jitter_calc_interval:
                self.jitter_fb_tm = cur_tm
                m = numpy.mean(seq)
                curr_sd = numpy.std(seq)
                sd = abs(self.calibration_std_dev)
                self.jitter_idx += l
                diff = m - self.calibration_mean_deviation

                self.block_diff.append(diff)
                #print('jitter', diff,m, sd)
                if diff >  10:
                    self.arrow.current_color = p.get_color('Red')
                elif diff > 5:
                    self.arrow.current_color = p.get_color('Yellow')
                else:
                    self.arrow.current_color = p.get_color('Green')
                logger.LogInfo('jitter','calc_dev', m, sd, curr_sd, l, diff, p.get_key(self.arrow.current_color))
            elif (cur_tm - self.jitter_fb_tm) > 15:
                self.arrow.current_color = p.get_color('Red')
            elif (cur_tm - self.jitter_fb_tm) > 10:
                self.arrow.current_color = p.get_color('Yellow')

        return

    def reset(self):
        self.jitter_fb_tm = 0
        self.jitter_seq = []
        self.jitter_idx = 0
        self.block_diff = []
        return


    def calibrate_jitter(self):
        '''Calculate mean and sd for jitter during burn in period'''
        seq = self.jitter_seq[0:]
        if seq:
            self.calibration_mean_deviation = numpy.mean(seq)
            self.calibration_std_dev = numpy.std(seq)
            self.jitter_idx = len(self.jitter_seq)
        logger.LogInfo('jitter','calibration', self.calibration_mean_deviation,
                           self.calibration_std_dev)
        self.reset()
        return

    def jitter( self, state):
        '''Move the arrow'''
        self.arrow.displace_arrow(self.road_width)
        self.arrow.draw_arrow()
        if state != 'jitter_burn_in':
            self.jitter_feedback()
        return


### Task Environment Classes ###
class Arrow():
    def __init__( self, x,y ):
        #self.image_g = pygame.image.load('images/green-arrow.bmp')
        #self.image_y = pygame.image.load('images/yellow-arrow.bmp')
        #self.image_r = pygame.image.load('images/red-arrow.bmp')
        #self.current_image = self.image_g
        self.current_color = p.get_color('Green')
        self.arrow_x_pos, self.arrow_y_pos = (x, y-(y // 4))
        #self.offset = (self.image_g.get_width() // 2)
        self.offset = 21
        self.center = x
        self.last_pos = None
        logger.LogInfo('jitter', 'arrow_init', self.center, self.arrow_x_pos, self.arrow_y_pos, self.offset)
        return

    def displace_arrow( self, width):
        save = self.arrow_x_pos
        r =  random.randrange(100)
        chance = 0
        if self.center - 5 < self.arrow_x_pos < self.center + 5:
            self.arrow_x_pos = self.center
        else:
            chance = width - abs(self.center - self.arrow_x_pos)
        #print (self.arrow_x_pos, r,chance)
        if (self.arrow_x_pos == self.center) and r < 25:
            self.arrow_x_pos -= 5
        elif (self.arrow_x_pos == self.center) and r > 75:
            self.arrow_x_pos += 5
        elif (self.arrow_x_pos < self.center) and r < chance:
                self.arrow_x_pos -= 5
        elif (self.arrow_x_pos > self.center) and r < chance:
                self.arrow_x_pos += 5
        logger.LogInfo('jitter', 'displace_arrow', save, self.arrow_x_pos, r, chance)
        return

    def draw_arrow( self ):
        o = self.offset
        t = self.arrow_y_pos
        b = t + 70
        if self.last_pos:
            c = self.last_pos
            l = c - o
            r = c + o
            pygame.draw.polygon(task_env.screen,task_env.background,
                        [[c,t], [c + o, t + o], [c + (o // 2), t + o], [c + (o // 2), b],
                         [c - (o //2),b], [c - (o //2), t + o], [c - o, t + o]])

        c = self.arrow_x_pos
        l = c - o
        r = c + o
        self.last_pos = c
        #task_env.screen.blit(self.current_image, ((self.arrow_x_pos - self.offset), self.arrow_y_pos))

        pygame.draw.polygon(task_env.screen,self.current_color,
                        [[c,t], [c + o, t + o], [c + (o // 2), t + o], [c + (o // 2), b],
                         [c - (o //2),b], [c - (o //2), t + o], [c - o, t + o]])
        task_env.actr.update_display([self.toChunk()])
        pygame.display.update()
        return

    def toChunk(self):
            return VisualChunk(None, "Arrow", self.arrow_x_pos,self.arrow_y_pos, value = self.arrow_x_pos,
                               width = 2 * self.offset, height = 70, color=':green')


class Cross ():

    def __init__ (self,font,x,y):
        self.cross_obj = font.render('+',True,p.get_color('Black'))
        self.crossRect = self.cross_obj.get_rect()
        self.crossRect.centerx = x
        self.crossRect.centery = y  - 50
        logger.LogInfo('display','cross', self.crossRect.centerx,self.crossRect.centery)
        return

class Road ():
    width = 10
    left = 60
    right = 50
    def __init__(self):
        logger.LogInfo('display','road',self.width,self.left,self.right)
        return

class Task_Environment():
    d = Dispatcher()
    state = 'init'
    def __init__(self, args):
        self.args = args

        pygame.display.init()
        pygame.font.init()
        pygame.mixer.init()

        info = pygame.display.Info()

        if int(self.args.fullscreen):
            self.screen = pygame.display.set_mode((info.current_w, info.current_h),
                                          pygame.FULLSCREEN)
        else:
            #(don't make the window larger than the screen. estimated frame size also taken into account.)
            self.screen = pygame.display.set_mode((min(info.current_w-10,1280),min(info.current_h-30,1024)))
        logger.LogInfo('system', 'resolution', self.screen.get_width(), self.screen.get_height())
        self.background = p.get_color('Grey')
        self.screen.fill(self.background)
        self.mid_x = self.screen.get_width() / 2
        self.mid_y = self.screen.get_height() / 2
        self.end_x = self.screen.get_width()
        self.end_y = self.screen.get_height()

        pygame.display.set_caption("Farback Task")
        pygame.mouse.set_visible(False)
        self.sound = pygame.mixer.Sound('sounds/movebeep.wav')
        self.sound_happy = pygame.mixer.Sound('sounds/Whistle.aif')
        self.sound_sad = pygame.mixer.Sound('sounds/Wap.aif')
        self.basicFont = pygame.font.SysFont(None, 48)

        self.cross = Cross(self.basicFont,self.screen.get_rect().centerx,self.screen.get_rect().centery) #make cross
        self.arrow = Arrow(self.mid_x, self.end_y) #make arrow
        self.road = Road()
        #self.tree = pygame.image.load('images/Tree1.png')
        
        if ACTR6:
            self.actr = JNI_Server(self)
            self.actr.addDispatcher(self.d)
            reactor.listenTCP(5555, self.actr)

        print(BB_Support,self.args.buttonbox)
        if BB_Support and self.args.buttonbox:
            self.bb = bb.Button_Box() #make Button Box
            self.jitter_port = 0
            self.farback_port = 1
            self.A_Key = 2
            self.D_Key = 3
        else:
            self.bb = None

        self.farback_task = FarBackTask(5,1) #make FarBackTask, one trial for 1st block
        self.jitter_task = JitterTask(self.arrow,args) #make JitterTask

        if self.args.eyetracker:
            self.client = iViewXClient( self.args.eyetracker, 4444 )
            self.client.addDispatcher(self.d)
            self.calibrator = Calibrator( self.client, self.screen, reactor = reactor )

        return

    def subj_msg(self,msgs):
        message="Press SPACE to continue or ESCAPE to exit"
        msgs.extend([message])
        font = pygame.font.Font(None, 30)
        s = font.get_linesize()
        max_w = 0
        tot_h =s * (len(msgs) -1)
        for i in range(len(msgs)):
            w,h = font.size(msgs[i])
            if w > max_w:
                max_w = w
            tot_h += h
        x = self.mid_x - (max_w / 2)
        y = (self.mid_y / 2) - (tot_h /2)

        for i in range(len(msgs)):
            text = font.render(msgs[i], 1, p.get_color('Black'))
            self.screen.blit(text,(x,y))
            w,h = font.size(msgs[i])
            y += (s + h)
        pygame.display.update()
        while True:
            event = pygame.event.wait()
            if event.type == pygame.KEYDOWN and event.key == pygame.K_SPACE:
                self.clear()
                return (True)
            elif event.type == pygame.QUIT:
                return(False)
            elif event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                return (False)

    ### clear the screen ###
    def clear(self):
        self.screen.fill(self.background)
        pygame.display.update()
        return

    def draw_circles (self, num,  x,y):
        for i in range(4):
            c = p.get_color('Black') if i < num else self.background
            pygame.draw.circle(self.screen,c,(x + (i * 10),y), 5)
        logger.LogInfo('draw_circles',num,x,y)
        return

    def handle_key_press(self, key, code):
        if key == pygame.K_ESCAPE:
            reactor.stop()
        elif key == pygame.K_SPACE:
            pass
        elif key == pygame.K_a:
            self.jitter_task.arrow.arrow_x_pos -= 5
            self.jitter_task.arrow.draw_arrow()
            logger.LogInfo('Keyboard', 'a', self.jitter_task.arrow.arrow_x_pos,
                           self.jitter_task.arrow.arrow_x_pos - self.mid_x)
            self.jitter_task.jitter_seq.append(abs(self.jitter_task.arrow.arrow_x_pos - self.mid_x))
        elif key == pygame.K_d:
            self.jitter_task.arrow.arrow_x_pos += 5
            self.jitter_task.arrow.draw_arrow()
            logger.LogInfo('Keyboard', 'd', self.jitter_task.arrow.arrow_x_pos,
                           self.jitter_task.arrow.arrow_x_pos - self.mid_x)
            self.jitter_task.jitter_seq.append(abs(self.jitter_task.arrow.arrow_x_pos - self.mid_x))
        return


    def process_events( self ):
        save = False
        while True:
            for event in pygame.event.get():
                if event.type == pygame.KEYDOWN:
                    logger.LogInfo('process_events','Keyboard', event.key)
                    if event.key == pygame.K_a:
                        self.jitter_task.arrow.arrow_x_pos -= 5
                        self.jitter_task.arrow.draw_arrow()
                        logger.LogInfo('Keyboard', 'a', self.jitter_task.arrow.arrow_x_pos,
                                    self.jitter_task.arrow.arrow_x_pos - self.mid_x)
                        self.jitter_task.jitter_seq.append(abs(self.jitter_task.arrow.arrow_x_pos - self.mid_x))
                    elif event.key == pygame.K_d:
                        self.jitter_task.arrow.arrow_x_pos += 5
                        self.jitter_task.arrow.draw_arrow()
                        logger.LogInfo('Keyboard', 'd', self.jitter_task.arrow.arrow_x_pos,
                                    self.jitter_task.arrow.arrow_x_pos - self.mid_x)
                        self.jitter_task.jitter_seq.append(abs(self.jitter_task.arrow.arrow_x_pos - self.mid_x))
                    elif event.key == pygame.K_ESCAPE:
                        self.lc.stop()
                    elif (event.key == pygame.K_UP or
                          event.key == pygame.K_DOWN or
                          event.key == pygame.K_RIGHT or
                          event.key == pygame.K_LEFT):
                        if self.state == 'wait_response':
                            self.farback_task.process_response(event.key, 'key')
                        else:
                            logger.LogInfo('process_events','Response_Time_Error')
                    elif event.key == pygame.K_p:
                        logger.LogInfo('process_events', 'pause', self.state)
                        if not save:
                            save = self.state
                            self.state = 'pause'
                    elif event.key == pygame.K_u:
                        logger.LogInfo('process_events', 'unpause', save)
                        if save:
                            self.state = save
                            save = False
                    else:
                        logger.LogInfo('process_events','Invalid Key',event.key)
                elif event.type == pygame.QUIT:
                    self.lc.stop()
                else:
                    if event.type != pygame.KEYUP:
                        logger.LogInfo('process_events','Invalid_Event', event.type)
            yield

    def process_bb_events( self ):


        while True: #not empty:
            empty = False
            while not empty:
                for e in self.bb.get_bb_events():
                    if e[0] == None:
                        empty = True
                    elif e[0][0] == self.jitter_port: #jitter
                        d = e[0][1]
                        if d['pressed'] == True:
                            temp = self.jitter_task.arrow.arrow_x_pos
                            if d['key'] == self.A_Key:
                                 self.jitter_task.arrow.arrow_x_pos -= 5
                                 self.jitter_task.arrow.draw_arrow()
                            elif d['key'] == self.D_Key:
                                self.jitter_task.arrow.arrow_x_pos += 5
                                self.jitter_task.arrow.draw_arrow()
                            logger.LogInfo('ButtonBox', 'jitter', d['key'], self.jitter_task.arrow.arrow_x_pos,
                                            self.jitter_task.arrow.arrow_x_pos - self.mid_x)
                            self.jitter_task.jitter_seq.append(abs(self.jitter_task.arrow.arrow_x_pos - self.mid_x))
                    elif e[0][0] == self.farback_port:  #farback
                        d = e[0][1]
                        if d['pressed'] == True:
                            logger.LogInfo('ButtonBox', 'farback', d['key'])
                            if self.state == 'wait_response':
                                self.farback_task.process_response( d['key'], 'bb')
                            else:
                                logger.LogInfo('process_events','Response_Time_Error', 'bb')
                    else:
                        logger.LogInfo('process_events','Invalid Port',e[0])

            yield

    def draw_road ( self, start ):
        diff = self.mid_y // 10
        o = self.arrow.offset
        pygame.draw.line(self.screen,p.get_color('Black'),(self.mid_x - (self.road.left + o), self.mid_y),
                                                          (self.mid_x - (self.road.left + o), self.end_y), self.road.width)
        pygame.draw.line(self.screen,p.get_color('Black'),(self.mid_x + (self.road.right + o),self.mid_y),
                                                          (self.mid_x + (self.road.right + o), self.end_y), self.road.width)
        for i in range(5):
            j = 2 * i
            k = j + 1
            l = 0 if start % 2 else diff
            m = diff if start % 2 else 0
            # print(self.mid_y + j*diff + m,self.mid_y + k*diff + m,self.mid_y + j*diff + l,self.mid_y + k*diff + l)
            pygame.draw.lines(self.screen,self.background,False,
                          [(self.mid_x, self.mid_y + j*diff + m),
                            (self.mid_x, self.mid_y + k*diff + m)],3)
            pygame.draw.lines(self.screen,p.get_color('Black'),False,
                          [(self.mid_x, self.mid_y + j*diff + l),
                            (self.mid_x, self.mid_y + k*diff + l)],3)
        return

    def draw_separator(self):
        #self.screen.blit(self.tree,(100, self.mid_y + 100))
        pygame.draw.lines(self.screen,p.get_color('Black'),False,
                          [(0, self.mid_y),(self.end_x, self.mid_y)],5)


    def draw_item(self,item):
        '''Displays the rectangle with a stimulus or blank'''
        # set up the text
        text = self.basicFont.render(item,True, p.get_color('Black'))
        self.textRect = text.get_rect()
        self.textRect.centerx = self.screen.get_rect().centerx
        self.textRect.centery = self.screen.get_rect().centery- 200
        # draw the text's background rectangle onto the surface
        pygame.draw.rect(self.screen,p.get_color('Black'),
                         (self.textRect.left - 20, self.textRect.top - 20,
                          self.textRect.width + 40, self.textRect.height + 40),
                         1)
        # draw the text onto the surface
        self.screen.blit(text, self.textRect)
        if item == ' ':
            self.farback_task.display_item_time = get_time()
        else:
            self.farback_task.display_blank_time = get_time()
        logger.LogInfo('draw_item', item, (self.textRect.left - 20, self.textRect.top - 20,
               self.textRect.width + 40, self.textRect.height + 40))

        return()

    def clear_item(self):
        '''Remove retangle from screen'''
        pygame.draw.rect(self.screen,self.background,
                         (self.textRect.left - 20, self.textRect.top - 20,
                          self.textRect.width + 40, self.textRect.height + 40),
                         0)
        logger.LogInfo('clear_item')
        return

    def block_feedback( self ):
        bd = self.jitter_task.block_diff
        mean_jitter = round(numpy.mean(bd if len(bd) > 0 else -1))
        if len(self.farback_task.actual_response) > 0:

            mean_accuracy = round (100 * (self.farback_task.actual_response.count(True) / float(self.farback_task.num_resp_required)))
        else:
            mean_accuracy  = -1
        m1 = 'End of Block %i' % self.blocks_done
        
        m2 = 'Deviation Score %i' % mean_jitter if self.blocks_done > 1 else ''
        m3 = 'Accuracy %i'% mean_accuracy
        m4 = 'Jitter Task and N-back Task next'

        logger.LogInfo('block_feedback', self.blocks_done, mean_jitter,mean_accuracy,
                           self.farback_task.actual_response.count(True),self.farback_task.num_resp_required)
        return(self.subj_msg([m1,m2,m3,m4]))

    def jitter_enabled( self ):
        if self.state == 'pause':
            return(False)
        elif self.args.exp_tasks == 3:
            return(False)
        elif self.blocks_done > 0:
            return(True)
        elif self.args.exp_tasks < 2:
            return(True)
        
        elif self.state == 'jitter_burn_in' or self.state == 'jitter_only':
            return(True)
        else:
            return(False)

    def do_jitter_burn_in( self ):
        if get_time() >= self.start_time + self.jitter_task.burn_in_time:
            self.jitter_task.calibrate_jitter()
            #self.sound.play(3)
            return('jitter_only')
        return(self.state)

    def do_jitter_only( self ):
        if get_time() >= self.start_time + self.jitter_task.jitter_only_time:
            if self.subj_msg(self.jitter_task.jitter_only_feedback()):
                self.draw_separator()
                self.jitter_task.reset()

                return('new_seq_display')
            else:
                return('stop')
        return(self.state)

    def present_item( self ):
        self.sequence_timer = get_time()
        seq_obj = self.farback_task.current_trial
        frame_type = seq_obj.seq[seq_obj.indx]
        item = random.choice(p.get_choices()) if frame_type == 'p' else ' '
        self.draw_item(item)
        seq_obj.indx += 1
        if not item == ' ':
            self.farback_task.item.append(item)
            self.draw_circles(seq_obj.num_bk,self.mid_x,100)
            logger.LogInfo('Farback', 'stimulus', item, seq_obj.dir, seq_obj.num_bk)
            return('wait')
        else:
            self.farback_task.nback = seq_obj.num_bk
            self.farback_task.num_resp_required += 1
            self.farback_task.resp_required = True
            return('wait_response')


    def new_sequence( self ):
        self.sequence_timer = get_time()
        nback = self.farback_task.current_trial.num_bk
        dir = self.farback_task.current_trial.dir
        item = "".join((str(nback),dir))
        text = self.basicFont.render(item,True, p.get_color('Black'))
        self.textRect = text.get_rect()
        self.textRect.centerx = self.screen.get_rect().centerx
        self.textRect.centery = self.screen.get_rect().centery- 200
        # draw the text's background rectangle onto the surface
        pygame.draw.rect(self.screen,
                         p.get_color('Red'),
                         (self.textRect.left - 20, self.textRect.top - 20,
                          self.textRect.width + 40, self.textRect.height + 40),
                         0)
        self.screen.blit(text, self.textRect)
        self.draw_circles(self.farback_task.current_trial.num_bk,self.mid_x,100)
        logger.LogInfo('display_new_seq', item,(self.textRect.left - 20, self.textRect.top - 20,
               self.textRect.width + 40, self.textRect.height + 40))
        return

    def clear_new_sequence ( self ):
        pygame.draw.rect(self.screen,self.background,
                         (self.textRect.left - 20, self.textRect.top - 20,
                          self.textRect.width + 40, self.textRect.height + 40),
                         0)
        logger.LogInfo('display_new_seq', 'clear')
        return



    def end_of_block( self ):
        self.blocks_done += 1
        self.farback_task.block_summary(self.blocks_done)
        if not self.block_feedback():
            return('stop')
        elif self.blocks_done < self.args.num_blocks:
            #next block
            self.draw_separator()
            self.jitter_task.reset()
            self.farback_task = FarBackTask(5,self.args.num_trials)
            self.farback_task.current_trial = self.farback_task.trial_sequence[self.farback_task.seq_num] #get next sequence
            return('new_seq_display')
        else:
            m1 = 'End of Experiment '
            m2 = 'Thank You'
            self.subj_msg([m1,m2])
            return('stop')

    def do_block( self ):
        logger.LogInfo('block',self.state,self.count)
        if ACTR6:
            if self.state == 'WAIT_CONNECT':
                return
            elif self.state == 'WAIT_MODEL':
                return
        self.count +=1
        if self.jitter_enabled():
            self.draw_road(self.count)
            self.jitter_task.jitter(self.state)
        if self.state == 'jitter_burn_in':
            self.state = self.do_jitter_burn_in()
        elif self.state == 'jitter_only':
            self.state = self.do_jitter_only()
        elif self.state == 'present_item':
            self.state = self.present_item()
        elif self.state == 'new_seq_display':
            self.new_sequence()
            self.state = 'new_seq_wait'
        elif self.state == 'new_seq_wait':
            if get_time() >= self.sequence_timer + self.farback_task.Display_Time:
                self.clear_new_sequence()
                self.state = 'present_item'
        elif self.state == 'wait' or self.state =='wait_response':
            if get_time() >= self.sequence_timer + self.farback_task.Display_Time:
                self.clear_item()
                self.farback_task.clear_item()
                self.state = 'wait_end'
        elif self.state == 'wait_end':
            if get_time() > self.sequence_timer + self.farback_task.Clear_Time:
                self.state = 'end_one_item'
        elif self.state == 'end_one_item':
            self.state = 'present_item'
            if self.farback_task.current_trial.indx >= len(self.farback_task.current_trial.seq):
                #end of 1 wind up or wind down sequence
                self.farback_task.sequence_summary(self.farback_task.current_trial, self.blocks_done)
                self.farback_task.seq_num += 1
                if self.farback_task.seq_num < len(self.farback_task.trial_sequence):
                    #next sequence
                    logger.LogInfo('next_seq', self.farback_task.seq_num)
                    self.farback_task.current_trial = self.farback_task.trial_sequence[self.farback_task.seq_num]
                    self.state = 'new_seq_display'
                else:
                    #end of block
                    self.state = self.end_of_block()
        elif self.state == 'pause':
            pass
        elif self.state == 'stop':
            self.lc.stop()

        pygame.display.update()
        return

    def cleanup( self, lc):
        print('cleanup')
        logger.LogInfo('cleanup')
        logger.CloseLog()
        reactor.stop()


    def start( self, lc, results ):
        print results
        logger.LogInfo('system','calibration', results)
        self.screen.fill(self.background)
        self.subj_msg(['Start Experiment','Jitter Task Only'])
        pygame.display.update()
        self.blocks_done = 0
        self.state = 'jitter_burn_in' if self.args.exp_tasks < 3 else 'new_seq_display'
        if ACTR6:
            self.state = 'WAIT_CONNECT'
        self.count = 0
        self.draw_separator()
        pygame.display.update()
        self.start_time = get_time()
        self.lc = LoopingCall( self.do_block )
        dd = self.lc.start( 1.0 / 5 )
        dd.addCallbacks( self.cleanup )
        return

    def run( self ):
        if self.args.eyetracker:
            reactor.listenUDP( 5555, self.client )
            self.calibrator.start( self.start )
        else:
            self.start( None, None )
        if not ACTR6:
            coop = Cooperator() # Process events as fast as possible with out being greedy
        if self.bb and self.bb.status and self.bb.status1:
            print('starting bb')
            logger.LogInfo('system','bb',self.bb.status, self.bb.status1)
            coop.coiterate(self.process_bb_events())
        else:
            if not ACTR6:
                coop.coiterate(self.process_events())
        reactor.run()
        return

    if ET_Support:
        @d.listen( 'ET_SPL' )
        def iViewXEvent( self, inResponse ):
            if self.state == 'init':
                return
            t = int( inResponse[0] )
            x = int( inResponse[2] )
            y = int( inResponse[4] )
            diam = int (inResponse[6] )
            logger.LogInfo('eyedata', x, y, diam)
            return

    if ACTR6:
        @d.listen('connectionMade')
        def ACTR6_JNI_Event(self, model, params):
            print("Connection Made")
            self.state = 'WAIT_MODEL'
            
        @d.listen('connectionLost')
        def ACTR6_JNI_Event(self, model, params):       
           self.state = 'WAIT_CONNECT'
           
        @d.listen('reset')
        def ACTR6_JNI_Event(self, model, params):
            self.state = 'WAIT_MODEL'
    
        @d.listen('model-run')
        def ACTR6_JNI_Event(self, model, params):
            self.state =  'jitter_burn_in'
            self.actr_running = True
    
        @d.listen('model-stop')
        def ACTR6_JNI_Event(self, model, params):
            print("model-stop")
            self.state = 'stop'
    
        @d.listen('keypress')
        def ACTR6_JNI_Event(self, model, params):
            print("keypress",  params[0], chr(params[0]))
            self.handle_key_press(params[0], chr(params[0]))
    
        @d.listen('mousemotion')
        def ACTR6_JNI_Event(self, model, params):
            # Store "ACT-R" cursor in variable since we are
            # not going to move the real mouse
            self.fake_cursor = params[0]
    
        @d.listen('mouseclick')
        def ACTR6_JNI_Event(self, model, params):
            # Simulate a button press using the "ACT-R" cursor loc
            self.handle_mouse_event(self.fake_cursor)

if __name__ == "__main__":
    parser = argparse.ArgumentParser( formatter_class = argparse.ArgumentDefaultsHelpFormatter )
    #logfile directory path
    parser.add_argument( '-L', '--log', action = "store", dest = "logfile", default = 'Data/', help = 'log to file instead of stdout.' )
    parser.add_argument( '-F', '--fullscreen', action = "store", dest = "fullscreen", default = False, help = 'Run in fullscreen mode.' )
    parser.add_argument( '-E', '--eyetracker', action = "store", dest = "eyetracker", default = '', help = 'URL for eyetracker.' )
    parser.add_argument( '-X', '--buttonbox', action = "store", dest = "buttonbox", default = True, help = 'Use Button Boxes.' )
    parser.add_argument( '-D', '--debug', action = "store", dest = "debug_level", default = 1, type = int, help = 'Debug level.' )
    parser.add_argument( '-T', '--trials', action = "store", dest = "num_trials", default = 2, type = int, help = 'Number of Trials' )
    #num_trials is only for blocks 2 and up, block 1 (blocks_done == 0) only has 1 trial, i.e. up and down sequence
    parser.add_argument( '-B', '--blocks', action = "store", dest = "num_blocks", default = 2, type = int, help = 'Number of Blocks' )
    parser.add_argument( '-J', '--jitter', action = "store", dest = "jitter_only", default = 60, type = int, help = 'Jitter only time')
    parser.add_argument( '-S', '--tasks', action = "store", dest = "exp_tasks", default = 2, type = int, help = 'Experiment task sequence' )
    #exp_tasks (applies only to first block) = 0 - jitter only (not implemented),
    #                                          1 - jitter only then both,  2 - jitter only then farback only, 3 - no jitter
    #

    args = parser.parse_args()
    p = Parameters()
    if (not p.eyetracking_enabled_p()) or args.eyetracker == '':
        setattr( args, 'eyetracker', False )
    logger = Logger(args)
    logger.LogHeader()
    #logger.LogInfo('args',args)
    subject = Subject(dbg = args.debug_level)
    task_env  = Task_Environment(args)
    task_env.run()
