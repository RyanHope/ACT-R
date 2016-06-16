#!/usr/bin/env python

import sys,os

# For general environment
from twisted.internet import reactor
from twisted.internet.task import LoopingCall, Cooperator
from twisted.protocols.basic import LineReceiver
from twisted.internet.protocol import Factory
import json
from panglery import Pangler

class Dispatcher(Pangler):

    def listen(self, event):
        def decorator(target):
            @self.subscribe(e=event, needs=['mp', 'model', 'params'])
            def wrapper(*args, **kwargs):
                newargs = tuple([arg for arg in args if not isinstance(arg, Pangler)])
                return target(*newargs, **kwargs)
            return wrapper
        return decorator

class ACTR_Module_Protocol(LineReceiver):

    def connectionMade(self):
        for d in self.factory.dispatchers:
            d.trigger(e="connectionMade", mp=None, model=None, params=None)

    def connectionLost(self, reason):
        self.clearLineBuffer()
        for d in self.factory.dispatchers:
            d.trigger(e="connectionLost", mp=None, model=None, params=None)

    def lineReceived(self, string):
        obj = json.loads(string)
        if obj['method'] == 'disconnect':
            self.factory.p.transport.loseConnection()
        else:
            for d in self.factory.dispatchers:
                d.trigger(e=obj['method'],
                          mp=obj['mp'],
                          model=obj['model'],
                          params=obj['params'])
        #self.sendCommand(obj['mp'], obj['model'], "sync")

    def sendCommand(self, mp, model, method, **params):
        self.sendLine(json.dumps({"mp": mp, "model": model, "method": method, "params": params}))

class Module_Server(Factory):

    def __init__(self, env):
        self.env = env
        self.dispatchers = []

    def addDispatcher(self, dispatcher):
        self.dispatchers.append(dispatcher)

    def buildProtocol(self, addr):
        self.p = ACTR_Module_Protocol()
        self.p.factory = self
        return self.p

class TestModule(object):

    d = Dispatcher()

    def __init__(self):
        self.param_defaults = {
            "TEST-P1": {"documentation": "test param 1", "default-value": 13},
            "TEST-P2": {"documentation": "test param 2", "default-value": 69}
        }
        self.buffers = {
            "TESTBUF1": {}
        }
        self.buffer_defaults = {
            "TESTBUF1": {
                "STATE": {
                    "FREE": True,
                    "BUSY": False,
                    "ERROR": False
                }
            }
        }
        self.ms = Module_Server(self)
        self.ms.addDispatcher(self.d)
        reactor.listenTCP(5555, self.ms)

    @d.listen('connectionMade')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('connectionMade', mp, model, params)
        self.models = {}

    @d.listen('connectionLost')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('connectionLost', mp, model, params)

    @d.listen('init')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('init', mp, model, params)
        self.ms.p.sendCommand(None,  None, "init",
            name="test-remote-module",
            version="1.0",
            description="A test remote module.",
            params=self.param_defaults,
            buffers=self.buffers,
            )

    @d.listen('creation')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('creation', mp, model, params)
        if not mp in self.models:
            self.models[mp] = {}
        if not model in self.models[mp]:
            self.models[mp][model] = {
                "params": self.param_defaults,
                "buffers": self.buffer_defaults
            }
        else:
            raise Exception("Duplicate model")
        self.ms.p.sendCommand(mp,  model, "creation")

    @d.listen('params')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('params', mp, model, params)
        if isinstance(params, list):
            if params[1] == []:
                params[1] = None
            self.models[mp][model]["params"][params[0]]["default-value"] = params[1]
            self.ms.p.sendCommand(mp,  model, "params", value=self.models[mp][model]["params"][params[0]]["default-value"])
        else:
            self.ms.p.sendCommand(mp,  model, "params", value=self.models[mp][model]["params"][params]["default-value"])

    @d.listen('query')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('query', mp, model, params)
        self.ms.p.sendCommand(mp,  model, "query", value=self.models[mp][model]["buffers"][params[0]][params[1]][params[2]])

    @d.listen('buffer-mod')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('buffer-mod', mp, model, params)
        self.ms.p.sendCommand(mp,  model, "buffer-mod")

    @d.listen('request')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('request', mp, model, params)
        self.ms.p.sendCommand(mp,  model, "request")

    @d.listen('run-start')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('run-start', mp, model, params)
        self.ms.p.sendCommand(mp,  model, "run-start")

    @d.listen('run-end')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('run-end', mp, model, params)
        self.ms.p.sendCommand(mp,  model, "run-end")

if __name__ == '__main__':

    tm = TestModule()
    reactor.run()
