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
        self.sendCommand(obj['mp'], obj['model'], "sync")

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

    def init(self, name, version, description):
        p = {
            "test-p1": {"documentation": "test param 1", "default-value": 13},
            "test-p2": {"documentation": "test param 2", "default-value": 69}
        }
        self.p.sendCommand(None,  None, "init", name=name, version=version, description=description, params=p)

class TestModule(object):

    d = Dispatcher()

    def __init__(self):
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
        self.ms.init("test-remote-module", "1.0", "a test remote module")

    @d.listen('creation')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('creation', mp, model, params)
        if not mp in self.models:
            self.models[mp] = {}
        if not model in self.models[mp]:
            self.models[mp] = [13, 69]
        else:
            raise Exception("Duplicate model")

    @d.listen('params')
    def ACTR6_JNI_Event(self, mp, model, params):
        if isinstance(params, list):
            self.
        else:
            pass
        print ('params', mp, model, params)

    @d.listen('run-start')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('run-start', mp, model, params)

    @d.listen('run-end')
    def ACTR6_JNI_Event(self, mp, model, params):
        print ('run-end', mp, model, params)

if __name__ == '__main__':

    tm = TestModule()
    reactor.run()
