from twisted.protocols.basic import NetstringReceiver
from twisted.internet.protocol import Factory
import json

class VisualChunk( object ):

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

class ACTR_Protocol( NetstringReceiver ):

    def connectionMade( self ):
        print "~ connection made ~"
        self.factory.ready = True

    def connectionLost( self, reason ):
        print "~ connection lost ~"

    def stringReceived( self, string ):
        model, method, params = json.loads( string )
        if method == "reset":
            self.factory.env.trial = 0
            self.factory.env.state = 0
        elif method == "model-run":
            self.factory.model = model
            self.factory.running = True
        elif method == "model-stop":
            self.factory.running = False
            self.factory.model = None
        elif method == "keypress":
            pygame.event.post( pygame.event.Event( pygame.KEYDOWN, unicode = chr( params[0] ), key = params[0], mod = None ) )

    def sendCommand( self, model, method, *params ):
        self.sendString( json.dumps( [model, method, params] ) )

class ACTR_Device( Factory ):

    ready = False
    running = True
    model = None

    def __init__( self, env ):
        self.env = env

    def buildProtocol( self, addr ):
        self.p = ACTR_Protocol()
        self.p.factory = self
        return self.p

    def update_display( self, chunks, clear = False ):
        visual_locations = "(define-chunks %s)" % " ".join( [chunk.get_visual_location() for chunk in chunks] )
        visual_objects = "(define-chunks %s)" % " ".join( [chunk.get_visual_object() for chunk in chunks] )
        self.p.sendCommand( self.model, "update-display", visual_locations, visual_objects, clear )
        
    def set_cursor_location( self, loc ):
        self.p.sendCommand( self.model, "set-cursor-loc", loc )

    def digit_sound( self, digit ):
        self.p.sendCommand( self.model, "new-digit-sound", digit )

    def tone_sound( self, freq, duration ):
        self.p.sendCommand( self.model, "new-tone-sound", freq, duration )

    def word_sound( self, word ):
        self.p.sendCommand( self.model, "new-word-sound", word )

    def other_sound( self, content, duration, delay, recode ):
        self.p.sendCommand( self.model, "new-other-sound", content, duration, delay, recode )