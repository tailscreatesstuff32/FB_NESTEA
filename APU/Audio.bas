#include once"SDL/SDL.bi"
#include once "SDL/SDL_mixer.bi"
#include once "crt.bi"
#include once "crt/math.bi"
#include once "fbthread.bi"
Dim shared audiothread As Any Ptr
#include "fbgfx.bi"
declare sub audio_loop_thread()

'
'type _audiohandler as AudioHandler
'
'	dim shared aud_handler as _audiohandler ptr




type AudioHandler
	
	
	
	declare sub nextbuffer()
	
	declare constructor
	
	'hasAudio as boolean = true
	hasAudio as long = true
	
	static that as AudioHandler ptr
	
	samples as double
	sampleBuffer(any) as double
	samplesperframe as double = 735
	
	inputbuffer(4096) as double
	inputreadpos as uint16_t
	inputbufferpos as uint16_t

	audiolen as uint32_t
	
	audio_spec as SDL_AudioSpec
	
	audioinit as long
	
	declare static Sub Process Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)
	
	declare sub start_aud()
	'declare sub resume_aud()
	
	declare sub stop_aud()
	
	
	private:
	x as double
	
	
	
End Type

constructor AudioHandler()

print "inializing SDL audio..."
this.hasaudio = SDL_Init(SDL_INIT_AUDIO)
 that = @this
If this.hasaudio <> null Then
	Print "failed!"
	'hasaudio = false
	End
Else
	Print "OK!"
	

this.audiolen = 2048
audio_spec.freq = 48000
audio_spec.format = AUDIO_F32SYS
audio_spec.channels = 1
audio_spec.samples = 2048
audio_spec.callback = @Process
audio_spec.userdata =NULL
	
	redim this.sampleBuffer(735) as double
	
		audioinit = SDL_OpenAudio(@audio_spec, NULL)
	If audioinit < 0 Then
		'Print "failed! ("; *SDL_GetError(); ")"
	Else
		'Print "OK!"
		
		SDL_PauseAudio(0)
	
	End If

	
	this.samples = audio_spec.freq /60
	redim this.sampleBuffer(samples) as double
	this.samplesPerframe = this.samples
	
	
	
	sdl_pauseaudio(0)
	
	
	
End If





End Constructor

Sub AudioHandler.Process Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)


if that->audiolen = 0 then
	
	
	exit sub
EndIf
	    if(that->inputReadPos + 2048 > that->inputBufferPos) then
      'we overran the buffer
     'log("Audio buffer overran");
      that->inputReadPos = that->inputBufferPos - 2048 
	    end if
	    
    if(that->inputReadPos + 4096 < that->inputBufferPos) then
      '// we underran the buffer
      '//log("Audio buffer underran");
     that->inputReadPos += 2048
    end if
    
	For n as integer = 0 to (audlen/2)-1  

	
		dim sample as Sint16 ptr = cast(Sint16_t ptr,audbuffer)
		sample[n]  = that->inputbuffer(that->inputReadPos and &HFFF)
		
		that->inputReadPos+=1

		that->audiolen -=1
	Next n
	

	
	'beep
	
	
End Sub



sub AudioHandler.start_aud()
	
sdl_pauseaudio(0)


	
End Sub

sub AudioHandler.stop_aud()
	
	sdl_pauseaudio(1)
	this.inputbufferpos = 0
	this.inputReadPos = 0 
	
End Sub

'sub AudioHandler.resume_aud()
'	
'	sdl_pauseaudio(0)
'End Sub


sub AudioHandler.nextbuffer()
 
 
 
 
	dim aud_val as double
	for i as integer = 0 to this.samplesPerframe-1
		this.x+=.010
		
		aud_val = this.sampleBuffer(i)
		this.inputbuffer(this.inputbufferpos and &HFFF) = (rnd*2-1)'sin((this.x*4))*5000
		'this.inputbuffer(this.inputbufferpos and &HFFF) = rnd*2-1'aud_val
		
		this.inputbufferpos+=1
	Next 
	this.audiolen = 2048
	
End Sub

dim AudioHandler.that as AudioHandler ptr






dim shared aud_handler as audiohandler ptr

audiothread = threadcreate(@audio_loop_thread)




'beep
	'print "separate thread1"
	
do
'print "separate thread1"	

'print "separate thread1"
'sdl_delay(1000)
'print "separate thread1..."
'sdl_delay(1000)

  	'if aud_handler->audiolen = 0 then
	'aud_handler->nextbuffer()
	'print "test"
	'
	'end if

 		
	if aud_handler <> null then
		'		for cnt as uint16_t = 0 to 800-1
 	'	aud_handler->sampleBuffer(cnt) = rnd*1
 	'	next
		
 	if aud_handler->audiolen = 0 then
 		
 		aud_handler->nextbuffer()
 		
 	
 		
 		


	
 	end if
 	

	end if

SwitchToThread()

Loop

ThreadWait(audiothread)
sdl_quit()


sub audio_loop_thread()

	
	

		aud_handler = new AudioHandler()
	
	

do
	
	
sdl_delay(1000)
loop



end sub

