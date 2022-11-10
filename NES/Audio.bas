
#include "SDL2/SDL.bi"
#include "windows.bi"
#include "crt.bi"


 



type AudioHandler
	
	
	
	declare sub nextbuffer()
	
	declare constructor()
	declare constructor(freq as long,frmat as SDL_AudioFormat,samps as Uint16, bitdpth as int16_t )
   declare constructor(freq as long,frmat as SDL_AudioFormat,samps as Uint16, bitdpth as int16_t, Callbck as SDL_AudioCallback)


	
	'hasAudio as boolean = true
	hasAudio as long = true
	
	static that as AudioHandler ptr
	audio_device as SDL_AudioDeviceID
	samples as float
	sampleBuffer(any) as double
	samplesperframe as integer = 735
	
	inputbuffer(4096) as float
	inputreadpos as uint32_t
	inputbufferpos as uint32_t

	audiolen as uint32_t
	
	audio_spec as SDL_AudioSpec
	
	audioinit as long
	
	declare static Sub Process Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)
	
	declare sub start_aud()
	'declare sub resume_aud()
	
	declare sub stop_aud()
	
	
	private:
	x as double
	

	sample as float
	maxAmplitude as Sint32
	intSample as Sint32
	
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
	
SDL_zero(audio_spec)

this.audiolen = 2048
audio_spec.freq = 48000
audio_spec.format = AUDIO_F32SYS
audio_spec.channels = 1
audio_spec.samples = 2048
audio_spec.callback = @Process
audio_spec.userdata =NULL
	
	redim this.sampleBuffer(735) as float
	
	
		'audioinit = SDL_OpenAudio(@audio_spec, NULL)
		
		
		  this.audio_device = SDL_OpenAudioDevice(NULL, 0, @audio_spec, NULL, 0)
		  SDL_PauseAudioDevice(audio_device, 1)
	'If audioinit < 0 Then
	'	'Print "failed! ("; *SDL_GetError(); ")"
	'Else
	'	'Print "OK!"
	'	
	'	SDL_PauseAudio(0)
	'
	'End If

	
	this.samples = audio_spec.freq / 60
	redim this.sampleBuffer(samples) as double
	this.samplesPerframe =  this.samples

	sdl_pauseaudio(0)
	
	
	
End If





End Constructor

constructor AudioHandler(freq as long,frmat as SDL_AudioFormat,samps as Uint16, bitdpth as int16_t )

print "inializing SDL audio..."
this.hasaudio = SDL_Init(SDL_INIT_AUDIO)
 that = @this
If this.hasaudio <> null Then
	Print "failed!"
	'hasaudio = false
	End
Else
	Print "OK!"
	

this.audiolen = samps
audio_spec.freq = freq
audio_spec.format = frmat
audio_spec.channels = 1
audio_spec.samples = samps
audio_spec.callback = @Process
audio_spec.userdata =NULL
	
	redim this.sampleBuffer(735) as double
	
	
		'audioinit = SDL_OpenAudio(@audio_spec, NULL)
		
		
		  this.audio_device = SDL_OpenAudioDevice(NULL, 0, @audio_spec, NULL, 0)
		  SDL_PauseAudioDevice(audio_device, 1)
	'If audioinit < 0 Then
	'	'Print "failed! ("; *SDL_GetError(); ")"
	'Else
	'	'Print "OK!"
	'	
	'	SDL_PauseAudio(0)
	'
	'End If

	
	this.samples = audio_spec.freq /60
	redim this.sampleBuffer(samples) as double
	this.samplesPerframe =  this.samples
	
		
	
	this.maxAmplitude = pow(2,bitdpth-1)-1
	
	
	
	
	sdl_pauseaudio(0)
	
	
	
End If





End Constructor


constructor AudioHandler(freq as long,frmat as SDL_AudioFormat,samps as Uint16, bitdpth as int16_t, callbck as SDL_AudioCallback)

print "inializing SDL audio..."
this.hasaudio = SDL_Init(SDL_INIT_AUDIO)
 that = @this
If this.hasaudio <> null Then
	Print "failed!"
	'hasaudio = false
	End
Else
	Print "OK!"
	

this.audiolen = samps
audio_spec.freq = freq
audio_spec.format = frmat
audio_spec.channels = 1
audio_spec.samples = samps
audio_spec.callback = callbck
audio_spec.userdata =NULL
	
	redim this.sampleBuffer(735) as double
	
	
		'audioinit = SDL_OpenAudio(@audio_spec, NULL)
		
		
		  this.audio_device = SDL_OpenAudioDevice(NULL, 0, @audio_spec, NULL, 0)
		  SDL_PauseAudioDevice(audio_device, 1)
	'If audioinit < 0 Then
	'	'Print "failed! ("; *SDL_GetError(); ")"
	'Else
	'	'Print "OK!"
	'	
	'	SDL_PauseAudio(0)
	'
	'End If

	
	this.samples = audio_spec.freq /60
	redim this.sampleBuffer(samples) as double
	this.samplesPerframe =  this.samples
	
		
	
	this.maxAmplitude = pow(2,bitdpth-1)-1
	
	
	
	
	sdl_pauseaudio(0)
	
	
	
End If





End Constructor





















Sub AudioHandler.Process Cdecl (ByVal userdata As Any Ptr, ByVal audbuffer As Ubyte Ptr, ByVal audlen As Integer)


'if that->audiolen = 0 then
'	
'	
'	exit sub
'EndIf

if that->audio_spec.format = AUDIO_F32SYS then

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
    
	For n as integer = 0 to (audlen/4)-1  
   that->x+=.010
	'sin((that->x*4))*0.15'
		dim sample as float ptr = cast(float ptr,audbuffer)
		sample[n]  = cast(float,that->inputbuffer(that->inputReadPos and &HFFF))
		'(rnd *2-1)*0.15'
		that->inputReadPos+=1

		'that->audiolen -=1
	Next n
	
elseif that->audio_spec.format  = AUDIO_S16SYS then
	
	
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
   'that->x+=.010
	
		dim sample as Sint16 ptr = cast(Sint16 ptr,audbuffer) 
		
		
		'that->sample = sin((that->x*4))*0.20'(rnd*2-1)*0.15
		
		that->intSample = cast(Sint32, that->inputbuffer(that->inputReadPos and &HFFF) * that->maxAmplitude)
		
		sample[n]  = cast(Sint16,that->intSample)
		
		
	
		that->inputReadPos+=1

	Next n
	
	
	
	
	
	
	
end if


	
End Sub



sub AudioHandler.start_aud()
	
SDL_PauseAudioDevice(this.audio_device, 0)


	
End Sub

sub AudioHandler.stop_aud()
	
	SDL_PauseAudioDevice(this.audio_device, 1)
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
		'this.x+=.010
			
		aud_val = this.sampleBuffer(i)'0.10528000000000003'rnd * 1 '0.0028454054054054055' this.sampleBuffer(i)
		this.inputbuffer(this.inputbufferpos and &HFFF) = aud_val'sgn(sin((this.x*4)))*0.15
		'(rnd *2-1)*0.15
		'this.inputbuffer(this.inputbufferpos and &HFFF) = rnd*2-1'aud_val
		
		this.inputbufferpos+=1
	Next 
	'this.audiolen = 2048
	'sin( (8* ATN(1)) * i * ( 400 / 48000) )*0.2'
End Sub

dim AudioHandler.that as AudioHandler ptr

