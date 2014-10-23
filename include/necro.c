#include "necro.h"

typedef struct
{
	float amplitude;
	float offset;
} UNum;

#define sampleRate 44100.0
#define recpSampleRate (1.0 / sampleRate)
#define pi (3.14159265359)
#define rSample2Pi (2.0 * pi * recpSampleRate)
	
UNum necroSin(UNum frequency, double frameTime)
{
	UNum uNum;
	uNum.amplitude = sin (frequency.offset * frameTime * rSample2Pi + frequency.amplitude);
	uNum.offset    = 0.0;
	return uNum;
}







