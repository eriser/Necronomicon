/*
    Necronomicon
    Copyright 2014-2015 Chad McKinney and Curtis McKinney
*/

#ifndef NECRONOMICON_ENDIAN_H_INCLUDED
#define NECRONOMICON_ENDIAN_H_INCLUDED

#if defined(__APPLE__)

# include <machine/endian.h>

#elif defined(__FreeBSD__)

# include <machine/endian.h>
# include <netinet/in.h>

#elif defined(_WIN32)

# define LITTLE_ENDIAN 1234
# define BIG_ENDIAN 4321
# define BYTE_ORDER LITTLE_ENDIAN
# include <winsock2.h>
# undef IN
# undef OUT

#elif defined(__linux__)

# include <endian.h>
# include <netinet/in.h>

#else

# error cannot find endianess on this platform

#endif

#endif // NECRONOMICON_ENDIAN_H_INCLUDED
