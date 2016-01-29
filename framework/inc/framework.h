#ifndef __FRAMEWORK_H__
#define __FRAMEWORK_H__

#ifdef __linux
	#include "config.h"
#else
	#include "D:\Projects\PRGM_WORKCRAFT\inc\config.h"
#endif

// prototypes called by Haskell

/*  ENCODING
 *  Description:
 *  
 *  Input parameters:
 *
 *  Ouput parameters:
 **/ 
extern "C" encoding_graphs();

#endif

