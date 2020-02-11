/*++ BUILD Version: 0002    // Increment this if a change has global effects

Copyright (c) Microsoft Corporation. All rights reserved. 

You may only use this code if you agree to the terms of the Windows Research Kernel Source Code License agreement (see License.txt).
If you do not agree to the terms, do not use the code.


Module Name:

    config.h

Abstract:

    Header file for kernel configuration.

--*/

#ifndef _NTOSKRNL_CFG_HEADER_
#define _NTOSKRNL_CFG_HEADER_

/*---- BEGIN STANDARD CONFIGURATION ----*/

/* Uncomment to remove branding and NTOSDrop CE specific code */
/* #define __REMOVE_BRANDING__ */

/* Uncomment to enable trace-level debugging */
/* #define __DEBUG_LEVEL_TRACE__ */

/* Uncomment to enable main() function in ntoskrnl.c */
/* #define __ENABLE_NT_MAIN__ */

/*----- END STANDARD CONFIGURATION -----*/

//
// DO NOT MODIFY ANYTHING BELOW OR IT WILL BREAK THE KERNEL.
// YOU HAVE BEEN WARNED.
// 
// N.B.:  DO NOT MODIFY ANYTHING BELOW OR RICHARDG WILL
//        SEND YOU A FORMER TAKEDOWN NOTICE!
//

#ifdef __DEBUG_LEVEL_TRACE__
#define DPRINT _ntprintf("(%s:%d:DEBUG) ", __FILE__, __LINE__), _ntprintf
#define TRACE  _ntprintf("(%s:%d:TRACE) ", __FILE__, __LINE__), _ntprintf
#define ERROR  _ntprintf("(%s:%d:ERROR) ", __FILE__, __LINE__), _ntprintf
VOID
NTAPI
_ntprintf(
      char* fmt,
      ...
      );
#else 
#define DPRINT
#define TRACE
#define ERROR
#endif

#endif // _NTOSKRNL_CFG_HEADER_
