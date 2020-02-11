/*++

Copyright (c) Microsoft Corporation. All rights reserved. 

You may only use this code if you agree to the terms of the Windows Research Kernel Source Code License agreement (see License.txt).
If you do not agree to the terms, do not use the code.


Module Name:

    ntoskrnl.c

Abstract:

    Null program for the INIT subcomponent of the NTOS project

--*/

#ifdef __ENABLE_NT_MAIN__

#include "ntos.h"

int
cdecl
main(
    IN PLOADER_PARAMETER_BLOCK LoaderBlock
    )
{
#ifdef i386

    KiSystemStartup(LoaderBlock);

#else

    KiSystemStartup();

#endif

    return 0;
}

#endif // __ENABLE_NT_MAIN__
