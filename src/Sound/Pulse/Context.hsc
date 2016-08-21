{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.Pulse.Context
where


#include <pulse/context.h>

import Foreign.Ptr
import Foreign.C.String

data PAContext a

data PAMainloopApi

foreign import ccall "pa_context_new" pa_context_new :: Ptr PAMainloopApi -> CString -> IO (Ptr (PAContext PAMainloopApi))
