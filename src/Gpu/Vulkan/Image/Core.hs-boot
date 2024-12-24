module Gpu.Vulkan.Image.Core (MemoryBarrier2, PtrMemoryBarrier2, I, Blit2, PtrBlit2) where

import Foreign.Ptr

data MemoryBarrier2
type PtrMemoryBarrier2 = Ptr MemoryBarrier2

data Blit2
type PtrBlit2 = Ptr Blit2

data ITag
type I = Ptr ITag
