{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Core (C, PtrC, PtrSubmitInfo) where

import Foreign.Ptr

data CTag
type C = Ptr CTag
type PtrC = Ptr C

data SubmitInfo
type PtrSubmitInfo = Ptr SubmitInfo
