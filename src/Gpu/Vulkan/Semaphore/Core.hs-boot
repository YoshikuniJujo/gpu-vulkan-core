{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Core (S, PtrS, PtrSubmitInfo) where

import Foreign.Ptr

data STag
type S = Ptr STag
type PtrS = Ptr S

data SubmitInfo
type PtrSubmitInfo = Ptr SubmitInfo
