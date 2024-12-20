{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LAnGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Core (

	-- * SUBMIT AND WAIT IDLE

	Q, submit, submit2, waitIdle,

	-- * BIND SPARSE

	bindSparse, BindSparseInfo, pattern BindSparseInfo,
	bindSparseInfoSType, bindSparseInfoPNext,
	bindSparseInfoWaitSemaphoreCount, bindSparseInfoPWaitSemaphores,
	bindSparseInfoBufferBindCount, bindSparseInfoPBufferBinds,
	bindSparseInfoImageOpaqueBindCount, bindSparseInfoPImageOpaqueBinds,
	bindSparseInfoImageBindCount, bindSparseInfoPImageBinds,
	bindSparseInfoSignalSemaphoreCount, bindSparseInfoPSignalSemaphores

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.Core

import {-# SOURCE #-} qualified Gpu.Vulkan.Fence.Core as Fence
import {-# SOURCE #-} Gpu.Vulkan.Semaphore.Core qualified as Semaphore

import Gpu.Vulkan.Sparse.Buffer.Core qualified as Spr.Bffr
import Gpu.Vulkan.Sparse.Image.Core qualified as Spr.Img

#include <vulkan/vulkan.h>

data QTag
type Q = Ptr QTag

foreign import ccall "vkQueueSubmit" submit ::
	Q -> #{type uint32_t} -> Ptr SubmitInfo -> Fence.F ->
	IO #{type VkResult}

foreign import ccall "vkQueueWaitIdle" waitIdle :: Q -> IO #{type VkResult}

struct "BindSparseInfo" #{size VkBindSparseInfo} #{alignment VkBindSparseInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkBufferCreateInfo, sType } p
			(#{const VK_STRUCTURE_TYPE_BIND_SPARSE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid,
		[| #{peek VkBindSparseInfo, pNext} |],
		[| #{poke VkBindSparseInfo, pNext} |]),
	("waitSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkBindSparseInfo, waitSemaphoreCount} |],
		[| #{poke VkBindSparseInfo, waitSemaphoreCount} |]),
	("pWaitSemaphores", ''Semaphore.PtrS,
		[| #{peek VkBindSparseInfo, pWaitSemaphores} |],
		[| #{poke VkBindSparseInfo, pWaitSemaphores} |]),
	("bufferBindCount", ''#{type uint32_t},
		[| #{peek VkBindSparseInfo, bufferBindCount} |],
		[| #{poke VkBindSparseInfo, bufferBindCount} |]),
	("pBufferBinds", ''Spr.Bffr.PtrMemoryBindInfo,
		[| #{peek VkBindSparseInfo, pBufferBinds} |],
		[| #{poke VkBindSparseInfo, pBufferBinds} |]),
	("imageOpaqueBindCount", ''#{type uint32_t},
		[| #{peek VkBindSparseInfo, imageOpaqueBindCount} |],
		[| #{poke VkBindSparseInfo, imageOpaqueBindCount} |]),
	("pImageOpaqueBinds", ''Spr.Img.PtrOpaqueMemoryBindInfo,
		[| #{peek VkBindSparseInfo, pImageOpaqueBinds} |],
		[| #{poke VkBindSparseInfo, pImageOpaqueBinds} |]),
	("imageBindCount", ''#{type uint32_t},
		[| #{peek VkBindSparseInfo, imageBindCount} |],
		[| #{poke VkBindSparseInfo, imageBindCount} |]),
	("pImageBinds", ''Spr.Img.PtrMemoryBindInfo,
		[| #{peek VkBindSparseInfo, pImageBinds} |],
		[| #{poke VkBindSparseInfo, pImageBinds} |]),
	("signalSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkBindSparseInfo, signalSemaphoreCount} |],
		[| #{poke VkBindSparseInfo, signalSemaphoreCount} |]),
	("pSignalSemaphores", ''Semaphore.PtrS,
		[| #{peek VkBindSparseInfo, pSignalSemaphores} |],
		[| #{poke VkBindSparseInfo, pSignalSemaphores} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkQueueBindSparse" bindSparse ::
	Q -> #{type uint32_t} -> Ptr BindSparseInfo -> Fence.F ->
	IO #{type VkResult}

foreign import ccall "vkQueueSubmit2" submit2 ::
	Q -> #{type uint32_t} -> Ptr SubmitInfo2 -> Fence.F ->
	IO #{type VkResult}
