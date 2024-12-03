{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Image.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.Core
import Gpu.Vulkan.Memory.Core qualified as Memory
import Gpu.Vulkan.Image.Core qualified as Image
import Gpu.Vulkan.Sparse.Core qualified as S

#include <vulkan/vulkan.h>

struct "OpaqueMemoryBindInfo" #{size VkSparseImageOpaqueMemoryBindInfo}
	#{alignment VkSparseImageOpaqueMemoryBindInfo} [
	("image", ''Image.I,
		[| #{peek VkSparseImageOpaqueMemoryBindInfo, image} |],
		[| #{poke VkSparseImageOpaqueMemoryBindInfo, image} |]),
	("bindCount", ''#{type uint32_t},
		[| #{peek VkSparseImageOpaqueMemoryBindInfo, bindCount} |],
		[| #{poke VkSparseImageOpaqueMemoryBindInfo, bindCount} |]),
	("pBinds", ''S.PtrMemoryBind,
		[| #{peek VkSparseImageOpaqueMemoryBindInfo, pBinds} |],
		[| #{poke VkSparseImageOpaqueMemoryBindInfo, pBinds} |]) ]
	[''Show, ''Storable]

type PtrOpaqueMemoryBindInfo = Ptr OpaqueMemoryBindInfo

struct "MemoryBind" #{size VkSparseImageMemoryBind}
	#{alignment VkSparseImageMemoryBind} [
	("subresource", ''Image.Subresource,
		[| #{peek VkSparseImageMemoryBind, subresource} |],
		[| #{poke VkSparseImageMemoryBind, subresource} |]),
	("offset", ''Offset3d,
		[| #{peek VkSparseImageMemoryBind, offset} |],
		[| #{poke VkSparseImageMemoryBind, offset} |]),
	("extent", ''Extent3d,
		[| #{peek VkSparseImageMemoryBind, extent} |],
		[| #{poke VkSparseImageMemoryBind, extent} |]),
	("memory", ''Memory.M,
		[| #{peek VkSparseImageMemoryBind, memory} |],
		[| #{poke VkSparseImageMemoryBind, memory} |]),
	("memoryOffset", ''#{type VkDeviceSize},
		[| #{peek VkSparseImageMemoryBind, memoryOffset} |],
		[| #{poke VkSparseImageMemoryBind, memoryOffset} |]),
	("flags", ''#{type VkSparseMemoryBindFlags},
		[| #{peek VkSparseImageMemoryBind, flags} |],
		[| #{poke VkSparseImageMemoryBind, flags} |]) ]
	[''Show, ''Storable]

type PtrMemoryBind = Ptr MemoryBind

struct "MemoryBindInfo" #{size VkSparseImageMemoryBindInfo}
	#{alignment VkSparseImageMemoryBindInfo} [
	("image", ''Image.I,
		[| #{peek VkSparseImageMemoryBindInfo, image} |],
		[| #{poke VkSparseImageMemoryBindInfo, image} |]),
	("bindCount", ''#{type uint32_t},
		[| #{peek VkSparseImageMemoryBindInfo, bindCount} |],
		[| #{poke VkSparseImageMemoryBindInfo, bindCount} |]),
	("pBinds", ''PtrMemoryBind,
		[| #{peek VkSparseImageMemoryBindInfo, pBinds} |],
		[| #{poke VkSparseImageMemoryBindInfo, pBinds} |]) ]
	[''Show, ''Storable]

type PtrMemoryBindInfo = Ptr MemoryBindInfo
