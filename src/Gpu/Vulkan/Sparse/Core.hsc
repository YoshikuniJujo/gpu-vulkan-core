{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.Memory.Core qualified as Memory

#include <vulkan/vulkan.h>

struct "MemoryBind" #{size VkSparseMemoryBind} #{alignment VkSparseMemoryBind} [
	("resourceOffset", ''#{type VkDeviceSize},
		[| #{peek VkSparseMemoryBind, resourceOffset} |],
		[| #{poke VkSparseMemoryBind, resourceOffset} |]),
	("size", ''#{type VkDeviceSize},
		[| #{peek VkSparseMemoryBind, size} |],
		[| #{poke VkSparseMemoryBind, size} |]),
	("memory", ''Memory.M,
		[| #{peek VkSparseMemoryBind, memory} |],
		[| #{poke VkSparseMemoryBind, memory} |]),
	("memoryOffset", ''#{type VkDeviceSize},
		[| #{peek VkSparseMemoryBind, memoryOffset} |],
		[| #{poke VkSparseMemoryBind, memoryOffset} |]),
	("flags", ''#{type VkSparseMemoryBindFlags},
		[| #{peek VkSparseMemoryBind, flags} |],
		[| #{poke VkSparseMemoryBind, flags} |]) ]
	[''Show, ''Storable]

type PtrMemoryBind = Ptr MemoryBind
