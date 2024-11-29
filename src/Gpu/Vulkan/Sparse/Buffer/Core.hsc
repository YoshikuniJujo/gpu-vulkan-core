{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Buffer.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.Buffer.Core qualified as Buffer
import Gpu.Vulkan.Sparse.Core

#include <vulkan/vulkan.h>

struct "MemoryBindInfo" #{size VkSparseBufferMemoryBindInfo}
	#{alignment VkSparseBufferMemoryBindInfo} [
	("buffer", ''Buffer.B,
		[| #{peek VkSparseBufferMemoryBindInfo, buffer} |],
		[| #{poke VkSparseBufferMemoryBindInfo, buffer} |]),
	("bindCount", ''#{type uint32_t},
		[| #{peek VkSparseBufferMemoryBindInfo, bindCount} |],
		[| #{poke VkSparseBufferMemoryBindInfo, bindCount} |]),
	("pBinds", ''PtrMemoryBind,
		[| #{peek VkSparseBufferMemoryBindInfo, pBinds} |],
		[| #{poke VkSparseBufferMemoryBindInfo, pBinds} |]) ]
	[''Show, ''Storable]

type PtrMemoryBindInfo = Ptr MemoryBindInfo
