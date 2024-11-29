{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sparse.Image.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.Image.Core qualified as Image
import Gpu.Vulkan.Sparse.Core

#include <vulkan/vulkan.h>

struct "OpaqueMemoryBindInfo" #{size VkSparseImageOpaqueMemoryBindInfo}
	#{alignment VkSparseImageOpaqueMemoryBindInfo} [
	("image", ''Image.I,
		[| #{peek VkSparseImageOpaqueMemoryBindInfo, image} |],
		[| #{poke VkSparseImageOpaqueMemoryBindInfo, image} |]),
	("bindCount", ''#{type uint32_t},
		[| #{peek VkSparseImageOpaqueMemoryBindInfo, bindCount} |],
		[| #{poke VkSparseImageOpaqueMemoryBindInfo, bindCount} |]),
	("pBinds", ''PtrMemoryBind,
		[| #{peek VkSparseImageOpaqueMemoryBindInfo, pBinds} |],
		[| #{poke VkSparseImageOpaqueMemoryBindInfo, pBinds} |]) ]
	[''Show, ''Storable]
