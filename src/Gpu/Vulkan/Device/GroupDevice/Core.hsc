{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.GroupDevice.Core (
	CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext,
	createInfoPhysicalDeviceCount, createInfoPPhysicalDevices ) where

import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

import Gpu.Vulkan.PhysicalDevice.Core qualified as PhysicalDevice

#include <vulkan/vulkan.h>

createInfoType :: #{type VkStructureType}
createInfoType = #{const VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO}

struct "CreateInfo" #{size VkDeviceGroupDeviceCreateInfo}
		#{alignment VkDeviceGroupDeviceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDeviceGroupDeviceCreateInfo, sType}
			p createInfoType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDeviceGroupDeviceCreateInfo, pNext} |],
		[| #{poke VkDeviceGroupDeviceCreateInfo, pNext} |]),
	("physicalDeviceCount", ''#{type uint32_t},
		[| #{peek VkDeviceGroupDeviceCreateInfo, physicalDeviceCount} |],
		[| #{poke VkDeviceGroupDeviceCreateInfo, physicalDeviceCount} |]),
	("pPhysicalDevices", ''PhysicalDevice.PtrP,
		[| #{peek VkDeviceGroupDeviceCreateInfo, pPhysicalDevices} |],
		[| #{poke VkDeviceGroupDeviceCreateInfo, pPhysicalDevices} |]) ]
	[''Show, ''Storable]
