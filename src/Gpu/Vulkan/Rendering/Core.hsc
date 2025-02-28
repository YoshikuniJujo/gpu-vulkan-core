{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Rendering.Core where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.List.ToolsYj
import Data.Word
import Data.Int

import Unsafe.Coerce

import Gpu.Vulkan.Core qualified as Vk
import Gpu.Vulkan.ImageView.Core qualified as ImageView

#include <vulkan/vulkan.h>

data ClearValue = ClearValue Word32 Word32 Word32 Word32
	deriving Show

struct "AttachmentInfo" #{size VkRenderingAttachmentInfo}
	#{alignment VkRenderingAttachmentInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ ->
			#{poke VkRenderingAttachmentInfo, sType} p sTypeA |]),
	("pNext", ''PtrVoid,
		[| #{peek VkRenderingAttachmentInfo, pNext} |],
		[| #{poke VkRenderingAttachmentInfo, pNext} |]),
	("imageView", ''ImageView.I,
		[| #{peek VkRenderingAttachmentInfo, imageView} |],
		[| #{poke VkRenderingAttachmentInfo, imageView} |]),
	("imageLayout", ''#{type VkImageLayout},
		[| #{peek VkRenderingAttachmentInfo, imageLayout} |],
		[| #{poke VkRenderingAttachmentInfo, imageLayout} |]),
	("resolveMode", ''#{type VkResolveModeFlagBits},
		[| #{peek VkRenderingAttachmentInfo, resolveMode} |],
		[| #{poke VkRenderingAttachmentInfo, resolveMode} |]),
	("resolveImageView", ''ImageView.I,
		[| #{peek VkRenderingAttachmentInfo, resolveImageView} |],
		[| #{poke VkRenderingAttachmentInfo, resolveImageView} |]),
	("resolveImageLayout", ''#{type VkImageLayout},
		[| #{peek VkRenderingAttachmentInfo, resolveImageLayout} |],
		[| #{poke VkRenderingAttachmentInfo, resolveImageLayout} |]),
	("loadOp", ''#{type VkAttachmentLoadOp},
		[| #{peek VkRenderingAttachmentInfo, loadOp} |],
		[| #{poke VkRenderingAttachmentInfo, loadOp} |]),
	("storeOp", ''#{type VkAttachmentStoreOp},
		[| #{peek VkRenderingAttachmentInfo, storeOp} |],
		[| #{poke VkRenderingAttachmentInfo, storeOp} |]),
	("clearValue", ''ClearValue,
		[| #{peek VkRenderingAttachmentInfo, clearValue} |],
		[| #{poke VkRenderingAttachmentInfo, clearValue} |]) ]
	[''Show, ''Storable]

type PtrAttachmentInfo = Ptr AttachmentInfo

sTypeA :: #{type VkStructureType}
sTypeA = #{const VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO}

getClearedAttachmentInfo :: IO AttachmentInfo
getClearedAttachmentInfo = do
	pa <- calloc
	AttachmentInfo_ <$> newForeignPtr pa (free pa)

instance Storable ClearValue where
	sizeOf _ = 4 * sizeOf (undefined :: Word32)
	alignment _ = alignment (undefined :: Word32)
	peek p = unc4 ClearValue . listToTuple4 <$> peekArray 4 (castPtr p)
	poke p (ClearValue r g b a) = pokeArray (castPtr p) [r, g, b, a]

unc4 :: (a -> b -> c -> d -> r) -> (a, b, c, d) -> r
unc4 f (x, y, z, w) = f x y z w

struct "Info" #{size VkRenderingInfo} #{alignment VkRenderingInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderingInfo, sType} p sTypeI |]),
	("pNext", ''PtrVoid,
		[| #{peek VkRenderingInfo, pNext} |],
		[| #{poke VkRenderingInfo, pNext} |]),
	("flags", ''#{type VkRenderingFlags},
		[| #{peek VkRenderingInfo, flags} |],
		[| #{poke VkRenderingInfo, flags} |]),
	("renderArea", ''Vk.Rect2d,
		[| #{peek VkRenderingInfo, renderArea} |],
		[| #{poke VkRenderingInfo, renderArea} |]),
	("layerCount", ''#{type uint32_t},
		[| #{peek VkRenderingInfo, layerCount} |],
		[| #{poke VkRenderingInfo, layerCount} |]),
	("viewMask", ''#{type uint32_t},
		[| #{peek VkRenderingInfo, viewMask} |],
		[| #{poke VkRenderingInfo, viewMask} |]),
	("colorAttachmentCount", ''#{type uint32_t},
		[| #{peek VkRenderingInfo, colorAttachmentCount} |],
		[| #{poke VkRenderingInfo, colorAttachmentCount} |]),
	("pColorAttachments", ''PtrAttachmentInfo,
		[| #{peek VkRenderingInfo, pColorAttachments} |],
		[| #{poke VkRenderingInfo, pColorAttachments} |]),
	("pDepthAttachment", ''PtrAttachmentInfo,
		[| #{peek VkRenderingInfo, pDepthAttachment} |],
		[| #{poke VkRenderingInfo, pDepthAttachment} |]),
	("pStencilAttachment", ''PtrAttachmentInfo,
		[| #{peek VkRenderingInfo, pStencilAttachment} |],
		[| #{poke VkRenderingInfo, pStencilAttachment} |]) ]
	[''Show, ''Storable]

sTypeI :: #{type VkStructureType}
sTypeI = #{const VK_STRUCTURE_TYPE_RENDERING_INFO}

class ClearValueToClearColorValue n where
	clearValueToClearColorValue :: ClearValue -> ClearColorValue n

instance ClearValueToClearColorValue Float where
	clearValueToClearColorValue (ClearValue r g b a) = ClearColorValueFloat
		(unsafeCoerce r) (unsafeCoerce g)
		(unsafeCoerce b) (unsafeCoerce a)

instance ClearValueToClearColorValue Int32 where
	clearValueToClearColorValue (ClearValue r g b a) = ClearColorValueInt
		(unsafeCoerce r) (unsafeCoerce g)
		(unsafeCoerce b) (unsafeCoerce a)

instance ClearValueToClearColorValue Word32 where
	clearValueToClearColorValue (ClearValue r g b a) = ClearColorValueUint
		(unsafeCoerce r) (unsafeCoerce g)
		(unsafeCoerce b) (unsafeCoerce a)

clearColorValueToClearValue :: ClearColorValue n -> ClearValue
clearColorValueToClearValue = \case
	ClearColorValueFloat r g b a -> ClearValue
		(unsafeCoerce r) (unsafeCoerce g)
		(unsafeCoerce b) (unsafeCoerce a)
	ClearColorValueInt r g b a -> ClearValue
		(unsafeCoerce r) (unsafeCoerce g)
		(unsafeCoerce b) (unsafeCoerce a)
	ClearColorValueUint r g b a -> ClearValue r g b a

data ClearColorValue n where
	ClearColorValueFloat ::
		Float -> Float -> Float -> Float -> ClearColorValue Float
	ClearColorValueInt ::
		Int32 -> Int32 -> Int32 -> Int32 -> ClearColorValue Int32
	ClearColorValueUint ::
		Word32 -> Word32 -> Word32 -> Word32 -> ClearColorValue Word32

deriving instance Show (ClearColorValue n)

clearValueToClearDepthStencilValue :: ClearValue -> Vk.ClearDepthStencilValue
clearValueToClearDepthStencilValue (ClearValue d s _ _) =
	Vk.ClearDepthStencilValue (unsafeCoerce d) s

clearDepthStencilValueToClearValue :: Vk.ClearDepthStencilValue -> ClearValue
clearDepthStencilValueToClearValue (Vk.ClearDepthStencilValue d s) =
	ClearValue (unsafeCoerce d) s 0 0
