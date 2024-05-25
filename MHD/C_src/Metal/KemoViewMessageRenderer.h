/*
//  KemoViewMessageRenderer.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoViewMesageRenderer_h_
#define KemoViewMesageRenderer_h_

@import simd;
@import MetalKit;
@import Foundation;

#import "KemoViewShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewMetalBuffers.h"
#import "KemoView2DRenderer.h"

#include "m_kemoview_object_buffers.h"

typedef struct
{
    /*  Vertex buffer for  color bar */
    id<MTLBuffer> _Nullable colorBarVertice;
    NSUInteger numColorBarVertice;
    /*  Vertex buffer for min label on color bar */
    id<MTLBuffer> _Nullable minLabelVertice;
    NSUInteger numMinLabelVertice;
    /*  Vertex buffer for max label on color bar */
    id<MTLBuffer> _Nullable maxLabelVertice;
    NSUInteger numMaxLabelVertice;
    /*  Vertex buffer for zero label on color bar */
    id<MTLBuffer> _Nullable zeroLabelVertice;
    NSUInteger numZeroLabelVertice;
    /*  Vertex buffer for time box */
    id<MTLBuffer> _Nullable timeLabelVertice;
    NSUInteger numtimeLabelVertice;
    /*  Vertex buffer for message box */
    id<MTLBuffer> _Nullable messageVertice;
    NSUInteger numMessageVertice;

    /*  Texure buffer for min label on color bar */
    id<MTLTexture> _Nullable minLabelTexure;
    /*  Texure buffer for max label on color bar */
    id<MTLTexture> _Nullable maxLabelTexure;
    /*  Texure buffer for zero label on color bar */
    id<MTLTexture> _Nullable zeroLabelTexure;
    /*  Texure buffer for time box */
    id<MTLTexture> _Nullable timeLabelTexure;
    /*  Texure buffer for message box */
    id<MTLTexture> _Nullable messageTexure;
} KemoViewMessageMetalBuffers;

@interface KemoViewMessageRenderer : NSObject
- (void) releaseMsgMetalBuffers:(KemoViewMessageMetalBuffers *_Nonnull) kemoViewMessageMetalBufs;

- (void) setMessageMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
                baseMetalBuffer:(KemoViewMetalBuffers *_Nonnull) kemo2DMetalBufBase
                    metalBuffer:(KemoViewMessageMetalBuffers *_Nonnull) kemoViewMessageMetalBufs
                        buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers;


- (void) encodeMessageObjects:(id<MTLRenderCommandEncoder> _Nonnull * _Nonnull) renderEncoder
                  base2Dclass:(KemoView2DRenderer *_Nonnull) KemoView2DRenderer
                    pipelines:(KemoView2DMetalPipelines *_Nonnull) kemoView2DPipelines
                  metalBuffer:(KemoViewMessageMetalBuffers *_Nonnull) kemoViewMessageMetalBufs
                   projection:(matrix_float4x4 * _Nonnull) projection_mat;
@end


#endif /* KemoViewMesageRenderer_h_ */
