/*
//  KemoViewMapRenderer.h
//
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoViewMapRenderer_h_
#define KemoViewMapRenderer_h_


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
    /*  Vertex buffer for Map solid node */
    id<MTLBuffer> _Nullable mapNodeVertice;
    NSUInteger numMapNodeVertice;
    /*  Index buffer for psf solid patch */
    id<MTLBuffer> _Nullable mapSolidIndices;
    NSUInteger numMapSolidIndices;

    /*  Vertex buffer for Map solid patch */
    id<MTLBuffer> _Nullable mapSolidVertice;
    NSUInteger numMapSolidVertice;
    /*  Vertex buffer for Map isolines */
    id<MTLBuffer> _Nullable mapLinesVertice;
    NSUInteger numMapLinesVertice;
    id<MTLBuffer> _Nullable mapLinesIndice;
    NSUInteger numMapLinesIndice;

    /*  Vertex buffer for Coast lines */
    id<MTLBuffer> _Nullable coastLineVertice;
    NSUInteger numCoastLineVertice;
    id<MTLBuffer> _Nullable coastTubeVertice;
    NSUInteger numCoastTubeVertice;
    id<MTLBuffer> _Nullable coastTubeIndice;
    NSUInteger numCoastTubeIndice;
} KemoViewMapMetalBuffers;

@interface KemoViewMapRenderer : NSObject

- (void) releaseMapMetalBuffers:(KemoViewMapMetalBuffers *_Nonnull) kemoViewMapMetalBufs;

- (void) setMapMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
            baseMetalBuffer:(KemoViewMetalBuffers *_Nonnull) kemo2DMetalBufBase
                metalBuffer:(KemoViewMapMetalBuffers *_Nonnull) kemoViewMapMetalBufs
                    buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers;

- (void) encodeMapObjects:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull) renderEncoder
              base2Dclass:(KemoView2DRenderer *_Nonnull) KemoView2DRenderer
                pipelines:(KemoView2DMetalPipelines *_Nonnull) kemoView2DPipelines
              metalBuffer:(KemoViewMapMetalBuffers *_Nonnull) kemoViewMapMetalBufs
               projection:(matrix_float4x4 * _Nonnull) map_proj_mat;
@end


#endif /* KemoViewMapRenderer_h_ */
