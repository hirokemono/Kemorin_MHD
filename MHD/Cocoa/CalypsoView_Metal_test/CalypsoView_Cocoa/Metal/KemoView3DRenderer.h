/*
//  KemoView3DRenderer.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoView3DRenderer_h_
#define KemoView3DRenderer_h_

@import simd;
@import MetalKit;
@import Foundation;

#import "KemoViewShaderTypes.h"
#import "KemoViewMetalBuffers.h"
#import "KemoViewRendererTools.h"

#include "m_kemoviewer_data.h"

typedef struct
{
    /*  Vertex buffer for initial cube */
    id<MTLBuffer> _Nullable cubeVertice;
    NSUInteger numCubeVertice;
    /*  Index buffer for initial cube */
    id<MTLBuffer> _Nullable cubeIndex;
} KemoView3DBuffers;

typedef struct
{
    /*  Shader functions for Phong shader  */
    id<MTLFunction> _Nonnull phongVertexFunction;
    id<MTLFunction> _Nonnull phongFragmentFunction;
    
    /*  Shader functions for Phong shader with colormap */
    id<MTLFunction> _Nonnull phongColorMapVertexFunction;
    id<MTLFunction> _Nonnull phongColorMapFragmentFunction;
} KemoViewMetalShaders;

typedef struct
{
    /*  Shader functions for Phong shader  */
    id<MTLRenderPipelineState> _Nonnull phongPipelineState;
    /*  Shader functions for Phong shader with colormap construction */
    id<MTLRenderPipelineState> _Nonnull phongColorMapPipelineState;

} KemoView3DPipelines;

@interface KemoView3DRenderer : NSObject
{
    KemoViewMetalBuffers * _kemo3DMetalBufBase;
}

- (void) setKemoView3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                          kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl;

- (void) releaseKemoView3DMetalBuffers;

-(void) addKemoView3DShaderLibrary:(id<MTLLibrary> _Nonnull *_Nonnull) defaultLibrary;

-(void) addKemoView3DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat;

- (void) encodeKemoSimpleObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                           depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
                           sides:(int) iflag_polygon;
@end


#endif /* KemoView3DRenderer_h_ */
