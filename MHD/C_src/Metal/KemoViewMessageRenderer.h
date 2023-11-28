/*
//  KemoViewMessageRenderer.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoViewMessageRenderer_h_
#define KemoViewMessageRenderer_h_

@import simd;
@import MetalKit;
@import Foundation;

#import "AAPLShaderTypes.h"
#import "AAPLImage.h"

#include "m_kemoview_object_buffers.h"

@interface KemoViewMessageRenderer : NSObject

typedef struct
{
/*  Shader functions for simple 2D shader  */
    id<MTLRenderPipelineState> _Nonnull trans2DPipelineState;
    id<MTLFunction> _Nonnull simple2DVertexFunction;
    id<MTLFunction> _Nonnull simple2DFragmentFunction;

/*  Shader functions for textured 2D shader  */
    id<MTLFunction> _Nonnull texured2DVertexFunction;
    id<MTLFunction> _Nonnull texured2DFragmentFunction;
    
/*  Shader functions for original 2D shader  */
    id<MTLFunction> _Nonnull base2DVertexFunction;
    id<MTLFunction> _Nonnull base2DFragmentFunction;
} KemoView2DMetalShaders;

typedef struct
{
/*  Shader functions for simple 2D shader  */
    id<MTLRenderPipelineState> _Nonnull simple2DPipelineState;
/*  Shader functions for transpearernt 2D shader  */
    id<MTLRenderPipelineState> _Nonnull trans2DPipelineState;
/*  Shader functions for textured 2D shader  */
    id<MTLRenderPipelineState> _Nonnull texured2DPipelineState;
/*  Shader functions for original 2D shader  */
    id<MTLRenderPipelineState> _Nonnull base2DPipelineState;
} KemoView2DMetalPipelines;

-(void) add2DShaderLibrary:(id<MTLLibrary>  *) shaderLibrary;

-(void) addKemoView2DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat;


- (void) encodeMapObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
              metalBuffer:(KemoViewMetalBuffers *) kemoViewMetalBuf
                   buffer:(struct kemoview_buffers * _Nonnull) kemo_buffers
               projection:(matrix_float4x4 * _Nonnull) map_proj_mat;
- (void) encodeMessageObjects:(id<MTLRenderCommandEncoder> *) renderEncoder
                  metalBuffer:(KemoViewMetalBuffers *) kemoViewMetalBuf
                       buffer:(struct kemoview_buffers * _Nonnull) kemo_buffers
                   projection:(matrix_float4x4 *) projection_mat;
@end


#endif /* KemoViewMessageRenderer_h_ */
