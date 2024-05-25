/*
//  KemoView3DBaseRenderer.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoView3DBaseRenderer_h_
#define KemoView3DBaseRenderer_h_

@import simd;
@import MetalKit;
@import Foundation;

#import "KemoViewShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewMetalBuffers.h"
#import "KemoViewRendererTools.h"

#include "m_kemoviewer_data.h"

typedef struct
{
    /*  Shader functions for simple shader  */
    id<MTLFunction>            _Nonnull simpleVertexFunction;
    id<MTLFunction>            _Nonnull simpleFragmentFunction;
    
    /*  Shader functions for textured  shader  */
    id<MTLFunction>            _Nonnull texuredVertexFunction;
    id<MTLFunction>            _Nonnull texuredFragmentFunction;
    
    /*  Shader functions for Phong shader  */
    id<MTLFunction> _Nonnull phongVertexFunction;
    id<MTLFunction> _Nonnull phongFragmentFunction;
    
    /*  Shader functions for Phong shader with colormap */
    id<MTLFunction> _Nonnull phongColorMapVertexFunction;
    id<MTLFunction> _Nonnull phongColorMapFragmentFunction;
    
    /*  Shader functions for textured Phong shader  */
    id<MTLFunction> _Nonnull texuredPhongVertexFunction;
    id<MTLFunction> _Nonnull texuredPhongFragmentFunction;
} KemoViewMetalShaders;

typedef struct
{
    /*  Shader functions for simple shader  */
    id<MTLRenderPipelineState> _Nonnull simplePipelineState;
    /*  Shader functions for Phong shader  */
    id<MTLRenderPipelineState> _Nonnull phongPipelineState;
    /*  Shader functions for Phong shader with colormap construction */
    id<MTLRenderPipelineState> _Nonnull phongColorMapPipelineState;
    /*  Shader functions for textured Phong shader  */
    id<MTLRenderPipelineState> _Nonnull phongTexturedPipelineState;

    /*  Shader functions for textured  shader  */
    id<MTLRenderPipelineState> _Nonnull texuredPipelineState;
} KemoView3DPipelines;

@interface KemoView3DBaseRenderer : NSObject

-(void) add3DShaderLibrary:(KemoViewMetalShaders *_Nullable) kemoViewShaders
                   library:(id<MTLLibrary> _Nullable *_Nullable) defaultLibrary;
-(void) addKemo3DPipelines:(nonnull MTKView *)mtkView
                   shaders:(KemoViewMetalShaders *_Nullable) kemoViewShaders
                 pipelines:(KemoView3DPipelines  *_Nullable) kemo3DPipelines
               targetPixel:(MTLPixelFormat) pixelformat;

- (void)drawSolidWithSimple:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                  pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                     unites:(KemoViewUnites *_Nullable) monoViewUnites
                      sides:(int) iflag_surface;

- (void)drawSolidWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                 pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                     depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                 numVertex:(NSUInteger) numVertex
                    vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                    unites:(KemoViewUnites *_Nullable) monoViewUnites
                     sides:(int) iflag_surface;
- (void)drawTexureWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                  pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                     texure:(id<MTLTexture> _Nullable *_Nullable) texture
                     unites:(KemoViewUnites *_Nullable) monoViewUnites
                      sides:(int) iflag_surface;

- (void)drawIndexPatchWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                      pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                          depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                      numVertex:(NSUInteger) numVertex
                         vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                           index:(id<MTLBuffer> _Nullable *_Nullable) indices
                         unites:(KemoViewUnites *_Nullable) monoViewUnites
                          sides:(int) iflag_surface;
- (void)drawIndexTexureWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                       pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                           depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                       numVertex:(NSUInteger) numVertex
                          vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                           index:(id<MTLBuffer> _Nullable *_Nullable) indices
                          texure:(id<MTLTexture> _Nullable *_Nullable) texture
                          unites:(KemoViewUnites *_Nullable) monoViewUnites
                           sides:(int) iflag_surface;

- (void)drawLineObject:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
             pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                 depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
             numVertex:(NSUInteger) numVertex
                vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                unites:(KemoViewUnites *_Nullable) monoViewUnites;
;
@end
#endif /* KemoView3DBaseRenderer_h_ */
