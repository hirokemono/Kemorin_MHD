/*
//  KemoView2DRenderer.h
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#ifndef KemoView2DRenderer_h_
#define KemoView2DRenderer_h_

@import simd;
@import MetalKit;
@import Foundation;

#import "KemoViewShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewMetalBuffers.h"

#include "m_kemoview_object_buffers.h"

typedef struct
{
    /*  Vertex buffer for Map solid patch */
    id<MTLBuffer> _Nullable mapSolidVertice;
    NSUInteger numMapSolidVertice;
    /*  Vertex buffer for Map isolines */
    id<MTLBuffer> _Nullable mapLinesVertice;
    NSUInteger numMapinesVertice;

    /*  Vertex buffer for Coast lines */
    id<MTLBuffer> _Nullable coastVertice;
    NSUInteger numCoastVertice;

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
} KemoView2DMetalBuffers;

typedef struct
{
/*  Shader functions for simple 2D shader  */
    id<MTLFunction> _Nonnull simple2DVertexFunction;
    id<MTLFunction> _Nonnull simple2DFragmentFunction;

/*  Shader functions for textured 2D shader  */
    id<MTLFunction> _Nonnull texured2DVertexFunction;
    id<MTLFunction> _Nonnull texured2DFragmentFunction;
    
/*  Shader functions for original 2D shader  */
    id<MTLFunction> _Nonnull base2DVertexFunction;
    id<MTLFunction> _Nonnull base2DFragmentFunction;
    
/*  Shader functions for original 2D shader  */
    id<MTLFunction> _Nonnull AnaglyphVertexFunction;
    id<MTLFunction> _Nonnull AnaglyphFragmentFunction;
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
/*  Shader functions for Anaglyph shader  */
    id<MTLRenderPipelineState> _Nonnull anaglyphPipelineState;
} KemoView2DMetalPipelines;


@interface KemoView2DRenderer : NSObject
{
    KemoViewMetalBuffers * _kemo2DMetalBufBase;
}

- (void) releaseMapMetalBuffers;
- (void) releaseMsgMetalBuffers;
- (void) setMapMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
                    buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers;
- (void) setMessageMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
                        buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers;


-(void) add2DShaderLibrary:(id<MTLLibrary> _Nonnull * _Nonnull) shaderLibrary;

-(void) addKemoView2DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat;

- (void) encodeMapObjects:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull) renderEncoder
               projection:(matrix_float4x4 * _Nonnull) map_proj_mat;
- (void) encodeMessageObjects:(id<MTLRenderCommandEncoder> _Nonnull * _Nonnull) renderEncoder
                   projection:(matrix_float4x4 * _Nonnull) projection_mat;

- (void) encodeAnaglyphObjects:(id<MTLRenderCommandEncoder> _Nonnull * _Nonnull) renderEncoder
                     numVertex:(NSUInteger) numVertex
                        vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  anaglyphVertex
                          left:(id<MTLTexture> _Nonnull *_Nonnull) leftTexure
                         right:(id<MTLTexture> _Nonnull *_Nonnull) righTtexure
                    projection:(matrix_float4x4 * _Nonnull) projection_mat;
@end


#endif /* KemoView2DRenderer_h_ */
