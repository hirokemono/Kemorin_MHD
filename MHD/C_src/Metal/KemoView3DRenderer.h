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
#import "AAPLImage.h"
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
    
    /*  Vertex buffer for PSF solid patch */
    id<MTLBuffer> _Nullable psfSolidVertice;
    NSUInteger numPSFSolidVertice;
    /*  Vertex buffer for PSF transoarent patch */
    id<MTLBuffer> _Nullable psfTransVertice;
    NSUInteger numPSFTransVertice;
    /*  Vertex buffer for PSF with texure patch */
    id<MTLBuffer> _Nullable psfSTexureVertice;
    NSUInteger numPSFSolidTexureVertice;
    /*  Vertex buffer for PSF with transoarent texure patch */
    id<MTLBuffer> _Nullable psfTTexureVertice;
    NSUInteger numPSFTransTexureVertice;
    /*  Vertex buffer for PSF arrows */
    id<MTLBuffer> _Nullable psfArrowVertice;
    NSUInteger numPSFArrowVertice;
    /*  Vertex buffer for PSF isolines */
    id<MTLBuffer> _Nullable psfLinesVertice;
    NSUInteger numPSFLinesVertice;

    /*  Texure buffer for PSF with texure */
    id<MTLTexture> _Nullable psfSolidTexure;
    /*  Texure buffer for PSF with transparent texure */
    id<MTLTexture> _Nullable psfTransTexure;
    
    /*  Vertex buffer for field lines solid patch */
    id<MTLBuffer> _Nullable fieldTubeVertice;
    NSUInteger numFieldTubeVertice;
    /*  Vertex buffer for field lines  */
    id<MTLBuffer> _Nullable fieldLineVertice;
    NSUInteger numFfieldLineVertice;

    /*  Vertex buffer for mesh solid patch */
    id<MTLBuffer> _Nullable meshSolidVertice;
    NSUInteger numMeshSolidVertice;
    /*  Vertex buffer for mesh transoarent patch */
    id<MTLBuffer> _Nullable meshTransVertice;
    NSUInteger numMeshTransVertice;
    /*  Vertex buffer for mesh grid */
    id<MTLBuffer> _Nullable meshGridVertice;
    NSUInteger numMeshGridVertice;
    /*  Vertex buffer for mesh nodes */
    id<MTLBuffer> _Nullable meshNodeVertice;
    NSUInteger numMeshNodeVertice;

    /*  Vertex buffer for Coast lines */
    id<MTLBuffer> _Nullable coastVertice;
    NSUInteger numCoastVertice;
    /*  Vertex buffer for sphere grids */
    id<MTLBuffer> _Nullable sphGridVertice;
    NSUInteger numSphGridVertice;
    /*  Vertex buffer for axis arrows */
    id<MTLBuffer> _Nullable axisVertice;
    NSUInteger numAxisVertice;
} KemoView3DBuffers;

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

@interface KemoView3DRenderer : NSObject
{
    KemoViewMetalBuffers * _kemo3DMetalBufBase;
}

- (void) setKemoTransparentMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                               kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl;
- (void) setKemoFastMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                        kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl;
- (void) setKemoView3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                          kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl;

- (void) releaseTransparentMetalBuffers;
- (void) releaseKemoFastMetalBuffers;
- (void) releaseKemoView3DMetalBuffers;

-(void) addKemoView3DShaderLibrary:(id<MTLLibrary> _Nonnull *_Nonnull) defaultLibrary;

-(void) addKemoView3DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat;

- (void) encodeKemoSimpleObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                           depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
                           sides:(int) iflag_polygon;

- (void) encodeKemoView3DObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                           depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
                           sides:(int) iflag_polygon
                       fieldTube:(int) iflag_tube;
@end


#endif /* KemoView3DRenderer_h_ */
