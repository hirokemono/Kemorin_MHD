/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for a platform independent renderer class, which performs Metal setup and per frame rendering.
*/

@import MetalKit;

#import "AAPLShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewerObject.h"

#include "m_kemoviewer_data.h"
#include "m_kemoview_object_buffers.h"
#include "m_gl_transfer_matrix.h"
#include "m_transfer_matrices.h"

@interface AAPLRenderer : NSObject<MTKViewDelegate>
typedef struct
{
/*    Texture to render screen to texture */
    matrix_float4x4 modelview_mat;
    matrix_float4x4 projection_mat;
    matrix_float4x4 normal_mat;
    
    LightSourceParameters lights;
    MaterialParameters    material;
} KemoViewUnites;

typedef struct
{
    /*  Vertex buffer for initial cube */
    id<MTLBuffer> _Nullable cubeVertice;
    /*  Index buffer for initial cube */
    id<MTLBuffer> _Nullable cubeIndex;
    
    /*  Vertex buffer for PSF solid patch */
    id<MTLBuffer> _Nullable psfSolidVertice;
    /*  Vertex buffer for PSF transoarent patch */
    id<MTLBuffer> _Nullable psfTransVertice;
    /*  Vertex buffer for PSF with texure patch */
    id<MTLBuffer> _Nullable psfSTexureVertice;
    /*  Vertex buffer for PSF with transoarent texure patch */
    id<MTLBuffer> _Nullable psfTTexureVertice;
    /*  Vertex buffer for PSF arrows */
    id<MTLBuffer> _Nullable psfArrowVertice;
    /*  Vertex buffer for PSF isolines */
    id<MTLBuffer> _Nullable psfLinesVertice;
    
    /*  Texure buffer for PSF with texure */
    id<MTLTexture> _Nullable psfSolidTexure;
    /*  Texure buffer for PSF with transparent texure */
    id<MTLTexture> _Nullable psfTransTexure;
    
    /*  Vertex buffer for field lines solid patch */
    id<MTLBuffer> _Nullable fieldTubeVertice;
    /*  Vertex buffer for field lines  */
    id<MTLBuffer> _Nullable fieldLineVertice;
    
    /*  Vertex buffer for mesh solid patch */
    id<MTLBuffer> _Nullable meshSolidVertice;
    /*  Vertex buffer for mesh transoarent patch */
    id<MTLBuffer> _Nullable meshTransVertice;
    /*  Vertex buffer for mesh grid */
    id<MTLBuffer> _Nullable meshGridVertice;
    /*  Vertex buffer for mesh nodes */
    id<MTLBuffer> _Nullable meshNodeVertice;
    
    /*  Vertex buffer for Coast lines */
    id<MTLBuffer> _Nullable coastVertice;
    /*  Vertex buffer for sphere grids */
    id<MTLBuffer> _Nullable sphGridVertice;
    /*  Vertex buffer for axis arrows */
    id<MTLBuffer> _Nullable axisVertice;
} KemoViewMetalBuffers;

typedef struct
{
    /*  Vertex buffer for Map solid patch */
    id<MTLBuffer> _Nullable mapSolidVertice;
    /*  Vertex buffer for Map isolines */
    id<MTLBuffer> _Nullable mapLinesVertice;
    
    /*  Vertex buffer for Coast lines */
    id<MTLBuffer> _Nullable coastVertice;
    /*  Vertex buffer for sphere grids */
    id<MTLBuffer> _Nullable sphGridVertice;

    /*  Vertex buffer for  color bar */
    id<MTLBuffer> _Nullable colorBarVertice;
    /*  Vertex buffer for min label on color bar */
    id<MTLBuffer> _Nullable minLabelVertice;
    /*  Vertex buffer for max label on color bar */
    id<MTLBuffer> _Nullable maxLabelVertice;
    /*  Vertex buffer for zero label on color bar */
    id<MTLBuffer> _Nullable zeroLabelVertice;
    /*  Vertex buffer for time box */
    id<MTLBuffer> _Nullable timeLabelVertice;
    /*  Vertex buffer for message box */
    id<MTLBuffer> _Nullable messageVertice;

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
} KemoView2DMetalShaders;

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
    
    /*  Shader functions for textured Phong shader  */
    id<MTLFunction> _Nonnull texuredPhongVertexFunction;
    id<MTLFunction> _Nonnull texuredPhongFragmentFunction;
    
    /*  Shader functions for textured Phong anaglyph shader  */
    id<MTLFunction> _Nonnull PhongAnaglyphVertexFunction;
    id<MTLFunction> _Nonnull PhongAnaglyphFragmentFunction;
    
    /*  Shader functions for simple 2D shader  */
    id<MTLFunction> _Nonnull simple2DVertexFunction;
    id<MTLFunction> _Nonnull simple2DFragmentFunction;

    /*  Shader functions for textured 2D shader  */
    id<MTLFunction> _Nonnull texured2DVertexFunction;
    id<MTLFunction> _Nonnull texured2DFragmentFunction;
    
    /*  Shader functions for original 2D shader  */
    id<MTLFunction> _Nonnull base2DVertexFunction;
    id<MTLFunction> _Nonnull base2DFragmentFunction;
} KemoViewMetalShaders;

typedef struct
{
    /*  Shader functions for simple shader  */
    id<MTLRenderPipelineState> _Nonnull simplePipelineState;
    /*  Shader functions for Phong shader  */
    id<MTLRenderPipelineState> _Nonnull phongPipelineState;
    /*  Shader functions for textured Phong shader  */
    id<MTLRenderPipelineState> _Nonnull phongTexturedPipelineState;

    /*  Shader functions for textured  shader  */
    id<MTLRenderPipelineState> _Nonnull texuredPipelineState;
    /*  Shader functions for textured Phong anaglyph shader  */
    id<MTLRenderPipelineState> _Nonnull phongAnaglyphPipelineState;
} KemoView3DPipelines;

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

- (void)setTransferMatrices;

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;
- (void)drawKemoMetalView:(nonnull MTKView *)view
                  eyeflag:(int) iflag_lr;
- (void)drawInMTKView:(nonnull MTKView *)view;

@end
