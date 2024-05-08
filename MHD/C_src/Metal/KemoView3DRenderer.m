/*
//  KemoView3DRenderer.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import <Foundation/Foundation.h>


#import  "KemoView3DRenderer.h"

#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation KemoView3DRenderer
{
    KemoViewMetalShaders   _kemoViewShaders;
    KemoView3DPipelines    _kemoViewPipelines;
    KemoView3DBuffers     _kemoViewMetalBuf;
}


-(id) init
{
    _kemo3DMetalBufBase = [[KemoViewMetalBuffers alloc] init];
    return self;
}

- (void) setTransMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                  metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                      buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers
                         PSFs:(struct kemoview_psf *_Nonnull) kemo_psf
{
    /*  Set transparent vertexs */
    kemoView3DMetalBuf->numPSFTransTexureVertice = [_kemo3DMetalBufBase setPSFTexture:device
                                                                               buffer:kemo_buffers->PSF_ttxur_buf
                                                                                image:kemo_psf->psf_a->psf_texure
                                                                               vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                                                                               texure:&(kemoView3DMetalBuf->psfTransTexure)];

    kemoView3DMetalBuf->numPSFTransVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:kemo_buffers->PSF_trns_buf
                                                                            vertex:&(kemoView3DMetalBuf->psfTransVertice)];
    kemoView3DMetalBuf->numMeshTransVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:kemo_buffers->mesh_trns_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshTransVertice)];
    return;
}
- (void) setAxisMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                  metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                      buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers
{
    /*  Set axis vertexs */
    kemoView3DMetalBuf->numAxisVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                       buffer:kemo_buffers->axis_buf
                                                                       vertex:&(kemoView3DMetalBuf->axisVertice)];
    return;
}

- (void) set3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
               metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                  kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                   buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers
                      PSFs:(struct kemoview_psf *_Nonnull) kemo_psf
{
    kemoView3DMetalBuf->numPSFSolidTexureVertice = [_kemo3DMetalBufBase setPSFTexture:device
                                                                               buffer:kemo_buffers->PSF_stxur_buf
                                                                                image:kemo_psf->psf_a->psf_texure
                                                                               vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                                                                               texure:&(kemoView3DMetalBuf->psfSolidTexure)];

    kemoView3DMetalBuf->numCoastVertice =    [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:kemo_buffers->coast_buf
                                                                           vertex:&(kemoView3DMetalBuf->coastVertice)];
    kemoView3DMetalBuf->numSphGridVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:kemo_buffers->sph_grid_buf
                                                                           vertex:&(kemoView3DMetalBuf->sphGridVertice)];

    kemoView3DMetalBuf->numPSFSolidVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:kemo_buffers->PSF_solid_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfSolidVertice)];
    kemoView3DMetalBuf->numPSFLinesVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:kemo_buffers->PSF_isoline_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfLinesVertice)];
    kemoView3DMetalBuf->numPSFArrowVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:kemo_buffers->PSF_arrow_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfArrowVertice)];
    
    if(kemoview_get_fline_field_param(kemo_sgl, LINETYPE_FLAG) == IFLAG_PIPE){
        [_kemo3DMetalBufBase setMetalVertexs:device
                                      buffer:kemo_buffers->FLINE_tube_buf
                                      vertex:&(kemoView3DMetalBuf->fieldTubeVertice)];
    };
    kemoView3DMetalBuf->numFieldTubeVertice = kemo_buffers->FLINE_tube_buf->num_nod_buf;
    
    kemoView3DMetalBuf->numFfieldLineVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                             buffer:kemo_buffers->FLINE_line_buf
                                                                             vertex:&(kemoView3DMetalBuf->fieldLineVertice)];

    kemoView3DMetalBuf->numMeshNodeVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:kemo_buffers->mesh_node_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshNodeVertice)];
    kemoView3DMetalBuf->numMeshGridVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:kemo_buffers->mesh_grid_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshGridVertice)];
    kemoView3DMetalBuf->numMeshSolidVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:kemo_buffers->mesh_solid_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshSolidVertice)];

    /*  Set Cube Vertex buffer */
    kemoView3DMetalBuf->numCubeVertice = [_kemo3DMetalBufBase setCubeVertexs:device
                                                                      buffer:kemo_buffers->cube_buf
                                                                    indexbuf:kemo_buffers->cube_index_buf
                                                                      vertex:&(kemoView3DMetalBuf->cubeVertice)
                                                                       index:&(kemoView3DMetalBuf->cubeIndex)];
    return;
}

- (void) releaseTransMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    /*  Release transparent vertexs */
    if(kemoView3DMetalBuf->numPSFTransTexureVertice > 0){
        [kemoView3DMetalBuf->psfTTexureVertice release];
        [kemoView3DMetalBuf->psfTransTexure release];
    };
    
    if(kemoView3DMetalBuf->numPSFTransVertice > 0)  {[kemoView3DMetalBuf->psfTransVertice  release];};
    if(kemoView3DMetalBuf->numMeshTransVertice > 0) {[kemoView3DMetalBuf->meshTransVertice release];};
    return;
}

- (void) releaseAxisMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    if(kemoView3DMetalBuf->numAxisVertice > 0)  {[kemoView3DMetalBuf->axisVertice      release];};
    return;
}

- (void) release3DMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    if(kemoView3DMetalBuf->numPSFSolidTexureVertice > 0){
        [kemoView3DMetalBuf->psfSTexureVertice release];
        [kemoView3DMetalBuf->psfSolidTexure    release];
    };
    
    if(kemoView3DMetalBuf->numPSFSolidVertice > 0) {[kemoView3DMetalBuf->psfSolidVertice release];};
    if(kemoView3DMetalBuf->numPSFLinesVertice > 0) {[kemoView3DMetalBuf->psfLinesVertice release];};
    if(kemoView3DMetalBuf->numPSFArrowVertice > 0) {[kemoView3DMetalBuf->psfArrowVertice release];};
    
    if(kemoView3DMetalBuf->numFieldTubeVertice >  0) {[kemoView3DMetalBuf->fieldTubeVertice release];};
    if(kemoView3DMetalBuf->numFfieldLineVertice > 0) {[kemoView3DMetalBuf->fieldLineVertice release];};
    
    if(kemoView3DMetalBuf->numMeshNodeVertice > 0)  {[kemoView3DMetalBuf->meshNodeVertice   release];};
    if(kemoView3DMetalBuf->numMeshGridVertice > 0)  {[kemoView3DMetalBuf->meshGridVertice   release];};
    if(kemoView3DMetalBuf->numMeshSolidVertice > 0) {[kemoView3DMetalBuf->meshSolidVertice release];};
    
    if(kemoView3DMetalBuf->numCoastVertice > 0)    {[kemoView3DMetalBuf->coastVertice   release];};
    if(kemoView3DMetalBuf->numSphGridVertice > 0) {[kemoView3DMetalBuf->sphGridVertice release];};
    
    /*  Set Cube Vertex buffer */
    if(kemoView3DMetalBuf->numCubeVertice > 0){
        [kemoView3DMetalBuf->cubeVertice release];
        [kemoView3DMetalBuf->cubeIndex   release];
    };
    return;
}

- (void) setKemoTransparentMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                               kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self setTransMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers
                          PSFs:kemo_sgl->kemo_psf];
    return;
};
- (void) setKemoFastMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                        kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self setAxisMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers];
    [self setTransMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers
                          PSFs:kemo_sgl->kemo_psf];
    return;
};

- (void) setKemoView3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                          kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self setAxisMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers];
    [self setTransMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers
                          PSFs:kemo_sgl->kemo_psf];
    [self set3DMetalBuffers:device
                metalbuffer:&_kemoViewMetalBuf
                   kemoview:kemo_sgl
                    buffers:kemo_sgl->kemo_buffers
                       PSFs:kemo_sgl->kemo_psf];
    return;
}


- (void) releaseTransparentMetalBuffers
{
    [self releaseTransMetalBuffers:&_kemoViewMetalBuf];
    return;
};
- (void) releaseKemoFastMetalBuffers
{
    [self releaseTransMetalBuffers:&_kemoViewMetalBuf];
    [self releaseAxisMetalBuffers:&_kemoViewMetalBuf];
    return;
};
- (void) releaseKemoView3DMetalBuffers
{
    [self release3DMetalBuffers:&_kemoViewMetalBuf];
    [self releaseTransMetalBuffers:&_kemoViewMetalBuf];
    [self releaseAxisMetalBuffers:&_kemoViewMetalBuf];
    return;
};

-(void) add3DShaderLibrary:(KemoViewMetalShaders *) kemoViewShaders
                   library:(id<MTLLibrary> *) defaultLibrary
{
    // Load all the shader files with a .metal file extension in the project.
    kemoViewShaders->phongVertexFunction =   [*defaultLibrary newFunctionWithName:@"PhongVertexShader"];
    kemoViewShaders->phongFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongFragmentShader"];
    
    kemoViewShaders->simpleVertexFunction =   [*defaultLibrary newFunctionWithName:@"SimpleVertexShader"];
    kemoViewShaders->simpleFragmentFunction = [*defaultLibrary newFunctionWithName:@"SimpleFragmentShader"];
    
    kemoViewShaders->texuredPhongVertexFunction = [*defaultLibrary newFunctionWithName:@"PhongTexureVertexShader"];
    kemoViewShaders->texuredPhongFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongTextureFragmentShader"];
    
    kemoViewShaders->phongColorMapVertexFunction =   [*defaultLibrary newFunctionWithName:@"PhongColorMapVertexShader"];
    kemoViewShaders->phongColorMapFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongColorMapFragmentShader"];

    kemoViewShaders->texuredVertexFunction =   [*defaultLibrary newFunctionWithName:@"SimpleTexureVertexShader"];
    kemoViewShaders->texuredFragmentFunction = [*defaultLibrary newFunctionWithName:@"SimpleTextureFragmentShader"];
    return;
}


-(void) addKemo3DPipelines:(nonnull MTKView *)mtkView
                   shaders:(KemoViewMetalShaders *) kemoViewShaders
                 pipelines:(KemoView3DPipelines *) kemo3DPipelines
               targetPixel:(MTLPixelFormat) pixelformat
{
    NSError *error;
    id<MTLDevice> device = mtkView.device;
    
    MTLRenderPipelineDescriptor *pipelineStateDescriptor;
/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
    
    pipelineStateDescriptor.label = @"Phong Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->phongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->phongFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemo3DPipelines->phongPipelineState
        = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor error:&error];
    NSAssert(kemo3DPipelines->phongPipelineState, @"Failed to create pipeline state: %@", error);
    
/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Phong Shader Pipeline with colormap construction";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->phongColorMapVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->phongColorMapFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemo3DPipelines->phongColorMapPipelineState
        = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor error:&error];
    NSAssert(kemo3DPipelines->phongColorMapPipelineState, @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Texure Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texuredPhongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texuredPhongFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemo3DPipelines->phongTexturedPipelineState
        = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor error:&error];
    NSAssert(kemo3DPipelines->phongTexturedPipelineState, @"Failed to create pipeline state: %@", error);
    
/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Simple Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->simpleVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->simpleFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    kemo3DPipelines->simplePipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                  error:&error];
    NSAssert(kemo3DPipelines->simplePipelineState, @"Failed to create pipeline state: %@", error);
    
    /* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
    
    pipelineStateDescriptor.label = @"Texure Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texuredVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texuredFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemo3DPipelines->texuredPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                   error:&error];
    NSAssert(kemo3DPipelines->texuredPipelineState, @"Failed to create pipeline state: %@", error);
}

- (void)drawSolidWithSimple:(id<MTLRenderCommandEncoder> *) renderEncoder
                  pipelines:(KemoView3DPipelines *) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> *) depthState
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> *) vertices
                     unites:(KemoViewUnites *) monoViewUnites
                      sides:(int) iflag_surface
                      solid:(int) iflag_solid
{
    if(numVertex > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        if(iflag_solid == SMOOTH_SHADE){
            [*renderEncoder setDepthStencilState:*depthState];
        }
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->simplePipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
};

- (void)drawTexureWithPhong:(id<MTLRenderCommandEncoder> *) renderEncoder
                  pipelines:(KemoView3DPipelines *) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> *) depthState
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> *) vertices
                     texure:(id<MTLTexture> *) texture
                     unites:(KemoViewUnites *) monoViewUnites
                      sides:(int) iflag_surface
                      solid:(int) iflag_solid
{
    if(numVertex > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        if(iflag_solid == SMOOTH_SHADE){
            [*renderEncoder setDepthStencilState:*depthState];
        }
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongTexturedPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->normal_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:&(monoViewUnites->lights)
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:&(monoViewUnites->material)
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder setFragmentTexture:*texture
                                   atIndex:AAPLTextureIndexBaseColor];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
};

- (void)drawCubeWithPhong:(id<MTLRenderCommandEncoder> *) renderEncoder
                pipelines:(KemoView3DPipelines *) kemo3DPipelines
                    depth:(id<MTLDepthStencilState> *) depthState
                numVertex:(NSUInteger) numVertex
                   vertex:(id<MTLBuffer> *) vertices
                    index:(id<MTLBuffer> *) indices
                   unites:(KemoViewUnites *) monoViewUnites
{
    if(numVertex > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setCullMode:MTLCullModeBack];
        [*renderEncoder setDepthStencilState:*depthState];
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->normal_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:&(monoViewUnites->lights)
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:&(monoViewUnites->material)
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                   indexCount:numVertex
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:*indices
                            indexBufferOffset:0];
    };
}

- (void)drawSolidWithPhong:(id<MTLRenderCommandEncoder> *) renderEncoder
                 pipelines:(KemoView3DPipelines *) kemo3DPipelines
                     depth:(id<MTLDepthStencilState> *) depthState
                 numVertex:(NSUInteger) numVertex
                    vertex:(id<MTLBuffer> *) vertices
                    unites:(KemoViewUnites *) monoViewUnites
                     sides:(int) iflag_surface
                     solid:(int) iflag_solid
{
    if(numVertex > 0){
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        if(iflag_solid == SMOOTH_SHADE){
            [*renderEncoder setDepthStencilState:*depthState];
        }
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->normal_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:&(monoViewUnites->lights)
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:&(monoViewUnites->material)
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
};

- (void)drawLineObject:(id<MTLRenderCommandEncoder> *) renderEncoder
             pipelines:(KemoView3DPipelines *) kemo3DPipelines
                 depth:(id<MTLDepthStencilState> *) depthState
             numVertex:(NSUInteger) numVertex
                vertex:(id<MTLBuffer> *)  vertices
                unites:(KemoViewUnites *) monoViewUnites
{
    if(numVertex > 0){
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setCullMode:MTLCullModeBack];
        [*renderEncoder setDepthStencilState:*depthState];
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->simplePipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&(monoViewUnites->modelview_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&(monoViewUnites->projection_mat)
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                           vertexStart:0
                           vertexCount:numVertex];
    }
};

- (void) encodeSimpleObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   pipelines:(KemoView3DPipelines *) kemo3DPipelines
                       depth:(id<MTLDepthStencilState> *) depthState
                 metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                      unites:(KemoViewUnites *) monoViewUnites
                       sides:(int) iflag_polygon
{
    /*  Draw solid objects */
    [self drawTexureWithPhong:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                    numVertex:kemoView3DMetalBuf->numPSFSolidTexureVertice
                       vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfSolidTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numAxisVertice
                      vertex:&(kemoView3DMetalBuf->axisVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFSolidVertice
                      vertex:&(kemoView3DMetalBuf->psfSolidVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numFfieldLineVertice
                  vertex:&(kemoView3DMetalBuf->fieldLineVertice)
                  unites:monoViewUnites];
    
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numMeshGridVertice
                  vertex:&(kemoView3DMetalBuf->meshGridVertice)
                  unites:monoViewUnites];
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshSolidVertice
                      vertex:&(kemoView3DMetalBuf->meshSolidVertice)
                      unites:monoViewUnites
                       sides:iflag_polygon
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                  numVertex:kemoView3DMetalBuf->numCubeVertice
                     vertex:&(kemoView3DMetalBuf->cubeVertice)
                      index:&(kemoView3DMetalBuf->cubeIndex)
                     unites:monoViewUnites];
    
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numCoastVertice
                  vertex:&(kemoView3DMetalBuf->coastVertice)
                  unites:monoViewUnites];
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numSphGridVertice
                  vertex:&(kemoView3DMetalBuf->sphGridVertice)
                  unites:monoViewUnites];
    
    /*  Draw transparent objects */
    [self drawTexureWithPhong:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                    numVertex:kemoView3DMetalBuf->numPSFTransTexureVertice
                       vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfTransTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFTransVertice
                      vertex:&(kemoView3DMetalBuf->psfTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshTransVertice
                      vertex:&(kemoView3DMetalBuf->meshTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];

    return;
}

- (void) encode3DObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
               pipelines:(KemoView3DPipelines *) kemo3DPipelines
                   depth:(id<MTLDepthStencilState> *) depthState
             metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                  unites:(KemoViewUnites *) monoViewUnites
                   sides:(int) iflag_polygon
               fieldTube:(int) iflag_tube
{

    /*  Draw solid objects */
    [self drawTexureWithPhong:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                    numVertex:kemoView3DMetalBuf->numPSFSolidTexureVertice
                       vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfSolidTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numAxisVertice
                      vertex:&(kemoView3DMetalBuf->axisVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFSolidVertice
                      vertex:&(kemoView3DMetalBuf->psfSolidVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFLinesVertice
                      vertex:&(kemoView3DMetalBuf->psfLinesVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFArrowVertice
                      vertex:&(kemoView3DMetalBuf->psfArrowVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    if(iflag_tube == IFLAG_PIPE){
        [self drawSolidWithPhong:renderEncoder
                       pipelines:kemo3DPipelines
                           depth:depthState
                       numVertex:kemoView3DMetalBuf->numFieldTubeVertice\
         
                          vertex:&(kemoView3DMetalBuf->fieldTubeVertice)
                          unites:monoViewUnites
                           sides:BOTH_SURFACES
                           solid:SMOOTH_SHADE];
    };
    
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numFfieldLineVertice
                  vertex:&(kemoView3DMetalBuf->fieldLineVertice)
                  unites:monoViewUnites];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshNodeVertice
                      vertex:&(kemoView3DMetalBuf->meshNodeVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numMeshGridVertice
                  vertex:&(kemoView3DMetalBuf->meshGridVertice)
                  unites:monoViewUnites];
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshSolidVertice
                      vertex:&(kemoView3DMetalBuf->meshSolidVertice)
                      unites:monoViewUnites
                       sides:iflag_polygon
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                  numVertex:kemoView3DMetalBuf->numCubeVertice
                     vertex:&(kemoView3DMetalBuf->cubeVertice)
                      index:&(kemoView3DMetalBuf->cubeIndex)
                     unites:monoViewUnites];
    
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numCoastVertice
                  vertex:&(kemoView3DMetalBuf->coastVertice)
                  unites:monoViewUnites];
    [self drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numSphGridVertice
                  vertex:&(kemoView3DMetalBuf->sphGridVertice)
                  unites:monoViewUnites];
    
/*  Draw transparent objects */
    [self drawTexureWithPhong:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                    numVertex:kemoView3DMetalBuf->numPSFTransTexureVertice
                       vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfTransTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFTransVertice
                      vertex:&(kemoView3DMetalBuf->psfTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshTransVertice
                      vertex:&(kemoView3DMetalBuf->meshTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];

    return;
}



-(void) addKemoView3DShaderLibrary:(id<MTLLibrary> _Nonnull *_Nonnull) defaultLibrary
{
    [self add3DShaderLibrary:&_kemoViewShaders
                     library:defaultLibrary];
    return;
}

-(void) addKemoView3DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat
{
    [self addKemo3DPipelines:mtkView
                     shaders:&_kemoViewShaders
                   pipelines:&_kemoViewPipelines
                 targetPixel:pixelformat];
    return;
}

- (void) encodeKemoSimpleObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                           depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
                           sides:(int) iflag_polygon
{
    [self encodeSimpleObjects:renderEncoder
                    pipelines:&_kemoViewPipelines
                        depth:depthState
                  metalbuffer:&_kemoViewMetalBuf
                       unites:monoViewUnites
                        sides:iflag_polygon];
    return;
};

- (void) encodeKemoView3DObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                           depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
                           sides:(int) iflag_polygon
                       fieldTube:(int) iflag_tube
{
    [self encode3DObjects:renderEncoder
                pipelines:&_kemoViewPipelines
                    depth:depthState
              metalbuffer:&_kemoViewMetalBuf
                   unites:monoViewUnites
                    sides:iflag_polygon
                fieldTube:iflag_tube];
    return;
}

@end
