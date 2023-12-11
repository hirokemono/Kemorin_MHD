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
    [_kemo3DMetalBufBase setPSFTexture:device
                                buffer:kemo_buffers->PSF_ttxur_buf
                                 image:kemo_psf->psf_a->psf_texure
                                vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                                texure:&(kemoView3DMetalBuf->psfTransTexure)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->PSF_trns_buf
                                  vertex:&(kemoView3DMetalBuf->psfTransVertice)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->mesh_trns_buf
                                  vertex:&(kemoView3DMetalBuf->meshTransVertice)];

    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->axis_buf
                                  vertex:&(kemoView3DMetalBuf->axisVertice)];
    return;
}

- (void) set3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
               metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                   buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers
                      PSFs:(struct kemoview_psf *_Nonnull) kemo_psf
                 fieldline:(struct kemoview_fline *_Nonnull) kemo_fline
{
    [_kemo3DMetalBufBase setPSFTexture:device
                                buffer:kemo_buffers->PSF_stxur_buf
                                 image:kemo_psf->psf_a->psf_texure
                                vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                                texure:&(kemoView3DMetalBuf->psfSolidTexure)];
    
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->coast_buf
                                  vertex:&(kemoView3DMetalBuf->coastVertice)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->sph_grid_buf
                                  vertex:&(kemoView3DMetalBuf->sphGridVertice)];
    
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->PSF_solid_buf
                                  vertex:&(kemoView3DMetalBuf->psfSolidVertice)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->PSF_isoline_buf
                                  vertex:&(kemoView3DMetalBuf->psfLinesVertice)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->PSF_arrow_buf
                                  vertex:&(kemoView3DMetalBuf->psfArrowVertice)];
    
    if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
        [_kemo3DMetalBufBase setMetalVertexs:device
                                      buffer:kemo_buffers->FLINE_tube_buf
                                      vertex:&(kemoView3DMetalBuf->fieldTubeVertice)];
    };
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->FLINE_line_buf
                                  vertex:&(kemoView3DMetalBuf->fieldLineVertice)];
    
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->mesh_node_buf
                                  vertex:&(kemoView3DMetalBuf->meshNodeVertice)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->mesh_grid_buf
                                  vertex:&(kemoView3DMetalBuf->meshGridVertice)];
    [_kemo3DMetalBufBase setMetalVertexs:device
                                  buffer:kemo_buffers->mesh_solid_buf
                                  vertex:&(kemoView3DMetalBuf->meshSolidVertice)];
    
    /*  Set Cube Vertex buffer */
    [_kemo3DMetalBufBase setCubeVertexs:device
                                 buffer:kemo_buffers->cube_buf
                               indexbuf:kemo_buffers->cube_index_buf
                                 vertex:&(kemoView3DMetalBuf->cubeVertice)
                                  index:&(kemoView3DMetalBuf->cubeIndex)];
    return;
}

- (void) releaseTransMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                          buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers;
{
    /*  Release transparent vertexs */
    if(kemo_buffers->PSF_ttxur_buf->num_nod_buf > 0){
        [kemoView3DMetalBuf->psfTTexureVertice release];
        [kemoView3DMetalBuf->psfTransTexure release];
    };
    
    if(kemo_buffers->PSF_trns_buf->num_nod_buf > 0)  {[kemoView3DMetalBuf->psfTransVertice release];};
    if(kemo_buffers->mesh_trns_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->meshTransVertice release];};

    if(kemo_buffers->axis_buf->num_nod_buf > 0){[kemoView3DMetalBuf->axisVertice     release];};
    return;
}

- (void) release3DMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                       buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers
{
    if(kemo_buffers->PSF_stxur_buf->num_nod_buf > 0){
        [kemoView3DMetalBuf->psfSTexureVertice release];
        [kemoView3DMetalBuf->psfSolidTexure    release];
    };
    
    if(kemo_buffers->PSF_solid_buf->num_nod_buf > 0)   {[kemoView3DMetalBuf->psfSolidVertice release];};
    if(kemo_buffers->PSF_isoline_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->psfLinesVertice release];};
    if(kemo_buffers->PSF_arrow_buf->num_nod_buf > 0)   {[kemoView3DMetalBuf->psfArrowVertice release];};
    
    if(kemo_buffers->FLINE_tube_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->fieldTubeVertice release];};
    if(kemo_buffers->FLINE_line_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->fieldLineVertice release];};
    
    if(kemo_buffers->mesh_node_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->meshNodeVertice   release];};
    if(kemo_buffers->mesh_grid_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->meshGridVertice   release];};
    if(kemo_buffers->mesh_solid_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->meshSolidVertice release];};
    
    if(kemo_buffers->coast_buf->num_nod_buf > 0)    {[kemoView3DMetalBuf->coastVertice   release];};
    if(kemo_buffers->sph_grid_buf->num_nod_buf > 0) {[kemoView3DMetalBuf->sphGridVertice release];};
    
    /*  Set Cube Vertex buffer */
    if(kemo_buffers->cube_buf->num_nod_buf > 0){
        [kemoView3DMetalBuf->cubeVertice release];
        [kemoView3DMetalBuf->cubeIndex   release];
    };
    return;
}

- (void) setKemoView3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                          kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self set3DMetalBuffers:device
                metalbuffer:&_kemoViewMetalBuf
                    buffers:kemo_sgl->kemo_buffers
                       PSFs:kemo_sgl->kemo_psf
                  fieldline:kemo_sgl->kemo_fline];
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

- (void) releaseKemoView3DMetalBuffers:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self release3DMetalBuffers:&_kemoViewMetalBuf
                        buffers:kemo_sgl->kemo_buffers];
    return;
};
- (void) releaseTransparentMetalBuffers:(struct kemoviewer_type *_Nonnull) kemo_sgl;
{
    [self releaseTransMetalBuffers:&_kemoViewMetalBuf
                           buffers:kemo_sgl->kemo_buffers];
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
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
    
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

- (void)drawSolidWithSimple:(struct gl_strided_buffer *) buf
                    encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                  pipelines:(KemoView3DPipelines *) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> *) depthState
                     vertex:(id<MTLBuffer> *) vertices
                     unites:(KemoViewUnites *) monoViewUnites
                      sides:(int) iflag_surface
                      solid:(int) iflag_solid
{
    if(buf->num_nod_buf > 0){
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
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawTexureWithPhong:(struct gl_strided_buffer *) buf
                    encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                  pipelines:(KemoView3DPipelines *) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> *) depthState
                     vertex:(id<MTLBuffer> *) vertices
                     texure:(id<MTLTexture> *) texture
                     unites:(KemoViewUnites *) monoViewUnites
                      sides:(int) iflag_surface
                      solid:(int) iflag_solid
{
    if(buf->num_nod_buf > 0){
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
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawCubeWithPhong:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                pipelines:(KemoView3DPipelines *) kemo3DPipelines
                    depth:(id<MTLDepthStencilState> *) depthState
                   vertex:(id<MTLBuffer> *) vertices
                    index:(id<MTLBuffer> *) indices
                   unites:(KemoViewUnites *) monoViewUnites
{
    if(buf->num_nod_buf > 0){
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
                                   indexCount:buf->num_nod_buf
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:*indices
                            indexBufferOffset:0];
    };
}

- (void)drawSolidWithPhong:(struct gl_strided_buffer *) buf
                   encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                 pipelines:(KemoView3DPipelines *) kemo3DPipelines
                     depth:(id<MTLDepthStencilState> *) depthState
                    vertex:(id<MTLBuffer> *) vertices
                    unites:(KemoViewUnites *) monoViewUnites
                     sides:(int) iflag_surface
                     solid:(int) iflag_solid
{
    if(buf->num_nod_buf > 0){
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
                           vertexCount:buf->num_nod_buf];
    };
};

- (void)drawLineObject:(struct gl_strided_buffer *) buf
               encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
             pipelines:(KemoView3DPipelines *) kemo3DPipelines
                 depth:(id<MTLDepthStencilState> *) depthState
                vertex:(id<MTLBuffer> *)  vertices
                unites:(KemoViewUnites *) monoViewUnites
{
    if(buf->num_nod_buf > 0){
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
                           vertexCount:buf->num_nod_buf];
    }
};

- (void) encodeSimpleObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   pipelines:(KemoView3DPipelines *) kemo3DPipelines
                       depth:(id<MTLDepthStencilState> *) depthState
                 metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                      buffer:(struct kemoview_buffers *) kemo_buffers
                        mesh:(struct kemoview_mesh *) kemo_mesh
                      unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfSolidTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->axis_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->axisVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->psfSolidVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->fieldLineVertice)
                  unites:monoViewUnites];
    
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->meshGridVertice)
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->meshSolidVertice)
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                     vertex:&(kemoView3DMetalBuf->cubeVertice)
                      index:&(kemoView3DMetalBuf->cubeIndex)
                     unites:monoViewUnites];
    
    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->coastVertice)
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->sphGridVertice)
                  unites:monoViewUnites];
    
    /*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfTransTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->psfTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
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
                  buffer:(struct kemoview_buffers *) kemo_buffers
               fieldline:(struct kemoview_fline *) kemo_fline
                    mesh:(struct kemoview_mesh *) kemo_mesh
                  unites:(KemoViewUnites *) monoViewUnites
{
    [self drawTexureWithPhong:kemo_buffers->PSF_stxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfSolidTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->axis_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->axisVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->psfSolidVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_isoline_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->psfLinesVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawSolidWithPhong:kemo_buffers->PSF_arrow_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->psfArrowVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    
    if(kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
        [self drawSolidWithPhong:kemo_buffers->FLINE_tube_buf
                         encoder:renderEncoder
                       pipelines:kemo3DPipelines
                           depth:depthState
                          vertex:&(kemoView3DMetalBuf->fieldTubeVertice)
                          unites:monoViewUnites
                           sides:BOTH_SURFACES
                           solid:SMOOTH_SHADE];
    };
    
    [self drawLineObject:kemo_buffers->FLINE_line_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->fieldLineVertice)
                  unites:monoViewUnites];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_node_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->meshNodeVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:SMOOTH_SHADE];
    [self drawLineObject:kemo_buffers->mesh_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->meshGridVertice)
                  unites:monoViewUnites];
    [self drawSolidWithPhong:kemo_buffers->mesh_solid_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->meshSolidVertice)
                      unites:monoViewUnites
                       sides:kemo_mesh->mesh_m->polygon_mode
                       solid:SMOOTH_SHADE];
    [self drawCubeWithPhong:kemo_buffers->cube_buf
                    encoder:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                     vertex:&(kemoView3DMetalBuf->cubeVertice)
                      index:&(kemoView3DMetalBuf->cubeIndex)
                     unites:monoViewUnites];
    
    [self drawLineObject:kemo_buffers->coast_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->coastVertice)
                  unites:monoViewUnites];
    [self drawLineObject:kemo_buffers->sph_grid_buf
                 encoder:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
                  vertex:&(kemoView3DMetalBuf->sphGridVertice)
                  unites:monoViewUnites];
    
    /*  Draw transparent objects */
    [self drawTexureWithPhong:kemo_buffers->PSF_ttxur_buf
                      encoder:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                       vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfTransTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES
                        solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->PSF_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                      vertex:&(kemoView3DMetalBuf->psfTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES
                       solid:FLAT_SHADE];
    
    [self drawSolidWithPhong:kemo_buffers->mesh_trns_buf
                     encoder:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
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
                        kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
{
    [self encodeSimpleObjects:renderEncoder
                    pipelines:&_kemoViewPipelines
                        depth:depthState
                  metalbuffer:&_kemoViewMetalBuf
                       buffer:kemo_sgl->kemo_buffers
                         mesh:kemo_sgl->kemo_mesh
                       unites:monoViewUnites];
    return;
};

- (void) encodeKemoView3DObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                           depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                        kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                          unites:(KemoViewUnites *_Nonnull) monoViewUnites
{
    [self encode3DObjects:renderEncoder
                pipelines:&_kemoViewPipelines
                    depth:depthState
              metalbuffer:&_kemoViewMetalBuf
                   buffer:kemo_sgl->kemo_buffers
                fieldline:kemo_sgl->kemo_fline
                     mesh:kemo_sgl->kemo_mesh
                   unites:monoViewUnites];
    return;
}

@end
