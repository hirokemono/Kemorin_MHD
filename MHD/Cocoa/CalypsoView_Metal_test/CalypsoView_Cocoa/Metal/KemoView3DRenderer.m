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


- (void) set3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
               metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                  kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                   buffers:(struct kemoview_buffers *_Nonnull) kemo_buffers
                      PSFs:(struct kemoview_mul_psf *_Nonnull) kemo_psf
{
    /*  Set Cube Vertex buffer */
    kemoView3DMetalBuf->numCubeVertice = [_kemo3DMetalBufBase setCubeVertexs:device
                                                                      buffer:kemo_buffers->cube_buf
                                                                    indexbuf:kemo_buffers->cube_index_buf
                                                                      vertex:&(kemoView3DMetalBuf->cubeVertice)
                                                                       index:&(kemoView3DMetalBuf->cubeIndex)];
    return;
}

- (void) release3DMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    /*  Set Cube Vertex buffer */
    if(kemoView3DMetalBuf->numCubeVertice > 0){
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
                   kemoview:kemo_sgl
                    buffers:kemo_sgl->kemo_buffers
                       PSFs:kemo_sgl->kemo_psf];
    return;
}


- (void) releaseKemoView3DMetalBuffers
{
    [self release3DMetalBuffers:&_kemoViewMetalBuf];
    return;
};

-(void) add3DShaderLibrary:(KemoViewMetalShaders *) kemoViewShaders
                   library:(id<MTLLibrary> *) defaultLibrary
{
    // Load all the shader files with a .metal file extension in the project.
    kemoViewShaders->phongVertexFunction =   [*defaultLibrary newFunctionWithName:@"PhongVertexShader"];
    kemoViewShaders->phongFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongFragmentShader"];

    kemoViewShaders->phongColorMapVertexFunction =   [*defaultLibrary newFunctionWithName:@"PhongColorMapVertexShader"];
    kemoViewShaders->phongColorMapFragmentFunction = [*defaultLibrary newFunctionWithName:@"PhongColorMapFragmentShader"];
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
    return;
}


KemoViewNormalize CubeColor;

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
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongColorMapPipelineState];
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
        
        CubeColor.id_cmap[0] = RED_BLUE_MODE;
        
        CubeColor.num_normalize[0] = 3;
        CubeColor.data_reference[0] =  0.0;
        CubeColor.data_normalized[0] = 0.0;
        CubeColor.data_reference[1] =  3.5;
        CubeColor.data_normalized[1] = 0.5;
        CubeColor.data_reference[2] =  7.0;
        CubeColor.data_normalized[2] = 1.0;
        
        CubeColor.num_opacity[0] = 4;
        CubeColor.alpha_reference[0] = -1.0;
        CubeColor.alpha_output[0] =     0.4;
        CubeColor.alpha_reference[1] =  2.0;
        CubeColor.alpha_output[1] =     0.1;
        CubeColor.alpha_reference[2] =  3.0;
        CubeColor.alpha_output[2] =     0.05;
        CubeColor.alpha_reference[3] =  0.0;
        CubeColor.alpha_output[3] =     4.0;

        [*renderEncoder setVertexBytes:&(CubeColor)
                                length:sizeof(KemoViewNormalize)
                               atIndex:AAPLColormapTable];

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

- (void) encodeSimpleObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                   pipelines:(KemoView3DPipelines *) kemo3DPipelines
                       depth:(id<MTLDepthStencilState> *) depthState
                 metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                      unites:(KemoViewUnites *) monoViewUnites
                       sides:(int) iflag_polygon
{
    [self drawCubeWithPhong:renderEncoder
                  pipelines:kemo3DPipelines
                      depth:depthState
                  numVertex:kemoView3DMetalBuf->numCubeVertice
                     vertex:&(kemoView3DMetalBuf->cubeVertice)
                      index:&(kemoView3DMetalBuf->cubeIndex)
                     unites:monoViewUnites];
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
@end
