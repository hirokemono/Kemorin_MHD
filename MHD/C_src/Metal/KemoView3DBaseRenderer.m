/*
//  KemoView3DBaseRenderer.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import <Foundation/Foundation.h>


#import  "KemoView3DBaseRenderer.h"

#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation KemoView3DBaseRenderer

-(void) add3DShaderLibrary:(KemoViewMetalShaders *_Nullable) kemoViewShaders
                   library:(id<MTLLibrary> _Nullable *_Nullable) defaultLibrary
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

-(void) setKemo3DColorDescriptr:(MTLRenderPipelineDescriptor *) pipelineStateDescriptor
                           view:(nonnull MTKView *) mtkView
                    targetPixel:(MTLPixelFormat) pixelformat
{
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
}


-(void) addKemo3DPipelines:(nonnull MTKView *)mtkView
                   shaders:(KemoViewMetalShaders *_Nullable) kemoViewShaders
                 pipelines:(KemoView3DPipelines  *_Nullable) kemo3DPipelines
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
    [self setKemo3DColorDescriptr:pipelineStateDescriptor
                             view:mtkView
                      targetPixel:pixelformat];
    
    kemo3DPipelines->phongPipelineState
        = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor error:&error];
    NSAssert(kemo3DPipelines->phongPipelineState, @"Failed to create pipeline state: %@", error);
    
/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Phong Shader Pipeline with colormap construction";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->phongColorMapVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->phongColorMapFragmentFunction;
    [self setKemo3DColorDescriptr:pipelineStateDescriptor
                             view:mtkView
                      targetPixel:pixelformat];
    
    kemo3DPipelines->phongColorMapPipelineState
        = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor error:&error];
    NSAssert(kemo3DPipelines->phongColorMapPipelineState, @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
    pipelineStateDescriptor.label = @"Texure Shader Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoViewShaders->texuredPhongVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoViewShaders->texuredPhongFragmentFunction;
    
    [self setKemo3DColorDescriptr:pipelineStateDescriptor
                             view:mtkView
                      targetPixel:pixelformat];
    
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
    [self setKemo3DColorDescriptr:pipelineStateDescriptor
                             view:mtkView
                      targetPixel:pixelformat];
    
    kemo3DPipelines->texuredPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                   error:&error];
    NSAssert(kemo3DPipelines->texuredPipelineState, @"Failed to create pipeline state: %@", error);
}

- (void)drawSolidWithSimple:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                  pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                     unites:(KemoViewUnites *_Nullable) monoViewUnites
                      sides:(int) iflag_surface
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
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
};

- (void)encode3DPatchSettings:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                        depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                       unites:(KemoViewUnites *_Nullable) monoViewUnites
                        sides:(int) iflag_surface
{
    [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
    [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
    if(iflag_surface == NORMAL_POLYGON){
        [*renderEncoder setCullMode:MTLCullModeBack];
    }else if(iflag_surface == REVERSE_POLYGON){
        [*renderEncoder setCullMode:MTLCullModeFront];
    }else{
        [*renderEncoder setCullMode:MTLCullModeNone];
    }
    [*renderEncoder setDepthStencilState:*depthState];
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
    return;
}

- (void)drawTexureWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                  pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                      depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                     texure:(id<MTLTexture> _Nullable *_Nullable) texture
                     unites:(KemoViewUnites *_Nullable) monoViewUnites
                      sides:(int) iflag_surface
{
    if(numVertex > 0){
        [self encode3DPatchSettings:renderEncoder
                        depth:depthState
                       unites:monoViewUnites
                        sides:iflag_surface];
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongTexturedPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setFragmentTexture:*texture
                                   atIndex:AAPLTextureIndexBaseColor];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
};

- (void)drawSolidWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                 pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                     depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                 numVertex:(NSUInteger) numVertex
                    vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                    unites:(KemoViewUnites *_Nullable) monoViewUnites
                     sides:(int) iflag_surface
{
    if(numVertex > 0){
        [self encode3DPatchSettings:renderEncoder
                        depth:depthState
                       unites:monoViewUnites
                        sides:iflag_surface];
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
};

- (void)drawIndexPatchWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                      pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                          depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                      numVertex:(NSUInteger) numVertex
                         vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                           index:(id<MTLBuffer> _Nullable *_Nullable) indices
                         unites:(KemoViewUnites *_Nullable) monoViewUnites
                          sides:(int) iflag_surface
{
    if(numVertex > 0){
        [self encode3DPatchSettings:renderEncoder
                        depth:depthState
                       unites:monoViewUnites
                        sides:iflag_surface];

        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                   indexCount:numVertex
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:*indices
                            indexBufferOffset:0];
    };
};

- (void)drawIndexTexureWithPhong:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
                       pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                           depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
                       numVertex:(NSUInteger) numVertex
                          vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                           index:(id<MTLBuffer> _Nullable *_Nullable) indices
                          texure:(id<MTLTexture> _Nullable *_Nullable) texture
                          unites:(KemoViewUnites *_Nullable) monoViewUnites
                           sides:(int) iflag_surface
{
    if(numVertex > 0){
        [self encode3DPatchSettings:renderEncoder
                        depth:depthState
                       unites:monoViewUnites
                        sides:iflag_surface];
        
        [*renderEncoder setRenderPipelineState:kemo3DPipelines->phongTexturedPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setFragmentTexture:*texture
                                   atIndex:AAPLTextureIndexBaseColor];
        [*renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                   indexCount:numVertex
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:*indices
                            indexBufferOffset:0];
    };
};

- (void)drawLineObject:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
             pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                 depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
             numVertex:(NSUInteger) numVertex
                vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                unites:(KemoViewUnites *_Nullable) monoViewUnites
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

- (void)drawPointObject:(id<MTLRenderCommandEncoder> _Nullable *_Nullable) renderEncoder
              pipelines:(KemoView3DPipelines *_Nullable) kemo3DPipelines
                  depth:(id<MTLDepthStencilState> _Nullable *_Nullable) depthState
              numVertex:(NSUInteger) numVertex
                 vertex:(id<MTLBuffer> _Nullable *_Nullable) vertices
                 unites:(KemoViewUnites *_Nullable) monoViewUnites
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
        [*renderEncoder drawPrimitives:MTLPrimitiveTypePoint
                           vertexStart:0
                           vertexCount:numVertex];
    }
};
@end
