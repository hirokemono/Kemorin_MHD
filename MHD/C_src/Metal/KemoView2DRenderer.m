/*
//  KemoView2DRenderer.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import <Foundation/Foundation.h>


#import "KemoView2DRenderer.h"
#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation KemoView2DRenderer
{
    KemoView2DMetalShaders   _kemoView2DShaders;
    KemoView2DMetalPipelines _kemoView2DPipelines;
    KemoView2DMetalBuffers   _kemoView2DMetalBufs;
}

-(id) init
{
    _kemo2DMetalBufBase = [[KemoViewMetalBuffers alloc] init];
    return self;
}

- (void) setMapMBuffers:(id<MTLDevice> *) device
            metalbuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
            node_buffer:(struct gl_strided_buffer *) PSF_node_buf
            map_buffers:(struct MAP_buffers *) MAP_bufs
{
    kemoView2DMetalBufs->numMapNodeVertice =  [_kemo2DMetalBufBase setMetalVertexs:device
                                                                            buffer:PSF_node_buf
                                                                            vertex:&(kemoView2DMetalBufs->mapNodeVertice)];
    kemoView2DMetalBufs->numMapSolidIndices = [_kemo2DMetalBufBase setMetalIndices:device
                                                                          indexbuf:MAP_bufs->MAP_solid_index_buf
                                                                             index:&(kemoView2DMetalBufs->mapSolidIndices)];
    
    kemoView2DMetalBufs->numMapSolidVertice =  [_kemo2DMetalBufBase setMetalVertexs:device
                                                                             buffer:MAP_bufs->MAP_solid_buf
                                                                             vertex:&(kemoView2DMetalBufs->mapSolidVertice)];
    kemoView2DMetalBufs->numMapinesVertice =   [_kemo2DMetalBufBase setMetalVertexs:device
                                                                             buffer:MAP_bufs->MAP_isoline_buf
                                                                             vertex:&(kemoView2DMetalBufs->mapLinesVertice)];
    kemoView2DMetalBufs->numCoastLineVertice = [_kemo2DMetalBufBase setMetalVertexs:device
                                                                             buffer:MAP_bufs->MAP_coast_line_buf
                                                                             vertex:&(kemoView2DMetalBufs->coastLineVertice)];
    kemoView2DMetalBufs->numCoastTubeVertice = [_kemo2DMetalBufBase setMetalVertexs:device
                                                                             buffer:MAP_bufs->MAP_coast_tube_buf
                                                                             vertex:&(kemoView2DMetalBufs->coastTubeVertice)];
};

- (void) releaseMapMBuffers:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
{
    if(kemoView2DMetalBufs->numMapNodeVertice > 0)  {[kemoView2DMetalBufs->mapNodeVertice    release];};
    if(kemoView2DMetalBufs->numMapSolidIndices > 0)  {[kemoView2DMetalBufs->mapSolidIndices  release];};

    if(kemoView2DMetalBufs->numMapSolidVertice > 0)  {[kemoView2DMetalBufs->mapSolidVertice  release];};
    if(kemoView2DMetalBufs->numMapinesVertice > 0)   {[kemoView2DMetalBufs->mapLinesVertice  release];};
    if(kemoView2DMetalBufs->numCoastLineVertice > 0) {[kemoView2DMetalBufs->coastLineVertice release];};
    if(kemoView2DMetalBufs->numCoastTubeVertice > 0) {[kemoView2DMetalBufs->coastTubeVertice release];};
    return;
}

- (void) setMsgMBuffers:(id<MTLDevice> *) device
            metalbuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                buffers:(struct MESSAGE_buffers *) MESSAGE_bufs
{
    kemoView2DMetalBufs->numColorBarVertice =    [_kemo2DMetalBufBase setMetalVertexs:device
                                                                               buffer:MESSAGE_bufs->cbar_buf
                                                                               vertex:&(kemoView2DMetalBufs->colorBarVertice)];
    kemoView2DMetalBufs->numMinLabelVertice =  [_kemo2DMetalBufBase setTextBoxTexture:device
                                                                               buffer:MESSAGE_bufs->cbar_min_buf
                                                                               vertex:&(kemoView2DMetalBufs->minLabelVertice)
                                                                               texure:&(kemoView2DMetalBufs->minLabelTexure)];
    kemoView2DMetalBufs->numMaxLabelVertice =  [_kemo2DMetalBufBase setTextBoxTexture:device
                                                                               buffer:MESSAGE_bufs->cbar_max_buf
                                                                               vertex:&(kemoView2DMetalBufs->maxLabelVertice)
                                                                               texure:&(kemoView2DMetalBufs->maxLabelTexure)];
    kemoView2DMetalBufs->numZeroLabelVertice = [_kemo2DMetalBufBase setTextBoxTexture:device
                                                                               buffer:MESSAGE_bufs->cbar_zero_buf
                                                                               vertex:&(kemoView2DMetalBufs->zeroLabelVertice)
                                                                               texure:&(kemoView2DMetalBufs->zeroLabelTexure)];
    
    kemoView2DMetalBufs->numtimeLabelVertice = [_kemo2DMetalBufBase setTextBoxTexture:device
                                                                               buffer:MESSAGE_bufs->timelabel_buf
                                                                               vertex:&(kemoView2DMetalBufs->timeLabelVertice)
                                                                               texure:&(kemoView2DMetalBufs->timeLabelTexure)];
    
    kemoView2DMetalBufs->numMessageVertice =   [_kemo2DMetalBufBase setTextBoxTexture:device
                                                                               buffer:MESSAGE_bufs->message_buf
                                                                               vertex:&(kemoView2DMetalBufs->messageVertice)
                                                                               texure:&(kemoView2DMetalBufs->messageTexure)];
    return;
}

- (void) releaseMsgMBuffers:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
{
    if(kemoView2DMetalBufs->numColorBarVertice > 0) {[kemoView2DMetalBufs->colorBarVertice release];};
    if(kemoView2DMetalBufs->numMinLabelVertice > 0){
        [kemoView2DMetalBufs->minLabelVertice release];
        [kemoView2DMetalBufs->minLabelTexure  release];
    };
    if(kemoView2DMetalBufs->numMaxLabelVertice > 0){
        [kemoView2DMetalBufs->maxLabelVertice release];
        [kemoView2DMetalBufs->maxLabelTexure  release];
    };
    if(kemoView2DMetalBufs->numZeroLabelVertice > 0){
        [kemoView2DMetalBufs->zeroLabelVertice release];
        [kemoView2DMetalBufs->zeroLabelTexure  release];
    };
    if(kemoView2DMetalBufs->numtimeLabelVertice > 0){
        [kemoView2DMetalBufs->timeLabelVertice release];
        [kemoView2DMetalBufs->timeLabelTexure  release];
    };
    if(kemoView2DMetalBufs->numMessageVertice > 0){
        [kemoView2DMetalBufs->messageVertice release];
        [kemoView2DMetalBufs->messageTexure  release];
    };
    return;
}

-(void) set2DShaderLibrary:(KemoView2DMetalShaders *) kemoView2DShaders
                   library:(id<MTLLibrary>  *) shaderLibrary
{
    // Load all the shader files with a .metal file extension in the project.
    kemoView2DShaders->base2DVertexFunction =   [*shaderLibrary newFunctionWithName:@"Base2dVertexShader"];
    kemoView2DShaders->base2DFragmentFunction = [*shaderLibrary newFunctionWithName:@"Base2DfragmentShader"];
    
    kemoView2DShaders->simple2DVertexFunction =   [*shaderLibrary newFunctionWithName:@"Simple2dVertexShader"];
    kemoView2DShaders->simple2DFragmentFunction = [*shaderLibrary newFunctionWithName:@"Simple2DfragmentShader"];
    
    kemoView2DShaders->texured2DVertexFunction =   [*shaderLibrary newFunctionWithName:@"Texture2dVertexShader"];
    kemoView2DShaders->texured2DFragmentFunction = [*shaderLibrary newFunctionWithName:@"sampling2dShader"];
    
    kemoView2DShaders->AnaglyphVertexFunction =   [*shaderLibrary newFunctionWithName:@"AnaglyphVertexShader"];
    kemoView2DShaders->AnaglyphFragmentFunction = [*shaderLibrary newFunctionWithName:@"AnaglyphFragmentShader"];
}


-(void) setKemoView2DPipelines:(nonnull MTKView *)mtkView
                       shaders:(KemoView2DMetalShaders *) kemoView2DShaders
                     pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
                   targetPixel:(MTLPixelFormat) pixelformat
{
    NSError *error;
    id<MTLDevice> device = mtkView.device;
    
    MTLRenderPipelineDescriptor *pipelineStateDescriptor;
    pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];
    
    pipelineStateDescriptor.label = @"2D Texture Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoView2DShaders->texured2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoView2DShaders->texured2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;

    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemoView2DPipelines->texured2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                         error:&error];
    /* Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
     //  If the Metal API validation is enabled, you can find out more information about what
     //  went wrong.  (Metal API validation is enabled by default when a debug build is run
     //  from Xcode.) */
    NSAssert(kemoView2DPipelines->texured2DPipelineState, @"Failed to create pipeline state: %@", error);
    
    /*  Create pipeline for simple 2D rendering */
    [pipelineStateDescriptor init];
    pipelineStateDescriptor.label = @"2D transpearent Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoView2DShaders->simple2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoView2DShaders->simple2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
    pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
    pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    
    kemoView2DPipelines->trans2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                       error:&error];
    NSAssert(kemoView2DPipelines->trans2DPipelineState, @"Failed to create pipeline state: %@", error);
    
    /*  Create pipeline for simple rendering */
    [pipelineStateDescriptor init];
    pipelineStateDescriptor.label = @"2D Simple Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoView2DShaders->simple2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoView2DShaders->simple2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    pipelineStateDescriptor.colorAttachments[0].blendingEnabled = NO;
    
    kemoView2DPipelines->simple2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                        error:&error];
    NSAssert(kemoView2DPipelines->simple2DPipelineState, @"Failed to create pipeline state: %@", error);
    
    
    /*  Create pipeline for Basic rendering */
    pipelineStateDescriptor.label = @"Base Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoView2DShaders->base2DVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoView2DShaders->base2DFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    kemoView2DPipelines->base2DPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                      error:&error];
    NSAssert(kemoView2DPipelines->base2DPipelineState, @"Failed to create pipeline state: %@", error);
    
    
    [pipelineStateDescriptor init];
    pipelineStateDescriptor.label = @"Anaglyph Pipeline";
    pipelineStateDescriptor.vertexFunction =   kemoView2DShaders->AnaglyphVertexFunction;
    pipelineStateDescriptor.fragmentFunction = kemoView2DShaders->AnaglyphFragmentFunction;
    pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
    pipelineStateDescriptor.colorAttachments[0].pixelFormat = pixelformat;
    
    kemoView2DPipelines->anaglyphPipelineState = [device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                                                                         error:&error];
    /* Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
     //  If the Metal API validation is enabled, you can find out more information about what
     //  went wrong.  (Metal API validation is enabled by default when a debug build is run
     //  from Xcode.) */
    NSAssert(kemoView2DPipelines->anaglyphPipelineState, @"Failed to create pipeline state: %@", error);

    return;
}

- (void)draw2DLineObject:(id<MTLRenderCommandEncoder> *) renderEncoder
               pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
               numVertex:(NSUInteger) numVertex
                  vertex:(id<MTLBuffer> *) vertices
              projection:(matrix_float4x4 *) projection_mat;
{
    if(numVertex > 0){
        [*renderEncoder setRenderPipelineState:kemoView2DPipelines->simple2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                           vertexStart:0
                           vertexCount:numVertex];
    };
    
}


- (void)draw2DElementObject:(id<MTLRenderCommandEncoder> *) renderEncoder
                  pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
                  numVertex:(NSUInteger) numVertex
                     vertex:(id<MTLBuffer> *) vertices
                      index:(id<MTLBuffer> *) indices
                 projection:(matrix_float4x4 *) projection_mat
{
    if(numVertex > 0){
        [*renderEncoder setRenderPipelineState: kemoView2DPipelines->trans2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                   indexCount:numVertex
                                    indexType:MTLIndexTypeUInt32
                                  indexBuffer:*indices
                            indexBufferOffset:0];
    };
    
}

- (void)draw2DPatchObject:(id<MTLRenderCommandEncoder> *) renderEncoder
                pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
                numVertex:(NSUInteger) numVertex
                   vertex:(id<MTLBuffer> *) vertices
               projection:(matrix_float4x4 *) projection_mat
{
    if(numVertex > 0){
        [*renderEncoder setRenderPipelineState: kemoView2DPipelines->trans2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
    
}

- (void)drawTextBoxObject:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull) renderEncoder
                pipelines:(KemoView2DMetalPipelines *_Nonnull) kemoView2DPipelines
                numVertex:(NSUInteger) numVertex
                   vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                   texure:(id<MTLTexture> _Nonnull *_Nonnull) texture
               projection:(matrix_float4x4 *_Nonnull) projection_mat;
{
    if(numVertex > 0){
        [*renderEncoder setRenderPipelineState:kemoView2DPipelines->texured2DPipelineState];
        /* Pass in the parameter data. */
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        
        /* Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
         ///  to the 'colorMap' argument in the 'samplingShader' function because its
         //   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index. */
        [*renderEncoder setFragmentTexture:*texture
                                   atIndex:AAPLTextureIndexBaseColor];
        /* Draw the triangles. */
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
}

- (void)drawAnaglyphObject:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull) renderEncoder
                 pipelines:(KemoView2DMetalPipelines *_Nonnull) kemoView2DPipelines
                 numVertex:(NSUInteger) numVertex
                    vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                      left:(id<MTLTexture> _Nonnull *_Nonnull) leftTexure
                     right:(id<MTLTexture> _Nonnull *_Nonnull) rightTexure
                projection:(matrix_float4x4 *_Nonnull) projection_mat;
{
    if(numVertex > 0){
        [*renderEncoder setRenderPipelineState:kemoView2DPipelines->anaglyphPipelineState];
        /* Pass in the parameter data. */
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        
        /* Set the texture objects.  The AAPLTextureIndexBaseColor enum value corresponds
         ///  to the 'colorMap' argument in the 'samplingShader' function because these
         //   textures attribute qualifier also uses AAPLTextureIndexBaseColor for its index. */
        [*renderEncoder setFragmentTexture:*leftTexure
                                   atIndex:AAPLTextureIndexBaseColor];
        [*renderEncoder setFragmentTexture:*rightTexure
                                   atIndex:AAPLTextureIndexRight];
        /* Draw the triangles. */
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:numVertex];
    };
}


- (void) setMapObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
             pipelines:(KemoView2DMetalPipelines * _Nonnull) kemoView2DPipelines
           metalBuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
            projection:(matrix_float4x4 * _Nonnull) map_proj_mat
{
    /*  Commands to render map projection */
    [self draw2DElementObject:renderEncoder
                    pipelines:kemoView2DPipelines
                    numVertex:kemoView2DMetalBufs->numMapSolidIndices
                       vertex:&(kemoView2DMetalBufs->mapNodeVertice)
                       index:&(kemoView2DMetalBufs->mapSolidIndices)
                   projection:map_proj_mat];

    /*  Commands to render map projection */
    [self draw2DPatchObject:renderEncoder
                  pipelines:kemoView2DPipelines
                  numVertex:kemoView2DMetalBufs->numMapSolidVertice
                     vertex:&(kemoView2DMetalBufs->mapSolidVertice)
                 projection:map_proj_mat];

     /*  Commands to render isolines on map */
    [self draw2DPatchObject:renderEncoder
                  pipelines:kemoView2DPipelines
                  numVertex:kemoView2DMetalBufs->numMapinesVertice
                     vertex:&(kemoView2DMetalBufs->mapLinesVertice)
                 projection:map_proj_mat];
    /*  Commands to render Coastline on map */
    [self draw2DPatchObject:renderEncoder
                 pipelines:kemoView2DPipelines
                 numVertex:kemoView2DMetalBufs->numCoastTubeVertice
                    vertex:&(kemoView2DMetalBufs->coastTubeVertice)
                projection:map_proj_mat];

    [self draw2DLineObject:renderEncoder
                 pipelines:kemoView2DPipelines
                 numVertex:kemoView2DMetalBufs->numCoastLineVertice
                    vertex:&(kemoView2DMetalBufs->coastLineVertice)
                projection:map_proj_mat];
    return;
}

- (void) setMessageObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                 pipelines:(KemoView2DMetalPipelines * _Nonnull) kemo2DPipelines
               metalBuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                projection:(matrix_float4x4 * _Nonnull) projection_mat
{
/*  Commands to render colorbar  box */
    [self draw2DPatchObject:renderEncoder
                  pipelines:kemo2DPipelines
                  numVertex:kemoView2DMetalBufs->numColorBarVertice
                     vertex:&(kemoView2DMetalBufs->colorBarVertice)
                 projection:projection_mat];
/*  Commands to render colorbar  label */
    [self drawTextBoxObject:renderEncoder
                  pipelines:kemo2DPipelines
                  numVertex:kemoView2DMetalBufs->numMinLabelVertice
                     vertex:&(kemoView2DMetalBufs->minLabelVertice)
                     texure:&(kemoView2DMetalBufs->minLabelTexure)
                 projection:projection_mat];
    [self drawTextBoxObject:renderEncoder
                  pipelines:kemo2DPipelines
                  numVertex:kemoView2DMetalBufs->numMaxLabelVertice
                     vertex:&(kemoView2DMetalBufs->maxLabelVertice)
                     texure:&(kemoView2DMetalBufs->maxLabelTexure)
                 projection:projection_mat];
    [self drawTextBoxObject:renderEncoder
                  pipelines:kemo2DPipelines
                  numVertex:kemoView2DMetalBufs->numZeroLabelVertice
                     vertex:&(kemoView2DMetalBufs->zeroLabelVertice)
                     texure:&(kemoView2DMetalBufs->zeroLabelTexure)
                 projection:projection_mat];

/*  Commands to render time label */
    [self drawTextBoxObject:renderEncoder
                  pipelines:kemo2DPipelines
                  numVertex:kemoView2DMetalBufs->numtimeLabelVertice
                     vertex:&(kemoView2DMetalBufs->timeLabelVertice)
                     texure:&(kemoView2DMetalBufs->timeLabelTexure)
                 projection:projection_mat];
/*  Commands to render colorbar  box */
    [self drawTextBoxObject:renderEncoder
                  pipelines:kemo2DPipelines
                  numVertex:kemoView2DMetalBufs->numMessageVertice
                     vertex:&(kemoView2DMetalBufs->messageVertice)
                     texure:&(kemoView2DMetalBufs->messageTexure)
                 projection:projection_mat];
    return;
}

- (void) releaseMapMetalBuffers
{
    [self releaseMapMBuffers:&_kemoView2DMetalBufs];
    return;
}
- (void) releaseMsgMetalBuffers
{
    [self releaseMsgMBuffers:&_kemoView2DMetalBufs];
    return;
}

- (void) setMapMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
                    buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self setMapMBuffers:device
             metalbuffer:&_kemoView2DMetalBufs
             node_buffer:kemo_buffers->PSF_node_buf
             map_buffers:kemo_buffers->MAP_bufs];
}
- (void) setMessageMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
                        buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self setMsgMBuffers:device
             metalbuffer:&_kemoView2DMetalBufs
                 buffers:kemo_buffers->MESSAGE_bufs];
    return;
}


-(void) add2DShaderLibrary:(id<MTLLibrary> _Nonnull * _Nonnull) shaderLibrary
{
    [self set2DShaderLibrary:&_kemoView2DShaders
                     library:shaderLibrary];
}
-(void) addKemoView2DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat
{
    [self setKemoView2DPipelines:mtkView
                         shaders:&_kemoView2DShaders
                       pipelines:&_kemoView2DPipelines
                     targetPixel:pixelformat];
}

- (void) encodeMapObjects:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull) renderEncoder
               projection:(matrix_float4x4 * _Nonnull) map_proj_mat
{
    [self setMapObjects:renderEncoder
              pipelines:&_kemoView2DPipelines
            metalBuffer:&_kemoView2DMetalBufs
              projection:map_proj_mat];
}
- (void) encodeMessageObjects:(id<MTLRenderCommandEncoder> _Nonnull * _Nonnull) renderEncoder
                   projection:(matrix_float4x4 * _Nonnull) projection_mat
{
    [self setMessageObjects:renderEncoder
               pipelines:&_kemoView2DPipelines
             metalBuffer:&_kemoView2DMetalBufs
              projection:projection_mat];
}

- (void) encodeAnaglyphObjects:(id<MTLRenderCommandEncoder> _Nonnull * _Nonnull) renderEncoder
                     numVertex:(NSUInteger) numVertex
                        vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  anaglyphVertex
                          left:(id<MTLTexture> _Nonnull *_Nonnull) leftTexure
                         right:(id<MTLTexture> _Nonnull *_Nonnull) rightTexure
                    projection:(matrix_float4x4 * _Nonnull) projection_mat
{
    [self drawAnaglyphObject:renderEncoder
                   pipelines:&_kemoView2DPipelines
                   numVertex:numVertex
                      vertex:anaglyphVertex
                        left:leftTexure
                       right:rightTexure
                  projection:projection_mat];
}

@end

