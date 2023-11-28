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

- (void)setMetalVertexs:(id<MTLDevice> *) device
                 buffer:(struct gl_strided_buffer * _Nonnull) buf
                 vertex:(id<MTLBuffer> *)  vertices
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytesNoCopy:buf->v_buf
                                               length:(buf->nsize_buf * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    };
};

- (void)setTextBoxTexture:(id<MTLDevice> *) device
                   buffer:(struct gl_strided_buffer *) buf
                    image:(struct line_text_image *) img
                   vertex:(id<MTLBuffer> *)  vertices
                   texure:(id<MTLTexture> *) texture
{
    if(buf->num_nod_buf > 0){
        *vertices = [*device newBufferWithBytes:((KemoViewVertex *) buf->v_buf)
                                         length:(buf->num_nod_buf * sizeof(KemoViewVertex))
                                        options:MTLResourceStorageModeShared];
        
/* Construct message texture */
        MTLTextureDescriptor *lineTextureDescriptor = [[MTLTextureDescriptor alloc] init];
        lineTextureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
        lineTextureDescriptor.width =  img->npix_img[0];
        lineTextureDescriptor.height = img->npix_img[1];

/*  Calculate the number of bytes per row in the image. */
        NSUInteger bytesPerRow = 4 * lineTextureDescriptor.width;
        MTLRegion region = {
            { 0, 0, 0 },                   // MTLOrigin
            {lineTextureDescriptor.width, lineTextureDescriptor.height, 1} // MTLSize
        };
        
/* Create the texture from the device by using the descriptor */
        *texture = [*device newTextureWithDescriptor:lineTextureDescriptor];
/* Copy the bytes from the data object into the texture */
        [*texture replaceRegion:region
                    mipmapLevel:0
                      withBytes:img->imgBMP
                    bytesPerRow:bytesPerRow];
    };

}

- (void) setMapMBuffers:(id<MTLDevice> *) device
            metalbuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                buffers:(struct kemoview_buffers *) kemo_buffers
{
        [self setMetalVertexs:device
                       buffer:kemo_buffers->MAP_solid_buf
                       vertex:&(kemoView2DMetalBufs->mapSolidVertice)];
        [self setMetalVertexs:device
                       buffer:kemo_buffers->MAP_isoline_buf
                       vertex:&(kemoView2DMetalBufs->mapLinesVertice)];
        [self setMetalVertexs:device
                       buffer:kemo_buffers->coast_buf
                       vertex:&(kemoView2DMetalBufs->coastVertice)];
        [self setMetalVertexs:device
                       buffer:kemo_buffers->sph_grid_buf
                       vertex:&(kemoView2DMetalBufs->sphGridVertice)];
};
- (void) releaseMapMBuffers:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                    buffers:(struct kemoview_buffers *) kemo_buffers
{
    if(kemo_buffers->MAP_solid_buf->num_nod_buf > 0)   {[kemoView2DMetalBufs->mapSolidVertice release];};
    if(kemo_buffers->MAP_isoline_buf->num_nod_buf > 0) {[kemoView2DMetalBufs->mapLinesVertice release];};
    if(kemo_buffers->coast_buf->num_nod_buf > 0)    {[kemoView2DMetalBufs->coastVertice   release];};
    if(kemo_buffers->sph_grid_buf->num_nod_buf > 0) {[kemoView2DMetalBufs->sphGridVertice release];};
    return;
}

- (void) setMsgMBuffers:(id<MTLDevice> *) device
            metalbuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                buffers:(struct kemoview_buffers *) kemo_buffers
{
    [self setMetalVertexs:device
                   buffer:kemo_buffers->cbar_buf
                   vertex:&(kemoView2DMetalBufs->colorBarVertice)];
    [self setTextBoxTexture:device
                     buffer:kemo_buffers->min_buf
                      image:kemo_buffers->cbar_min_image
                     vertex:&(kemoView2DMetalBufs->minLabelVertice)
                     texure:&(kemoView2DMetalBufs->minLabelTexure)];
    [self setTextBoxTexture:device
                     buffer:kemo_buffers->max_buf
                      image:kemo_buffers->cbar_max_image
                     vertex:&(kemoView2DMetalBufs->maxLabelVertice)
                     texure:&(kemoView2DMetalBufs->maxLabelTexure)];
    [self setTextBoxTexture:device
                     buffer:kemo_buffers->zero_buf
                      image:kemo_buffers->cbar_zero_image
                     vertex:&(kemoView2DMetalBufs->zeroLabelVertice)
                     texure:&(kemoView2DMetalBufs->zeroLabelTexure)];
    [self setTextBoxTexture:device
                     buffer:kemo_buffers->time_buf
                      image:kemo_buffers->tlabel_image
                     vertex:&(kemoView2DMetalBufs->timeLabelVertice)
                     texure:&(kemoView2DMetalBufs->timeLabelTexure)];
    [self setTextBoxTexture:device
                     buffer:kemo_buffers->msg_buf
                      image:kemo_buffers->message_image
                     vertex:&(kemoView2DMetalBufs->messageVertice)
                     texure:&(kemoView2DMetalBufs->messageTexure)];
    return;
}
- (void) releaseMsgMBuffers:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                    buffers:(struct kemoview_buffers *) kemo_buffers
{
    if(kemo_buffers->cbar_buf->num_nod_buf > 0) {[kemoView2DMetalBufs->colorBarVertice release];};
    if(kemo_buffers->min_buf->num_nod_buf > 0){
        [kemoView2DMetalBufs->minLabelVertice release];
        [kemoView2DMetalBufs->minLabelTexure  release];
    };
    if(kemo_buffers->max_buf->num_nod_buf > 0){
        [kemoView2DMetalBufs->maxLabelVertice release];
        [kemoView2DMetalBufs->maxLabelTexure  release];
    };
    if(kemo_buffers->zero_buf->num_nod_buf > 0){
        [kemoView2DMetalBufs->zeroLabelVertice release];
        [kemoView2DMetalBufs->zeroLabelTexure  release];
    };
    if(kemo_buffers->time_buf->num_nod_buf > 0){
        [kemoView2DMetalBufs->timeLabelVertice release];
        [kemoView2DMetalBufs->timeLabelTexure  release];
    };
    if(kemo_buffers->msg_buf->num_nod_buf > 0){
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
    return;
}

- (void)draw2DLineObject:(struct gl_strided_buffer *) buf
                 encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
               pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
                  vertex:(id<MTLBuffer> *) vertices
              projection:(matrix_float4x4 *) projection_mat;
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setRenderPipelineState:kemoView2DPipelines->simple2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
    
}


- (void)draw2DPatchObject:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
                   vertex:(id<MTLBuffer> *) vertices
               projection:(matrix_float4x4 *) projection_mat
{
    if(buf->num_nod_buf > 0){
        [*renderEncoder setRenderPipelineState: kemoView2DPipelines->trans2DPipelineState];
        [*renderEncoder setVertexBuffer:*vertices
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLOrthogonalMatrix];
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
    
}

- (void)drawTextBoxObject:(struct gl_strided_buffer *) buf
                  encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                pipelines:(KemoView2DMetalPipelines *) kemoView2DPipelines
                   vertex:(id<MTLBuffer> *)  vertices
                   texure:(id<MTLTexture> *) texture
               projection:(matrix_float4x4 *) projection_mat
{
    if(buf->num_nod_buf > 0){
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
                           vertexCount:buf->num_nod_buf];
    };
}

- (void) setMapObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
             pipelines:(KemoView2DMetalPipelines * _Nonnull) kemoView2DPipelines
           metalBuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                buffer:(struct kemoview_buffers *) kemo_buffers
            projection:(matrix_float4x4 * _Nonnull) map_proj_mat
{
    /*  Commands to render map projection */
    [self draw2DPatchObject:kemo_buffers->MAP_solid_buf
                    encoder:renderEncoder
                  pipelines:kemoView2DPipelines
                     vertex:&(kemoView2DMetalBufs->mapSolidVertice)
                 projection:map_proj_mat];
    /*  Commands to render isolines on map */
    [self draw2DPatchObject:kemo_buffers->MAP_isoline_buf
                    encoder:renderEncoder
                  pipelines:kemoView2DPipelines
                     vertex:&(kemoView2DMetalBufs->mapLinesVertice)
                 projection:map_proj_mat];
    /*  Commands to render Coastline on map */
    [self draw2DLineObject:kemo_buffers->coast_buf
                   encoder:renderEncoder
                 pipelines:kemoView2DPipelines
                    vertex:&(kemoView2DMetalBufs->coastVertice)
                projection:map_proj_mat];
    /*  Commands to render grids on map */
    [self draw2DLineObject:kemo_buffers->sph_grid_buf
                   encoder:renderEncoder
                 pipelines:kemoView2DPipelines
                    vertex:&(kemoView2DMetalBufs->sphGridVertice)
                projection:map_proj_mat];
    return;
}

- (void) setMessageObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                 pipelines:(KemoView2DMetalPipelines * _Nonnull) kemo2DPipelines
               metalBuffer:(KemoView2DMetalBuffers *) kemoView2DMetalBufs
                    buffer:(struct kemoview_buffers * _Nonnull) kemo_buffers
                projection:(matrix_float4x4 * _Nonnull) projection_mat
{
/*  Commands to render colorbar  box */
    [self draw2DPatchObject:kemo_buffers->cbar_buf
                     encoder:renderEncoder
                   pipelines:kemo2DPipelines
                      vertex:&(kemoView2DMetalBufs->colorBarVertice)
                  projection:projection_mat];
/*  Commands to render colorbar  label */
    [self drawTextBoxObject:kemo_buffers->min_buf
                    encoder:renderEncoder
                  pipelines:kemo2DPipelines
                     vertex:&(kemoView2DMetalBufs->minLabelVertice)
                     texure:&(kemoView2DMetalBufs->minLabelTexure)
                 projection:projection_mat];
    [self drawTextBoxObject:kemo_buffers->max_buf
                    encoder:renderEncoder
                  pipelines:kemo2DPipelines
                     vertex:&(kemoView2DMetalBufs->maxLabelVertice)
                     texure:&(kemoView2DMetalBufs->maxLabelTexure)
                 projection:projection_mat];
    [self drawTextBoxObject:kemo_buffers->zero_buf
                    encoder:renderEncoder
                  pipelines:kemo2DPipelines
                     vertex:&(kemoView2DMetalBufs->zeroLabelVertice)
                     texure:&(kemoView2DMetalBufs->zeroLabelTexure)
                 projection:projection_mat];

/*  Commands to render time label */
    [self drawTextBoxObject:kemo_buffers->time_buf
                    encoder:renderEncoder
                  pipelines:kemo2DPipelines
                     vertex:&(kemoView2DMetalBufs->timeLabelVertice)
                     texure:&(kemoView2DMetalBufs->timeLabelTexure)
                 projection:projection_mat];
/*  Commands to render colorbar  box */
    [self drawTextBoxObject:kemo_buffers->msg_buf
                    encoder:renderEncoder
                  pipelines:kemo2DPipelines
                     vertex:&(kemoView2DMetalBufs->messageVertice)
                     texure:&(kemoView2DMetalBufs->messageTexure)
                 projection:projection_mat];
    return;
}

- (void) releaseMapMetalBuffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self releaseMapMBuffers:&_kemoView2DMetalBufs
                         buffers:kemo_buffers];
    return;
}
- (void) releaseMsgMetalBuffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self releaseMsgMBuffers:&_kemoView2DMetalBufs
                     buffers:kemo_buffers];
    return;
}

- (void) setMapMetalBuffers:(id<MTLDevice> *) device
                    buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self setMapMBuffers:device
             metalbuffer:&_kemoView2DMetalBufs
                 buffers:kemo_buffers];
}
- (void) setMessageMetalBuffers:(id<MTLDevice> *) device
                        buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self setMsgMBuffers:device
             metalbuffer:&_kemoView2DMetalBufs
                 buffers:kemo_buffers];
    return;
}


-(void) add2DShaderLibrary:(id<MTLLibrary>  *) shaderLibrary
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
- (void) encodeMapObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
               projection:(matrix_float4x4 * _Nonnull) map_proj_mat
{
    [self setMapObjects:renderEncoder
              pipelines:&_kemoView2DPipelines
            metalBuffer:&_kemoView2DMetalBufs
                 buffer:kemo_buffers
              projection:map_proj_mat];
}
- (void) encodeMessageObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                      buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
                   projection:(matrix_float4x4 * _Nonnull) projection_mat
{
    [self setMessageObjects:renderEncoder
               pipelines:&_kemoView2DPipelines
             metalBuffer:&_kemoView2DMetalBufs
                  buffer:kemo_buffers
              projection:projection_mat];
}

@end

