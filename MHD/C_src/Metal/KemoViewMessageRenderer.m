/*
//  KemoViewMessageRenderer.m
//  
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import <Foundation/Foundation.h>


#import "KemoViewMessageRenderer.h"
#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation KemoViewMessageRenderer

- (void) setMessageMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
                baseMetalBuffer:(KemoViewMetalBuffers *_Nonnull) kemo2DMetalBufBase
                    metalBuffer:(KemoViewMessageMetalBuffers *_Nonnull) kemoViewMessageMetalBufs
                        buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    kemoViewMessageMetalBufs->numColorBarVertice =    [kemo2DMetalBufBase  setMetalVertexs:device
                                                                                    buffer:kemo_buffers->MESSAGE_bufs->cbar_buf
                                                                                    vertex:&(kemoViewMessageMetalBufs->colorBarVertice)];
    kemoViewMessageMetalBufs->numMinLabelVertice =  [kemo2DMetalBufBase  setTextBoxTexture:device
                                                                                    buffer:kemo_buffers->MESSAGE_bufs->cbar_min_buf
                                                                                    vertex:&(kemoViewMessageMetalBufs->minLabelVertice)
                                                                                    texure:&(kemoViewMessageMetalBufs->minLabelTexure)];
    kemoViewMessageMetalBufs->numMaxLabelVertice =  [kemo2DMetalBufBase  setTextBoxTexture:device
                                                                                    buffer:kemo_buffers->MESSAGE_bufs->cbar_max_buf
                                                                                    vertex:&(kemoViewMessageMetalBufs->maxLabelVertice)
                                                                                    texure:&(kemoViewMessageMetalBufs->maxLabelTexure)];
    kemoViewMessageMetalBufs->numZeroLabelVertice = [kemo2DMetalBufBase  setTextBoxTexture:device
                                                                                    buffer:kemo_buffers->MESSAGE_bufs->cbar_zero_buf
                                                                                    vertex:&(kemoViewMessageMetalBufs->zeroLabelVertice)
                                                                                    texure:&(kemoViewMessageMetalBufs->zeroLabelTexure)];
    
    kemoViewMessageMetalBufs->numtimeLabelVertice = [kemo2DMetalBufBase  setTextBoxTexture:device
                                                                                    buffer:kemo_buffers->MESSAGE_bufs->timelabel_buf
                                                                                    vertex:&(kemoViewMessageMetalBufs->timeLabelVertice)
                                                                                    texure:&(kemoViewMessageMetalBufs->timeLabelTexure)];
    
    kemoViewMessageMetalBufs->numMessageVertice =   [kemo2DMetalBufBase  setTextBoxTexture:device
                                                                                    buffer:kemo_buffers->MESSAGE_bufs->message_buf
                                                                                    vertex:&(kemoViewMessageMetalBufs->messageVertice)
                                                                                    texure:&(kemoViewMessageMetalBufs->messageTexure)];
    return;
}


- (void) releaseMsgMetalBuffers:(KemoViewMessageMetalBuffers *_Nonnull) kemoViewMessageMetalBufs
{
    if(kemoViewMessageMetalBufs->numColorBarVertice > 0) {[kemoViewMessageMetalBufs->colorBarVertice release];};
    if(kemoViewMessageMetalBufs->numMinLabelVertice > 0){
        [kemoViewMessageMetalBufs->minLabelVertice release];
        [kemoViewMessageMetalBufs->minLabelTexure  release];
    };
    if(kemoViewMessageMetalBufs->numMaxLabelVertice > 0){
        [kemoViewMessageMetalBufs->maxLabelVertice release];
        [kemoViewMessageMetalBufs->maxLabelTexure  release];
    };
    if(kemoViewMessageMetalBufs->numZeroLabelVertice > 0){
        [kemoViewMessageMetalBufs->zeroLabelVertice release];
        [kemoViewMessageMetalBufs->zeroLabelTexure  release];
    };
    if(kemoViewMessageMetalBufs->numtimeLabelVertice > 0){
        [kemoViewMessageMetalBufs->timeLabelVertice release];
        [kemoViewMessageMetalBufs->timeLabelTexure  release];
    };
    if(kemoViewMessageMetalBufs->numMessageVertice > 0){
        [kemoViewMessageMetalBufs->messageVertice release];
        [kemoViewMessageMetalBufs->messageTexure  release];
    };
    return;
}



- (void)drawTextBoxObject:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull)  renderEncoder
                pipelines:(id<MTLRenderPipelineState>  _Nonnull *_Nonnull) texured2DPipelineState
                numVertex:(NSUInteger) numVertex
                   vertex:(id<MTLBuffer> _Nonnull *_Nonnull)  vertices
                   texure:(id<MTLTexture> _Nonnull *_Nonnull) texture
               projection:(matrix_float4x4 *_Nonnull) projection_mat;
{
    if(numVertex > 0){
        [*renderEncoder setRenderPipelineState:*texured2DPipelineState];
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

- (void) encodeMessageObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  base2Dclass:(KemoView2DRenderer *_Nonnull) KemoView2DRenderer
                    pipelines:(KemoView2DMetalPipelines * _Nonnull) kemo2DPipelines
                  metalBuffer:(KemoViewMessageMetalBuffers *) kemoViewMessageMetalBufs
                   projection:(matrix_float4x4 * _Nonnull) projection_mat
{
/*  Commands to render colorbar  box */
    [KemoView2DRenderer draw2DPatchObject:renderEncoder
                                pipelines:&(kemo2DPipelines->trans2DPipelineState)
                                numVertex:kemoViewMessageMetalBufs->numColorBarVertice
                                   vertex:&(kemoViewMessageMetalBufs->colorBarVertice)
                               projection:projection_mat];
/*  Commands to render colorbar  label */
    [self drawTextBoxObject:renderEncoder
                  pipelines:&(kemo2DPipelines->texured2DPipelineState)
                  numVertex:kemoViewMessageMetalBufs->numMinLabelVertice
                     vertex:&(kemoViewMessageMetalBufs->minLabelVertice)
                     texure:&(kemoViewMessageMetalBufs->minLabelTexure)
                 projection:projection_mat];
    [self drawTextBoxObject:renderEncoder
                  pipelines:&(kemo2DPipelines->texured2DPipelineState)
                  numVertex:kemoViewMessageMetalBufs->numMaxLabelVertice
                     vertex:&(kemoViewMessageMetalBufs->maxLabelVertice)
                     texure:&(kemoViewMessageMetalBufs->maxLabelTexure)
                 projection:projection_mat];
    [self drawTextBoxObject:renderEncoder
                  pipelines:&(kemo2DPipelines->texured2DPipelineState)
                  numVertex:kemoViewMessageMetalBufs->numZeroLabelVertice
                     vertex:&(kemoViewMessageMetalBufs->zeroLabelVertice)
                     texure:&(kemoViewMessageMetalBufs->zeroLabelTexure)
                 projection:projection_mat];

/*  Commands to render time label */
    [self drawTextBoxObject:renderEncoder
                  pipelines:&(kemo2DPipelines->texured2DPipelineState)
                  numVertex:kemoViewMessageMetalBufs->numtimeLabelVertice
                     vertex:&(kemoViewMessageMetalBufs->timeLabelVertice)
                     texure:&(kemoViewMessageMetalBufs->timeLabelTexure)
                 projection:projection_mat];
/*  Commands to render colorbar  box */
    [self drawTextBoxObject:renderEncoder
                  pipelines:&(kemo2DPipelines->texured2DPipelineState)
                  numVertex:kemoViewMessageMetalBufs->numMessageVertice
                     vertex:&(kemoViewMessageMetalBufs->messageVertice)
                     texure:&(kemoViewMessageMetalBufs->messageTexure)
                 projection:projection_mat];
    return;
}

@end

