/*
//  KemoViewMapRenderer.m
//
//
//  Created by Hiroaki Matsui on 11/28/23.
*/

#import <Foundation/Foundation.h>


#import "KemoViewMapRenderer.h"
#include "m_gl_transfer_matrix.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation KemoViewMapRenderer
- (void) setMapMBuffers:(id<MTLDevice> *) device
        baseMetalBuffer:(KemoViewMetalBuffers *) kemo2DMetalBufBase
            metalbuffer:(KemoViewMapMetalBuffers *) kemoViewMapMetalBufs
            node_buffer:(struct gl_strided_buffer *) PSF_node_buf
            map_buffers:(struct MAP_buffers *) MAP_bufs
{
    kemoViewMapMetalBufs->numMapNodeVertice =  [kemo2DMetalBufBase  setMetalVertexs:device
                                                                             buffer:PSF_node_buf
                                                                             vertex:&(kemoViewMapMetalBufs->mapNodeVertice)];
    kemoViewMapMetalBufs->numMapSolidIndices = [kemo2DMetalBufBase  setMetalIndices:device
                                                                           indexbuf:MAP_bufs->MAP_solid_index_buf
                                                                              index:&(kemoViewMapMetalBufs->mapSolidIndices)];
    
    kemoViewMapMetalBufs->numMapSolidVertice =  [kemo2DMetalBufBase  setMetalVertexs:device
                                                                              buffer:MAP_bufs->MAP_solid_buf
                                                                              vertex:&(kemoViewMapMetalBufs->mapSolidVertice)];
    kemoViewMapMetalBufs->numMapinesVertice =   [kemo2DMetalBufBase  setMetalVertexs:device
                                                                              buffer:MAP_bufs->MAP_isoline_buf
                                                                              vertex:&(kemoViewMapMetalBufs->mapLinesVertice)];
    kemoViewMapMetalBufs->numCoastLineVertice = [kemo2DMetalBufBase  setMetalVertexs:device
                                                                              buffer:MAP_bufs->MAP_coast_line_buf
                                                                              vertex:&(kemoViewMapMetalBufs->coastLineVertice)];
    kemoViewMapMetalBufs->numCoastTubeVertice = [kemo2DMetalBufBase  setMetalVertexs:device
                                                                              buffer:MAP_bufs->MAP_coast_tube_buf
                                                                              vertex:&(kemoViewMapMetalBufs->coastTubeVertice)];
};

- (void) releaseMapMetalBuffers:(KemoViewMapMetalBuffers *_Nonnull) kemoViewMapMetalBufs
{
    if(kemoViewMapMetalBufs->numMapNodeVertice > 0)  {[kemoViewMapMetalBufs->mapNodeVertice    release];};
    if(kemoViewMapMetalBufs->numMapSolidIndices > 0)  {[kemoViewMapMetalBufs->mapSolidIndices  release];};
    
    if(kemoViewMapMetalBufs->numMapSolidVertice > 0)  {[kemoViewMapMetalBufs->mapSolidVertice  release];};
    if(kemoViewMapMetalBufs->numMapinesVertice > 0)   {[kemoViewMapMetalBufs->mapLinesVertice  release];};
    if(kemoViewMapMetalBufs->numCoastLineVertice > 0) {[kemoViewMapMetalBufs->coastLineVertice release];};
    if(kemoViewMapMetalBufs->numCoastTubeVertice > 0) {[kemoViewMapMetalBufs->coastTubeVertice release];};
    return;
}


- (void) encodeMapObjects:(id<MTLRenderCommandEncoder> _Nonnull *_Nonnull) renderEncoder
              base2Dclass:(KemoView2DRenderer *_Nonnull) KemoView2DRenderer
                pipelines:(KemoView2DMetalPipelines *_Nonnull) kemoView2DPipelines
              metalBuffer:(KemoViewMapMetalBuffers *) kemoViewMapMetalBufs
               projection:(matrix_float4x4 * _Nonnull) map_proj_mat
{
    /*  Commands to render map projection */
    [KemoView2DRenderer draw2DElementObject:renderEncoder
                                  pipelines:&(kemoView2DPipelines->trans2DPipelineState)
                                  numVertex:kemoViewMapMetalBufs->numMapSolidIndices
                                     vertex:&(kemoViewMapMetalBufs->mapNodeVertice)
                                      index:&(kemoViewMapMetalBufs->mapSolidIndices)
                                 projection:map_proj_mat];

    /*  Commands to render map projection */
    [KemoView2DRenderer draw2DPatchObject:renderEncoder
                                pipelines:&(kemoView2DPipelines->trans2DPipelineState)
                                numVertex:kemoViewMapMetalBufs->numMapSolidVertice
                                   vertex:&(kemoViewMapMetalBufs->mapSolidVertice)
                               projection:map_proj_mat];

    /*  Commands to render isolines on map */
    [KemoView2DRenderer draw2DPatchObject:renderEncoder
                                pipelines:&(kemoView2DPipelines->trans2DPipelineState)
                                numVertex:kemoViewMapMetalBufs->numMapinesVertice
                                   vertex:&(kemoViewMapMetalBufs->mapLinesVertice)
                               projection:map_proj_mat];
    /*  Commands to render Coastline on map */
    [KemoView2DRenderer draw2DPatchObject:renderEncoder
                                pipelines:&(kemoView2DPipelines->trans2DPipelineState)
                                numVertex:kemoViewMapMetalBufs->numCoastTubeVertice
                                   vertex:&(kemoViewMapMetalBufs->coastTubeVertice)
                               projection:map_proj_mat];

    [KemoView2DRenderer draw2DLineObject:renderEncoder
                               pipelines:&(kemoView2DPipelines->simple2DPipelineState)
                               numVertex:kemoViewMapMetalBufs->numCoastLineVertice
                                  vertex:&(kemoViewMapMetalBufs->coastLineVertice)
                              projection:map_proj_mat];
    return;
}


- (void) setMapMetalBuffers:(id<MTLDevice> _Nonnull * _Nonnull) device
            baseMetalBuffer:(KemoViewMetalBuffers *_Nonnull) kemo2DMetalBufBase
                metalBuffer:(KemoViewMapMetalBuffers *_Nonnull) kemoViewMapMetalBufs
                    buffers:(struct kemoview_buffers * _Nonnull) kemo_buffers
{
    [self setMapMBuffers:device
         baseMetalBuffer:kemo2DMetalBufBase
             metalbuffer:kemoViewMapMetalBufs
             node_buffer:kemo_buffers->PSF_node_buf
             map_buffers:kemo_buffers->MAP_bufs];
}

@end
