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
    KemoView3DBaseRenderer   *_Kemo3DBaseRenderer;

    KemoViewMetalShaders   _kemoViewShaders;
    KemoView3DPipelines    _kemoViewPipelines;
    KemoView3DBuffers     _kemoViewMetalBuf;
}

-(id) init
{
    _kemo3DMetalBufBase = [[KemoViewMetalBuffers alloc] init];
    _Kemo3DBaseRenderer = [[KemoView3DBaseRenderer alloc] init];
    return self;
}

- (void) setTransMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                  metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                   psfBuffers:(struct PSF_trans_buffers *_Nonnull) PSF_transes
                  meshBbuffer:(struct gl_strided_buffer *_Nonnull) mesh_trns_buf
                         PSFs:(struct kemoview_psf *_Nonnull) kemo_psf
{
    /*  Set transparent vertexs */
    kemoView3DMetalBuf->numPSFTransTexurePixsel = [_kemo3DMetalBufBase setPSFTexture:device
                                                                               image:kemo_psf->psf_a->psf_texure
                                                                              texure:&(kemoView3DMetalBuf->psfTransTexure)];

    kemoView3DMetalBuf->numPSFTransTexureVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                                  buffer:PSF_transes->PSF_ttxur_buf
                                                                                  vertex:&(kemoView3DMetalBuf->psfTTexureVertice)];
    kemoView3DMetalBuf->numPSFTransVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:PSF_transes->PSF_trns_buf
                                                                            vertex:&(kemoView3DMetalBuf->psfTransVertice)];
    kemoView3DMetalBuf->numMeshTransVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:mesh_trns_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshTransVertice)];
    
/*  Set PSF index buffer */
    kemoView3DMetalBuf->numPsfTransIndices =   [_kemo3DMetalBufBase setMetalIndices:device
                                                                          indexbuf:PSF_transes->PSF_trns_index_buf
                                                                             index:&(kemoView3DMetalBuf->psfTransIndices)];
    kemoView3DMetalBuf->numPsfTTexureIndices = [_kemo3DMetalBufBase setMetalIndices:device
                                                                          indexbuf:PSF_transes->PSF_ttxur_index_buf
                                                                             index:&(kemoView3DMetalBuf->psfTTexureIndices)];

    return;
}
- (void) setAxisMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                  metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                      buffers:(struct gl_strided_buffer *_Nonnull) axis_buf
{
    /*  Set axis vertexs */
    kemoView3DMetalBuf->numAxisVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                       buffer:axis_buf
                                                                       vertex:&(kemoView3DMetalBuf->axisVertice)];
    return;
}

- (void) set3DLineBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
              metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
           isoLineBuffers:(struct PSF_line_buffers *_Nonnull) PSF_lines
          fileLineBuffers:(struct FieldLine_buffers *_Nonnull) Fline_bufs
                     PSFs:(struct kemoview_psf *_Nonnull) kemo_psf
{
    kemoView3DMetalBuf->numPSFTubesVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:PSF_lines->PSF_isotube_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfTubesVertice)];
    kemoView3DMetalBuf->numPSFLinesVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:PSF_lines->PSF_isoline_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfLinesVertice)];

    
    kemoView3DMetalBuf->numFieldTubeVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:Fline_bufs->FLINE_tube_buf
                                                                            vertex:&(kemoView3DMetalBuf->fieldTubeVertice)];
    kemoView3DMetalBuf->numFfieldLineVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                             buffer:Fline_bufs->FLINE_line_buf
                                                                             vertex:&(kemoView3DMetalBuf->fieldLineVertice)];

    kemoView3DMetalBuf->numCoastLineVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:PSF_lines->coast_line_buf
                                                                            vertex:&(kemoView3DMetalBuf->coastLineVertice)];
    kemoView3DMetalBuf->numCoastTubeVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:PSF_lines->coast_tube_buf
                                                                            vertex:&(kemoView3DMetalBuf->coastTubeVertice)];

    kemoView3DMetalBuf->numPSFArrowVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:PSF_lines->PSF_arrow_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfArrowVertice)];
        return;
}

- (void) set3DMeshBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
               metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                   buffers:(struct MESH_buffers *_Nonnull) MESH_bufs
{
    kemoView3DMetalBuf->numMeshNodeVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:MESH_bufs->mesh_node_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshNodeVertice)];
    kemoView3DMetalBuf->numMeshGridVertice =  [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:MESH_bufs->mesh_grid_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshGridVertice)];
    kemoView3DMetalBuf->numMeshSolidVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                            buffer:MESH_bufs->mesh_solid_buf
                                                                            vertex:&(kemoView3DMetalBuf->meshSolidVertice)];
    return;
}

- (void) set3DCubeBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
               metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
                   buffers:(struct initial_cube_buffers *_Nonnull) initial_bufs
{
/*  Set Cube index buffer */
        kemoView3DMetalBuf->numCubeVertice = [_kemo3DMetalBufBase setCubeVertexs:device
                                                                          buffer:initial_bufs->cube_buf
                                                                        indexbuf:initial_bufs->cube_index_buf
                                                                          vertex:&(kemoView3DMetalBuf->cubeVertice)
                                                                           index:&(kemoView3DMetalBuf->cubeIndex)];
}

- (void) set3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
               metalbuffer:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
               nodeBuffers:(struct gl_strided_buffer *_Nonnull) PSF_node_buf
              patchBuffers:(struct PSF_solid_buffers *_Nonnull) PSF_solids
                      PSFs:(struct kemoview_psf *_Nonnull) kemo_psf
{
    kemoView3DMetalBuf->numPSFSolidTexurePixsel = [_kemo3DMetalBufBase setPSFTexture:device
                                                                               image:kemo_psf->psf_a->psf_texure
                                                                              texure:&(kemoView3DMetalBuf->psfSolidTexure)];
    
    kemoView3DMetalBuf->numPSFSolidTexureVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                                 buffer:PSF_solids->PSF_stxur_buf
                                                                                 vertex:&(kemoView3DMetalBuf->psfSTexureVertice)];
    
    kemoView3DMetalBuf->numPsfNodeVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:PSF_node_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfNodeVertice)];

    
    kemoView3DMetalBuf->numPSFSolidVertice = [_kemo3DMetalBufBase setMetalVertexs:device
                                                                           buffer:PSF_solids->PSF_solid_buf
                                                                           vertex:&(kemoView3DMetalBuf->psfSolidVertice)];

/*  Set PSF index buffer */
    kemoView3DMetalBuf->numPsfSolidIndices =   [_kemo3DMetalBufBase setMetalIndices:device
                                                                          indexbuf:PSF_solids->PSF_solid_index_buf
                                                                             index:&(kemoView3DMetalBuf->psfSolidIndices)];
    kemoView3DMetalBuf->numPsfSTexureIndices = [_kemo3DMetalBufBase setMetalIndices:device
                                                                          indexbuf:PSF_solids->PSF_stxur_index_buf
                                                                             index:&(kemoView3DMetalBuf->psfSTexureIndices)];
    return;
}

- (void) releaseTransMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    /*  Release transparent vertexs */
    if(kemoView3DMetalBuf->numPSFTransTexurePixsel > 0){[kemoView3DMetalBuf->psfTransTexure release];};
    if(kemoView3DMetalBuf->numPSFTransTexureVertice > 0){[kemoView3DMetalBuf->psfTTexureVertice release];};

    if(kemoView3DMetalBuf->numPSFTransVertice > 0)  {[kemoView3DMetalBuf->psfTransVertice  release];};
    if(kemoView3DMetalBuf->numMeshTransVertice > 0) {[kemoView3DMetalBuf->meshTransVertice release];};
    
    if(kemoView3DMetalBuf->numPsfTransIndices > 0) {[kemoView3DMetalBuf->psfTransIndices release];};
    if(kemoView3DMetalBuf->numPsfTTexureIndices > 0) {[kemoView3DMetalBuf->psfTTexureIndices release];};
    return;
}

- (void) releaseAxisMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    if(kemoView3DMetalBuf->numAxisVertice > 0)  {[kemoView3DMetalBuf->axisVertice      release];};
    return;
}

- (void) release3DLineBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    if(kemoView3DMetalBuf->numPSFSolidVertice > 0) {[kemoView3DMetalBuf->psfSolidVertice release];};
    if(kemoView3DMetalBuf->numPSFTubesVertice > 0) {[kemoView3DMetalBuf->psfTubesVertice release];};

    if(kemoView3DMetalBuf->numFieldTubeVertice >  0) {[kemoView3DMetalBuf->fieldTubeVertice release];};
    if(kemoView3DMetalBuf->numFfieldLineVertice > 0) {[kemoView3DMetalBuf->fieldLineVertice release];};
    
    if(kemoView3DMetalBuf->numCoastTubeVertice > 0)    {[kemoView3DMetalBuf->coastTubeVertice   release];};
    if(kemoView3DMetalBuf->numCoastLineVertice > 0)    {[kemoView3DMetalBuf->coastLineVertice   release];};
    return;
}

- (void) release3DMetalBuffers:(KemoView3DBuffers *_Nonnull) kemoView3DMetalBuf
{
    if(kemoView3DMetalBuf->numPSFSolidTexurePixsel > 0){[kemoView3DMetalBuf->psfSolidTexure release];};
    
    if(kemoView3DMetalBuf->numPsfNodeVertice > 0) {[kemoView3DMetalBuf->psfNodeVertice release];};
    if(kemoView3DMetalBuf->numPSFSolidTexureVertice > 0){[kemoView3DMetalBuf->psfSTexureVertice release];};
    if(kemoView3DMetalBuf->numPsfSolidIndices > 0) {[kemoView3DMetalBuf->psfSolidIndices release];};
    if(kemoView3DMetalBuf->numPsfSTexureIndices > 0) {[kemoView3DMetalBuf->psfSTexureIndices release];};
    
    if(kemoView3DMetalBuf->numPSFLinesVertice > 0) {[kemoView3DMetalBuf->psfLinesVertice release];};
    if(kemoView3DMetalBuf->numPSFArrowVertice > 0) {[kemoView3DMetalBuf->psfArrowVertice release];};
    
    if(kemoView3DMetalBuf->numMeshNodeVertice > 0)  {[kemoView3DMetalBuf->meshNodeVertice   release];};
    if(kemoView3DMetalBuf->numMeshGridVertice > 0)  {[kemoView3DMetalBuf->meshGridVertice   release];};
    if(kemoView3DMetalBuf->numMeshSolidVertice > 0) {[kemoView3DMetalBuf->meshSolidVertice release];};

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
                    psfBuffers:kemo_sgl->kemo_buffers->PSF_transes
                   meshBbuffer:kemo_sgl->kemo_buffers->mesh_trns_buf
                          PSFs:kemo_sgl->kemo_psf];
    return;
};
- (void) setKemoFastMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                        kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self setAxisMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers->axis_buf];
    
    _kemoViewMetalBuf.numCoastTubeVertice = 0;
    _kemoViewMetalBuf.numFieldTubeVertice = 0;
    _kemoViewMetalBuf.numPSFTubesVertice =  0;
    _kemoViewMetalBuf.numMeshNodeVertice =  0;

    [self setTransMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                    psfBuffers:kemo_sgl->kemo_buffers->PSF_transes
                   meshBbuffer:kemo_sgl->kemo_buffers->mesh_trns_buf
                          PSFs:kemo_sgl->kemo_psf];
    return;
};

- (void) setKemoMovieMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                         kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self setAxisMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers->axis_buf];
    [self setTransMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                    psfBuffers:kemo_sgl->kemo_buffers->PSF_transes
                   meshBbuffer:kemo_sgl->kemo_buffers->mesh_trns_buf
                          PSFs:kemo_sgl->kemo_psf];
    return;
};

- (void) setKemoView3DMetalBuffers:(id<MTLDevice> _Nonnull *_Nonnull) device
                          kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    [self setAxisMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                       buffers:kemo_sgl->kemo_buffers->axis_buf];
    [self set3DLineBuffers:device
               metalbuffer:&_kemoViewMetalBuf
            isoLineBuffers:kemo_sgl->kemo_buffers->PSF_lines
           fileLineBuffers:kemo_sgl->kemo_buffers->Fline_bufs
                      PSFs:kemo_sgl->kemo_psf];
    [self set3DMetalBuffers:device
                metalbuffer:&_kemoViewMetalBuf
                nodeBuffers:kemo_sgl->kemo_buffers->PSF_node_buf
               patchBuffers:kemo_sgl->kemo_buffers->PSF_solids
                       PSFs:kemo_sgl->kemo_psf];
    [self set3DMeshBuffers:device
               metalbuffer:&_kemoViewMetalBuf
                   buffers:kemo_sgl->kemo_buffers->MESH_bufs];

    [self setTransMetalBuffers:device
                   metalbuffer:&_kemoViewMetalBuf
                    psfBuffers:kemo_sgl->kemo_buffers->PSF_transes
                   meshBbuffer:kemo_sgl->kemo_buffers->mesh_trns_buf
                          PSFs:kemo_sgl->kemo_psf];

    [self set3DCubeBuffers:device
               metalbuffer:&_kemoViewMetalBuf
                  buffers:kemo_sgl->kemo_buffers->initial_bufs];
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
- (void) releaseKemoMovieMetalBuffers
{
    [self releaseTransMetalBuffers:&_kemoViewMetalBuf];
    [self releaseAxisMetalBuffers:&_kemoViewMetalBuf];
    return;
};
- (void) releaseKemoView3DMetalBuffers
{
    [self release3DLineBuffers:&_kemoViewMetalBuf];
    [self release3DMetalBuffers:&_kemoViewMetalBuf];
    [self releaseTransMetalBuffers:&_kemoViewMetalBuf];
    [self releaseAxisMetalBuffers:&_kemoViewMetalBuf];
    return;
};

- (void) encodeSolidPSFObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                     pipelines:(KemoView3DPipelines *) kemo3DPipelines
                         depth:(id<MTLDepthStencilState> *) depthState
                   metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                        unites:(KemoViewUnites *) monoViewUnites
{
    [_Kemo3DBaseRenderer drawIndexTexureWithPhong:renderEncoder
                         pipelines:kemo3DPipelines
                             depth:depthState
                         numVertex:kemoView3DMetalBuf->numPsfSTexureIndices
                            vertex:&(kemoView3DMetalBuf->psfNodeVertice)
                             index:&(kemoView3DMetalBuf->psfSTexureIndices)
                            texure:&(kemoView3DMetalBuf->psfSolidTexure)
                            unites:monoViewUnites
                             sides:BOTH_SURFACES];
    [_Kemo3DBaseRenderer drawIndexPatchWithPhong:renderEncoder
                        pipelines:kemo3DPipelines
                            depth:depthState
                        numVertex:kemoView3DMetalBuf->numPsfSolidIndices
                           vertex:&(kemoView3DMetalBuf->psfNodeVertice)
                            index:&(kemoView3DMetalBuf->psfSolidIndices)
                           unites:monoViewUnites
                            sides:BOTH_SURFACES];
    
    
    [_Kemo3DBaseRenderer drawTexureWithPhong:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                    numVertex:kemoView3DMetalBuf->numPSFSolidTexureVertice
                       vertex:&(kemoView3DMetalBuf->psfSTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfSolidTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES];
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFSolidVertice
                      vertex:&(kemoView3DMetalBuf->psfSolidVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
}

- (void) encodeTransPSFObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                     pipelines:(KemoView3DPipelines *) kemo3DPipelines
                         depth:(id<MTLDepthStencilState> *) depthState
                   metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                        unites:(KemoViewUnites *) monoViewUnites
{
    [_Kemo3DBaseRenderer drawIndexTexureWithPhong:renderEncoder
                         pipelines:kemo3DPipelines
                             depth:depthState
                         numVertex:kemoView3DMetalBuf->numPsfTTexureIndices
                            vertex:&(kemoView3DMetalBuf->psfNodeVertice)
                             index:&(kemoView3DMetalBuf->psfTTexureIndices)
                            texure:&(kemoView3DMetalBuf->psfTransTexure)
                            unites:monoViewUnites
                             sides:BOTH_SURFACES];
    [_Kemo3DBaseRenderer drawIndexPatchWithPhong:renderEncoder
                        pipelines:kemo3DPipelines
                            depth:depthState
                        numVertex:kemoView3DMetalBuf->numPsfTransIndices
                           vertex:&(kemoView3DMetalBuf->psfNodeVertice)
                            index:&(kemoView3DMetalBuf->psfTransIndices)
                           unites:monoViewUnites
                            sides:BOTH_SURFACES];
    
    [_Kemo3DBaseRenderer drawTexureWithPhong:renderEncoder
                    pipelines:kemo3DPipelines
                        depth:depthState
                    numVertex:kemoView3DMetalBuf->numPSFTransTexureVertice
                       vertex:&(kemoView3DMetalBuf->psfTTexureVertice)
                       texure:&(kemoView3DMetalBuf->psfTransTexure)
                       unites:monoViewUnites
                        sides:BOTH_SURFACES];
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFTransVertice
                      vertex:&(kemoView3DMetalBuf->psfTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
    return;
}


- (void) encodeLinePSFObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
                    pipelines:(KemoView3DPipelines *) kemo3DPipelines
                        depth:(id<MTLDepthStencilState> *) depthState
                  metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                       unites:(KemoViewUnites *) monoViewUnites
{
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFArrowVertice
                      vertex:&(kemoView3DMetalBuf->psfArrowVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
    
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numPSFTubesVertice
                      vertex:&(kemoView3DMetalBuf->psfTubesVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
    if(kemoView3DMetalBuf->numPSFTubesVertice == 0){
        [_Kemo3DBaseRenderer drawLineObject:renderEncoder
                                  pipelines:kemo3DPipelines
                                      depth:depthState
                                  numVertex:kemoView3DMetalBuf->numPSFLinesVertice
                                     vertex:&(kemoView3DMetalBuf->psfLinesVertice)
                                     unites:monoViewUnites];
    }

    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                                  pipelines:kemo3DPipelines
                                      depth:depthState
                                  numVertex:kemoView3DMetalBuf->numFieldTubeVertice
                                     vertex:&(kemoView3DMetalBuf->fieldTubeVertice)
                                     unites:monoViewUnites
                                      sides:BOTH_SURFACES];
    if(kemoView3DMetalBuf->fieldTubeVertice == 0){
        [_Kemo3DBaseRenderer drawLineObject:renderEncoder
                                  pipelines:kemo3DPipelines
                                      depth:depthState
                                  numVertex:kemoView3DMetalBuf->numFfieldLineVertice
                                     vertex:&(kemoView3DMetalBuf->fieldLineVertice)
                                     unites:monoViewUnites];
    }
    
    /* Draw coastlines */
    
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numCoastTubeVertice
                      vertex:&(kemoView3DMetalBuf->coastTubeVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
    if(kemoView3DMetalBuf->numCoastTubeVertice == 0){
        [_Kemo3DBaseRenderer drawLineObject:renderEncoder
                                  pipelines:kemo3DPipelines
                                      depth:depthState
                                  numVertex:kemoView3DMetalBuf->numCoastLineVertice
                                     vertex:&(kemoView3DMetalBuf->coastLineVertice)
                                     unites:monoViewUnites];
    }
    return;
}

- (void) encode3DObjects:(id<MTLRenderCommandEncoder>  *) renderEncoder
               pipelines:(KemoView3DPipelines *) kemo3DPipelines
                   depth:(id<MTLDepthStencilState> *) depthState
             metalbuffer:(KemoView3DBuffers *_Nullable) kemoView3DMetalBuf
                  unites:(KemoViewUnites *) monoViewUnites
                   sides:(int) iflag_polygon
{
    /* Draw initial cube */
        [_Kemo3DBaseRenderer drawIndexPatchWithPhong:renderEncoder
                            pipelines:kemo3DPipelines
                                depth:depthState
                            numVertex:kemoView3DMetalBuf->numCubeVertice
                               vertex:&(kemoView3DMetalBuf->cubeVertice)
                                index:&(kemoView3DMetalBuf->cubeIndex)
                               unites:monoViewUnites
                                sides:BOTH_SURFACES];
    if(kemoView3DMetalBuf->numCubeVertice > 0) return;

    /*  Draw solid objects */
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numAxisVertice
                      vertex:&(kemoView3DMetalBuf->axisVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
    
    [self encodeSolidPSFObjects:renderEncoder
                      pipelines:kemo3DPipelines
                          depth:depthState
                    metalbuffer:kemoView3DMetalBuf
                         unites:monoViewUnites];
    
    [self encodeLinePSFObjects:renderEncoder
                     pipelines:kemo3DPipelines
                         depth:depthState
                   metalbuffer:kemoView3DMetalBuf
                        unites:monoViewUnites];
    
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshNodeVertice
                      vertex:&(kemoView3DMetalBuf->meshNodeVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];
    [_Kemo3DBaseRenderer drawLineObject:renderEncoder
               pipelines:kemo3DPipelines
                   depth:depthState
               numVertex:kemoView3DMetalBuf->numMeshGridVertice
                  vertex:&(kemoView3DMetalBuf->meshGridVertice)
                  unites:monoViewUnites];
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshSolidVertice
                      vertex:&(kemoView3DMetalBuf->meshSolidVertice)
                      unites:monoViewUnites
                       sides:iflag_polygon];

    
/*  Draw transparent objects */
    [self encodeTransPSFObjects:renderEncoder
                      pipelines:kemo3DPipelines
                          depth:depthState
                    metalbuffer:kemoView3DMetalBuf
                         unites:monoViewUnites];
    
    [_Kemo3DBaseRenderer drawSolidWithPhong:renderEncoder
                   pipelines:kemo3DPipelines
                       depth:depthState
                   numVertex:kemoView3DMetalBuf->numMeshTransVertice
                      vertex:&(kemoView3DMetalBuf->meshTransVertice)
                      unites:monoViewUnites
                       sides:BOTH_SURFACES];

    return;
}



-(void) addKemoView3DShaderLibrary:(id<MTLLibrary> _Nonnull *_Nonnull) defaultLibrary
{
    [_Kemo3DBaseRenderer add3DShaderLibrary:&_kemoViewShaders
                                    library:defaultLibrary];
    return;
}

-(void) addKemoView3DPipelines:(nonnull MTKView *)mtkView
                   targetPixel:(MTLPixelFormat) pixelformat
{
    [_Kemo3DBaseRenderer addKemo3DPipelines:mtkView
                                    shaders:&_kemoViewShaders
                                  pipelines:&_kemoViewPipelines
                                targetPixel:pixelformat];
    return;
}

-(void) encodeKemoView3DObjects:(id<MTLRenderCommandEncoder> _Nonnull  *_Nonnull) renderEncoder
                          depth:(id<MTLDepthStencilState> _Nonnull *_Nonnull) depthState
                         unites:(KemoViewUnites *_Nonnull) monoViewUnites
                          sides:(int) iflag_polygon
{
    [self encode3DObjects:renderEncoder
                pipelines:&_kemoViewPipelines
                    depth:depthState
              metalbuffer:&_kemoViewMetalBuf
                   unites:monoViewUnites
                    sides:iflag_polygon];
    return;
}

@end
