/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "KemoViewerRenderer.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation KemoViewerRenderer
{
    id<MTLDevice> _device;

    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    // Combined depth and stencil state object.
    id<MTLDepthStencilState> _depthState;

    id<MTLRenderCommandEncoder> _renderEncoder;

    KemoViewUnites _monoViewUnites;

    matrix_float4x4 _map_proj_mat;
    matrix_float4x4 _cbar_proj_mat;
}

- (nonnull instancetype)initKemoMetalKitView:(nonnull MTKView *)mtkView
{
    _kemoRendererTools = [[KemoViewRendererTools alloc] init];
    _kemoMetalBufBase = [[KemoViewMetalBuffers alloc] init];
    _kemo2DRenderer = [[KemoView2DRenderer alloc] init];
    _kemo3DRenderer = [[KemoView3DRenderer alloc] init];

    self = [super init];
    if(self)
    {
        _device = mtkView.device;

// Indicate that each pixel in the depth buffer is a 32-bit floating point value.
        mtkView.depthStencilPixelFormat = MTLPixelFormatDepth32Float;

/* Load all the shader files with a .metal file extension in the project. */
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];
        [_kemo3DRenderer addKemoView3DShaderLibrary:&defaultLibrary];
        [_kemo2DRenderer add2DShaderLibrary:&defaultLibrary];

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        [_kemo2DRenderer addKemoView2DPipelines:mtkView
                         targetPixel:mtkView.colorPixelFormat];
        [_kemo3DRenderer addKemoView3DPipelines:mtkView
                                    targetPixel:mtkView.colorPixelFormat];
        [_kemo3DRenderer addKemoViewAnaglyphPipelines:mtkView
                                          targetPixel:mtkView.colorPixelFormat];

/* Add Depth buffer description in command */
        MTLDepthStencilDescriptor *depthDescriptor;
        depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = YES;
        _depthState =  [_device newDepthStencilStateWithDescriptor:depthDescriptor];
    }
/* Create the command queue */
    _commandQueue = [_device newCommandQueue];
    return self;
}



- (void) releaseKemoViewMetalBuffers:(struct kemoviewer_type *) kemo_sgl
{
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_MAP){
/*  Release Map vertexs */
        [_kemo2DRenderer releaseMapMetalBuffers:kemo_sgl->kemo_buffers];
    }else{
/*  Release 3D vertexs */
        [_kemo3DRenderer releaseKemoView3DMetalBuffers:kemo_sgl];
/*  Release transparent vertexs */
        [_kemo3DRenderer releaseTransparentMetalBuffers:kemo_sgl];
    };
/*  Release Message vertexs */
    [_kemo2DRenderer releaseMsgMetalBuffers:kemo_sgl->kemo_buffers];
    return;
}


- (void) setKemoViewMetalBuffers:(id<MTLDevice> *) device
                        kemoview:(struct kemoviewer_type *) kemo_sgl
{
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_MAP){
/*  Set Map vertexs to Metal buffers */
        [_kemo2DRenderer setMapMetalBuffers:device
                                    buffers:kemo_sgl->kemo_buffers];
    }else{
/*  Set 3D vertexs to Metal buffers */
        [_kemo3DRenderer setKemoView3DMetalBuffers:device
                                          kemoview:kemo_sgl];
/*  Set transparent vertexs to Metal buffers */
        [_kemo3DRenderer setKemoTransparentMetalBuffers:device
                                               kemoview:kemo_sgl];
    };
    
/*  Set message vertexs to Metal buffers */
    [_kemo2DRenderer setMessageMetalBuffers:device
                                    buffers:kemo_sgl->kemo_buffers];
    return;
}

- (void)refreshKemoViewMetalBuffers:(id<MTLDevice> *) device
                           kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int iflag = kemoview_get_draw_mode();
    if(iflag == FULL_DRAW){
        [self releaseKemoViewMetalBuffers:kemo_sgl];
        
        kemoview_const_buffers(kemo_sgl);
        
        [self setKemoViewMetalBuffers:device
                             kemoview:kemo_sgl];
    }else if(iflag == FAST_DRAW){
        if(kemoview_get_view_type_flag(kemo_sgl) != VIEW_MAP){
            [_kemo3DRenderer releaseTransparentMetalBuffers:kemo_sgl];
            
            kemoview_transparent_buffers(kemo_sgl);
            
            [_kemo3DRenderer setKemoTransparentMetalBuffers:device
                                                   kemoview:kemo_sgl];
        };
    };
    return;
}

- (void) encodeKemoViewers:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  kemoview:(struct kemoviewer_type *) kemo_sgl
                    unites:(KemoViewUnites *) monoViewUnites
{
    int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
    if(iflag_view == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_map_proj_mat];
    }else if(kemoview_get_draw_mode() == SIMPLE_DRAW){
        [_kemo3DRenderer encodeKemoSimpleObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:monoViewUnites];
    }else{
        [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:monoViewUnites];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_cbar_proj_mat];
}

-(void) KemoViewEncodeAll:(nonnull MTKView *)view
                 kemoview:(struct kemoviewer_type *) kemo_sgl
{
/* Create a new command buffer for each render pass to the current drawable. */
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];

    commandBuffer.label = @"KemoViewerCommands";
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";
        [self encodeKemoViewers:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:&_monoViewUnites];
        
/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    [commandBuffer commit];
    return;
}

- (id<MTLTexture>) kemoViewEncodeToTexure:(nonnull MTKView *)view
                                 kemoview:(struct kemoviewer_type *) kemo_sgl
                                   unites:(KemoViewUnites *) viewUnites
{
/* Create a new command buffer for each render pass to the current drawable. */
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];

    commandBuffer.label = @"KemoViewerCommands";
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";
        [self encodeKemoViewers:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:viewUnites];

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [_renderEncoder endEncoding];
    }
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return view.currentDrawable.texture;
}

- (id<MTLTexture>_Nonnull) drawKemoViewToTexure:(struct kemoviewer_type *) kemo_sgl
                                      metalView:(nonnull MTKView *)view
                                         unites:(KemoViewUnites *) viewUnites
{
    [self refreshKemoViewMetalBuffers:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];
    id<MTLTexture> _imageOutputTexture = [self kemoViewEncodeToTexure:view
                                                             kemoview:kemo_sgl
                                                               unites:viewUnites];
    return _imageOutputTexture;
}

@end
