/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
#include "draw_colorbar_gl.h"

// Main class performing the rendering
@implementation AAPLRenderer
{

    id<MTLDevice> _device;

    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    // Combined depth and stencil state object.
    id<MTLDepthStencilState> _depthState;
    id<MTLDepthStencilState> _depthState2;
    id<MTLDepthStencilState> _noDepthState;

    id<MTLRenderCommandEncoder> _renderEncoder;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    
    KemoViewUnites _monoViewUnites;
    KemoViewUnites _rightViewUnites;
    KemoViewUnites _leftViewUnites;

    matrix_float4x4 _map_proj_mat;
    matrix_float4x4 _cbar_proj_mat;

    NSUInteger _frameNum;
    
    IBOutlet KemoViewerObject * _singleKemoView;
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    _kemoRendererTools = [[KemoViewRendererTools alloc] init];
    _kemoMetalBufBase = [[KemoViewMetalBuffers alloc] init];
    _kemo2DRenderer = [[KemoView2DRenderer alloc] init];
    _kemo3DRenderer = [[KemoView3DRenderer alloc] init];

    _frameNum = 0;
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

        depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = YES;
        _depthState2 = [_device newDepthStencilStateWithDescriptor:depthDescriptor];

        depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = NO;
        _noDepthState = [_device newDepthStencilStateWithDescriptor:depthDescriptor];
    }
/* Create the command queue */
    _commandQueue = [_device newCommandQueue];
    return self;
}



- (void) releaseKemoViewMetalBuffers:(struct kemoviewer_type *) kemo_sgl
{
    if(kemoview_get_view_type_flag() == VIEW_MAP){
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
    if(kemoview_get_view_type_flag() == VIEW_MAP){
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
    if(kemoview_get_draw_mode() == FULL_DRAW){
        [self releaseKemoViewMetalBuffers:kemo_sgl];
        
        kemoview_const_buffers(kemo_sgl);
        
        [self setKemoViewMetalBuffers:device
                             kemoview:kemo_sgl];
    }else if(kemoview_get_draw_mode() == FAST_DRAW){
        if(kemoview_get_view_type_flag() != VIEW_MAP){
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
                   eyeflag:(int) iflag_lr
{
    int iflag_view = kemoview_get_view_type_flag();
    if(iflag_view == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_map_proj_mat];
    } else if(iflag_view == VIEW_STEREO){
        [_kemoRendererTools set2dProjectionMatrices:&_cbar_proj_mat
                                      MapProjection:&_map_proj_mat];
        if(iflag_lr < 0){
            update_left_projection_struct(kemo_sgl->view_s);
            modify_left_view_by_struct(kemo_sgl->view_s);
            [_kemoRendererTools setTransferMatrices:&_leftViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                              unites:&_leftViewUnites];
            [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                               depth:&_depthState
                                            kemoview:kemo_sgl
                                              unites:&_leftViewUnites];
        }else if(iflag_lr > 0){
            update_right_projection_struct(kemo_sgl->view_s);
            modify_right_view_by_struct(kemo_sgl->view_s);
            [_kemoRendererTools setTransferMatrices:&_rightViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                              unites:&_rightViewUnites];
            [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                               depth:&_depthState
                                            kemoview:kemo_sgl
                                              unites:&_rightViewUnites];
        }else{
            update_left_projection_struct(kemo_sgl->view_s);
            modify_left_view_by_struct(kemo_sgl->view_s);
            [_kemoRendererTools setTransferMatrices:&_leftViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                              unites:&_leftViewUnites];
            [_kemoRendererTools leftMaterialParams:&_leftViewUnites.material];
            [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                               depth:&_depthState
                                            kemoview:kemo_sgl
                                              unites:&_leftViewUnites];

            [_kemoRendererTools setTransferMatrices:&_rightViewUnites];
            [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                              unites:&_rightViewUnites];
            [_kemoRendererTools rightMaterialParams:&_rightViewUnites.material];
            [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                               depth:&_depthState2
                                            kemoview:kemo_sgl
                                              unites:&_rightViewUnites];
        }
    } else {
        [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:monoViewUnites];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_cbar_proj_mat];
    return;
}

- (void) encodeSimpleView:(id<MTLRenderCommandEncoder>  *) renderEncoder
                 kemoview:(struct kemoviewer_type *) kemo_sgl
                   unites:(KemoViewUnites *) monoViewUnites
{
    if(kemoview_get_view_type_flag() == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_map_proj_mat];
    }else{
        [_kemo3DRenderer encodeKemoSimpleObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:monoViewUnites];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_cbar_proj_mat];
    return;
}

-(void) kemoViewEncodeAll:(nonnull MTKView *)view
                 kemoview:(struct kemoviewer_type *) kemo_sgl
                  eyeflag:(int) iflag_lr
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

        if(kemoview_get_draw_mode() == SIMPLE_DRAW){
            [self encodeSimpleView:&_renderEncoder
                          kemoview:kemo_sgl
                            unites:&_monoViewUnites];
        }else{
            [self encodeKemoViewers:&_renderEncoder
                           kemoview:kemo_sgl
                             unites:&_monoViewUnites
                            eyeflag:iflag_lr];
        };

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    [commandBuffer  commit];
    [commandBuffer waitUntilCompleted];
    return;
}

- (void)drawKemoMetalView:(nonnull MTKView *)view
                  eyeflag:(int) iflag_lr
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();

    [self refreshKemoViewMetalBuffers:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                      unites:&_monoViewUnites];

    [_kemoRendererTools set2dProjectionMatrices:&_cbar_proj_mat
                                  MapProjection:&_map_proj_mat];
    [_kemoRendererTools setTransferMatrices:&_monoViewUnites];

    [self kemoViewEncodeAll:view
                   kemoview:kemo_sgl
                    eyeflag:iflag_lr];
    return;
}

// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    [self drawKemoMetalView:view
                    eyeflag:0];
    return;
}

// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
