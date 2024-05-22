/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "KemoViewerRenderer.h"
#include "draw_colorbar_gl.h"

/* The maximum number of frames in flight. */
static const NSUInteger MaxFramesInFlight = 3;

/* Main class performing the rendering */
@implementation KemoViewerRenderer
{
    KemoViewMetalBuffers * _kemoMetalBufBase;
    KemoView3DRenderer *   _kemo3DRenderer[MaxFramesInFlight];
    KemoViewRendererTools * _kemoRendererTools;

    id<MTLDevice> _device;

/* A semaphore used to ensure that buffers read by the GPU are not simultaneously written by the CPU. */
    dispatch_semaphore_t _inFlightSemaphore;
/* The index of the Metal buffer in _vertexBuffers to write to for the current frame. */
    NSUInteger _currentBuffer;

/* The command queue used to pass commands to the device. */
    id<MTLCommandQueue> _commandQueue;
/* The command encoder for the device. */
    id<MTLRenderCommandEncoder> _renderEncoder;
/* Combined depth and stencil state object. */
    id<MTLDepthStencilState> _depthState;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    
    KemoViewUnites _monoViewUnites;
    KemoViewUnites _rightViewUnites;
    KemoViewUnites _leftViewUnites;

    matrix_float4x4 _map_proj_mat;
    matrix_float4x4 _cbar_proj_mat;

    /*  Vertex buffer to render anaglyph on screen */
    id<MTLBuffer> _Nullable _anaglyphVertice;
    /*  Texture buffer to render anaglyph on screen */
    id<MTLTexture> _Nullable _leftTexure;
    id<MTLTexture> _Nullable _rightTexure;

    struct kemoviewer_type *_kemoMetal;
}

- (void) setKemoViewPointer:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    _kemoMetal = kemo_sgl;
    return;
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    int i;
    
    _kemoRendererTools = [[KemoViewRendererTools alloc] init];
    _kemoMetalBufBase = [[KemoViewMetalBuffers alloc] init];
    for(i=0;i<MaxFramesInFlight;i++){
        _kemo3DRenderer[i] = [[KemoView3DRenderer alloc] init];
    };
    
    self = [super init];
    if(self)
    {
        _device = mtkView.device;

        _inFlightSemaphore = dispatch_semaphore_create(MaxFramesInFlight);

/* Indicate that each pixel in the depth buffer is a 32-bit floating point value. */
        mtkView.depthStencilPixelFormat = MTLPixelFormatDepth32Float;

/* Load all the shader files with a .metal file extension in the project. */
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];
        for(i=0;i<MaxFramesInFlight;i++){
            [_kemo3DRenderer[i] addKemoView3DShaderLibrary:&defaultLibrary];
        }

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        for(i=0;i<MaxFramesInFlight;i++){
            [_kemo3DRenderer[i] addKemoView3DPipelines:mtkView
                                           targetPixel:mtkView.colorPixelFormat];
        };

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



- (void) releaseKemoViewMetalBuffers:(KemoView3DRenderer *) kemo3DRenderer
                            viewflag:(int) iflag_view
{
/*  Release 3D vertexs */
    [kemo3DRenderer releaseKemoView3DMetalBuffers];
    return;
}


- (void) setKemoViewMetalBuffers:(KemoView3DRenderer *) kemo3DRenderer
                    metalDevice:(id<MTLDevice> *) device
                        kemoview:(struct kemoviewer_type *) kemo_sgl
                        viewflag:(int) viewflag
{
/*  Set 3D vertexs to Metal buffers */
    [kemo3DRenderer setKemoView3DMetalBuffers:device
                                     kemoview:kemo_sgl];
    return;
}

- (void)refreshTripleBuffers:(NSUInteger) i_current
                 metalDevice:(id<MTLDevice> *) device
                    kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int i, i_update;
    for(i=1;i<MaxFramesInFlight;i++){
        i_update = (i_current + i) % MaxFramesInFlight;
        [_kemo3DRenderer[i_update] releaseKemoView3DMetalBuffers];
        
        [_kemo3DRenderer[i_update] setKemoView3DMetalBuffers:device
                                                    kemoview:kemo_sgl];
    }
    return;
}



- (void)refreshKemoViewMetalBuffers:(NSUInteger) i_current
                        metalDevice:(id<MTLDevice> *) device
                           kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int iflag = kemoview_get_draw_mode(kemo_sgl);
    int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
    if(iflag == MOVIE_DRAW){
        kemoview_fast_buffers(kemo_sgl);
    }else{
        [self releaseKemoViewMetalBuffers:_kemo3DRenderer[i_current]
                                 viewflag:iflag_view];
        
        kemoview_const_buffers(kemo_sgl);
        
        [self setKemoViewMetalBuffers:_kemo3DRenderer[i_current]
                          metalDevice:device
                             kemoview:kemo_sgl
                             viewflag:iflag_view];

        if(iflag == TRIPLE_UPDATE){
            [self refreshTripleBuffers:i_current
                           metalDevice:device
                              kemoview:kemo_sgl];
       }
    };
    return;
}

- (void) encodeKemoViewers:(NSUInteger) i_current
                   encoder:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  kemoview:(struct kemoviewer_type *) kemo_sgl
                    unites:(KemoViewUnites *) monoViewUnites
{
    int iflag_polygon = kemoview_get_object_property_flags(kemo_sgl, POLYGON_SWITCH);
    int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
    [_kemo3DRenderer[i_current] encodeKemoSimpleObjects:renderEncoder
                                                  depth:&_depthState
                                                 unites:monoViewUnites
                                                  sides:iflag_polygon];
}

-(void) KemoViewEncodeAll:(NSUInteger) i_current
                metalView:(nonnull MTKView *)view
                 kemoview:(struct kemoviewer_type *) kemo_sgl
{
/*
    Wait to ensure only `MaxFramesInFlight` number of frames are getting processed
    by any stage in the Metal pipeline (CPU, GPU, Metal, Drivers, etc.).
*/
    dispatch_semaphore_wait(_inFlightSemaphore, DISPATCH_TIME_FOREVER);

/* Create a new command buffer for each render pass to the current drawable. */
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];

    commandBuffer.label = @"KemoViewerCommands";
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";
        [self encodeKemoViewers:i_current
                        encoder:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:&_monoViewUnites];
        
/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
/*
    Add a completion handler that signals `_inFlightSemaphore` when Metal and the GPU have fully
    finished processing the commands that were encoded for this frame.
    This completion indicates that the dynamic buffers that were written-to in this frame, are no
    longer needed by Metal and the GPU; therefore, the CPU can overwrite the buffer contents
    without corrupting any rendering operations.
*/
    __block dispatch_semaphore_t block_semaphore = _inFlightSemaphore;
    [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> buffer)
     {
         dispatch_semaphore_signal(block_semaphore);
     }];

    [commandBuffer commit];
    return;
}

- (id<MTLTexture>) kemoViewEncodeToTexure:(NSUInteger) i_current
                                metalView:(nonnull MTKView *)view
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
        [self encodeKemoViewers:i_current
                        encoder:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:viewUnites];

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [_renderEncoder endEncoding];
    }
    // Add a completion handler and commit the command buffer.
    [commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> cb) {
        // Populate private buffer.
    }];
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return view.currentDrawable.texture;
}

- (id<MTLTexture>_Nonnull) drawKemoViewToTexure:(NSUInteger) i_current
                                      metalView:(nonnull MTKView *)view
                                       kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                                         unites:(KemoViewUnites *_Nonnull) viewUnites
{
    [self refreshKemoViewMetalBuffers:i_current
                          metalDevice:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:kemo_sgl
                                               ModelView:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];
    id<MTLTexture> _imageOutputTexture = [self kemoViewEncodeToTexure:i_current
                                                            metalView:view
                                                             kemoview:kemo_sgl
                                                               unites:viewUnites];
    return _imageOutputTexture;
}

- (void)drawKemoMetalView:(nonnull MTKView *)view
                 kemoview:(struct kemoviewer_type *) kemo_sgl
{
/*
  Iterate through the Metal buffers, and cycle back to the first when you've written to the last.
*/
    _currentBuffer = (_currentBuffer + 1) % MaxFramesInFlight;

    [self refreshKemoViewMetalBuffers:_currentBuffer
                          metalDevice:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:kemo_sgl
                                               ModelView:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];

    int iflag = kemoview_get_view_type_flag(kemo_sgl);
    [self KemoViewEncodeAll:_currentBuffer
                  metalView:view
                   kemoview:kemo_sgl];
    return;
}

// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    [self drawKemoMetalView:view
                   kemoview:_kemoMetal];
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
