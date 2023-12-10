/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
#include "draw_colorbar_gl.h"

/* The maximum number of frames in flight. */
static const NSUInteger MaxFramesInFlight = 3;

/* Main class performing the rendering */
@implementation AAPLRenderer
{
    KemoViewMetalBuffers * _kemoMetalBufBase;
    KemoView2DRenderer *   _kemo2DRenderer;
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
    _kemo2DRenderer = [[KemoView2DRenderer alloc] init];
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
        [_kemo2DRenderer add2DShaderLibrary:&defaultLibrary];
        for(i=0;i<MaxFramesInFlight;i++){
            [_kemo3DRenderer[i] addKemoView3DShaderLibrary:&defaultLibrary];
        }

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        [_kemo2DRenderer addKemoView2DPipelines:mtkView
                                    targetPixel:mtkView.colorPixelFormat];
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



- (void) releaseKemoViewMetalBuffers:(NSUInteger) i_current
                            kemoview:(struct kemoviewer_type *) kemo_sgl
{
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_MAP){
/*  Release Map vertexs */
        [_kemo2DRenderer releaseMapMetalBuffers:kemo_sgl->kemo_buffers];
    }else{
/*  Release 3D vertexs */
        [_kemo3DRenderer[i_current] releaseKemoView3DMetalBuffers:kemo_sgl];
/*  Release transparent vertexs */
        [_kemo3DRenderer[i_current] releaseTransparentMetalBuffers:kemo_sgl];
    };
/*  Release Message vertexs */
    [_kemo2DRenderer releaseMsgMetalBuffers:kemo_sgl->kemo_buffers];
    return;
}


- (void) setKemoViewMetalBuffers:(NSUInteger) i_current
                    metalDevice:(id<MTLDevice> *) device
                        kemoview:(struct kemoviewer_type *) kemo_sgl
{
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_MAP){
/*  Set Map vertexs to Metal buffers */
        [_kemo2DRenderer setMapMetalBuffers:device
                                    buffers:kemo_sgl->kemo_buffers];
    }else{
/*  Set 3D vertexs to Metal buffers */
        [_kemo3DRenderer[i_current] setKemoView3DMetalBuffers:device
                                                     kemoview:kemo_sgl];
/*  Set transparent vertexs to Metal buffers */
        [_kemo3DRenderer[i_current] setKemoTransparentMetalBuffers:device
                                                          kemoview:kemo_sgl];
    };
    
/*  Set message vertexs to Metal buffers */
    [_kemo2DRenderer setMessageMetalBuffers:device
                                    buffers:kemo_sgl->kemo_buffers];
    return;
}

- (void)refreshKemoViewMetalBuffers:(NSUInteger) i_current
                        metalDevice:(id<MTLDevice> *) device
                           kemoview:(struct kemoviewer_type *) kemo_sgl
{

    int iflag = kemoview_get_draw_mode(kemo_sgl);
    if(iflag == FULL_DRAW){
        [self releaseKemoViewMetalBuffers:i_current
                                 kemoview:kemo_sgl];
        kemoview_const_buffers(kemo_sgl);
        
        [self setKemoViewMetalBuffers:i_current
                          metalDevice:device
                             kemoview:kemo_sgl];
    }else if(iflag == FAST_DRAW){
        if(kemoview_get_view_type_flag(kemo_sgl) != VIEW_MAP){
            [_kemo3DRenderer[i_current] releaseTransparentMetalBuffers:kemo_sgl];
            
            kemoview_transparent_buffers(kemo_sgl);
            
            [_kemo3DRenderer[i_current] setKemoTransparentMetalBuffers:device
                                                              kemoview:kemo_sgl];
        };
    };
    return;
}

- (void) encodeKemoViewers:(NSUInteger) i_current
                   encoder:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  kemoview:(struct kemoviewer_type *) kemo_sgl
                    unites:(KemoViewUnites *) monoViewUnites
{
    int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
    if(iflag_view == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_map_proj_mat];
    }else if(kemoview_get_draw_mode(kemo_sgl) == SIMPLE_DRAW){
        [_kemo3DRenderer[i_current] encodeKemoSimpleObjects:renderEncoder
                                                      depth:&_depthState
                                                   kemoview:kemo_sgl
                                                     unites:monoViewUnites];
    }else{
        [_kemo3DRenderer[i_current] encodeKemoView3DObjects:renderEncoder
                                                      depth:&_depthState
                                                   kemoview:kemo_sgl
                                                     unites:monoViewUnites];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_cbar_proj_mat];
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
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return view.currentDrawable.texture;
}

- (id<MTLTexture>_Nonnull) drawKemoViewToTexure:(NSUInteger) i_current
                                       kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                                      metalView:(nonnull MTKView *)view
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

-(void) setAnaglyphToMetalBuffer:(id<MTLDevice> *)device
                        kemoview:(struct kemoviewer_type *) kemo_sgl
                         num_pix:(NSUInteger *) pix_xy
                          vertex:(id<MTLBuffer> *) anaglyphVertice
                            left:(id<MTLTexture> *) leftTexure
                           right:(id<MTLTexture> *) rightTexure
{
    [*anaglyphVertice release];
    [*leftTexure  release];
    [*rightTexure  release];
    [_kemoMetalBufBase setAnaglyphTexture:device
                                   buffer:kemo_sgl->kemo_buffers->screen_buf
                                   pixels:pix_xy
                                   vertex:anaglyphVertice
                                     left:leftTexure
                                    right:rightTexure];
    return;
}

-(void) encodeAnaglyphRender:(nonnull MTKView *)view
                    kemoview:(struct kemoviewer_type *) kemo_sgl
                      vertex:(id<MTLBuffer> *) anaglyphVertice
                        left:(id<MTLTexture> *) leftTexure
                       right:(id<MTLTexture> *) rightTexure
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

        [_kemo2DRenderer encodeAnaglyphObjects:&_renderEncoder
                                       buffers:kemo_sgl->kemo_buffers
                                        vertex:anaglyphVertice
                                          left:leftTexure
                                         right:rightTexure
                                    projection:&_cbar_proj_mat];
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

- (id<MTLTexture>) encodeAnaglyphToTexure:(nonnull MTKView *)view
                                 kemoview:(struct kemoviewer_type *) kemo_sgl
                                   vertex:(id<MTLBuffer> *) anaglyphVertice
                                     left:(id<MTLTexture> *) leftTexure
                                    right:(id<MTLTexture> *) rightTexure
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

        [_kemo2DRenderer encodeAnaglyphObjects:&_renderEncoder
                                       buffers:kemo_sgl->kemo_buffers
                                        vertex:anaglyphVertice
                                          left:leftTexure
                                         right:rightTexure
                                    projection:&_cbar_proj_mat];
/*Schedule a present once the framebuffer is complete using the current drawable. */
        [_renderEncoder endEncoding];
    }
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return view.currentDrawable.texture;
}

-(void) setKemoAnaglyphUnites:(nonnull MTKView *)view
                     kemoview:(struct kemoviewer_type *) kemo_sgl
                   leftunites:(KemoViewUnites *) leftUnites
                  rightunites:(KemoViewUnites *) rightUnites
{
    kemoview_left_viewmatrix(kemo_sgl);
    [_kemoRendererTools setTransferMatrices:kemo_sgl
                                     unites:leftUnites];
    [_kemoRendererTools setKemoViewLightings:kemo_sgl
                                      unites:leftUnites];
    
    kemoview_right_viewmatrix(kemo_sgl);
    [_kemoRendererTools setTransferMatrices:kemo_sgl
                                     unites:rightUnites];
    [_kemoRendererTools setKemoViewLightings:kemo_sgl
                                      unites:rightUnites];
}

-(void) KemoViewRenderAnaglyph:(NSUInteger) i_current
                     metalView:(nonnull MTKView *)view
                      kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSUInteger pix_xy[2];
    if(kemo_sgl->kemo_buffers->screen_buf->num_nod_buf > 0){

        pix_xy[0] = view.drawableSize.width;
        pix_xy[1] = view.drawableSize.height;
        [self setKemoAnaglyphUnites:view
                           kemoview:kemo_sgl
                         leftunites:&_leftViewUnites
                        rightunites:&_rightViewUnites];
        [self setAnaglyphToMetalBuffer:&_device
                              kemoview:kemo_sgl
                               num_pix:pix_xy
                                vertex:&_anaglyphVertice
                                  left:&_leftTexure
                                 right:&_rightTexure];

        id<MTLTexture> _imageOutputTexture;
        _imageOutputTexture = [self drawKemoViewToTexure:i_current
                                                kemoview:kemo_sgl
                                               metalView:view
                                                  unites:&_leftViewUnites];
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTexture
                                               target:&_leftTexure];
        _imageOutputTexture = [self drawKemoViewToTexure:i_current
                                                kemoview:kemo_sgl
                                               metalView:view
                                                  unites:&_rightViewUnites];
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTexture
                                               target:&_rightTexure];


    }
    [self encodeAnaglyphRender:view
                      kemoview:kemo_sgl
                        vertex:&_anaglyphVertice
                          left:&_leftTexure
                         right:&_rightTexure];
    return;
};

-(id<MTLTexture>) KemoViewAnaglyphToTexure:(nonnull MTKView *)view
                                  kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSUInteger pix_xy[2];
    id<MTLTexture> _imageOutputTexture;
    if(kemo_sgl->kemo_buffers->screen_buf->num_nod_buf > 0){
        pix_xy[0] = view.drawableSize.width;
        pix_xy[1] = view.drawableSize.height;
        
        [self setKemoAnaglyphUnites:view
                           kemoview:kemo_sgl
                         leftunites:&_leftViewUnites
                        rightunites:&_rightViewUnites];
        [self setAnaglyphToMetalBuffer:&_device
                              kemoview:kemo_sgl
                               num_pix:pix_xy
                                vertex:&_anaglyphVertice
                                  left:&_leftTexure
                                 right:&_rightTexure];

        _imageOutputTexture = [self drawKemoViewToTexure:kemo_sgl
                                               metalView:view
                                                  unites:&_leftViewUnites];
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTexture
                                               target:&_leftTexure];

        _imageOutputTexture = [self drawKemoViewToTexure:kemo_sgl
                                               metalView:view
                                                  unites:&_rightViewUnites];
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTexture
                                               target:&_rightTexure];
    }
    _imageOutputTexture = [self encodeAnaglyphToTexure:view
                                              kemoview:kemo_sgl
                                              vertex:&_anaglyphVertice
                                                left:&_leftTexure
                                               right:&_rightTexure];
    return _imageOutputTexture;
};

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
    if(iflag == VIEW_STEREO){
        [self KemoViewRenderAnaglyph:_currentBuffer
                           metalView:view
                            kemoview:kemo_sgl];
    }else{
        [self KemoViewEncodeAll:_currentBuffer
                      metalView:view
                       kemoview:kemo_sgl];
    };
    return;
}

- (id<MTLTexture>_Nonnull)KemoViewToTexure:(nonnull MTKView *)view
                                  kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
   [_kemoRendererTools setKemoViewLightsAndViewMatrices:kemo_sgl
                                              ModelView:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];
    id<MTLTexture> _imageOutputTexture;
    int iflag = kemoview_get_view_type_flag(kemo_sgl);
    if(iflag == VIEW_STEREO){
        _imageOutputTexture = [self KemoViewAnaglyphToTexure:view
                                                  kemoview:kemo_sgl];
    }else{
        _imageOutputTexture = [self kemoViewEncodeToTexure:ZERO
                                                 metalView:view
                                                  kemoview:kemo_sgl
                                                    unites:&_monoViewUnites];
    };
    return _imageOutputTexture;
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
