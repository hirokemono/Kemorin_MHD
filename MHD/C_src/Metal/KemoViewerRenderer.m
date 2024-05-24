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



- (void) releaseKemoViewMetalBuffers:(KemoView3DRenderer *) kemo3DRenderer
                            viewflag:(int) iflag_view
{
    if(iflag_view == VIEW_MAP){
/*  Release Map vertexs */
        [_kemo2DRenderer releaseMapMetalBuffers];
    }else{
/*  Release 3D vertexs */
        [kemo3DRenderer releaseKemoView3DMetalBuffers];
    };
/*  Release Message vertexs */
    [_kemo2DRenderer releaseMsgMetalBuffers];
    return;
}


- (void) setKemoViewMetalBuffers:(KemoView3DRenderer *) kemo3DRenderer
                    metalDevice:(id<MTLDevice> *) device
                        kemoview:(struct kemoviewer_type *) kemo_sgl
                        viewflag:(int) viewflag
{
    if(viewflag == VIEW_MAP){
/*  Set Map vertexs to Metal buffers */
        [_kemo2DRenderer setMapMetalBuffers:device
                                    buffers:kemo_sgl->kemo_buffers];
    }else{
/*  Set 3D vertexs to Metal buffers */
        [kemo3DRenderer setKemoView3DMetalBuffers:device
                                         kemoview:kemo_sgl];
    };
    
/*  Set message vertexs to Metal buffers */
    [_kemo2DRenderer setMessageMetalBuffers:device
                                    buffers:kemo_sgl->kemo_buffers];
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
    int iflag = kemoview_get_view_integer(kemo_sgl, ISET_DRAW_MODE);
    int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
    if(iflag == SIMPLE_DRAW && iflag_view != VIEW_MAP){
        [_kemo3DRenderer[i_current] releaseKemoFastMetalBuffers];
        kemoview_fast_buffers(kemo_sgl);
        [_kemo3DRenderer[i_current] setKemoFastMetalBuffers:device
                                                   kemoview:kemo_sgl];
    }else if(iflag == MOVIE_DRAW && iflag_view != VIEW_MAP){
        [_kemo3DRenderer[i_current] releaseKemoMovieMetalBuffers];
        kemoview_fast_buffers(kemo_sgl);
        [_kemo3DRenderer[i_current] setKemoMovieMetalBuffers:device
                                                    kemoview:kemo_sgl];
    }else if(iflag == QUILT_DRAW && iflag_view != VIEW_MAP){
        [_kemo3DRenderer[i_current] releaseTransparentMetalBuffers];
        kemoview_transparent_buffers(kemo_sgl);
        [_kemo3DRenderer[i_current] setKemoTransparentMetalBuffers:device
                                                          kemoview:kemo_sgl];
    }else{
        [self releaseKemoViewMetalBuffers:_kemo3DRenderer[i_current]
                                 viewflag:iflag_view];
        
        kemoview_const_buffers(kemo_sgl);
        
        [self setKemoViewMetalBuffers:_kemo3DRenderer[i_current]
                          metalDevice:device
                             kemoview:kemo_sgl
                             viewflag:iflag_view];

        [self refreshTripleBuffers:i_current
                           metalDevice:device
                              kemoview:kemo_sgl];
    };
    return;
}

- (void)refreshKemoViewTripleBuffers:(struct kemoviewer_type *) kemo_sgl
{
    [self refreshTripleBuffers:_currentBuffer
                   metalDevice:&_device
                      kemoview:kemo_sgl];
    return;
}


- (void) encodeKemoViewers:(NSUInteger) i_current
                   encoder:(id<MTLRenderCommandEncoder>  *) renderEncoder
                  kemoview:(struct kemoviewer_type *) kemo_sgl
                    unites:(KemoViewUnites *) monoViewUnites
{
    int iflag_polygon = kemoview_get_object_property_flags(kemo_sgl, POLYGON_SWITCH);
    int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
    if(iflag_view == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                               projection:&_map_proj_mat];
    }else if(kemoview_get_view_integer(kemo_sgl, ISET_DRAW_MODE) == SIMPLE_DRAW){
        [_kemo3DRenderer[i_current] encodeKemoSimpleObjects:renderEncoder
                                                      depth:&_depthState
                                                     unites:monoViewUnites
                                                      sides:iflag_polygon];
    }else{
        [_kemo3DRenderer[i_current] encodeKemoView3DObjects:renderEncoder
                                                      depth:&_depthState
                                                     unites:monoViewUnites
                                                      sides:iflag_polygon];
    };
    [_kemo2DRenderer encodeMessageObjects:renderEncoder
                               projection:&_cbar_proj_mat];
}

-(void) KemoViewEncodeAll:(NSUInteger) i_current
                metalView:(nonnull MTKView *)view
                 kemoview:(struct kemoviewer_type *) kemo_sgl
            commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [*commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";
        [self encodeKemoViewers:i_current
                        encoder:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:&_monoViewUnites];
        
/*Schedule a present once the framebuffer is complete using the current drawable. */
        [*commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    return;
}

- (void) kemoViewEncodeToTexure:(NSUInteger) i_current
                      metalView:(nonnull MTKView *)view
                       kemoview:(struct kemoviewer_type *) kemo_sgl
                         unites:(KemoViewUnites *) viewUnites
                  commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [*commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";
        [self encodeKemoViewers:i_current
                        encoder:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:viewUnites];

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [_renderEncoder endEncoding];
    }
    // Add a completion handler and commit the command buffer.
    [*commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> cb) {
        // Populate private buffer.
    }];
    return;
}

- (void) drawKemoViewToTexure:(NSUInteger) i_current
                    metalView:(nonnull MTKView *)view
                     kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
                       unites:(KemoViewUnites *_Nonnull) viewUnites
                commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
    [self refreshKemoViewMetalBuffers:i_current
                          metalDevice:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:kemo_sgl
                                               ModelView:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];
   [self kemoViewEncodeToTexure:i_current
                       metalView:view
                        kemoview:kemo_sgl
                          unites:viewUnites
                   commandbuffer:commandBuffer];
    return;
}

-(void) encodeAnaglyphRender:(nonnull MTKView *) view
                   numVertex:(NSUInteger) numVertex
                      vertex:(id<MTLBuffer> *) anaglyphVertice
                        left:(id<MTLTexture> *) leftTexure
                       right:(id<MTLTexture> *) rightTexure
               commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
    /* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if(renderPassDescriptor != nil){
    /* Create a render command encoder. */
        _renderEncoder = [*commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";

        [_kemo2DRenderer encodeAnaglyphObjects:&_renderEncoder
                                     numVertex:numVertex
                                        vertex:anaglyphVertice
                                          left:leftTexure
                                         right:rightTexure
                                    projection:&_cbar_proj_mat];
    /*Schedule a present once the framebuffer is complete using the current drawable. */
        [*commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    return;
}

- (void) encodeAnaglyphToTexure:(nonnull MTKView *)view
                      numVertex:(NSUInteger) numVertex
                         vertex:(id<MTLBuffer> *) anaglyphVertice
                           left:(id<MTLTexture> *) leftTexure
                          right:(id<MTLTexture> *) rightTexure
                  commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
/* Obtain a renderPassDescriptor generated from the view's drawable textures. */
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
    if(renderPassDescriptor != nil){
/* Create a render command encoder. */
        _renderEncoder = [*commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";

        [_kemo2DRenderer encodeAnaglyphObjects:&_renderEncoder
                                     numVertex:numVertex
                                        vertex:anaglyphVertice
                                          left:leftTexure
                                         right:rightTexure
                                    projection:&_cbar_proj_mat];
/*Schedule a present once the framebuffer is complete using the current drawable. */
        [_renderEncoder endEncoding];
    }
    // Add a completion handler and commit the command buffer.
    [*commandBuffer addCompletedHandler:^(id<MTLCommandBuffer> cb) {
        // Populate private buffer.
    }];
    return;
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
                 commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
    NSUInteger pix_xy[2];
    NSUInteger numScreenBuf = kemo_sgl->kemo_buffers->screen_buf->num_nod_buf;
    if(numScreenBuf > 0){
        pix_xy[0] = view.drawableSize.width;
        pix_xy[1] = view.drawableSize.height;
        [self setKemoAnaglyphUnites:view
                           kemoview:kemo_sgl
                         leftunites:&_leftViewUnites
                        rightunites:&_rightViewUnites];
        [_anaglyphVertice release];
        [_leftTexure  release];
        [_rightTexure  release];

        [_kemoMetalBufBase setAnaglyphTexture:&_device
                                       buffer:kemo_sgl->kemo_buffers->screen_buf
                                       pixels:pix_xy
                                       vertex:&_anaglyphVertice
                                         left:&_leftTexure
                                        right:&_rightTexure];

/* Create a new command buffer for each render pass to the current drawable. */
        id<MTLCommandBuffer> commandBufferLeft = [_commandQueue commandBuffer];
        commandBufferLeft.label = @"KemoViewerDrawLeftViewCommands";
        [self drawKemoViewToTexure:i_current
                         metalView:view
                          kemoview:kemo_sgl
                            unites:&_leftViewUnites
                     commandbuffer:&commandBufferLeft];
        [commandBufferLeft commit];
        [commandBufferLeft waitUntilCompleted];
        id<MTLTexture> _imageOutputTextureLeft = view.currentDrawable.texture;
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTextureLeft
                                               target:&_leftTexure];
        
        /* Create a new command buffer for each render pass to the current drawable. */
        id<MTLCommandBuffer> commandBufferRight = [_commandQueue commandBuffer];
        commandBufferRight.label = @"KemoViewerDrawRightViewCommands";
        [self drawKemoViewToTexure:i_current
                         metalView:view
                          kemoview:kemo_sgl
                            unites:&_rightViewUnites
                     commandbuffer:&commandBufferRight];
        [commandBufferRight commit];
        [commandBufferRight waitUntilCompleted];
        id<MTLTexture> _imageOutputTextureRight = view.currentDrawable.texture;
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTextureRight
                                               target:&_rightTexure];
    }

/* Encode a command buffer for anaglyph render pass to the current drawable. */
    [self encodeAnaglyphRender:view
                     numVertex:numScreenBuf
                        vertex:&_anaglyphVertice
                          left:&_leftTexure
                         right:&_rightTexure
                 commandbuffer:commandBuffer];
    return;
};

-(void) KemoViewAnaglyphToTexure:(NSUInteger) i_current
                       metalView:(nonnull MTKView *)view
                        kemoview:(struct kemoviewer_type *) kemo_sgl
                   commandbuffer:(id<MTLCommandBuffer> *) commandBuffer
{
    NSUInteger pix_xy[2];
    NSUInteger numScreenBuf = kemo_sgl->kemo_buffers->screen_buf->num_nod_buf;
    if(numScreenBuf > 0){
        pix_xy[0] = view.drawableSize.width;
        pix_xy[1] = view.drawableSize.height;
        
        [self setKemoAnaglyphUnites:view
                           kemoview:kemo_sgl
                         leftunites:&_leftViewUnites
                        rightunites:&_rightViewUnites];
        [_anaglyphVertice release];
        [_leftTexure  release];
        [_rightTexure  release];

        [_kemoMetalBufBase setAnaglyphTexture:&_device
                                       buffer:kemo_sgl->kemo_buffers->screen_buf
                                       pixels:pix_xy
                                       vertex:&_anaglyphVertice
                                         left:&_leftTexure
                                        right:&_rightTexure];

        /* Create a new command buffer for each render pass to the current drawable. */
        id<MTLCommandBuffer> commandBufferLeft = [_commandQueue commandBuffer];
        commandBufferLeft.label = @"KemoViewerDrawLeftViewCommands";
        [self drawKemoViewToTexure:i_current
                         metalView:view
                          kemoview:kemo_sgl
                            unites:&_leftViewUnites
                     commandbuffer:&commandBufferLeft];
        [commandBufferLeft commit];
        [commandBufferLeft waitUntilCompleted];
        id<MTLTexture> _imageOutputTextureLeft = view.currentDrawable.texture;
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTextureLeft
                                               target:&_leftTexure];

        /* Create a new command buffer for each render pass to the current drawable. */
        id<MTLCommandBuffer> commandBufferRight = [_commandQueue commandBuffer];
        commandBufferRight.label = @"KemoViewerDrawRightViewCommands";
        [self drawKemoViewToTexure:i_current
                         metalView:view
                          kemoview:kemo_sgl
                            unites:&_rightViewUnites
                     commandbuffer:&commandBufferRight];
        [commandBufferRight commit];
        [commandBufferRight waitUntilCompleted];
        id<MTLTexture> _imageOutputTextureRight = view.currentDrawable.texture;
        [_kemoRendererTools encodeCopyTexureToPrivate:&_commandQueue
                                              num_pix:pix_xy
                                               source:&_imageOutputTextureRight
                                               target:&_rightTexure];
    }

    [self encodeAnaglyphToTexure:view
                       numVertex:numScreenBuf
                          vertex:&_anaglyphVertice
                            left:&_leftTexure
                           right:&_rightTexure
                   commandbuffer:commandBuffer];
    return;
};

- (void)drawKemoMetalView:(nonnull MTKView *)view
                 kemoview:(struct kemoviewer_type *) kemo_sgl
{
/*
  Iterate through the Metal buffers, and cycle back to the first when you've written to the last.
*/
    _currentBuffer = (_currentBuffer + 1) % MaxFramesInFlight;
/*
                 Wait to ensure only `MaxFramesInFlight` number of frames are getting processed
                 by any stage in the Metal pipeline (CPU, GPU, Metal, Drivers, etc.).
*/
    dispatch_semaphore_wait(_inFlightSemaphore, DISPATCH_TIME_FOREVER);
/* Create a new command buffer for anaglyph render pass to the current drawable. */
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];

    [self refreshKemoViewMetalBuffers:_currentBuffer
                          metalDevice:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:kemo_sgl
                                               ModelView:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];

    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_STEREO){
        commandBuffer.label = @"KemoViewerAnaglyphCommands";
        [self KemoViewRenderAnaglyph:_currentBuffer
                           metalView:view
                            kemoview:kemo_sgl
                       commandbuffer:&commandBuffer];
    }else{
        commandBuffer.label = @"KemoViewerCommands";
        [self KemoViewEncodeAll:_currentBuffer
                      metalView:view
                       kemoview:kemo_sgl
                  commandbuffer:&commandBuffer];
    };
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

- (id<MTLTexture>_Nonnull)KemoViewToTexure:(nonnull MTKView *)view
                                  kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl
{
    _currentBuffer = (_currentBuffer + 1) % MaxFramesInFlight;

/* Create a new command buffer for each render pass to the current drawable. */
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];

    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_STEREO){
        commandBuffer.label = @"KemoViewerAnaglyphImageCommands";
        [self KemoViewAnaglyphToTexure:_currentBuffer
                             metalView:view
                              kemoview:kemo_sgl
                         commandbuffer:&commandBuffer];
    }else{
        commandBuffer.label = @"KemoViewerDrawTextureCommands";
        [self drawKemoViewToTexure:_currentBuffer
                         metalView:view
                          kemoview:kemo_sgl
                            unites:&_monoViewUnites
                     commandbuffer:&commandBuffer];
    };
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return view.currentDrawable.texture;
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
