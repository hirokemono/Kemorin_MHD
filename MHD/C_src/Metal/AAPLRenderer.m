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
                leftUnites:(KemoViewUnites *) leftViewUnites
              rightUunites:(KemoViewUnites *) rightViewUnites
                   eyeflag:(int) iflag_lr
{
    int iflag_view = kemoview_get_view_type_flag();
    if(iflag_view == VIEW_MAP){
        [_kemo2DRenderer encodeMapObjects:renderEncoder
                                  buffers:kemo_sgl->kemo_buffers
                               projection:&_map_proj_mat];
    }else if(kemoview_get_draw_mode() == SIMPLE_DRAW){
        [_kemo3DRenderer encodeKemoSimpleObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:monoViewUnites];
    }else if(iflag_view == VIEW_STEREO && iflag_lr < 0){
        update_left_projection_struct(kemo_sgl->view_s);
        modify_left_view_by_struct(kemo_sgl->view_s);
        [_kemoRendererTools setTransferMatrices:&_leftViewUnites];
        [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                          unites:leftViewUnites];
        [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:leftViewUnites];
    }else if(iflag_view == VIEW_STEREO && iflag_lr > 0){
        update_right_projection_struct(kemo_sgl->view_s);
        modify_right_view_by_struct(kemo_sgl->view_s);
        [_kemoRendererTools setTransferMatrices:rightViewUnites];
        [_kemoRendererTools setKemoViewLightings:kemo_sgl->kemo_buffers
                                          unites:rightViewUnites];
        [_kemo3DRenderer encodeKemoView3DObjects:renderEncoder
                                           depth:&_depthState
                                        kemoview:kemo_sgl
                                          unites:rightViewUnites];
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
        [self encodeKemoViewers:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:&_monoViewUnites
                     leftUnites:&_leftViewUnites
                   rightUunites:&_rightViewUnites
                        eyeflag:iflag_lr];

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [commandBuffer presentDrawable:view.currentDrawable];
        [_renderEncoder endEncoding];
    }
    [commandBuffer commit];
    return;
}

- (id<MTLTexture>) kemoViewEncodetoTexure:(nonnull MTKView *)view
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
        [self encodeKemoViewers:&_renderEncoder
                       kemoview:kemo_sgl
                         unites:&_monoViewUnites
                     leftUnites:&_leftViewUnites
                   rightUunites:&_rightViewUnites
                        eyeflag:iflag_lr];

/*Schedule a present once the framebuffer is complete using the current drawable. */
        [_renderEncoder endEncoding];
    }
    [commandBuffer commit];
    [commandBuffer waitUntilCompleted];
    return view.currentDrawable.texture;
}

- (id<MTLTexture>_Nonnull) drawKemoViewToTexure:(nonnull MTKView *)view
                                        eyeflag:(int) iflag_lr;
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();

    [self refreshKemoViewMetalBuffers:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];
    id<MTLTexture> _imageOutputTexture = [self kemoViewEncodetoTexure:view
                                                             kemoview:kemo_sgl
                                                              eyeflag:iflag_lr];
    return _imageOutputTexture;
}

-(unsigned char *) getRenderedbyMetalToBGRA:(nonnull MTKView *)view
                                    eyeflag:(int) iflag_lr
                                     Pixels:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte
{
    id<MTLTexture> _imageOutputTexture = [self drawKemoViewToTexure:view
                                                            eyeflag:iflag_lr];

    /*    Texture to render screen to texture */
    pix_xy[0] = _imageOutputTexture.width;
    pix_xy[1] = _imageOutputTexture.height;
    pixelByte[0] = 4;
    NSUInteger bpRaw = pixelByte[0] * pix_xy[0] ;
    NSUInteger num_pixel = pix_xy[0] * pix_xy[1];
    unsigned char *bgra = (unsigned char *) malloc(pixelByte[0]*num_pixel * sizeof(unsigned char));

    [_imageOutputTexture getBytes:bgra
                      bytesPerRow:bpRaw
                       fromRegion:MTLRegionMake2D(0, 0, pix_xy[0], pix_xy[1])
                      mipmapLevel:0];
    return bgra;
};

-(unsigned char *) getAnaglyphbyMetalToBGRA:(nonnull MTKView *)view
                                     Pixels:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte
{
    int i;
    unsigned char *left_bgra =  [self getRenderedbyMetalToBGRA:view
                                                       eyeflag:-1
                                                        Pixels:pix_xy
                                                  PixelPerByte:pixelByte];
    unsigned char *right_bgra = [self getRenderedbyMetalToBGRA:view
                                                       eyeflag: 1
                                                        Pixels:pix_xy
                                                  PixelPerByte:pixelByte];
    
    NSUInteger num_pixel = pix_xy[0] * pix_xy[1];
    unsigned char *bgra = (unsigned char *) malloc(pixelByte[0]*num_pixel * sizeof(unsigned char));
    
    for(i=0;i<num_pixel;i++){
        bgra[4*i  ] = right_bgra[4*i  ];
        bgra[4*i+1] = right_bgra[4*i+1];
        bgra[4*i+2] = left_bgra[4*i+2];
        bgra[4*i+3] = left_bgra[4*i+3];
    };
    free(left_bgra);
    free(right_bgra);
    return bgra;
}

-(void) setAnaglyphToMetalBuffer:(nonnull MTKView *)view
                          Pixels:(unsigned char *) bgra
                          Pixels:(NSUInteger *) pix_xy
                    PixelPerByte:(NSUInteger *) pixelByte
{
    int i;
    NSUInteger num_pixel = pix_xy[0] * pix_xy[1];
    struct line_text_image * anaglyph_img
        = alloc_line_text_image((int) pix_xy[0], (int) pix_xy[1], 20);
    
    for(i=0;i<num_pixel;i++){
        anaglyph_img->imgBMP[4*i+3] = bgra[4*i  ];
        anaglyph_img->imgBMP[4*i+2] = bgra[4*i+1];
        anaglyph_img->imgBMP[4*i+1] = bgra[4*i+2];
        anaglyph_img->imgBMP[4*i  ] = bgra[4*i+3];
    };

    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    
    /*  Vertex buffer to render anaglyph on screen */
    id<MTLBuffer> _Nullable _anaglyphVertice;
    /*  Texture buffer to render anaglyph on screen */
    id<MTLTexture> _Nullable _anaglyphTexure;
    [_kemoMetalBufBase setTextBoxTexture:&_device
                                  buffer:kemo_sgl->kemo_buffers->screen_buf
                                   image:anaglyph_img
                                  vertex:&_anaglyphVertice
                                  texure:&_anaglyphTexure];
    dealloc_line_text_image(anaglyph_img);
    
    
    /* Create a new command buffer for each render pass to the current drawable. */
        id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];

        commandBuffer.label = @"KemoViewerCommands";
    /* Obtain a renderPassDescriptor generated from the view's drawable textures. */
        MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;
        if(renderPassDescriptor != nil){
    /* Create a render command encoder. */
            _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
            _renderEncoder.label = @"MyRenderEncoder";

            [_kemo2DRenderer encodeTextBoxObject:kemo_sgl->kemo_buffers
                                         encoder:&_renderEncoder
                                          vertex:&_anaglyphVertice
                                          texure:&_anaglyphTexure
                                      projection:&_cbar_proj_mat];
    /*Schedule a present once the framebuffer is complete using the current drawable. */
            [commandBuffer presentDrawable:view.currentDrawable];
            [_renderEncoder endEncoding];
        }
        [commandBuffer commit];
    return;
}



- (void)drawKemoMetalView:(nonnull MTKView *)view
                  eyeflag:(int) iflag_lr
{
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();

    [self refreshKemoViewMetalBuffers:&_device
                             kemoview:kemo_sgl];
    [_kemoRendererTools setKemoViewLightsAndViewMatrices:&_monoViewUnites
                                           MsgProjection:&_cbar_proj_mat
                                           MapProjection:&_map_proj_mat];
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
