/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Implementation of a platform independent renderer class, which performs Metal setup and per frame rendering
*/

@import simd;
@import MetalKit;

#import "AAPLRenderer.h"
#include "m_gl_transfer_matrix.h"
#include "draw_messages_gl.h"

// Main class performing the rendering
@implementation AAPLRenderer
{
    id<MTLDevice> _device;
    
    // The render pipeline generated from the vertex and fragment shaders in the .metal shader file.
    id<MTLRenderPipelineState> _pipelineState[4];
    
    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    id<MTLFunction> _vertexFunction[3];
    id<MTLFunction> _fragmentFunction[3];

    // The Metal buffer that holds the vertex data.
    id<MTLBuffer> _vertices[14];
    // The Metal texture object
    id<MTLTexture> _texture[7];
    
    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    float           _scalechange;
    matrix_float4x4 _projection_mat;
    matrix_float4x4 _map_proj_mat;

    NSUInteger _frameNum;
    
    
    IBOutlet KemoViewerObject * _singleKemoView;
}

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView
{
    _frameNum = 0;
    self = [super init];
    if(self)
    {
        NSError *error;

        _device = mtkView.device;

        // Load all the shader files with a .metal file extension in the project.
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];

        _vertexFunction[0] =   [defaultLibrary newFunctionWithName:@"Base2dVertexShader"];
        _fragmentFunction[0] = [defaultLibrary newFunctionWithName:@"Base2DfragmentShader"];

        _vertexFunction[1] =   [defaultLibrary newFunctionWithName:@"Simple2dVertexShader"];
        _fragmentFunction[1] = [defaultLibrary newFunctionWithName:@"Simple2DfragmentShader"];

        _vertexFunction[2] =   [defaultLibrary newFunctionWithName:@"Texture2dVertexShader"];
        _fragmentFunction[2] = [defaultLibrary newFunctionWithName:@"sampling2dShader"];

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        MTLRenderPipelineDescriptor *pipelineStateDescriptor;
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"2D Texture Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[2];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[2];
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
        _pipelineState[2] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
/* Pipeline State creation could fail if the pipeline descriptor isn't set up properly.
//  If the Metal API validation is enabled, you can find out more information about what
//  went wrong.  (Metal API validation is enabled by default when a debug build is run
//  from Xcode.) */
        NSAssert(_pipelineState[2], @"Failed to create pipeline state: %@", error);

/*  Create pipeline for simple 2D rendering */
        pipelineStateDescriptor.label = @"2D transpearent Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[1];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[1];
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        
        _pipelineState[1] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[1], @"Failed to create pipeline state: %@", error);

        /*  Create pipeline for simple 2D rendering */
        pipelineStateDescriptor.label = @"2D Simple Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[1];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[1];
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = NO;
        
        _pipelineState[3] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[3], @"Failed to create pipeline state: %@", error);


/*  Create pipeline for Basic rendering */
        pipelineStateDescriptor.label = @"Base Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[0];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[0];
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[0] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[0], @"Failed to create pipeline state: %@", error);
    }

/* Create the command queue */
            _commandQueue = [_device newCommandQueue];
    return self;
}


/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    int i;
    int iflag;
    int iflag_psf = 0;
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    
    double *orthogonal;
    struct transfer_matrices *matrices;
    vector_float4 col_wk[4];

    struct gl_strided_buffer *map_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    map_buf->num_nod_buf =   0;
    struct gl_strided_buffer *mline_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    mline_buf->num_nod_buf = 0;
    struct gl_strided_buffer *coast_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    coast_buf->num_nod_buf = 0;
    struct gl_strided_buffer *mflame_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    mflame_buf->num_nod_buf = 0;
    if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP) {
        iflag_psf = sort_by_patch_distance_psfs(kemo_sgl->kemo_psf->psf_d,
                                                kemo_sgl->kemo_psf->psf_m,
                                                kemo_sgl->kemo_psf->psf_a,
                                                kemo_sgl->view_s);
        iflag_psf = check_draw_map(kemo_sgl->kemo_psf->psf_a);

        set_map_patch_buffer(IZERO, kemo_sgl->kemo_psf->psf_a->istack_solid_psf_patch,
                             kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                             kemo_sgl->kemo_psf->psf_a, map_buf);
        set_map_PSF_isolines_buffer(kemo_sgl->kemo_psf->psf_d,
                                    kemo_sgl->kemo_psf->psf_m,
                                    kemo_sgl->kemo_psf->psf_a,
                                    kemo_sgl->view_s, mline_buf);

        set_map_coastline_buffer(kemo_sgl->kemo_mesh->mesh_m->iflag_draw_coast, coast_buf);
        set_map_flame_buffer(kemo_sgl->kemo_mesh->mesh_m->iflag_draw_sph_grid, mflame_buf);
/*
        for(i=0;i<coast_buf->num_nod_buf;i++){
            printf("%d, color %f %f %f %f \n", i,
                   coast_buf->v_buf[i*coast_buf->ncomp_buf + coast_buf->ist_csurf],
                   coast_buf->v_buf[i*coast_buf->ncomp_buf + coast_buf->ist_csurf+1],
                   coast_buf->v_buf[i*coast_buf->ncomp_buf + coast_buf->ist_csurf+2],
                   coast_buf->v_buf[i*coast_buf->ncomp_buf + coast_buf->ist_csurf+3]);
        }
*/
        matrices = init_projection_matrix_for_map(kemo_sgl->view_s->nx_frame,
                                                  kemo_sgl->view_s->ny_frame);
        for(i=0;i<4;i++){
            col_wk[i].x = matrices->proj[4*i  ];
            col_wk[i].y = matrices->proj[4*i+1];
            col_wk[i].z = matrices->proj[4*i+2];
            col_wk[i].w = matrices->proj[4*i+3];
        };
        _map_proj_mat = simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
        free(matrices);
    };

    
    orthogonal = orthogonal_projection_mat_c( 0.0, kemo_sgl->kemo_mesh->msg_wk->xwin,
                                              0.0, kemo_sgl->kemo_mesh->msg_wk->ywin,
                                             -1.0, 1.0);
    matrices = plane_transfer_matrices(orthogonal);
    for(i=0;i<4;i++){
        col_wk[i].x = matrices->proj[4*i  ];
        col_wk[i].y = matrices->proj[4*i+1];
        col_wk[i].z = matrices->proj[4*i+2];
        col_wk[i].w = matrices->proj[4*i+3];
    };
    _projection_mat = simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
    free(matrices);
    free(orthogonal);

    struct gl_strided_buffer *msg_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_message_buffer(kemo_sgl->view_s->iflag_retina,
                         kemo_sgl->view_s->nx_frame,
                         kemo_sgl->view_s->ny_frame,
                         kemo_sgl->kemo_mesh->msg_wk, msg_buf);
    
    struct gl_strided_buffer *time_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_timelabel_buffer(kemo_sgl->view_s->iflag_retina,
                           kemo_sgl->view_s->nx_frame,
                           kemo_sgl->view_s->ny_frame,
                           kemo_sgl->kemo_mesh->mesh_m->text_color,
                           kemo_sgl->kemo_mesh->mesh_m->bg_color,
                           kemo_sgl->kemo_psf->psf_a, time_buf);
    
    struct gl_strided_buffer *cbar_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_colorbar_buffer(kemo_sgl->view_s->iflag_retina,
                          kemo_sgl->view_s->nx_frame,
                          kemo_sgl->view_s->ny_frame,
                          kemo_sgl->kemo_mesh->mesh_m->text_color,
                          kemo_sgl->kemo_mesh->mesh_m->bg_color,
                          kemo_sgl->kemo_psf->psf_m,
                          kemo_sgl->kemo_psf->psf_a, cbar_buf);
    struct gl_strided_buffer *min_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    struct gl_strided_buffer *max_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    struct gl_strided_buffer *zero_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    const_cbar_text_buffer(kemo_sgl->view_s->iflag_retina,
                           kemo_sgl->kemo_mesh->mesh_m->text_color,
                           kemo_sgl->kemo_psf->psf_m,
                           kemo_sgl->kemo_psf->psf_a, min_buf, max_buf, zero_buf);

    /* draw example cube for empty data */
    iflag = kemo_sgl->kemo_mesh->mesh_m->iflag_draw_mesh
               + iflag_psf + kemo_sgl->kemo_fline->fline_m->iflag_draw_fline;
    struct gl_strided_buffer *cube_buf = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    cube_buf->num_nod_buf = 0;
    if(iflag == 0){
        struct initial_cube_lighting *init_light = init_inital_cube_lighting();
        const_initial_cube_buffer(cube_buf);
        free(cube_buf->v_buf);
    };
    free(cube_buf);


    
    MTLTextureDescriptor *textureDescriptor = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow;
/* Construct message texture */
//    textureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    textureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor.width = kemo_sgl->kemo_mesh->msg_wk->npix_x;
    textureDescriptor.height = kemo_sgl->kemo_mesh->msg_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture[1] = [_device newTextureWithDescriptor:textureDescriptor];
    
    // Calculate the number of bytes per row in the image.
    bytesPerRow = 4 * textureDescriptor.width;
    
    MTLRegion region = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor.width, textureDescriptor.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [_texture[1] replaceRegion:region
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_mesh->msg_wk->msgBMP
                   bytesPerRow:bytesPerRow];

    MTLTextureDescriptor *textureDescriptor2 = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow2;
    /* Construct time texture */
    textureDescriptor2.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor2.width =  kemo_sgl->kemo_psf->psf_a->tlabel_wk->npix_x;
    textureDescriptor2.height = kemo_sgl->kemo_psf->psf_a->tlabel_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture[2] = [_device newTextureWithDescriptor:textureDescriptor2];
    
    // Calculate the number of bytes per row in the image.
    bytesPerRow2 = 4 * textureDescriptor2.width;
    MTLRegion region2 = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor2.width, textureDescriptor2.height, 1} // MTLSize
    };
    
    // Copy the bytes from the data object into the texture
    [_texture[2] replaceRegion:region2
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->tlabel_wk->numBMP
                   bytesPerRow:bytesPerRow2];

    MTLTextureDescriptor *textureDescriptor3 = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow3;
    /* Construct time texture */
    textureDescriptor3.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor3.width =  kemo_sgl->kemo_psf->psf_a->cbar_wk->npix_x;
    textureDescriptor3.height = kemo_sgl->kemo_psf->psf_a->cbar_wk->npix_y;

    // Create the texture from the device by using the descriptor
    _texture[4] = [_device newTextureWithDescriptor:textureDescriptor3];
    _texture[5] = [_device newTextureWithDescriptor:textureDescriptor3];
    _texture[6] = [_device newTextureWithDescriptor:textureDescriptor3];
    // Calculate the number of bytes per row in the image.
    bytesPerRow3 = 4 * textureDescriptor3.width;
    MTLRegion region3 = {
        { 0, 0, 0 },                   // MTLOrigin
        {textureDescriptor3.width, textureDescriptor3.height, 1} // MTLSize
    };

    // Copy the bytes from the data object into the texture
    [_texture[4] replaceRegion:region3
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->cbar_wk->minBMP
                   bytesPerRow:bytesPerRow3];
    [_texture[5] replaceRegion:region3
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->cbar_wk->maxBMP
                   bytesPerRow:bytesPerRow3];
    [_texture[6] replaceRegion:region3
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->cbar_wk->zeroBMP
                   bytesPerRow:bytesPerRow3];

    _frameNum++;
    _scalechange = 0.2 + (1.0 + 0.2 * sin(_frameNum * 0.1));
    
    // Set up a simple MTLBuffer with the vertices, including position and texture coordinates
    int n_quad_vertex = 6;
    static const AAPLVertex quadVertices[] =
    {
        // Pixel positions, Color coordinates
        { {  0.5f,  -0.5f },  { 1.f, 0.f, 0.f, 1.f } },
        { { -0.5f,  -0.5f },  { 0.f, 1.f, 0.f, 1.f } },
        { { -0.5f,   0.5f },  { 0.f, 0.f, 1.f, 1.f } },

        { {  0.5f,  -0.5f },  { 1.f, 0.f, 0.f, 1.f } },
        { { -0.5f,   0.5f },  { 0.f, 0.f, 1.f, 1.f } },
        { {  0.5f,   0.5f },  { 1.f, 0.f, 1.f, 1.f } },
    };
    _vertices[0] = [_device newBufferWithBytes:quadVertices
                                        length:(n_quad_vertex*sizeof(AAPLVertex))
                                       options:MTLResourceStorageModeShared];


    // Create a vertex buffer, and initialize it with the quadVertices array
    _vertices[1] = [_device newBufferWithBytes:((KemoViewVertex *) msg_buf->v_buf)
                                        length:(msg_buf->num_nod_buf * sizeof(KemoViewVertex))
                                       options:MTLResourceStorageModeShared];
    if(time_buf->num_nod_buf > 0){
        _vertices[2] = [_device newBufferWithBytes:((KemoViewVertex *) time_buf->v_buf)
                                            length:(time_buf->num_nod_buf * sizeof(KemoViewVertex))
                                           options:MTLResourceStorageModeShared];
    }
    if(cbar_buf->num_nod_buf > 0){
        _vertices[3] = [_device newBufferWithBytes:((KemoViewVertex *) cbar_buf->v_buf)
                                            length:(cbar_buf->num_nod_buf * sizeof(KemoViewVertex))
                                           options:MTLResourceStorageModeShared];
    }
    if(min_buf->num_nod_buf > 0){
        _vertices[4] = [_device newBufferWithBytes:((KemoViewVertex *) min_buf->v_buf)
                                            length:(min_buf->num_nod_buf * sizeof(KemoViewVertex))
                                           options:MTLResourceStorageModeShared];
    };
    if(max_buf->num_nod_buf > 0){
        _vertices[5] = [_device newBufferWithBytes:((KemoViewVertex *) max_buf->v_buf)
                                            length:(max_buf->num_nod_buf * sizeof(KemoViewVertex))
                                           options:MTLResourceStorageModeShared];
    };
    if(zero_buf->num_nod_buf > 0){
        _vertices[6] = [_device newBufferWithBytes:((KemoViewVertex *) zero_buf->v_buf)
                                            length:(zero_buf->num_nod_buf * sizeof(KemoViewVertex))
                                           options:MTLResourceStorageModeShared];
    };

    // Create a new command buffer for each render pass to the current drawable.
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"MyCommand";

    // Obtain a renderPassDescriptor generated from the view's drawable textures.
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil)
    {
        // Create a render command encoder.
        id<MTLRenderCommandEncoder> renderEncoder =
        [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        renderEncoder.label = @"MyRenderEncoder";

        // Set the region of the drawable to draw into.
//        [renderEncoder setViewport:(MTLViewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, 0.0, 1.0 }];

        
        if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP){
/*  Commands to render map projection */
            if(map_buf->num_nod_buf > 0){
                _vertices[10] = [_device newBufferWithBytesNoCopy:map_buf->v_buf
                                                           length:(map_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
/*                _vertices[10] = [_device newBufferWithBytes:((KemoViewVertex *) map_buf->v_buf)
                                                     length:(map_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
*/
                [renderEncoder setRenderPipelineState:_pipelineState[1]];
                [renderEncoder setVertexBuffer:_vertices[10]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:map_buf->num_nod_buf];
            };
/*  Commands to render isolines on map */
            if(mline_buf->num_nod_buf > 0){
                _vertices[11] = [_device newBufferWithBytesNoCopy:mline_buf->v_buf
                                                           length:(mline_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
/*                _vertices[11] = [_device newBufferWithBytes:((KemoViewVertex *) mline_buf->v_buf)
                                                     length:(mline_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
 */
                [renderEncoder setRenderPipelineState:_pipelineState[3]];
                [renderEncoder setVertexBuffer:_vertices[11]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:mline_buf->num_nod_buf];
            };
/*  Commands to render Coastline on map */
            if(coast_buf->num_nod_buf > 0){
                _vertices[12] = [_device newBufferWithBytes:((KemoViewVertex *) coast_buf->v_buf)
                                                     length:(coast_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
                [renderEncoder setRenderPipelineState:_pipelineState[3]];
                [renderEncoder setVertexBuffer:_vertices[12]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:coast_buf->num_nod_buf];
            };
/*  Commands to render grids on map */
            if(mflame_buf->num_nod_buf > 0){
                _vertices[13] = [_device newBufferWithBytes:((KemoViewVertex *) mflame_buf->v_buf)
                                                     length:(mflame_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
                [renderEncoder setRenderPipelineState:_pipelineState[3]];
                [renderEncoder setVertexBuffer:_vertices[13]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:mflame_buf->num_nod_buf];
            };
        } else {
            /*  Commands to render simple quadrature */
        
            [renderEncoder setRenderPipelineState:_pipelineState[0]];
            [renderEncoder setVertexBuffer:_vertices[0]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_viewportSize
                                   length:sizeof(_viewportSize)
                                  atIndex:AAPLVertexInputIndexViewportSize];
            [renderEncoder setVertexBytes:&_scalechange
                                   length:sizeof(_scalechange)
                                  atIndex:AAPLVertexInputIndexScale];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:n_quad_vertex];
        };

/*  Commands to render screen message */

        [renderEncoder setRenderPipelineState:_pipelineState[2]];
        // Pass in the parameter data.
        [renderEncoder setVertexBuffer:_vertices[1]
                                offset:0
                               atIndex:AAPLVertexInputIndexVertices];
        
        [renderEncoder setVertexBytes:&_projection_mat
                               length:sizeof(_projection_mat)
                              atIndex:AAPLOrthogonalMatrix];

        // Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
        ///  to the 'colorMap' argument in the 'samplingShader' function because its
        //   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index.
        [renderEncoder setFragmentTexture:_texture[1]
                                  atIndex:AAPLTextureIndexBaseColor];

        // Draw the triangles.
        [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                          vertexStart:0
                          vertexCount:msg_buf->num_nod_buf];

/*  Commands to render time label */
        if(time_buf->num_nod_buf > 0){
            [renderEncoder setRenderPipelineState:_pipelineState[2]];
            [renderEncoder setVertexBuffer:_vertices[2]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_projection_mat
                                   length:sizeof(_projection_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [renderEncoder setFragmentTexture:_texture[2]
                                      atIndex:AAPLTextureIndexBaseColor];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:time_buf->num_nod_buf];
        };
/*  Commands to render colorbar  box */
        if(cbar_buf->num_nod_buf > 0){
            [renderEncoder setRenderPipelineState:_pipelineState[1]];
            [renderEncoder setVertexBuffer:_vertices[3]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_projection_mat
                                   length:sizeof(_projection_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:cbar_buf->num_nod_buf];
        };
/*  Commands to render colorbar  label */
        if(min_buf->num_nod_buf > 0){
            [renderEncoder setRenderPipelineState:_pipelineState[2]];
            [renderEncoder setVertexBuffer:_vertices[4]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_projection_mat
                                   length:sizeof(_projection_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [renderEncoder setFragmentTexture:_texture[4]
                                      atIndex:AAPLTextureIndexBaseColor];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:min_buf->num_nod_buf];
        };
        if(max_buf->num_nod_buf > 0){
            [renderEncoder setRenderPipelineState:_pipelineState[2]];
            [renderEncoder setVertexBuffer:_vertices[5]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_projection_mat
                                   length:sizeof(_projection_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [renderEncoder setFragmentTexture:_texture[5]
                                      atIndex:AAPLTextureIndexBaseColor];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:max_buf->num_nod_buf];
        };
        if(zero_buf->num_nod_buf > 0){
            [renderEncoder setRenderPipelineState:_pipelineState[2]];
            [renderEncoder setVertexBuffer:_vertices[6]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [renderEncoder setVertexBytes:&_projection_mat
                                   length:sizeof(_projection_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [renderEncoder setFragmentTexture:_texture[6]
                                      atIndex:AAPLTextureIndexBaseColor];
            [renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:zero_buf->num_nod_buf];
        };

        [renderEncoder endEncoding];

        // Schedule a present once the framebuffer is complete using the current drawable.
        [commandBuffer presentDrawable:view.currentDrawable];
    }
    [commandBuffer  commit];

//    [_texture[1] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[1] release];
//    [_texture[2] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[2] release];
//    [_texture[4] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[4] release];
//    [_texture[5] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[5] release];
//    [_texture[6] setPurgeableState:MTLPurgeableStateEmpty];
    [_texture[6] release];

    if(map_buf->num_nod_buf > 0){
//        [_vertices[10] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[10] release];
        free(map_buf->v_buf);
    };
    free(map_buf);

    if(mline_buf->num_nod_buf > 0){
//        [_vertices[11] setPurgeableState:MTLPurgeableStateEmpty];
//        [_vertices[11] release];
        free(mline_buf->v_buf);
    };
    free(mline_buf);

    if(coast_buf->num_nod_buf > 0){
//        [_vertices[12] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[12] release];
        free(coast_buf->v_buf);
    };
    free(coast_buf);

    if(mflame_buf->num_nod_buf > 0){
//        [_vertices[13] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[13] release];
        free(mflame_buf->v_buf);
    };
    free(mflame_buf);

    free(cbar_buf->v_buf);
    free(cbar_buf);
    free(min_buf->v_buf);
    free(min_buf);
    free(max_buf->v_buf);
    free(max_buf);
    free(zero_buf->v_buf);
    free(zero_buf);
    free(time_buf->v_buf);
    free(time_buf);
    free(msg_buf->v_buf);
    free(msg_buf);
}


/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
