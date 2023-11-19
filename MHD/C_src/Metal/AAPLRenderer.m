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
    id<MTLRenderPipelineState> _pipelineState[40];
    
    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _commandQueue;
    
    id<MTLFunction> _vertexFunction[6];
    id<MTLFunction> _fragmentFunction[6];

    // Combined depth and stencil state object.
    id<MTLDepthStencilState> _depthState;

    // The Metal buffer that holds the vertex data.
    id<MTLBuffer> _vertices[61];
    // The Metal texture object
    id<MTLTexture> _texture[30];
/*  Index buffer for initial cube */
    id<MTLBuffer> _index_buffer;

    id<MTLRenderCommandEncoder> _renderEncoder;

    // The current size of the view, used as an input to the vertex shader.
    vector_uint2    _viewportSize;
    float           _scalechange;
    
    matrix_float4x4 _modelview_mat;
    matrix_float4x4 _projection_mat;
    matrix_float4x4 _normal_mat;

    matrix_float4x4 _map_proj_mat;
    matrix_float4x4 _cbar_proj_mat;

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

// Indicate that each pixel in the depth buffer is a 32-bit floating point value.
        mtkView.depthStencilPixelFormat = MTLPixelFormatDepth32Float;

         // Load all the shader files with a .metal file extension in the project.
        id<MTLLibrary> defaultLibrary = [_device newDefaultLibrary];

        _vertexFunction[0] =   [defaultLibrary newFunctionWithName:@"Base2dVertexShader"];
        _fragmentFunction[0] = [defaultLibrary newFunctionWithName:@"Base2DfragmentShader"];

        _vertexFunction[1] =   [defaultLibrary newFunctionWithName:@"Simple2dVertexShader"];
        _fragmentFunction[1] = [defaultLibrary newFunctionWithName:@"Simple2DfragmentShader"];

        _vertexFunction[2] =   [defaultLibrary newFunctionWithName:@"Texture2dVertexShader"];
        _fragmentFunction[2] = [defaultLibrary newFunctionWithName:@"sampling2dShader"];

        _vertexFunction[3] =   [defaultLibrary newFunctionWithName:@"PhongVertexShader"];
        _fragmentFunction[3] = [defaultLibrary newFunctionWithName:@"PhongFragmentShader"];

        _vertexFunction[4] =   [defaultLibrary newFunctionWithName:@"SimpleVertexShader"];
        _fragmentFunction[4] = [defaultLibrary newFunctionWithName:@"SimpleFragmentShader"];

        _vertexFunction[5] =   [defaultLibrary newFunctionWithName:@"PhongTexureVertexShader"];
        _fragmentFunction[5] = [defaultLibrary newFunctionWithName:@"PhongTextureFragmentShader"];

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        MTLRenderPipelineDescriptor *pipelineStateDescriptor;
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"2D Texture Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[2];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[2];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
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
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
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

        /*  Create pipeline for simple rendering */
        pipelineStateDescriptor.label = @"2D Simple Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[1];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[1];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = NO;
        
        _pipelineState[3] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[3], @"Failed to create pipeline state: %@", error);


/*  Create pipeline for Basic rendering */
        pipelineStateDescriptor.label = @"Base Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[0];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[0];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[0] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[0], @"Failed to create pipeline state: %@", error);

        /* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor.label = @"Simple Shader Pipeline";
        pipelineStateDescriptor.vertexFunction =   _vertexFunction[4];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[4];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        _pipelineState[5] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[5], @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"Phong Shader Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[3];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[3];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
            
        _pipelineState[4] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[4], @"Failed to create pipeline state: %@", error);

        _pipelineState[14] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[14], @"Failed to create pipeline state: %@", error);

/* Configure a pipeline descriptor that is used to create a pipeline state. */
        pipelineStateDescriptor = [[MTLRenderPipelineDescriptor alloc] init];

        pipelineStateDescriptor.label = @"Phong Shader Pipeline";
        pipelineStateDescriptor.vertexFunction = _vertexFunction[5];
        pipelineStateDescriptor.fragmentFunction = _fragmentFunction[5];
        pipelineStateDescriptor.depthAttachmentPixelFormat = mtkView.depthStencilPixelFormat;
        pipelineStateDescriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat;

        pipelineStateDescriptor.colorAttachments[0].blendingEnabled = YES;
        pipelineStateDescriptor.colorAttachments[0].rgbBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].alphaBlendOperation = MTLBlendOperationAdd;
        pipelineStateDescriptor.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineStateDescriptor.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
            
        _pipelineState[6] = [_device newRenderPipelineStateWithDescriptor:pipelineStateDescriptor
                                           error:&error];
        NSAssert(_pipelineState[6], @"Failed to create pipeline state: %@", error);

/* Add Depth buffer description in command */
        MTLDepthStencilDescriptor *depthDescriptor = [MTLDepthStencilDescriptor new];
        depthDescriptor.depthCompareFunction = MTLCompareFunctionLessEqual;
        depthDescriptor.depthWriteEnabled = YES;
        _depthState = [_device newDepthStencilStateWithDescriptor:depthDescriptor];
    }
/* Create the command queue */
    _commandQueue = [_device newCommandQueue];
    return self;
}

- (matrix_float4x4) setMetalViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int i=0;i<4;i++){
        col_wk[i].x = matrix[4*i  ];
        col_wk[i].y = matrix[4*i+1];
        col_wk[i].z = matrix[4*i+2];
        col_wk[i].w = matrix[4*i+3];
    };
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}


- (matrix_float4x4) setMetalModelViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int j=0;j<4;j++){col_wk[0][j] =  matrix[4*0+j];}
    for(int j=0;j<4;j++){col_wk[1][j] =  matrix[4*1+j];}
    for(int j=0;j<4;j++){col_wk[2][j] =  matrix[4*2+j];}
    for(int j=0;j<4;j++){col_wk[3][j] =  matrix[4*3+j];}
//    for(int j=0;j<4;j++){col_wk[j][2] = -matrix[4*j+2];}
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}
- (matrix_float4x4) setMetalProjViewMatrices:(float *) matrix
{
    vector_float4 col_wk[4];
    for(int j=0;j<4;j++){col_wk[0][j] =  matrix[4*0+j];}
    for(int j=0;j<4;j++){col_wk[1][j] =  matrix[4*1+j];}
    for(int j=0;j<4;j++){col_wk[2][j] =  matrix[4*2+j];}
    for(int j=0;j<4;j++){col_wk[3][j] =  matrix[4*3+j];}
/* Shift viewport */
    col_wk[2][2] = (0.5*matrix[4*2+2] + 0.5*matrix[4*2+3]);
    col_wk[3][2] = (0.5*matrix[4*3+2] + 0.5*matrix[4*3+3]);

//    for(int j=0;j<4;j++){col_wk[j][2] = -matrix[4*j+2];}
    return simd_matrix(col_wk[0], col_wk[1], col_wk[2], col_wk[3]);
}

- (void)drawSolidObject:(struct gl_strided_buffer *) buf
                encoder:(id<MTLRenderCommandEncoder> *) renderEncoder
                 vertex:(id<MTLBuffer> *) _verticess
                 lights:(LightSourceParameters *) lights
              materials:(MaterialParameters *) material
                  sides:(int) iflag_surface
{
    if(buf->num_nod_buf > 0){
        _verticess[0] = [_device newBufferWithBytesNoCopy:buf->v_buf
                                                   length:(buf->nsize_buf * sizeof(float))
                                                  options:MTLResourceStorageModeShared
                                              deallocator:nil];
        

        [*renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
        [*renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
        if(iflag_surface == NORMAL_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeBack];
        }else if(iflag_surface == REVERSE_POLYGON){
            [*renderEncoder setCullMode:MTLCullModeFront];
        }else{
            [*renderEncoder setCullMode:MTLCullModeNone];
        }
        [*renderEncoder setDepthStencilState:_depthState];
        
        [*renderEncoder setRenderPipelineState:_pipelineState[4]];
        [*renderEncoder setVertexBuffer:_verticess[0]
                                 offset:0
                                atIndex:AAPLVertexInputIndexVertices];
        [*renderEncoder setVertexBytes:&_modelview_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelViewMatrix];
        [*renderEncoder setVertexBytes:&_projection_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLProjectionMatrix];
        [*renderEncoder setVertexBytes:&_normal_mat
                                length:sizeof(matrix_float4x4)
                               atIndex:AAPLModelNormalMatrix];
        
        [*renderEncoder setFragmentBytes:lights
                                  length:(sizeof(LightSourceParameters))
                                 atIndex:AAPLLightsParams];
        [*renderEncoder setFragmentBytes:material
                                  length:sizeof(MaterialParameters)
                                 atIndex:AAPLMaterialParams];
        
        [*renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                           vertexStart:0
                           vertexCount:buf->num_nod_buf];
    };
};




/// Called whenever the view needs to render a frame.
- (void)drawInMTKView:(nonnull MTKView *)view
{
    int i;
    int iflag;
    int iflag_psf = 0;
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    
    /* Set transfer matrices */
    double *orthogonal = orthogonal_projection_mat_c(0.0, kemo_sgl->view_s->nx_frame,
                                                     0.0, kemo_sgl->view_s->ny_frame,
                                                    -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    struct transfer_matrices *view_matrices = transfer_matrix_to_shader(kemo_sgl->view_s);
    struct transfer_matrices *map_matrices
            = init_projection_matrix_for_map(kemo_sgl->view_s->nx_frame,
                                             kemo_sgl->view_s->ny_frame);
    free(orthogonal);

    _cbar_proj_mat =  [self setMetalViewMatrices:cbar_matrices->proj];
    _map_proj_mat =   [self setMetalViewMatrices:map_matrices->proj];

    _modelview_mat =  [self setMetalViewMatrices:view_matrices->model];
    _projection_mat = [self setMetalProjViewMatrices:view_matrices->proj];
    _normal_mat =     [self setMetalViewMatrices:view_matrices->nrmat];
    free(cbar_matrices);
    free(view_matrices);
    free(map_matrices);

    
    struct gl_strided_buffer *mline_buf
        = (struct gl_strided_buffer *) malloc(sizeof(struct gl_strided_buffer));
    mline_buf->num_nod_buf = 0;
    
    struct psf_menu_val *psf_stexure;
    struct psf_menu_val *psf_ttexure;
    if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP) {
        iflag_psf = sort_by_patch_distance_psfs(kemo_sgl->kemo_psf->psf_d,
                                                kemo_sgl->kemo_psf->psf_m,
                                                kemo_sgl->kemo_psf->psf_a,
                                                kemo_sgl->view_s);
        iflag_psf = check_draw_map(kemo_sgl->kemo_psf->psf_a);

        set_map_patch_buffer(IZERO, kemo_sgl->kemo_psf->psf_a->istack_solid_psf_patch,
                             kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                             kemo_sgl->kemo_psf->psf_a, kemo_sgl->kemo_buffers->MAP_solid_buf);
        set_map_PSF_isolines_buffer(kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                                    kemo_sgl->kemo_psf->psf_a, kemo_sgl->view_s,
                                    kemo_sgl->kemo_buffers->MAP_isoline_buf);

        set_map_coastline_buffer(kemo_sgl->kemo_mesh->mesh_m,
                                 kemo_sgl->kemo_buffers->coast_buf);
        set_map_flame_buffer(kemo_sgl->kemo_mesh->mesh_m,
                             kemo_sgl->kemo_buffers->sph_grid_buf);
    } else {
/* Set Axis data into buffer */
        double axis_radius = 16.0;
        set_axis_to_buf(kemo_sgl->view_s, kemo_sgl->kemo_mesh->mesh_m->iflag_draw_axis,
                        kemo_sgl->kemo_mesh->mesh_m->dist_domains,
                        kemo_sgl->kemo_buffers->ncorner_axis,
                        axis_radius, kemo_sgl->kemo_buffers->axis_buf);

        iflag_psf = sort_by_patch_distance_psfs(kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                                                kemo_sgl->kemo_psf->psf_a, kemo_sgl->view_s);
        iflag_psf = iflag_psf + check_draw_psf(kemo_sgl->kemo_psf->psf_a);

        set_color_code_for_psfs(kemo_sgl->kemo_psf->psf_d, kemo_sgl->kemo_psf->psf_m,
                                kemo_sgl->kemo_psf->psf_a);
        const_PSF_solid_objects_buffer(kemo_sgl->view_s, kemo_sgl->kemo_psf->psf_d,
                                       kemo_sgl->kemo_psf->psf_m, kemo_sgl->kemo_psf->psf_a,
                                       kemo_sgl->kemo_buffers->PSF_solid_buf,
                                       kemo_sgl->kemo_buffers->PSF_stxur_buf,
                                       kemo_sgl->kemo_buffers->PSF_isoline_buf,
                                       kemo_sgl->kemo_buffers->PSF_arrow_buf);
        int ipsf_texure = kemo_sgl->kemo_psf->psf_a->ipsf_viz_far[IZERO]-1;
        psf_stexure = kemo_sgl->kemo_psf->psf_m[ipsf_texure];

        const_PSF_trans_objects_buffer(kemo_sgl->view_s, kemo_sgl->kemo_psf->psf_d,
                                       kemo_sgl->kemo_psf->psf_m, kemo_sgl->kemo_psf->psf_a,
                                       kemo_sgl->kemo_buffers->PSF_trns_buf,
                                       kemo_sgl->kemo_buffers->PSF_ttxur_buf);
        int ist_psf = kemo_sgl->kemo_psf->psf_a->istack_solid_psf_patch;
        ipsf_texure = kemo_sgl->kemo_psf->psf_a->ipsf_viz_far[ist_psf]-1;
        psf_ttexure = kemo_sgl->kemo_psf->psf_m[ipsf_texure];

        set_coastline_buffer(kemo_sgl->kemo_mesh->mesh_m,
                             kemo_sgl->kemo_buffers->coast_buf);
        set_sph_flame_buffer(kemo_sgl->kemo_mesh->mesh_m,
                             kemo_sgl->kemo_buffers->sph_grid_buf);

        const_fieldlines_buffer(kemo_sgl->kemo_fline->fline_d, kemo_sgl->kemo_fline->fline_m,
                                kemo_sgl->kemo_buffers->FLINE_tube_buf, kemo_sgl->kemo_buffers->FLINE_line_buf);
 
        const_solid_mesh_buffer(kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->kemo_mesh->mesh_m, kemo_sgl->view_s,
                                kemo_sgl->kemo_buffers->mesh_solid_buf, kemo_sgl->kemo_buffers->mesh_grid_buf,
                                kemo_sgl->kemo_buffers->mesh_node_buf);
        const_trans_mesh_buffer(kemo_sgl->kemo_mesh->mesh_d, kemo_sgl->kemo_mesh->mesh_m, kemo_sgl->view_s,
                                kemo_sgl->kemo_buffers->mesh_trns_buf);

    };

    
    const_message_buffer(kemo_sgl->view_s->iflag_retina,
                         kemo_sgl->view_s->nx_frame,
                         kemo_sgl->view_s->ny_frame,
                         kemo_sgl->kemo_mesh->msg_wk,
                         kemo_sgl->kemo_buffers->msg_buf);
    
    const_timelabel_buffer(kemo_sgl->view_s->iflag_retina,
                           kemo_sgl->view_s->nx_frame,
                           kemo_sgl->view_s->ny_frame,
                           kemo_sgl->kemo_mesh->mesh_m->text_color,
                           kemo_sgl->kemo_mesh->mesh_m->bg_color,
                           kemo_sgl->kemo_psf->psf_a,
                           kemo_sgl->kemo_buffers->time_buf);
    
    const_colorbar_buffer(kemo_sgl->view_s->iflag_retina,
                          kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame,
                          kemo_sgl->kemo_mesh->mesh_m->text_color,
                          kemo_sgl->kemo_mesh->mesh_m->bg_color,
                          kemo_sgl->kemo_psf->psf_m, kemo_sgl->kemo_psf->psf_a,
                          kemo_sgl->kemo_buffers->cbar_buf,
                          kemo_sgl->kemo_buffers->min_buf,
                          kemo_sgl->kemo_buffers->max_buf,
                          kemo_sgl->kemo_buffers->zero_buf);

/* draw example cube for empty data */
    struct initial_cube_lighting *init_light = init_inital_cube_lighting();

    iflag = kemo_sgl->kemo_mesh->mesh_m->iflag_draw_mesh
               + iflag_psf + kemo_sgl->kemo_fline->fline_m->iflag_draw_fline;
    if(iflag > 0){kemo_sgl->kemo_buffers->cube_buf->num_nod_buf = 0;}

    MTLTextureDescriptor *textureDescriptor = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow;
/* Construct message texture */
//    textureDescriptor.pixelFormat = MTLPixelFormatBGRA8Unorm;
    textureDescriptor.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor.width = kemo_sgl->kemo_mesh->msg_wk->message_image->npix_img[0];
    textureDescriptor.height = kemo_sgl->kemo_mesh->msg_wk->message_image->npix_img[1];

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
                     withBytes:kemo_sgl->kemo_mesh->msg_wk->message_image->imgBMP
                   bytesPerRow:bytesPerRow];

    MTLTextureDescriptor *textureDescriptor2 = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow2;
    /* Construct time texture */
    textureDescriptor2.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor2.width =  kemo_sgl->kemo_psf->psf_a->tlabel_wk->tlabel_image->npix_img[0];
    textureDescriptor2.height = kemo_sgl->kemo_psf->psf_a->tlabel_wk->tlabel_image->npix_img[1];

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
                     withBytes:kemo_sgl->kemo_psf->psf_a->tlabel_wk->tlabel_image->imgBMP
                   bytesPerRow:bytesPerRow2];

    MTLTextureDescriptor *textureDescriptor3 = [[MTLTextureDescriptor alloc] init];
    NSUInteger bytesPerRow3;
    /* Construct time texture */
    textureDescriptor3.pixelFormat = MTLPixelFormatRGBA8Unorm;
    textureDescriptor3.width =  kemo_sgl->kemo_psf->psf_a->cbar_wk->cbar_min_image->npix_img[0];
    textureDescriptor3.height = kemo_sgl->kemo_psf->psf_a->cbar_wk->cbar_min_image->npix_img[1];

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
                     withBytes:kemo_sgl->kemo_psf->psf_a->cbar_wk->cbar_min_image->imgBMP
                   bytesPerRow:bytesPerRow3];
    [_texture[5] replaceRegion:region3
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->cbar_wk->cbar_max_image->imgBMP
                   bytesPerRow:bytesPerRow3];
    [_texture[6] replaceRegion:region3
                   mipmapLevel:0
                     withBytes:kemo_sgl->kemo_psf->psf_a->cbar_wk->cbar_zero_image->imgBMP
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
    int num =  kemo_sgl->kemo_buffers->cube_buf->num_nod_buf
             + kemo_sgl->kemo_buffers->coast_buf->num_nod_buf
             + kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf;

    // Create a new command buffer for each render pass to the current drawable.
    id<MTLCommandBuffer> commandBuffer = [_commandQueue commandBuffer];
    commandBuffer.label = @"MyCommand";

    // Obtain a renderPassDescriptor generated from the view's drawable textures.
    MTLRenderPassDescriptor *renderPassDescriptor = view.currentRenderPassDescriptor;

    if(renderPassDescriptor != nil)
    {
        // Create a render command encoder.
        _renderEncoder = [commandBuffer renderCommandEncoderWithDescriptor:renderPassDescriptor];
        _renderEncoder.label = @"MyRenderEncoder";

        // Set the region of the drawable to draw into.
//        [_renderEncoder setViewport:(MTLViewport){0.0, 0.0, _viewportSize.x, _viewportSize.y, 0.0, 1.0 }];

        
        if(kemo_sgl->view_s->iflag_view_type == VIEW_MAP){
/*  Commands to render map projection */
            if(kemo_sgl->kemo_buffers->MAP_solid_buf->num_nod_buf > 0){
                _vertices[10] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->MAP_solid_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->MAP_solid_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];

                [_renderEncoder setRenderPipelineState:_pipelineState[1]];
                [_renderEncoder setVertexBuffer:_vertices[10]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->MAP_solid_buf->num_nod_buf];
            };
/*  Commands to render isolines on map */
            if(kemo_sgl->kemo_buffers->MAP_isoline_buf->num_nod_buf > 0){
                _vertices[11] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->MAP_isoline_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->MAP_isoline_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                [_renderEncoder setRenderPipelineState:_pipelineState[3]];
                [_renderEncoder setVertexBuffer:_vertices[11]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->MAP_isoline_buf->num_nod_buf];
            };
/*  Commands to render Coastline on map */
            if(kemo_sgl->kemo_buffers->coast_buf->num_nod_buf > 0){
                _vertices[12] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->coast_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->coast_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                [_renderEncoder setRenderPipelineState:_pipelineState[3]];
                [_renderEncoder setVertexBuffer:_vertices[12]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->coast_buf->num_nod_buf];
            };
/*  Commands to render grids on map */
            if(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf > 0){
                _vertices[13] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->sph_grid_buf->v_buf)
                                                     length:(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
                [_renderEncoder setRenderPipelineState:_pipelineState[3]];
                [_renderEncoder setVertexBuffer:_vertices[13]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_map_proj_mat
                                       length:sizeof(_map_proj_mat)
                                      atIndex:AAPLOrthogonalMatrix];
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf];
            };
        } else {
            /*  Commands to render simple quadrature
             [_renderEncoder setRenderPipelineState:_pipelineState[0]];
             [_renderEncoder setVertexBuffer:_vertices[0]
             offset:0
             atIndex:AAPLVertexInputIndexVertices];
             [_renderEncoder setVertexBytes:&_viewportSize
             length:sizeof(_viewportSize)
             atIndex:AAPLVertexInputIndexViewportSize];
             [_renderEncoder setVertexBytes:&_scalechange
             length:sizeof(_scalechange)
             atIndex:AAPLVertexInputIndexScale];
             [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
             vertexStart:0
             vertexCount:n_quad_vertex];
             */
            float x, y, z;
            LightSourceParameters lights;
            MaterialParameters    material;
            
            if(kemo_sgl->kemo_buffers->cube_buf->num_nod_buf > 0){
                lights.num_lights = init_light->num_light;
                for(i=0;i<lights.num_lights;i++){
                    lights.position[i].x = init_light->lightposition[i][0];
                    lights.position[i].y = init_light->lightposition[i][1];
                    lights.position[i].z = init_light->lightposition[i][2];
                    lights.position[i].w = init_light->lightposition[i][3];
                };
                material.ambient.x = init_light->whitelight[0][0];
                material.diffuse.x = init_light->whitelight[1][0];
                material.specular.x = init_light->whitelight[2][0];
                material.shininess =  init_light->shine[0];
            }else{
                material.ambient.x = kemoview_get_material_parameter(AMBIENT_FLAG);
                material.diffuse.x = kemoview_get_material_parameter(DIFFUSE_FLAG);
                material.specular.x = kemoview_get_material_parameter(SPECULAR_FLAG);
                material.shininess = kemoview_get_material_parameter(SHINENESS_FLAG);
                
                lights.num_lights = kemoview_get_num_light_position();
                for(i=0;i<lights.num_lights;i++){
                    kemoview_get_each_light_xyz(i, &x, &y, &z);
                    lights.position[i].x = x;
                    lights.position[i].y = y;
                    lights.position[i].z = z;
                    lights.position[i].w = 1.0;
                };
            };
            material.ambient.y = material.ambient.x;
            material.ambient.z = material.ambient.x;
            material.ambient.w = 1.0;
            material.diffuse.y = material.diffuse.x;
            material.diffuse.z = material.diffuse.x;
            material.diffuse.w = 1.0;
            material.specular.y = material.specular.x;
            material.specular.z = material.specular.x;
            material.specular.w = 1.0;
            
            if(kemo_sgl->kemo_buffers->PSF_stxur_buf->num_nod_buf > 0){
                _vertices[36] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->PSF_stxur_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->PSF_stxur_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                
                /* Construct time texture */
                MTLTextureDescriptor *textureDescriptor11 = [[MTLTextureDescriptor alloc] init];
                textureDescriptor11.pixelFormat = MTLPixelFormatRGBA8Unorm;
                textureDescriptor11.width =  psf_stexure->texture_width;
                textureDescriptor11.height = psf_stexure->texture_height;
                NSUInteger bytesPerRow11;
                bytesPerRow11 = 4 * textureDescriptor11.width;
                MTLRegion region11 = {
                    { 0, 0, 0 },                   // MTLOrigin
                    {textureDescriptor11.width, textureDescriptor11.height, 1} // MTLSize
                };
                _texture[11] = [_device newTextureWithDescriptor:textureDescriptor11];
                [_texture[11] replaceRegion:region11
                                mipmapLevel:0
                                  withBytes:psf_stexure->texture_rgba
                                bytesPerRow:bytesPerRow11];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                [_renderEncoder setCullMode:MTLCullModeBack];
                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[5]];
                [_renderEncoder setVertexBuffer:_vertices[36]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                [_renderEncoder setVertexBytes:&_normal_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelNormalMatrix];
                
                [_renderEncoder setFragmentBytes:&lights
                                         length:(sizeof(LightSourceParameters))
                                        atIndex:AAPLLightsParams];
                [_renderEncoder setFragmentBytes:&material
                                         length:sizeof(MaterialParameters)
                                        atIndex:AAPLMaterialParams];
                [_renderEncoder setFragmentTexture:_texture[11]
                                          atIndex:AAPLTextureImageIndex];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->PSF_stxur_buf->num_nod_buf];
            }
            [self drawSolidObject:kemo_sgl->kemo_buffers->PSF_solid_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[35]
                           lights:&lights
                        materials:&material
                            sides:BOTH_SURFACES];
            [self drawSolidObject:kemo_sgl->kemo_buffers->PSF_isoline_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[31]
                           lights:&lights
                        materials:&material
                            sides:BOTH_SURFACES];
            [self drawSolidObject:kemo_sgl->kemo_buffers->PSF_arrow_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[32]
                           lights:&lights
                        materials:&material
                            sides:BOTH_SURFACES];

            if(kemo_sgl->kemo_fline->fline_m->fieldline_type == IFLAG_PIPE){
                [self drawSolidObject:kemo_sgl->kemo_buffers->FLINE_tube_buf
                              encoder:&_renderEncoder
                               vertex:&_vertices[55]
                               lights:&lights
                            materials:&material
                                sides:BOTH_SURFACES];
            }else{
                if(kemo_sgl->kemo_buffers->FLINE_line_buf->num_nod_buf > 0){
                    _vertices[56] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->FLINE_line_buf->v_buf
                                                               length:(kemo_sgl->kemo_buffers->FLINE_line_buf->nsize_buf * sizeof(float))
                                                              options:MTLResourceStorageModeShared
                                                          deallocator:nil];
                    [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                    [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                    [_renderEncoder setCullMode:MTLCullModeBack];
                    [_renderEncoder setDepthStencilState:_depthState];
                    
                    [_renderEncoder setRenderPipelineState:_pipelineState[5]];
                    [_renderEncoder setVertexBuffer:_vertices[56]
                                            offset:0
                                           atIndex:AAPLVertexInputIndexVertices];
                    [_renderEncoder setVertexBytes:&_modelview_mat
                                           length:sizeof(matrix_float4x4)
                                          atIndex:AAPLModelViewMatrix];
                    [_renderEncoder setVertexBytes:&_projection_mat
                                           length:sizeof(matrix_float4x4)
                                          atIndex:AAPLProjectionMatrix];
                    
                    [_renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                      vertexStart:0
                                      vertexCount:kemo_sgl->kemo_buffers->FLINE_line_buf->num_nod_buf];
                };
            };

            [self drawSolidObject:kemo_sgl->kemo_buffers->mesh_node_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[51]
                           lights:&lights
                        materials:&material
                            sides:BOTH_SURFACES];
            if(kemo_sgl->kemo_buffers->mesh_grid_buf->num_nod_buf > 0){
                _vertices[52] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->mesh_grid_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->mesh_grid_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                [_renderEncoder setCullMode:MTLCullModeBack];
                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[5]];
                [_renderEncoder setVertexBuffer:_vertices[52]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->mesh_grid_buf->num_nod_buf];
            }
            [self drawSolidObject:kemo_sgl->kemo_buffers->mesh_solid_buf
                          encoder:&_renderEncoder
                           vertex:&_vertices[53]
                           lights:&lights
                        materials:&material
                            sides:kemo_sgl->kemo_mesh->mesh_m->polygon_mode];
            
            if(kemo_sgl->kemo_buffers->cube_buf->num_nod_buf > 0){
                _vertices[30] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->cube_buf->v_buf)
                                                     length:(kemo_sgl->kemo_buffers->cube_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
                _index_buffer = [_device newBufferWithBytes:kemo_sgl->kemo_buffers->cube_index_buf->ie_buf
                                                     length:(kemo_sgl->kemo_buffers->cube_index_buf->nsize_buf * sizeof(unsigned int))
                                                    options:MTLResourceStorageModeShared];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                [_renderEncoder setCullMode:MTLCullModeBack];
                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[4]];
                [_renderEncoder setVertexBuffer:_vertices[30]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                [_renderEncoder setVertexBytes:&_normal_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelNormalMatrix];
                
                [_renderEncoder setFragmentBytes:&lights
                                         length:(sizeof(LightSourceParameters))
                                        atIndex:AAPLLightsParams];
                [_renderEncoder setFragmentBytes:&material
                                         length:sizeof(MaterialParameters)
                                        atIndex:AAPLMaterialParams];
                
                [_renderEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                          indexCount:36
                                           indexType:MTLIndexTypeUInt32
                                         indexBuffer:_index_buffer
                                   indexBufferOffset:0];
            }
            
            if(kemo_sgl->kemo_buffers->axis_buf->num_nod_buf > 0){
                _vertices[43] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->axis_buf->v_buf)
                                                     length:(kemo_sgl->kemo_buffers->axis_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                //                [_renderEncoder setCullMode:MTLCullModeBack];
                [_renderEncoder setCullMode:MTLCullModeNone];
                //                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[4]];
                [_renderEncoder setVertexBuffer:_vertices[43]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                
                [_renderEncoder setVertexBytes:&_normal_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelNormalMatrix];
                
                [_renderEncoder setFragmentBytes:&lights
                                         length:(sizeof(LightSourceParameters))
                                        atIndex:AAPLLightsParams];
                [_renderEncoder setFragmentBytes:&material
                                         length:sizeof(MaterialParameters)
                                        atIndex:AAPLMaterialParams];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->axis_buf->num_nod_buf];
            }
            
            if(kemo_sgl->kemo_buffers->coast_buf->num_nod_buf > 0){
                _vertices[41] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->coast_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->coast_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                [_renderEncoder setDepthStencilState:_depthState];
                [_renderEncoder setRenderPipelineState:_pipelineState[5]];
                [_renderEncoder setVertexBuffer:_vertices[41]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->coast_buf->num_nod_buf];
            };
            
            if(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf > 0){
                _vertices[42] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->sph_grid_buf->v_buf)
                                                     length:(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf * sizeof(KemoViewVertex))
                                                    options:MTLResourceStorageModeShared];
                [_renderEncoder setDepthStencilState:_depthState];
                [_renderEncoder setRenderPipelineState:_pipelineState[5]];
                [_renderEncoder setVertexBuffer:_vertices[42]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeLine
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf];
            };
/*  Draw transparent objects */
            if(kemo_sgl->kemo_buffers->PSF_ttxur_buf->num_nod_buf > 0){
                _vertices[38] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->PSF_ttxur_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->PSF_ttxur_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                
                /* Construct time texture */
                MTLTextureDescriptor *textureDescriptor12 = [[MTLTextureDescriptor alloc] init];
                textureDescriptor12.pixelFormat = MTLPixelFormatRGBA8Unorm;
                textureDescriptor12.width =  psf_ttexure->texture_width;
                textureDescriptor12.height = psf_ttexure->texture_height;
                NSUInteger bytesPerRow11;
                bytesPerRow11 = 4 * textureDescriptor12.width;
                MTLRegion region12 = {
                    { 0, 0, 0 },                   // MTLOrigin
                    {textureDescriptor12.width, textureDescriptor12.height, 1} // MTLSize
                };
                _texture[12] = [_device newTextureWithDescriptor:textureDescriptor12];
                [_texture[12] replaceRegion:region12
                                mipmapLevel:0
                                  withBytes:psf_ttexure->texture_rgba
                                bytesPerRow:bytesPerRow11];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                [_renderEncoder setCullMode:MTLCullModeNone];
//                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[5]];
                [_renderEncoder setVertexBuffer:_vertices[38]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                [_renderEncoder setVertexBytes:&_normal_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelNormalMatrix];
                
                [_renderEncoder setFragmentBytes:&lights
                                         length:(sizeof(LightSourceParameters))
                                        atIndex:AAPLLightsParams];
                [_renderEncoder setFragmentBytes:&material
                                         length:sizeof(MaterialParameters)
                                        atIndex:AAPLMaterialParams];
                [_renderEncoder setFragmentTexture:_texture[12]
                                          atIndex:AAPLTextureImageIndex];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->PSF_ttxur_buf->num_nod_buf];
            }
            if(kemo_sgl->kemo_buffers->PSF_trns_buf->num_nod_buf > 0){
                _vertices[37] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->PSF_trns_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->PSF_trns_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                [_renderEncoder setCullMode:MTLCullModeNone];
//                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[4]];
                [_renderEncoder setVertexBuffer:_vertices[37]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                [_renderEncoder setVertexBytes:&_normal_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelNormalMatrix];
                
                [_renderEncoder setFragmentBytes:&lights
                                         length:(sizeof(LightSourceParameters))
                                        atIndex:AAPLLightsParams];
                [_renderEncoder setFragmentBytes:&material
                                         length:sizeof(MaterialParameters)
                                        atIndex:AAPLMaterialParams];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->PSF_trns_buf->num_nod_buf];
            }
            
            if(kemo_sgl->kemo_buffers->mesh_trns_buf->num_nod_buf > 0){
                _vertices[54] = [_device newBufferWithBytesNoCopy:kemo_sgl->kemo_buffers->mesh_trns_buf->v_buf
                                                           length:(kemo_sgl->kemo_buffers->mesh_trns_buf->nsize_buf * sizeof(float))
                                                          options:MTLResourceStorageModeShared
                                                      deallocator:nil];
                
                [_renderEncoder setFrontFacingWinding:MTLWindingCounterClockwise];
                [_renderEncoder setTriangleFillMode:MTLTriangleFillModeFill];
                [_renderEncoder setCullMode:MTLCullModeBack];
                [_renderEncoder setDepthStencilState:_depthState];
                
                [_renderEncoder setRenderPipelineState:_pipelineState[4]];
                [_renderEncoder setVertexBuffer:_vertices[54]
                                        offset:0
                                       atIndex:AAPLVertexInputIndexVertices];
                [_renderEncoder setVertexBytes:&_modelview_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelViewMatrix];
                [_renderEncoder setVertexBytes:&_projection_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLProjectionMatrix];
                [_renderEncoder setVertexBytes:&_normal_mat
                                       length:sizeof(matrix_float4x4)
                                      atIndex:AAPLModelNormalMatrix];
                
                [_renderEncoder setFragmentBytes:&lights
                                         length:(sizeof(LightSourceParameters))
                                        atIndex:AAPLLightsParams];
                [_renderEncoder setFragmentBytes:&material
                                         length:sizeof(MaterialParameters)
                                        atIndex:AAPLMaterialParams];
                
                [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                  vertexStart:0
                                  vertexCount:kemo_sgl->kemo_buffers->mesh_trns_buf->num_nod_buf];
            }
            

            
        };


        if(kemo_sgl->kemo_buffers->msg_buf->num_nod_buf > 0){
            _vertices[1] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->msg_buf->v_buf)
                                                length:(kemo_sgl->kemo_buffers->msg_buf->num_nod_buf * sizeof(KemoViewVertex))
                                               options:MTLResourceStorageModeShared];
            [_renderEncoder setRenderPipelineState:_pipelineState[2]];
            // Pass in the parameter data.
            [_renderEncoder setVertexBuffer:_vertices[1]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            
            [_renderEncoder setVertexBytes:&_cbar_proj_mat
                                   length:sizeof(_cbar_proj_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            
            // Set the texture object.  The AAPLTextureIndexBaseColor enum value corresponds
            ///  to the 'colorMap' argument in the 'samplingShader' function because its
            //   texture attribute qualifier also uses AAPLTextureIndexBaseColor for its index.
            [_renderEncoder setFragmentTexture:_texture[1]
                                      atIndex:AAPLTextureIndexBaseColor];
            
            // Draw the triangles.
            [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:kemo_sgl->kemo_buffers->msg_buf->num_nod_buf];
        };

/*  Commands to render time label */
        if(kemo_sgl->kemo_buffers->time_buf->num_nod_buf > 0){
            _vertices[2] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->time_buf->v_buf)
                                                length:(kemo_sgl->kemo_buffers->time_buf->num_nod_buf * sizeof(KemoViewVertex))
                                               options:MTLResourceStorageModeShared];
            [_renderEncoder setRenderPipelineState:_pipelineState[2]];
            [_renderEncoder setVertexBuffer:_vertices[2]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [_renderEncoder setVertexBytes:&_cbar_proj_mat
                                   length:sizeof(_cbar_proj_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [_renderEncoder setFragmentTexture:_texture[2]
                                      atIndex:AAPLTextureIndexBaseColor];
            [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:kemo_sgl->kemo_buffers->time_buf->num_nod_buf];
        };
/*  Commands to render colorbar  box */
        if(kemo_sgl->kemo_buffers->cbar_buf->num_nod_buf > 0){
            _vertices[3] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->cbar_buf->v_buf)
                                                length:(kemo_sgl->kemo_buffers->cbar_buf->num_nod_buf * sizeof(KemoViewVertex))
                                               options:MTLResourceStorageModeShared];
            [_renderEncoder setRenderPipelineState:_pipelineState[1]];
            [_renderEncoder setVertexBuffer:_vertices[3]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [_renderEncoder setVertexBytes:&_cbar_proj_mat
                                   length:sizeof(_cbar_proj_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:kemo_sgl->kemo_buffers->cbar_buf->num_nod_buf];
        };
/*  Commands to render colorbar  label */
        if(kemo_sgl->kemo_buffers->min_buf->num_nod_buf > 0){
            _vertices[4] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->min_buf->v_buf)
                                                length:(kemo_sgl->kemo_buffers->min_buf->num_nod_buf * sizeof(KemoViewVertex))
                                               options:MTLResourceStorageModeShared];
            [_renderEncoder setRenderPipelineState:_pipelineState[2]];
            [_renderEncoder setVertexBuffer:_vertices[4]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [_renderEncoder setVertexBytes:&_cbar_proj_mat
                                   length:sizeof(_cbar_proj_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [_renderEncoder setFragmentTexture:_texture[4]
                                      atIndex:AAPLTextureIndexBaseColor];
            [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:kemo_sgl->kemo_buffers->min_buf->num_nod_buf];
        };
        if(kemo_sgl->kemo_buffers->max_buf->num_nod_buf > 0){
            _vertices[5] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->max_buf->v_buf)
                                                length:(kemo_sgl->kemo_buffers->max_buf->num_nod_buf * sizeof(KemoViewVertex))
                                               options:MTLResourceStorageModeShared];
            [_renderEncoder setRenderPipelineState:_pipelineState[2]];
            [_renderEncoder setVertexBuffer:_vertices[5]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [_renderEncoder setVertexBytes:&_cbar_proj_mat
                                   length:sizeof(_cbar_proj_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [_renderEncoder setFragmentTexture:_texture[5]
                                      atIndex:AAPLTextureIndexBaseColor];
            [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:kemo_sgl->kemo_buffers->max_buf->num_nod_buf];
        };
        if(kemo_sgl->kemo_buffers->zero_buf->num_nod_buf > 0){
            _vertices[6] = [_device newBufferWithBytes:((KemoViewVertex *) kemo_sgl->kemo_buffers->zero_buf->v_buf)
                                                length:(kemo_sgl->kemo_buffers->zero_buf->num_nod_buf * sizeof(KemoViewVertex))
                                               options:MTLResourceStorageModeShared];
            [_renderEncoder setRenderPipelineState:_pipelineState[2]];
            [_renderEncoder setVertexBuffer:_vertices[6]
                                    offset:0
                                   atIndex:AAPLVertexInputIndexVertices];
            [_renderEncoder setVertexBytes:&_cbar_proj_mat
                                   length:sizeof(_cbar_proj_mat)
                                  atIndex:AAPLOrthogonalMatrix];
            [_renderEncoder setFragmentTexture:_texture[6]
                                      atIndex:AAPLTextureIndexBaseColor];
            [_renderEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                              vertexStart:0
                              vertexCount:kemo_sgl->kemo_buffers->zero_buf->num_nod_buf];
        };

        [_renderEncoder endEncoding];

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

    if(kemo_sgl->kemo_buffers->MAP_solid_buf->num_nod_buf > 0){
//        [_vertices[10] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[10] release];
    };

    if(kemo_sgl->kemo_buffers->MAP_isoline_buf->num_nod_buf > 0){
//        [_vertices[11] setPurgeableState:MTLPurgeableStateEmpty];
//        [_vertices[11] release];
    };

    if(kemo_sgl->kemo_buffers->coast_buf->num_nod_buf > 0){
//        [_vertices[12] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[12] release];
    };
    
    if(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf > 0){
//        [_vertices[13] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[13] release];
    };

    if(kemo_sgl->kemo_buffers->coast_buf->num_nod_buf > 0){
//        [_vertices[12] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[12] release];
    };
    
    if(kemo_sgl->kemo_buffers->sph_grid_buf->num_nod_buf > 0){
//        [_vertices[13] setPurgeableState:MTLPurgeableStateEmpty];
        [_vertices[13] release];
    };

    if(kemo_sgl->kemo_buffers->axis_buf->num_nod_buf > 0){[_vertices[43] release];};

//    [_vertices[0] setPurgeableState:MTLPurgeableStateEmpty];
//    [_vertices[0] release];
}



/// Called whenever view changes orientation or is resized
- (void)mtkView:(nonnull MTKView *)view drawableSizeWillChange:(CGSize)size
{
    // Save the size of the drawable to pass to the vertex shader.
    _viewportSize.x = size.width;
    _viewportSize.y = size.height;
}

@end
