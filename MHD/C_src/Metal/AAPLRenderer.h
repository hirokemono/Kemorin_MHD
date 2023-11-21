/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for a platform independent renderer class, which performs Metal setup and per frame rendering.
*/

@import MetalKit;

#import "AAPLShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewerObject.h"

#include "m_kemoviewer_data.h"
#include "m_gl_transfer_matrix.h"
#include "move_draw_objects_gl.h"
#include "vartex_array_object_gl.h"

@interface AAPLRenderer : NSObject<MTKViewDelegate>

- (void)setTransferMatrices;

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;
- (void)drawInMTKView:(nonnull MTKView *)view;

@end
