/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for a platform independent renderer class, which performs Metal setup and per frame rendering.
*/

@import MetalKit;

#import "KemoViewShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewerObject.h"
#import "KemoViewRendererTools.h"
#import "KemoView2DRenderer.h"
#import "KemoView3DRenderer.h"
#import "KemoViewMetalBuffers.h"
#import "KemoViewMapRenderer.h"
#import "KemoViewMessageRenderer.h"

#include "m_kemoviewer_data.h"
#include "m_kemoview_object_buffers.h"
#include "m_gl_transfer_matrix.h"

@interface KemoViewerRenderer : NSObject<MTKViewDelegate>

- (void) setKemoViewPointer:(struct kemoviewer_type *_Nonnull) kemo_sgl;

- (void)refreshKemoViewTripleBuffers:(struct kemoviewer_type *_Nonnull) kemo_sgl;

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;

- (id<MTLTexture>_Nonnull)KemoViewToTexure:(nonnull MTKView *)view
                                  kemoview:(struct kemoviewer_type *_Nonnull) kemo_sgl;

- (void)drawInMTKView:(nonnull MTKView *)view;
@end
