/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for a platform independent renderer class, which performs Metal setup and per frame rendering.
*/

@import MetalKit;

#import "AAPLShaderTypes.h"
#import "AAPLImage.h"
#import "KemoViewerObject.h"
#import "KemoViewRendererTools.h"
#import "KemoView2DRenderer.h"
#import "KemoView3DRenderer.h"
#import "KemoViewMetalBuffers.h"

#include "m_kemoviewer_data.h"
#include "m_kemoview_object_buffers.h"
#include "m_gl_transfer_matrix.h"

@interface AAPLRenderer : NSObject<MTKViewDelegate>{
    KemoViewMetalBuffers * _kemoMetalBufBase;
    KemoView2DRenderer *   _kemo2DRenderer;
    KemoView3DRenderer *   _kemo3DRenderer;
    KemoViewRendererTools * _kemoRendererTools;
}

- (void) setKemoViewPointer:(struct kemoviewer_type *) kemo_sgl;

- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;

- (id<MTLTexture>_Nonnull) drawKemoViewToTexure:(struct kemoviewer_type *) kemo_sgl
                                      metalView:(nonnull MTKView *)view
                                         unites:(KemoViewUnites *) viewUnites;
- (id<MTLTexture>_Nonnull)KemoViewToTexure:(nonnull MTKView *)view
                                  kemoview:(struct kemoviewer_type *) kemo_sgl;

- (void)drawInMTKView:(nonnull MTKView *)view;

@end
