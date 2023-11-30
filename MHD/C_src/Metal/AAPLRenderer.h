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


- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;

-(unsigned char *) getRenderedbyMetalToBGRA:(nonnull MTKView *)view
                                    eyeflag:(int) iflag_lr
                                     Pixels:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte;


- (id<MTLTexture>_Nonnull) drawKemoViewToTexure:(nonnull MTKView *)view
                                        eyeflag:(int) iflag_lr;
- (void)drawKemoMetalView:(nonnull MTKView *) view
                  eyeflag:(int) iflag_lr;

- (void)drawInMTKView:(nonnull MTKView *)view;

@end
