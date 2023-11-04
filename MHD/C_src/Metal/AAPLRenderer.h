/*
See LICENSE folder for this sample’s licensing information.

Abstract:
Header for a platform independent renderer class, which performs Metal setup and per frame rendering.
*/

@import MetalKit;

#import "AAPLShaderTypes.h"
#import "AAPLImage.h"

@interface AAPLRenderer : NSObject<MTKViewDelegate>

- (id<MTLTexture>)loadTextureUsingAAPLImage: (NSURL *) url;
- (nonnull instancetype)initWithMetalKitView:(nonnull MTKView *)mtkView;

@end
