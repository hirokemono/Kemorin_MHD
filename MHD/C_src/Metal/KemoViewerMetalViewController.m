//
//  KemoViewerMetalViewController.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/2/23.
//

#import <Foundation/Foundation.h>
#import "KemoViewerMetalViewController.h"

@implementation KemoViewerMetalViewController
{
    IBOutlet KemoViewerMetalView *_metalView;

    IBOutlet AAPLRenderer *_renderer;
}

-(void) awakeFromNib
{
    _metalView.enableSetNeedsDisplay = YES;
/*    viewDidLoad is called by linkning self.viwew to metal view */
    self.view = _metalView;
//    printf("TAko0 %d \n", self.viewLoaded);

    _metalView.id_window = kemoview_get_current_viewer_id();
    return;
}

-(void) viewDidLoad
{
    [super viewDidLoad];
    
    _metalView.device = MTLCreateSystemDefaultDevice();
    _metalView.framebufferOnly = false;
    
    [_metalView updateBackground];

    _renderer = [[AAPLRenderer alloc] initWithMetalKitView:_metalView];

    if(!_renderer)
    {
        NSLog(@"Renderer initialization failed");
        return;
    }

    // Initialize the renderer with the view size.
    [_renderer mtkView:_metalView drawableSizeWillChange:_metalView.drawableSize];
    
    _metalView.delegate = _renderer;

    return;
}

- (void)viewDidLayout
{
    [_metalView setViewerSize];
    return;
};

-(nonnull void *) loadImageOutputTexture
{
    return [_renderer loadImageOutputTextureFromRenderer];
}

-(void) FastUpdateImageController:(NSImage *) Image
{
    NSRect rectView = [_metalView convertRectToBacking:[_metalView bounds]];
    
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    [_renderer drawInMTKView:_metalView];
    
    /*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture = _metalView.currentDrawable.texture;
    int npix[2];
    npix[0] = (int) _imageOutputTexture.width;
    npix[1] = (int) _imageOutputTexture.height;
    NSUInteger bpRaw = 4 * npix[0];
    unsigned char *bgra = (unsigned char *) malloc( (npix[1]*bpRaw) * sizeof(unsigned char));
    
    [_imageOutputTexture getBytes:bgra
                      bytesPerRow:bpRaw
                       fromRegion:MTLRegionMake2D(0, 0, (NSUInteger) npix[0], (NSUInteger) npix[1])
                      mipmapLevel:0];
    unsigned char rtmp;
    for(int i= 0;i<npix[0]*npix[1];i++){
        rtmp = bgra[4*i];
        bgra[4*i] = bgra[4*i+2];
        bgra[4*i+2] = rtmp;
    }
    
    
    int bitsPerComponent = 8;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(bgra, npix[0], npix[1], bitsPerComponent,
                                                 bpRaw, colorSpace,
                                                 (kCGBitmapAlphaInfoMask & kCGImageAlphaPremultipliedLast));
    CGColorSpaceRelease(colorSpace);
    CGImageRef dstImage = CGBitmapContextCreateImage(context);
    
    NSBitmapImageRep *imageRep = [[NSBitmapImageRep alloc] initWithCGImage:dstImage];
    [Image initWithSize:NSSizeFromCGSize(rectView.size)];
    [Image addRepresentation:imageRep];
    [imageRep release];
    return;
};
@end
