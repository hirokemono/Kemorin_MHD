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

-(unsigned char *) getRenderedbyMetalToBGRA
{
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    [_renderer drawInMTKView:_metalView];
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW);

    /*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture = _metalView.currentDrawable.texture;
    NSUInteger height = _imageOutputTexture.height;
    NSUInteger width =  _imageOutputTexture.width;
    NSUInteger bpRaw = 4 * _imageOutputTexture.width;
    NSUInteger num_pixel = _imageOutputTexture.width * _imageOutputTexture.height;
    unsigned char *bgra = (unsigned char *) malloc(4*num_pixel * sizeof(unsigned char));

    [_imageOutputTexture getBytes:bgra
                      bytesPerRow:bpRaw
                       fromRegion:MTLRegionMake2D(0, 0, width, height)
                      mipmapLevel:0];
    return bgra;
};

-(CGImageRef) getRenderedbyMetalToCGref
{
    /*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture = _metalView.currentDrawable.texture;
    NSUInteger height = _imageOutputTexture.height;
    NSUInteger width =  _imageOutputTexture.width;
    NSUInteger bpRaw = 4 * _imageOutputTexture.width;
    NSUInteger num_pixel = _imageOutputTexture.width * _imageOutputTexture.height;
    NSUInteger i;
    unsigned char *bgra = [self getRenderedbyMetalToBGRA];
    unsigned char rtmp;
    
    for(i=0;i<num_pixel;i++){
        rtmp = bgra[4*i];
        bgra[4*i] = bgra[4*i+2];
        bgra[4*i+2] = rtmp;
    }
    
    int bitsPerComponent = 8;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(bgra, width, height,
                                                 bitsPerComponent, bpRaw, colorSpace,
                                                 (kCGBitmapAlphaInfoMask & kCGImageAlphaPremultipliedLast));
    free(bgra);
    CGColorSpaceRelease(colorSpace);
    CGImageRef dstImage = CGBitmapContextCreateImage(context);
    return dstImage;
};

-(void) getRenderedbyMetal:(NSBitmapImageRep *) imageRep
{
    CGImageRef dstImage = [self getRenderedbyMetalToCGref];
    [imageRep initWithCGImage:dstImage];
    if(dstImage) {CGImageRelease(dstImage);};
    return;
};
@end
