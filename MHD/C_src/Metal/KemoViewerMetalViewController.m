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
    kemoviewer_reset_to_init_angle();
    kemoview_init_lighting();

    _metalView.enableSetNeedsDisplay = YES;
/*    viewDidLoad is called by linkning self.viwew to metal view */
    self.view = _metalView;

    [_metalView initMessageTimer];
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

- (void) RenderUpdate
{
    [_renderer drawInMTKView:_metalView];
    return;
};

- (void)viewDidLayout
{
    [_metalView setViewerSize];
    return;
};

-(unsigned char *) getRenderedbyMetalToBGRA:(int) iflag_lr
                                     Pixels:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte
{
    kemoview_set_view_integer(ISET_DRAW_MODE, FAST_DRAW);
    [_renderer drawKemoMetalView:_metalView
                         eyeflag:iflag_lr];
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW);

    /*    Texture to render screen to texture */
    id<MTLTexture> _imageOutputTexture = _metalView.currentDrawable.texture;
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

-(unsigned char *) getAnaglyphbyMetalToBGRA:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte
{
    int i;
    unsigned char *left_bgra =  [self getRenderedbyMetalToBGRA:-1
                                                        Pixels:pix_xy
                                                  PixelPerByte:pixelByte];
    unsigned char *right_bgra = [self getRenderedbyMetalToBGRA:1
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


-(CGImageRef) getRenderedbyMetalToCGref
{
    unsigned char rtmp;
    NSUInteger i;
    NSUInteger pix_xy[2];
    NSUInteger pixelByte[1];
    unsigned char *bgra;
    if(kemoview_get_view_type_flag() == VIEW_STEREO){
        bgra = [self getAnaglyphbyMetalToBGRA:pix_xy
                                 PixelPerByte:pixelByte];
    }else{
        bgra = [self getRenderedbyMetalToBGRA:0
                                       Pixels:pix_xy
                                 PixelPerByte:pixelByte];
    }
    NSUInteger num_pixel = pix_xy[0] * pix_xy[1];
    NSUInteger bpRaw =     pixelByte[0] * pix_xy[0];

    
    for(i=0;i<num_pixel;i++){
        rtmp = bgra[4*i];
        bgra[4*i] = bgra[4*i+2];
        bgra[4*i+2] = rtmp;
    }
    
    int bitsPerComponent = 8;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef context = CGBitmapContextCreate(bgra, pix_xy[0], pix_xy[1],
                                                 bitsPerComponent, bpRaw, colorSpace,
                                                 (kCGBitmapAlphaInfoMask & kCGImageAlphaPremultipliedLast));
    CGColorSpaceRelease(colorSpace);
    CGImageRef dstImage = CGBitmapContextCreateImage(context);
    free(bgra);
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
