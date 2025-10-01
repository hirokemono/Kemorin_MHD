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

    IBOutlet KemoViewerRenderer *_renderer;
}

-(void) awakeFromNib
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoviewer_reset_to_init_angle(kemo_sgl);
    kemoview_init_lighting(kemo_sgl);

    _metalView.enableSetNeedsDisplay = TRUE;
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
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalView updateBackground:kemo_sgl];

    _renderer = [[KemoViewerRenderer alloc] initWithMetalKitView:_metalView];
    [_renderer setKemoViewPointer:[_kmv KemoViewPointer]];

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

- (void)refreshKemoViewTripleBuffersForRotation:(struct kemoviewer_type *) kemo_sgl
{
    [_renderer refreshKemoViewTripleBuffers:kemo_sgl];
    return;
}


-(unsigned char *) getRenderedbyMetalToBGRA:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte
                                   kemoview:(struct kemoviewer_type *) kemo_sgl
{
    id<MTLTexture> _imageOutputTexture = [_renderer KemoViewToTexure:_metalView
                                                            kemoview:kemo_sgl];

    /*    Texture to render screen to texture */
    pix_xy[0] = _imageOutputTexture.width;
    pix_xy[1] = _imageOutputTexture.height;
    pixelByte[0] = 4;
    NSUInteger bpRaw = pixelByte[0] * pix_xy[0] ;
    NSUInteger num_pixel = pix_xy[0] * pix_xy[1];
    unsigned char *bgra = (unsigned char *) malloc(pixelByte[0]*num_pixel * sizeof(unsigned char));
    if(bgra == NULL){
        printf("malloc error for bgra\n");
        exit(0);
    };

    [_imageOutputTexture getBytes:bgra
                      bytesPerRow:bpRaw
                       fromRegion:MTLRegionMake2D(0, 0, pix_xy[0], pix_xy[1])
                      mipmapLevel:0];
    return bgra;
};


-(CGImageRef) getRenderedbyMetalToCGref:(struct kemoviewer_type *) kemo_sgl
{
    unsigned char rtmp;
    NSUInteger i;
    NSUInteger pix_xy[2];
    NSUInteger pixelByte[1];
    unsigned char *bgra = [self getRenderedbyMetalToBGRA:pix_xy
                                            PixelPerByte:pixelByte
                                                kemoview:kemo_sgl];
    NSUInteger num_pixel = pix_xy[0] * pix_xy[1];
    NSUInteger bpRaw =     pixelByte[0] * pix_xy[0];

    
    for(i=0;i<num_pixel;i++){
        rtmp = bgra[4*i];
        bgra[4*i] = bgra[4*i+2];
        bgra[4*i+2] = rtmp;
        bgra[4*i+3] = 255;

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
                  kemoview:(struct kemoviewer_type *) kemo_sgl
{
    CGImageRef dstImage = [self getRenderedbyMetalToCGref:kemo_sgl];
    [imageRep initWithCGImage:dstImage];
    if(dstImage) {CGImageRelease(dstImage);};
    return;
};
@end
