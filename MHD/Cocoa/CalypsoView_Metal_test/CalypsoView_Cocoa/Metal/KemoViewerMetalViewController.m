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
@end
