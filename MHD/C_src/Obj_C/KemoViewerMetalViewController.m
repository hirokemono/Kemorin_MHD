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

    AAPLRenderer *_renderer;
}

-(void) awakeFromNib
{
//    _metalView.enableSetNeedsDisplay = YES;
/*    viewDidLoad is called by linkning self.viwew to metal view */
    self.view = _metalView;
//    printf("TAko0 %d \n", self.viewLoaded);
    return;
}

-(void) viewDidLoad
{
    printf("TAko1 \n");
    [super viewDidLoad];
    
    printf("TAko2 \n");
    _metalView.device = MTLCreateSystemDefaultDevice();
    
    [_metalView updateBackground];
    printf("TAkoTako \n");

    _renderer = [[AAPLRenderer alloc] initWithMetalKitView:_metalView];

    if(!_renderer)
    {
        NSLog(@"Renderer initialization failed");
        return;
    }

    // Initialize the renderer with the view size.
    [_renderer mtkView:_metalView drawableSizeWillChange:_metalView.drawableSize];
    printf("TAko 5 \n");

    
    _metalView.delegate = _renderer;
    printf("TAko 6 \n");

    return;
}
@end
