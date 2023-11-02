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
    IBOutlet KemoViewerMetalView *_view;

    AAPLRenderer *_renderer;
}

-(void) init
{
    return;
}

-(void) awakeFromNib
{
    printf("TAko1 \n");
    _view.enableSetNeedsDisplay = YES;
    
    printf("TAko2 \n");
    _view.device = MTLCreateSystemDefaultDevice();
    
    printf("TAko3 \n");
    _view.clearColor = MTLClearColorMake(0.0, 0.5, 1.0, 1.0);

    printf("TAkoTako \n");

    _renderer = [[AAPLRenderer alloc] initWithMetalKitView:_view];

    if(!_renderer)
    {
        NSLog(@"Renderer initialization failed");
        return;
    }

    // Initialize the renderer with the view size.
    [_renderer mtkView:_view drawableSizeWillChange:_view.drawableSize];
    printf("TAko 5 \n");

    
    _view.delegate = _renderer;
    printf("TAko 6 \n");

    return;
}
@end
