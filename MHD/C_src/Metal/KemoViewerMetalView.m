/*
//  KemoViewerMetalView.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/1/23.
*/

#import "KemoViewerMetalView.h"

@implementation KemoViewerMetalView

-(void)InitBackGroundColor
{
    float BgColor4f[4];
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
	kemoview_set_background_color(BgColor4f);
    return;
};

-(void) updateBackground
{
    float bgcolor[4];
    [self InitBackGroundColor];
	kemoview_get_background_color(bgcolor);
    self.clearColor = MTLClearColorMake(bgcolor[0], bgcolor[1], bgcolor[2], bgcolor[3]);
    return;
}


@end
