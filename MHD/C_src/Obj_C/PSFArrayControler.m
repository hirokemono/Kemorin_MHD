//
//  PSFArrayControler.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/04.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "PSFArrayControler.h"
int i = 0;

@implementation PSFArrayControler

- (IBAction) OpenPsfWindow:(id)pId;
{
	[_psfObject 
	 initWithContentRect:NSMakeRect(300.0, 300.0, 500.0,500.0) 
	 styleMask:(NSTitledWindowMask | NSClosableWindowMask)
	 backing:NSBackingStoreNonretained
	 defer:NO
	 screen:nil
	 ];
}
- (IBAction) ClosePsfWindow:(id)pId;
{
	[_psfObject close];
}

@end
