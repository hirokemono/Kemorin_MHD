//
//  PSFArrayControler.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/09/04.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "PsfController.h"


@interface PSFArrayControler : NSObject {
	IBOutlet NSWindow * _psfObject;

}

- (IBAction) OpenPsfWindow:(id)pId;
- (IBAction) ClosePsfWindow:(id)pId;

@end
