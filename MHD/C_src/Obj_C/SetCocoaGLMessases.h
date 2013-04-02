//
//  SetCocoaGLMessases.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/02/28.
//  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "GLString.h"

#include "kemoviewer.h"


@interface SetCocoaGLMessases : NSObject {

	// string attributes
	NSMutableDictionary * stanStringAttrib;

	GLString * camStringTex;
	GLString * helpStringTex;
	GLString * infoStringTex;
	GLString * msgStringTex;

	bool fDrawHelp;
}

CFAbsoluteTime getElapsedTime ();
GLenum glReportError ();
bool checkForceRedraw();

-(void) setQuickHelpFlag:(NSInteger)flag;

- (void) updateInfoString:(GLfloat)xWinSize:(GLfloat)yWinSize;
- (void) createHelpString;
- (void) createMessageString;

- (void) drawInfo:(GLfloat) xWinSize:(GLfloat) yWinSize;

- (void) awakeFromNib;

@end
