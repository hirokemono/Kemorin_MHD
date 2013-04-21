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

	GLString * helpStringTex;
	GLString * infoStringTex;
	GLString * msgStringTex;

	bool fDrawinfo;
	bool fDrawHelp;
	bool fDrawResolution;
}

CFAbsoluteTime getElapsedTime ();
GLenum glReportError ();
bool checkForceRedraw();

-(void) setQuickHelpFlag:(NSInteger)flag;

- (void) updateInfoString;
- (void) updateRsolutionString:(GLfloat)xWinSize :(GLfloat)yWinSize;
- (void) createHelpString;

- (void) drawInfo:(GLfloat) xWinSize : (GLfloat) yWinSize;

- (void) setDrawInfoFlag : (NSInteger)flag;
- (void) awakeFromNib;

@end
