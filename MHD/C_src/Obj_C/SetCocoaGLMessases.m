//
//  SetCocoaGLMessases.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/02/28.
//  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "SetCocoaGLMessases.h"


static CFAbsoluteTime gStartTime = 0.0f;
CFAbsoluteTime msgTime; // message posting time for expiration
// time and message info
CFAbsoluteTime gMsgPresistance = 5.0f;

GLString * gErrStringTex;
float gErrorTime;

// set app start time
static void setStartTime()
{	
	gStartTime = CFAbsoluteTimeGetCurrent ();
}
// return float elpased time in seconds since app start
CFAbsoluteTime getElapsedTime ()
{	
	return CFAbsoluteTimeGetCurrent () - gStartTime;
}

bool checkForceRedraw()
{
	if (((getElapsedTime () - msgTime) < gMsgPresistance)
		|| ((getElapsedTime () - gErrorTime) < gMsgPresistance))
		return YES;
	else {
		return NO;
	}
}

// ---------------------------------

// error reporting as both window message and debugger string
static void reportError (char * strError)
{
    NSMutableDictionary *attribs = [NSMutableDictionary dictionary];
    [attribs setObject: [NSFont fontWithName: @"Monaco" size: 12.0f] forKey: NSFontAttributeName];
    [attribs setObject: [NSColor whiteColor] forKey: NSForegroundColorAttributeName];
	
	gErrorTime = getElapsedTime ();
	NSString * errString = [NSString stringWithFormat:@"Error: %s (at time: %0.1f secs).", strError, gErrorTime];
	NSLog (@"%@\n", errString);
	if (gErrStringTex)
		[gErrStringTex setString:errString withAttributes:attribs];
	else {
		gErrStringTex = [[GLString alloc] initWithString:errString withAttributes:attribs withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:1.0f green:0.0f blue:0.0f alpha:0.3f] withBorderColor:[NSColor colorWithDeviceRed:1.0f green:0.0f blue:0.0f alpha:0.8f]];
	}
}

// ---------------------------------

// if error dump gl errors to debugger string, return error
GLenum glReportError ()
{
	GLenum err = glGetError();
	if (GL_NO_ERROR != err)
		reportError ((char *) gluErrorString (err));
	return err;
}

@implementation SetCocoaGLMessases
-(void) setQuickHelpFlag:(NSInteger)flag
{
    fDrawHelp = flag;
}
- (void) setDrawInfoFlag : (NSInteger)flag
{
	fDrawinfo = flag;
}



// these functions create or update GLStrings one should expect to have to regenerate the image, bitmap and texture when the string changes thus these functions are not particularly light weight

- (void) updateInfoString
{ // update info string texture
	NSString * string = [NSString stringWithFormat:@"Graphic Driver\n %s \n%s", glGetString (GL_RENDERER), glGetString (GL_VERSION)];
	if (infoStringTex)
		[infoStringTex setString:string withAttributes:stanStringAttrib];
	else {
		infoStringTex = [[GLString alloc] initWithString:string withAttributes:stanStringAttrib withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:0.5f green:0.5f blue:0.5f alpha:0.5f] withBorderColor:[NSColor colorWithDeviceRed:0.8f green:0.8f blue:0.8f alpha:0.8f]];
	}
}

- (void) updateRsolutionString:(GLfloat)xWinSize :(GLfloat)yWinSize
{ // update info string texture
	NSString * string = [NSString stringWithFormat:@"Resolution: (%0.0f x %0.0f)", xWinSize, yWinSize];
	if (msgStringTex)
		[msgStringTex setString:string withAttributes:stanStringAttrib];
	else {
		msgStringTex = [[GLString alloc] initWithString:string withAttributes:stanStringAttrib withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:0.5f green:0.5f blue:0.5f alpha:0.5f] withBorderColor:[NSColor colorWithDeviceRed:0.8f green:0.8f blue:0.8f alpha:0.8f]];
	}
    msgTime = getElapsedTime ();
    fDrawResolution = YES;
}

// ---------------------------------

- (void) createHelpString
{
	NSString * string = [NSString stringWithFormat:@"Cmd-Shift-A: animate \nCmd-Shift-I: show driver info"];
	helpStringTex = [[GLString alloc] initWithString:string withAttributes:stanStringAttrib withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:0.0f green:0.5f blue:0.0f alpha:0.5f] withBorderColor:[NSColor colorWithDeviceRed:0.3f green:0.8f blue:0.3f alpha:0.8f]];
}

// draw text info using our GLString class for much more optimized text drawing

- (void) drawInfo : (GLfloat) xWinSize : (GLfloat) yWinSize
{
	GLint matrixMode;
	GLboolean depthTest = glIsEnabled (GL_DEPTH_TEST);
	GLfloat messageTop;
	GLfloat TextColor4f[4], ErrTextColor4f[4];
    
    if(fDrawHelp == NO && fDrawResolution==NO && fDrawinfo==NO) return;
	send_text_color_code(TextColor4f);
	
	// set orthograhic 1:1  pixel transform in local view coords
	glGetIntegerv (GL_MATRIX_MODE, &matrixMode);
	glMatrixMode (GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity ();
	glMatrixMode (GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity ();
	glScalef (1.0f / xWinSize, -1.0f / yWinSize, 1.0f);
	
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, TextColor4f);
    
    messageTop = -floor(yWinSize) / 1.05;
	if (fDrawinfo){
        [infoStringTex drawAtPoint:NSMakePoint (-floor(xWinSize) / 1.05,
                                                (floor(yWinSize) / 1.05 - 2.0*[infoStringTex frameSize].height))];
    }

	// help string
	if (fDrawHelp){
		[helpStringTex drawAtPoint:NSMakePoint (floor (-[helpStringTex frameSize].width),
                                                floor (-[helpStringTex frameSize].height))];
	}
    
	float currTime = getElapsedTime ();
	if ((currTime - msgTime) < gMsgPresistance) {
    // Resoluiton string
        if (fDrawResolution){
            TextColor4f[3] = (gMsgPresistance - getElapsedTime () + msgTime) * 0.2; // premultiplied fade
            glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE, TextColor4f);
            [msgStringTex drawAtPoint:NSMakePoint (-floor(xWinSize) / 1.05,
                                                   messageTop)];
            messageTop += [msgStringTex frameSize].height + 28.0f;
        }
	} else {
        fDrawResolution = NO;
    }
	// global error message
	if ((currTime - gErrorTime) < gMsgPresistance) {
		ErrTextColor4f[3] = (gMsgPresistance - getElapsedTime () + gErrorTime) * 0.2; // premultiplied fade
		ErrTextColor4f[0] = ONE;
		ErrTextColor4f[1] = ZERO;
		ErrTextColor4f[2] = ZERO;
		glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE, ErrTextColor4f);
		[gErrStringTex drawAtPoint:NSMakePoint (-floor(xWinSize) / 1.05, messageTop)];
	}
	
	// reset orginal martices
	glPopMatrix(); // GL_MODELVIEW
	glMatrixMode (GL_PROJECTION);
	glPopMatrix();
	glMatrixMode (matrixMode);
	
	glDisable (GL_TEXTURE_RECTANGLE_EXT);
	glDisable (GL_BLEND);
	if (depthTest) glEnable (GL_DEPTH_TEST);
	glReportError ();
}

- (void) awakeFromNib;
{
	setStartTime (); // get app start time

	fDrawHelp = IZERO;
    fDrawResolution = IONE;

	// init fonts for use with strings
	NSFont * font =[NSFont fontWithName:@"Helvetica" size:18.0];
	stanStringAttrib = [[NSMutableDictionary dictionary] retain];
	[stanStringAttrib setObject:font forKey:NSFontAttributeName];
	[stanStringAttrib setObject:[NSColor whiteColor] forKey:NSForegroundColorAttributeName];
	[font release];
	
	// ensure strings are created
	[self createHelpString];
}



@end
