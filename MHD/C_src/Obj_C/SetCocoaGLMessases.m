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
CFAbsoluteTime gMsgPresistance = 10.0f;

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
    [attribs setObject: [NSFont fontWithName: @"Monaco" size: 9.0f] forKey: NSFontAttributeName];
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


// these functions create or update GLStrings one should expect to have to regenerate the image, bitmap and texture when the string changes thus these functions are not particularly light weight

- (void) updateInfoString:(GLfloat)xWinSize :(GLfloat)yWinSize
{ // update info string texture
	NSString * string = [NSString stringWithFormat:@"(%0.0f x %0.0f) \n%s \n%s", xWinSize, yWinSize, glGetString (GL_RENDERER), glGetString (GL_VERSION)];
	if (infoStringTex)
		[infoStringTex setString:string withAttributes:stanStringAttrib];
	else {
		infoStringTex = [[GLString alloc] initWithString:string withAttributes:stanStringAttrib withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:0.5f green:0.5f blue:0.5f alpha:0.5f] withBorderColor:[NSColor colorWithDeviceRed:0.8f green:0.8f blue:0.8f alpha:0.8f]];
	}
}

// ---------------------------------

- (void) createHelpString
{
	NSString * string = [NSString stringWithFormat:@"Cmd-Shift-A: animate    Cmd-Shift-I: show info"];
	helpStringTex = [[GLString alloc] initWithString:string withAttributes:stanStringAttrib withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:0.0f green:0.5f blue:0.0f alpha:0.5f] withBorderColor:[NSColor colorWithDeviceRed:0.3f green:0.8f blue:0.3f alpha:0.8f]];
}

// ---------------------------------

- (void) createMessageString
{
	NSString * string = [NSString stringWithFormat:@"No messages..."];
	msgStringTex = [[GLString alloc] initWithString:string withAttributes:stanStringAttrib withTextColor:[NSColor colorWithDeviceRed:1.0f green:1.0f blue:1.0f alpha:1.0f] withBoxColor:[NSColor colorWithDeviceRed:0.5f green:0.5f blue:0.5f alpha:0.5f] withBorderColor:[NSColor colorWithDeviceRed:0.8f green:0.8f blue:0.8f alpha:0.8f]];
}

// draw text info using our GLString class for much more optimized text drawing
- (void) drawInfo:(GLfloat) xWinSize:(GLfloat) yWinSize
{	
	GLint matrixMode;
	GLboolean depthTest = glIsEnabled (GL_DEPTH_TEST);
	GLfloat messageTop = 10.0f;
	GLfloat TextColor4f[4], ErrTextColor4f[4];
		
	send_text_color_code(TextColor4f);
	
	// set orthograhic 1:1  pixel transform in local view coords
	glGetIntegerv (GL_MATRIX_MODE, &matrixMode);
	glMatrixMode (GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity ();
	glMatrixMode (GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity ();
	glScalef (2.0f / xWinSize, -2.0f / xWinSize, 1.0f);
	glTranslatef (-xWinSize / 2.0f, -xWinSize / 2.0f, 0.0f);
	
	glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE, TextColor4f);
	[infoStringTex drawAtPoint:NSMakePoint (10.0f, xWinSize - [infoStringTex frameSize].height - 10.0f)];
	[camStringTex drawAtPoint:NSMakePoint (10.0f, messageTop)];
	messageTop += [camStringTex frameSize].height + 3.0f;
	if (fDrawHelp){
		[helpStringTex drawAtPoint:NSMakePoint (floor ((xWinSize - [helpStringTex frameSize].width) / 2.0f), floor ((yWinSize - [helpStringTex frameSize].height) / 3.0f))];
	};		
	// message string
	float currTime = getElapsedTime ();
	if ((currTime - msgTime) < gMsgPresistance) {
		TextColor4f[3] = (gMsgPresistance - getElapsedTime () + msgTime) * 0.1; // premultiplied fade
		glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE, TextColor4f);
		[msgStringTex drawAtPoint:NSMakePoint (10.0f, messageTop)];
		messageTop += [msgStringTex frameSize].height + 3.0f;
	}
	// global error message
	if ((currTime - gErrorTime) < gMsgPresistance) {
		ErrTextColor4f[3] = (gMsgPresistance - getElapsedTime () + gErrorTime) * 0.1; // premultiplied fade
		ErrTextColor4f[0] = ONE;
		ErrTextColor4f[1] = ZERO;
		ErrTextColor4f[2] = ZERO;
		glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE, ErrTextColor4f);
		[gErrStringTex drawAtPoint:NSMakePoint (10.0f, messageTop)];
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

	fDrawHelp = 1;

	// init fonts for use with strings
	NSFont * font =[NSFont fontWithName:@"Helvetica" size:12.0];
	stanStringAttrib = [[NSMutableDictionary dictionary] retain];
	[stanStringAttrib setObject:font forKey:NSFontAttributeName];
	[stanStringAttrib setObject:[NSColor whiteColor] forKey:NSForegroundColorAttributeName];
	[font release];
	
	// ensure strings are created
	[self createHelpString];
	[self createMessageString];
}
@end
