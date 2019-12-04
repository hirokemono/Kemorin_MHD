//
//  FlineController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "KemoviewerController.h"

@interface FlineController : NSObject {

    IBOutlet NSWindow*  window;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
	IBOutlet KemoviewerController*  _kemoviewControl;

	NSInteger DrawFlineFlag;
	NSString *FlineOpenDirectory;
	NSString *FlineOpenFilehead;
	NSString *FlineOpenFileext;
	NSString *FlineOpenFileheadExStep;
	
	NSString *FlineWindowlabel;
	
	NSInteger FlineNumberOfField;
	NSInteger FlineTotalComponent;
	NSMutableArray *FlineNumberOfComponent;
	NSMutableArray *FlineFieldName;
	NSMutableArray *FlineMinimum;
	NSMutableArray *FlineMaximum;
	
	NSNumber *FieldlineFlag;
	NSNumber *FlineDrawFieldId;
	NSNumber *FlineDrawComponentId;
	NSNumber *FieldlineColor;

	IBOutlet NSMatrix *_flinetype_matrix;
	NSInteger Flinetype;

	
	NSNumber *FlineMinimumRange;
	NSNumber *FlineMaximumRange;
	
	IBOutlet id _FlineFieldMenu;
	IBOutlet id _FlineComponentMenu;
	
	CGFloat FlineDisplayMinimum;
	CGFloat FlineDisplayMaximum;
	CGFloat FlineDisplayMinDigit;
	CGFloat FlineDisplayMaxDigit;
	
	
	CGFloat FlineMinimumValue;
	CGFloat FlineMaximumValue;
	
	CGFloat FlineThickFactor;
	CGFloat FlineThickDigit;

	IBOutlet id FieldlineColorItem;
}

@property(retain) NSString* FlineWindowlabel;

@property NSInteger DrawFlineFlag;
@property CGFloat FlineDisplayMinimum;
@property CGFloat FlineDisplayMaximum;
@property CGFloat FlineDisplayMinDigit;
@property CGFloat FlineDisplayMaxDigit;

@property CGFloat FlineMinimumValue;
@property CGFloat FlineMaximumValue;

@property NSInteger Flinetype;
@property CGFloat FlineThickFactor;
@property CGFloat FlineThickDigit;

- (id)init;
- (id)dealloc;

- (id) CopyFlineDisplayFlagsFromC;

- (void) OpenFieldlineFile:(NSString*) fieldlineFilehead;
- (IBAction) UpdateFieldline:(id)pId;
- (IBAction) DrawFlineFile:(id)pId;

- (IBAction) CloseFlineFile:(id)pId;
- (IBAction) FlineFieldAction:(id)sender;
- (IBAction) FlineComponentAction:(id)sender;

- (void) SetFlineFieldMenu;
- (void) SetFlineComponentMenu:(int)isel;

- (IBAction)ChooseFieldlineColorAction:(id)sender;

- (IBAction)ShowFlineRange:(id)pSender;
- (IBAction)ChooseFieldlineTypeAction:(id)sender;
- (IBAction)SetFieldlineThicknessAction:(id)sender;

@end
