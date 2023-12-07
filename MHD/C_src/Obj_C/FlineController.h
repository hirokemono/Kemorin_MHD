//
//  FlineController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "KemoviewerController.h"
#import "KemoViewerObject.h"

#include "Kemoviewer.h"

@interface FlineController : NSObject {

    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
	IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet KemoViewerObject *_kmv;

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

- (void) OpenFieldlineFile:(NSString*) fieldlineFilehead
                  kemoview:(struct kemoviewer_type *) kemo_sgl;
- (IBAction) UpdateFieldline:(id)pId;
- (IBAction) DrawFlineFile:(id)pId;

- (IBAction) CloseFlineFile:(id)pId;
- (IBAction) FlineFieldAction:(id)sender;
- (IBAction) FlineComponentAction:(id)sender;

- (void) SetFlineFieldMenu;
- (void) SetFlineComponentMenu:(NSInteger)isel
                      kemoview:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)ChooseFieldlineColorAction:(id)sender;

- (IBAction)ShowFlineRange:(id)pSender;
- (IBAction)ChooseFieldlineTypeAction:(id)sender;
- (IBAction)SetFieldlineThicknessAction:(id)sender;

@end
