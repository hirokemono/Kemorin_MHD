//
//  TracerController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "KemoviewerController.h"
#import "KemoViewerObject.h"
#import "PsfController.h"
#import "fillRectView.h"

#include "Kemoviewer.h"

@interface TracerController : NSObject {

    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet PsfController         * _psfController;
    IBOutlet fillRectView* _fillRectView;
    IBOutlet KemoViewerObject *_kmv;

    NSInteger DrawFlineFlag;
    NSString *FlineOpenDirectory;
    NSString *FlineOpenFilehead;
    NSString *FlineOpenFileext;
    NSString *FlineOpenFileheadExStep;
    
    NSString *FlineWindowlabel;
    
    NSNumber *FlineDrawFieldId;
    NSNumber *FlineDrawComponentId;
    NSNumber *FieldlineColor;

    IBOutlet NSMatrix *_flinetype_matrix;
    NSInteger Flinetype;

    
    NSNumber *FlineMinimumRange;
    NSNumber *FlineMaximumRange;
    
    IBOutlet NSPopUpButton * _FlineFieldMenu;
    IBOutlet NSPopUpButton * _FlineComponentMenu;
    
    CGFloat TracerColorMinimum;
    CGFloat TracerColorMaximum;
    CGFloat TracerColorMinDigit;
    CGFloat TracerColorMaxDigit;
    
    
    CGFloat TracerMinimumValue;
    CGFloat FlineMaximumValue;
    
    CGFloat TracerRadiusFactor;
    CGFloat TracerRadiusDigit;

    IBOutlet id FieldlineColorItem;
}

@property(retain) NSString* FlineWindowlabel;

@property NSInteger DrawFlineFlag;
@property CGFloat TracerColorMinimum;
@property CGFloat TracerColorMaximum;
@property CGFloat TracerColorMinDigit;
@property CGFloat TracerColorMaxDigit;

@property CGFloat TracerMinimumValue;
@property CGFloat TracerMaximumValue;

@property NSInteger Flinetype;
@property CGFloat TracerRadiusFactor;
@property CGFloat TracerRadiusDigit;

- (id)init;
- (id)dealloc;
-(void) awakeFromNib;

- (void) OpenFieldlineFile:(NSString*) fieldlineFilehead
                  kemoview:(struct kemoviewer_type *) kemo_sgl;
- (IBAction) UpdateFieldline:(id)pId;
- (IBAction) DrawFlineFile:(id)pId;

- (IBAction) CloseFlineFile:(id)pId;
- (IBAction) FlineFieldAction:(id)sender;
- (IBAction) FlineComponentAction:(id)sender;

- (IBAction)ChooseFieldlineColorAction:(id)sender;

- (IBAction)ShowTracerRange:(id)pSender;
- (IBAction)ChooseFieldlineTypeAction:(id)sender;
- (IBAction)SetTracerRadiusAction:(id)sender;

@end
