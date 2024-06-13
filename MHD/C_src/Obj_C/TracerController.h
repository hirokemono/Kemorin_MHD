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

    NSInteger DrawTracerFlag;
        
    NSNumber *FieldlineColor;

    IBOutlet NSMatrix *_flinetype_matrix;
    NSInteger Flinetype;

    IBOutlet NSPopUpButton * _TracerFieldMenu;
    IBOutlet NSPopUpButton * _TracerComponentMenu;
    
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

@property NSInteger DrawTracerFlag;
@property CGFloat TracerColorMinimum;
@property CGFloat TracerColorMaximum;
@property CGFloat TracerColorMinDigit;
@property CGFloat TracerColorMaxDigit;

@property CGFloat TracerMinimumValue;
@property CGFloat TracerMaximumValue;

@property NSInteger Tracertype;
@property CGFloat TracerRadiusFactor;
@property CGFloat TracerRadiusDigit;

- (id)init;
- (id)dealloc;
-(void) awakeFromNib;

- (void) OpenTracerFile:(NSString*) TracerFilehead
               kemoview:(struct kemoviewer_type *) kemo_sgl;

- (IBAction) CloseFlineFile:(id)pId;
- (IBAction) TracerFieldAction:(id)sender;
- (IBAction) TracerComponentAction:(id)sender;

- (IBAction)ChooseFieldlineColorAction:(id)sender;

- (IBAction)ShowTracerRange:(id)pSender;
- (IBAction)ChooseFieldlineTypeAction:(id)sender;
- (IBAction)SetTracerRadiusAction:(id)sender;

@end
