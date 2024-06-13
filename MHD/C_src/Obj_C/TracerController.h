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
#import "ElasticMenuWindow.h"
#import "PsfController.h"
#import "fillRectView.h"

#include "Kemoviewer.h"

@interface TracerController : NSObject {

    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet PsfController         * _psfController;
    IBOutlet fillRectView* _fillRectView;
    IBOutlet ElasticMenuWindowController * _ElasticControl;
    
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet NSPathControl *_tracerPathControl;
    NSInteger currentTracerStep;
 
    IBOutlet NSColorWell *_tracerColorWell;
   
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

    IBOutlet id TracerColorItem;
}

@property NSInteger currentTracerStep;
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

- (IBAction) DrawTracerFile:(id)pId;
- (IBAction) CloseTracerFile:(id)pId;
- (IBAction) TracerFieldAction:(id)sender;
- (IBAction) TracerComponentAction:(id)sender;

- (IBAction)ChooseTracerColorAction:(id)sender;

- (IBAction)ShowTracerRange:(id)pSender;
- (IBAction)ChooseFieldlineTypeAction:(id)sender;
- (IBAction)SetTracerRadiusAction:(id)sender;

@end
