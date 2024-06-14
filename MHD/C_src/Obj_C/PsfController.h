//
//  PsfController.h
//  Open_three_windows_kemo_3
//
//  Created by Hiroaki Matsui on 10/09/21.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;

#include "math.h"

#import "KemoviewerMovieMaker.h"
#import "KemoviewerController.h"
#import "RGBAMapController.h"
#import "ElasticMenuWindow.h"
#import "KemoViewerObject.h"
#import "fillRectView.h"

#include "Kemoviewer.h"

@interface PsfController : NSObject {
    
    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
	IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet fillRectView* _fillRectView;
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet NSPathControl *_psfPathControl;
    IBOutlet NSColorWell *PSFPatchColorWell;
    
    IBOutlet ElasticMenuWindowController * _ElasticControl;
    
    IBOutlet RGBAMapController * _rgbaMapObject;
	
	NSInteger psfMoreOpenFlag;
	NSInteger DrawPsfFlag;
    
	NSString *PsfWindowlabel;
    
	NSInteger PsfNumberOfdata;
	NSMutableArray *LoadedPsfID;
	NSMutableArray *LoadedPsfFileHead;
    
    NSNumber *PsfPatchFlag;
	NSNumber *PsfIsolineFlag;
	NSNumber *PsfZerolineFlag;
	NSNumber *PsfDrawFieldId;
	NSNumber *PsfDrawComponentId;
	NSNumber *PsfShadingFlag;
	NSNumber *PsfPatchColor;
	NSNumber *PsfIsolineColor;
	NSNumber *PsfIsolineNumber;
	
	IBOutlet id _currentPsfMenu;
	IBOutlet NSPopUpButton * _psfFieldMenu;
	IBOutlet NSPopUpButton * _psfComponentMenu;
    IBOutlet NSTabView     * _controlTabView;
    
	IBOutlet id _psfLineColorMenu;
	IBOutlet id _psfPatchColorMenu;
	IBOutlet id _psfVectorColorMenu;
    
	NSInteger currentPSFID;
	NSInteger currentPSFStep;
    
	
	CGFloat PsfMinimumValue;
	CGFloat PsfMaximumValue;
	NSInteger IsolineNumber;
	CGFloat IsolineWidth;
	CGFloat IsolineDigit;
    CGFloat PSFOpacity;
    
    
    IBOutlet id _PsfSurfSwitchOutlet;
    IBOutlet id _PsfLineSwitchOutlet;
    IBOutlet id _PsfZeroLineSwitchOutlet;
    IBOutlet id _PsfCOlorbarOutlet;
    
    IBOutlet NSMatrix *_psfPatchDirMatrix;
    
	NSInteger PSFSelectedField;
	NSInteger PSFSelectedComponent;
    
	NSInteger PSFSurfaceSwitch;
	NSInteger PSFIsolineSwitch;
	NSInteger PSFZerolineSwitch;
	NSInteger PSFLineSwitch;
	NSInteger PSFColorbarSwitch;
    
	NSInteger psfPatchColorTag;
	NSInteger psfLineColorTag;
	NSInteger psfVectorColorTag;
    
	NSInteger psfPatchDirectionTag;
	NSInteger psfTangentialVectorTag;
    
	CGFloat PsfMinimumRange;
	CGFloat PsfMaximumRange;
	CGFloat PsfMinimumDigit;
	CGFloat PsfMaximumDigit;
    CGFloat PsfSingleOpacity;
	
	NSInteger EvolutionStartStep;
    
	IBOutlet id _PSFVectorSwitchOutlet;
	NSInteger PSFVectorMenuAcrive;
	NSInteger DrawPSFVectorFlag;
	CGFloat PSFVectorIncrement;
	CGFloat PSFVectorIncDigit;
	CGFloat ScaleVector;
	CGFloat ScaleDigit;
    CGFloat VectorThickness;    
	CGFloat VectorDigit;
}
@property NSInteger psfMoreOpenFlag;
@property NSInteger EvolutionStartStep;

@property(retain) NSString* PsfWindowlabel;

@property NSInteger DrawPsfFlag;

@property CGFloat PsfMinimumValue;
@property CGFloat PsfMaximumValue;
@property NSInteger IsolineNumber;
@property CGFloat IsolineWidth;
@property CGFloat IsolineDigit;
@property CGFloat PSFOpacity;

@property NSInteger PSFSelectedField;
@property NSInteger PSFSelectedComponent;

@property NSInteger psfPatchColorTag;
@property NSInteger psfLineColorTag;
@property NSInteger psfVectorColorTag;

@property NSInteger psfPatchDirectionTag;
@property NSInteger psfTangentialVectorTag;

@property NSInteger currentPSFID;
@property NSInteger currentPSFStep;
@property NSInteger PSFSurfaceSwitch;
@property NSInteger PSFIsolineSwitch;
@property NSInteger PSFZerolineSwitch;
@property NSInteger PSFLineSwitch;
@property NSInteger PSFColorbarSwitch;

@property CGFloat PsfMinimumRange;
@property CGFloat PsfMaximumRange;
@property CGFloat PsfMinimumDigit;
@property CGFloat PsfMaximumDigit;

@property NSInteger PSFVectorMenuAcrive;
@property NSInteger DrawPSFVectorFlag;
@property CGFloat PSFVectorIncrement;
@property CGFloat PSFVectorIncDigit;
@property CGFloat ScaleVector;
@property CGFloat ScaleDigit;
@property CGFloat VectorThickness;
@property CGFloat VectorDigit;


void SetDataRanges(int id_model, struct kemoviewer_type *kemo_sgl,
                   double *dataMin, double *dataMax,
                   double *cmapMinValue, int *cmapMinDigit,
                   double *cmapMaxValue, int *cmapMaxDigit);


- (id)init;
- (id)dealloc;

- (void)awakeFromNib;

- (void) SetFieldMenuItems:(int) id_model
                  kemoview:(struct kemoviewer_type *) kemo_sgl
                 fieldMenu:(NSPopUpButton *) psfFieldMenu;

- (void) SetComponentMenuItems:(NSInteger) isel
                   activeModel:(NSInteger) id_model
                      kemoview:(struct kemoviewer_type *) kemo_sgl
                 componentMenu:(NSPopUpButton *) psfComponentMenu;

- (void) DrawPsfFile:(NSString*) PsfOpenFilehead
            kemoview:(struct kemoviewer_type *) kemo_sgl;

- (int) PSFColorbarSwitchStatus;
- (void) setPSFColorbarSwitchStatus:(int) isel;


- (IBAction) OpenPsfFile:(id)pId;
- (IBAction) ClosePsfFile:(id)pId;

- (IBAction) CurrentPsfAction:(id)sender;
- (IBAction) PsfFieldAction:(id)sender;
- (IBAction) PsfComponentAction:(id)sender;

- (IBAction)PsfSurfSwitchAction:(id)sender;
- (IBAction)PsfLineSwitchAction:(id)sender;
- (IBAction)PsfZeroLineSwitchAction:(id)sender;

- (IBAction)ChoosePsfPatchColorAction:(id)sender;
- (IBAction)ChoosePsfLineColorAction:(id)sender;
- (IBAction)ChoosePsfVectorColorAction:(id)sender;
- (IBAction)ChoosePsfVectorModeAction:(id)sender;

- (IBAction)SetPSFDisplayrange:(id)sender;
- (IBAction)ShowIsolineNumber:(id)pSender;
- (IBAction)SetIsolineWidth:(id)pSender;

- (IBAction)DrawPSFVectorAction:(id)sender;
- (IBAction)SetReferenceVector:(id)pSender;
- (IBAction)SetVectorIncrement:(id)pSender;
- (IBAction)SetVectorThickness:(id)pSender;

- (IBAction)ChoosePsfPatchDirection:(id)sender;

- (void)SetPSFColorFromColorWell:(struct kemoviewer_type *) kemo_sgl
                       colorwell:(NSColorWell *) viewColorWell;
- (void)SetPSFSingleOpacity:(struct kemoviewer_type *) kemo_sgl
                    opacity:(CGFloat) const_opacity
                  colorwell:(NSColorWell *) viewColorWell;

- (IBAction)SetPSFPatchColorAction:(id)sender;
- (IBAction)SetPSFSingleOpacityAction:(id)sender;
@end
