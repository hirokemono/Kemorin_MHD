//
//  PsfController.h
//  Open_three_windows_kemo_3
//
//  Created by Hiroaki Matsui on 10/09/21.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "KemoviewerController.h"
#import "RGBAMapController.h"
#include "kemoviewer.h"

@interface PsfController : NSObject {
    
    IBOutlet NSWindow*  window;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
	IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet NSColorWell *PSFPatchColorWell;
    
	RGBAMapController * rgbaMapObject;
	ColorMapController * colorMapObject;
	OpacityMapController * opacityMapObject;
	
	NSInteger psfMoreOpenFlag;
	NSInteger DrawPsfFlag;
	NSString *PsfOpenDirectory;
    
	NSString *PsfWindowlabel;
    
	NSInteger PsfNumberOfdata;
	NSMutableArray *LoadedPsfID;
	NSMutableArray *LoadedPsfFileHead;
    
	
	NSInteger PsfNumberOfField;
	NSInteger PsfTotalComponent;
	NSMutableArray *PsfNumberOfComponent;
	NSMutableArray *PsfFieldName;
	NSMutableArray *PsfMinimum;
	NSMutableArray *PsfMaximum;
	
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
	IBOutlet id _psfFieldMenu;
	IBOutlet id _psfComponentMenu;
    
	IBOutlet id _psfLineColorMenu;
	IBOutlet id _psfPatchColorMenu;
	IBOutlet id _psfVectorColorMenu;
    
	NSInteger currentPSFID;
	NSInteger currentPSFStep;
    
	
	CGFloat PsfMinimumValue;
	CGFloat PsfMaximumValue;
	NSInteger IsolineNumber;
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
	NSInteger PSFColorbarSwitch;
    
	NSInteger psfPatchColorTag;
	NSInteger psfLineColorTag;
	NSInteger psfVectorColorTag;
    
	NSInteger psfPatchDirectionTag;
	NSInteger psfTangentialVectorTag;
    
	CGFloat PsfMinimumRange;
	CGFloat PsfMaximumRange;
    CGFloat PsfSingleOpacity;
	
	NSInteger EvolutionStartStep;
    
	IBOutlet id _PSFVectorSwitchOutlet;
	NSInteger PSFVectorMenuAcrive;
	NSInteger DrawPSFVectorFlag;
	NSInteger PSFVectorIncrement;
	CGFloat ScaleVector;
    
}
@property NSInteger psfMoreOpenFlag;
@property NSInteger EvolutionStartStep;

@property (assign) RGBAMapController * rgbaMapObject;
@property (assign) ColorMapController * colorMapObject;
@property (assign) OpacityMapController * opacityMapObject;

@property(retain) NSString* PsfWindowlabel;

@property NSInteger DrawPsfFlag;

@property CGFloat PsfMinimumValue;
@property CGFloat PsfMaximumValue;
@property NSInteger IsolineNumber;
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
@property NSInteger PSFColorbarSwitch;

@property CGFloat PsfMinimumRange;
@property CGFloat PsfMaximumRange;

@property NSInteger PSFVectorMenuAcrive;
@property NSInteger DrawPSFVectorFlag;
@property NSInteger PSFVectorIncrement;
@property CGFloat ScaleVector;


- (id)init;
- (id)dealloc;

- (void)awakeFromNib;


- (id) CopyPsfDisplayFlagsFromC;

- (void) ChooseTextureFile;
- (void) SetCurrentPsfMenu;
- (void) SetPsfFieldMenu;
- (void) UpdateCurrentPsfMenu;
- (void) ResetCurrentPsfParam;
- (void) SetPsfComponentMenu:(int)isel;
- (void) SetPsfRanges;
- (void) DrawPsfFile:(NSString*) PsfOpenFilehead;

- (IBAction) OpenPsfFile:(id)pId;

- (IBAction) ClosePsfFile:(id)pId;

- (IBAction) CurrentPsfAction:(id)sender;
- (IBAction) PsfFieldAction:(id)sender;
- (IBAction) PsfComponentAction:(id)sender;

- (IBAction)PsfSurfSwitchAction:(id)sender;
- (IBAction)PsfLineSwitchAction:(id)sender;
- (IBAction)PsfZeroLineSwitchAction:(id)sender;
- (IBAction)PsfColorbarSwitchAction:(id)sender;

- (IBAction)ChoosePsfPatchColorAction:(id)sender;
- (IBAction)ChoosePsfLineColorAction:(id)sender;
- (IBAction)ChoosePsfVectorColorAction:(id)sender;
- (IBAction)ChoosePsfVectorModeAction:(id)sender;

- (IBAction)SetPSFDisplayrange:(id)sender;
- (IBAction)ShowIsolineNumber:(id)pSender;

- (IBAction)DrawPSFVectorAction:(id)sender;
- (IBAction)SetReferenceVector:(id)pSender;
- (IBAction)SetVectorIncrement:(id)pSender;

- (IBAction)ChoosePsfPatchDirection:(id)sender;

- (void)SetPSFColorFromColorWell;
- (IBAction)SetPSFPatchColorAction:(id)sender;
- (IBAction)SetPSFSingleOpacityAction:(id)sender;
@end
