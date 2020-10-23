//
//  KemoviewerController.h
//
//  Created by Hiroaki Matsui on 10/09/20.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"

@interface KemoviewerController : NSObject {

	IBOutlet ResetViewControll*  _resetview;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;

	IBOutlet id _streoViewTypeMenu;
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	
	CGFloat coastlineRadius;
	NSInteger ShadingMode;
	NSInteger PolygonMode;
	
	NSInteger MeshColorMode;

    IBOutlet NSMatrix *_polygontype_matrix;
    IBOutlet NSMatrix *_surfacetype_matrix;
    IBOutlet NSMatrix *_colormode_matrix;
	
	CGFloat ColorLoopCount;
	CGFloat NodeSizeFactor;
	CGFloat NodeSizedigits;

	// Viewer type handling
	IBOutlet id _viewtypeItem;

	IBOutlet id _view3dItem;
	IBOutlet id _viewMapItem;
	IBOutlet id _viewStereoItem;
	IBOutlet id _viewXYItem;
	IBOutlet id _viewYZItem;
	IBOutlet id _viewZXItem;

	NSInteger StereoFlag;
	NSInteger AnaglyphFlag;
	NSInteger psfTexTureEnable;
	
	NSInteger fInfo;
	NSInteger fAnimate;
	NSInteger fDrawHelp;

    
    NSInteger timeDisplayAccess;
    NSInteger fileStepDisplayAccess;
    NSInteger timeDisplayFlag;
    NSInteger fileStepDisplayFlag;

    NSInteger coastLineDrawFlag;
    NSInteger globeGridDrawFlag;
    NSInteger axisDrawFlag;
    NSInteger axisDrawAccess;
}

@property CGFloat ColorLoopCount;
@property CGFloat NodeSizeFactor;
@property CGFloat NodeSizedigits;
@property NSInteger fInfo;
@property NSInteger fAnimate;
@property NSInteger fDrawHelp;
@property NSInteger StereoFlag;
@property CGFloat coastlineRadius;
@property NSInteger psfTexTureEnable;
@property NSInteger timeDisplayAccess;
@property NSInteger fileStepDisplayAccess;
@property NSInteger timeDisplayFlag;
@property NSInteger fileStepDisplayFlag;
@property NSInteger coastLineDrawFlag;
@property NSInteger globeGridDrawFlag;
@property NSInteger axisDrawFlag;
@property NSInteger axisDrawAccess;


- (id)init;
- (void)awakeFromNib;
- (id)dealloc;

- (void)SetViewTypeMenu:(NSInteger) selected;
- (void)UpdateViewtype:(NSInteger) selected;

- (IBAction)AxisSwitchAction:(id)sender;
- (IBAction)CoastSwitchAction:(id)sender;
- (IBAction)SphGridSwitchAction:(id)sender;
- (IBAction)SphRadiusAction:(id)sender;
- (IBAction)ChoosePolygontypeAction:(id)sender;
- (IBAction)ChooseSurfcetypeAction:(id)sender;
- (IBAction)ChooseColorModeAction:(id)sender;
- (IBAction)SetColorLoopCount:(id)pSender;
- (IBAction)ShowNodeSizeValue:(id)pSender;

- (IBAction) SetViewtypeAction:(id)pSender;

- (IBAction) SetStereoViewType:(id)sender;

- (void) Set3DView;
- (IBAction) ChangeViewByInpit:(id)sender;
- (IBAction) ResetviewAction:(id)sender;

-(IBAction) ToggleAnimate: (id) sender;
-(IBAction) Toggleinfo: (id) sender;
-(IBAction) ToggleQuickhelp: (id) sender;

- (void) TimeLabelAvaiability;
- (void) FileStepLabelAvaiability;

- (IBAction)TimeLabelSwitchAction:(id)sender;
- (IBAction)FileStepLabelSwitchAction:(id)sender;

@end
