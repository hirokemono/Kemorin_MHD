//
//  KemoviewerController.m
//
//  Created by Hiroaki Matsui on 10/09/20.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoviewerController.h"
#include "kemoviewer.h"

@implementation KemoviewerController
@synthesize ColorLoopCount;
@synthesize NodeSizeFactor;
@synthesize NodeSizedigits;
@synthesize fAnimate;
@synthesize fInfo;
@synthesize fDrawHelp;
@synthesize StereoFlag;
@synthesize coastlineRadius;
@synthesize psfTexTureEnable;
@synthesize timeDisplayFlag;
@synthesize fileStepDisplayFlag;
@synthesize timeDisplayAccess;
@synthesize fileStepDisplayAccess;
@synthesize coastLineDrawFlag;
@synthesize globeGridDrawFlag;
@synthesize axisDrawFlag;
@synthesize axisDrawAccess;
- (id)init
{
	NodeSizeFactor =  1;
	NodeSizedigits = -2;
	ColorLoopCount =  6;
    
    self.timeDisplayAccess =     0;
    self.fileStepDisplayAccess = 0;
    self.timeDisplayFlag =     0;
    self.fileStepDisplayFlag = 0;

    self.coastLineDrawFlag = 0;
    self.globeGridDrawFlag = 0;
    self.axisDrawFlag =      0;
    self.axisDrawAccess =    1;
	
	fAnimate = 0;
	return self;
}

-(void) awakeFromNib
{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	AnaglyphFlag = [[defaults stringForKey:@"AnaglyphFlag"] intValue];
	[_streoViewTypeMenu selectItemAtIndex:(1-AnaglyphFlag)];

    self.coastlineRadius = kemoview_get_coastline_radius();
    kemoview_set_object_property_flags(TIME_LABEL_AVAIL, (int) self.timeDisplayAccess);
    kemoview_set_object_property_flags(TIME_LABEL_SWITCH, (int) self.timeDisplayFlag);
    kemoview_set_object_property_flags(FILE_STEP_LABEL_AVAIL, (int) self.fileStepDisplayAccess);
    kemoview_set_object_property_flags(FILE_STEP_LABEL_SWITCH, (int) self.fileStepDisplayFlag);
    return;
}

- (id)dealloc
{
    [super dealloc];
	return self;
}

- (void)SetViewTypeMenu:(NSInteger) selected;
{
	[_viewtypeItem selectItemAtIndex:selected];
	
	[_view3dItem setState:NSOffState];
	[_viewMapItem setState:NSOffState];
	[_viewStereoItem setState:NSOffState];
	[_viewXYItem setState:NSOffState];
	[_viewYZItem setState:NSOffState];
	[_viewZXItem setState:NSOffState];
	
    self.axisDrawAccess = 1;
	self.StereoFlag = 0;
	psfTexTureEnable = 1;
	if (selected == VIEW_3D) {[_view3dItem setState:NSOnState];}
	else if (selected == VIEW_MAP) {
        self.axisDrawAccess = 0;
		psfTexTureEnable = 0;
		[_viewMapItem setState:NSOnState];
	}
	else if (selected == VIEW_STEREO) {
		self.StereoFlag = 1;
		[_viewStereoItem setState:NSOnState];
	}
	else if (selected == VIEW_XY) {[_viewXYItem setState:NSOnState];}
	else if (selected == VIEW_YZ) {[_viewYZItem setState:NSOnState];}
	else if (selected == VIEW_XZ) {[_viewZXItem setState:NSOnState];};
}

- (void)UpdateViewtype:(NSInteger) selected;
{
	[self SetViewTypeMenu:selected];

	[_kemoviewer setViewerType:selected];
	[_kemoviewer updateProjection];
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePolygontypeAction:(id)sender;
{
	PolygonMode = [[_polygontype_matrix selectedCell] tag];
	kemoview_set_object_property_flags(POLYGON_SWITCH, (int) PolygonMode);
	
	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseSurfcetypeAction:(id)sender;
{
	ShadingMode = [[_surfacetype_matrix selectedCell] tag];
	kemoview_set_object_property_flags(SHADING_SWITCH, (int) ShadingMode);
	[_kemoviewer UpdateImage];
}

- (IBAction)AxisSwitchAction:(id)sender;
{
	self.axisDrawFlag = kemoview_toggle_object_properties(AXIS_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)CoastSwitchAction:(id)sender;
{
	self.coastLineDrawFlag = kemoview_toggle_object_properties(COASTLINE_SWITCH);
	[_kemoviewer UpdateImage];
}
- (IBAction)SphGridSwitchAction:(id)sender;
{
	self.globeGridDrawFlag = kemoview_toggle_object_properties(SPHEREGRID_SWITCH);
	[_kemoviewer UpdateImage];
}
- (IBAction)SphRadiusAction:(id)sender;
{
	kemoview_set_coastline_radius((double) coastlineRadius);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseColorModeAction:(id)sender
{
	MeshColorMode = [[_colormode_matrix selectedCell] tag];
	kemoview_set_mesh_color_mode((int) MeshColorMode);

	[_kemoviewer UpdateImage];
}

- (IBAction)SetColorLoopCount:(id)pSender {
	kemoview_set_num_of_color_loop((int) ColorLoopCount);

	[_kemoviewer UpdateImage];
}

- (IBAction) ShowNodeSizeValue:(id)pSender {
	kemoview_set_node_diamater((double) NodeSizeFactor, (int) NodeSizedigits);

	[_kemoviewer UpdateImage];
}

- (IBAction) SetViewtypeAction:(id)pSender{
	[self UpdateViewtype:[_viewtypeItem selectedTag]];
}

- (IBAction) SetStereoViewType:(id)sender;
{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	
	AnaglyphFlag = (1-[_streoViewTypeMenu indexOfSelectedItem]);
	kemoview_set_view_integer(ISET_ANAGYLYPH, (int) AnaglyphFlag);
	[defaults setInteger:((int) AnaglyphFlag) forKey:@"AnaglyphFlag"];

    printf("AnaglyphFlag %d\n", (int) AnaglyphFlag);
	[_kemoviewer UpdateImage];
}

- (void) Set3DView
{
    [self SetViewTypeMenu:VIEW_3D];
    [_kemoviewer setViewerType:VIEW_3D];
}

- (IBAction) UpdateViewByInpit:(id)sender;
{
    [_kemoviewer UpdateImage];
};

- (IBAction) ChangeViewByInpit:(id)sender;
{
	[_resetview  SetViewByInpit];
	[_kemoviewer updateProjection];
//	[_kemoviewer UpdateImage];
}

- (IBAction) ResetviewAction:(id)sender;
{
	[self Set3DView];
	[_kemoviewer Resetview];
}

-(IBAction) ToggleAnimate: (id) sender
{
	fAnimate = 1 - fAnimate;
	[_kemoviewer setAnimate:fAnimate];
}

-(IBAction) Toggleinfo: (id) sender
{
	fInfo = 1 - fInfo;
	[_kemoviewer setInfo:fInfo];
}

-(IBAction) ToggleQuickhelp: (id) sender
{
	fDrawHelp = 1 - fDrawHelp;
	[_kemoviewer setQuickHelp:fDrawHelp];
}

- (void) TimeLabelAvaiability
{
    self.timeDisplayAccess = kemoview_get_object_property_flags(TIME_LABEL_AVAIL);
}
- (void) FileStepLabelAvaiability
{
    self.fileStepDisplayAccess = kemoview_get_object_property_flags(FILE_STEP_LABEL_AVAIL);
}

- (IBAction)TimeLabelSwitchAction:(id)sender{
    self.timeDisplayFlag = kemoview_toggle_object_properties(TIME_LABEL_SWITCH);
    if(self.timeDisplayFlag > 0){
        self.fileStepDisplayFlag = 0;
        kemoview_set_object_property_flags(FILE_STEP_LABEL_SWITCH, (int) self.fileStepDisplayFlag);
    };
    [_kemoviewer UpdateImage];
};

- (IBAction)FileStepLabelSwitchAction:(id)sender{
    self.fileStepDisplayFlag = kemoview_toggle_object_properties(FILE_STEP_LABEL_SWITCH);
    if(self.fileStepDisplayFlag > 0){
        self.timeDisplayFlag = 0;
        kemoview_set_object_property_flags(TIME_LABEL_SWITCH, (int) self.timeDisplayFlag);
    };
    [_kemoviewer UpdateImage];
};


@end
