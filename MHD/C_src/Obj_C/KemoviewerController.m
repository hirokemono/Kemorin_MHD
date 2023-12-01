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
	
	return self;
}

-(void) awakeFromNib
{
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
	
	[_view3dItem setState:NSControlStateValueOff];
	[_viewMapItem setState:NSControlStateValueOff];
	[_viewStereoItem setState:NSControlStateValueOff];
	[_viewXYItem setState:NSControlStateValueOff];
	[_viewYZItem setState:NSControlStateValueOff];
	[_viewZXItem setState:NSControlStateValueOff];
	
    self.axisDrawAccess = 1;
    self.StereoFlag = (NSInteger) kemoview_get_quilt_nums(ISET_QUILT_MODE);
	psfTexTureEnable = 1;
	if (selected == VIEW_3D) {[_view3dItem setState:NSControlStateValueOn];}
	else if (selected == VIEW_MAP) {
        self.axisDrawAccess = 0;
        self.StereoFlag = 0;
		psfTexTureEnable = 0;
		[_viewMapItem setState:NSControlStateValueOn];
	}
	else if (selected == VIEW_STEREO) {
		self.StereoFlag = 1;
		[_viewStereoItem setState:NSControlStateValueOn];
	}
	else if (selected == VIEW_XY) {[_viewXYItem setState:NSControlStateValueOn];}
	else if (selected == VIEW_YZ) {[_viewYZItem setState:NSControlStateValueOn];}
	else if (selected == VIEW_XZ) {[_viewZXItem setState:NSControlStateValueOn];};
}

- (void)UpdateViewtype:(NSInteger) selected
{
	[self SetViewTypeMenu:selected];

    kemoview_set_viewtype((int) selected);
	[_metalView setViewerType:selected];
    kemoview_update_distance();
	[_metalView UpdateImage];
}

- (IBAction)ChoosePolygontypeAction:(id)sender
{
	PolygonMode = [[_polygontype_matrix selectedCell] tag];
	kemoview_set_object_property_flags(POLYGON_SWITCH, (int) PolygonMode);
	
	[_metalView UpdateImage];
}

- (IBAction)ChooseSurfcetypeAction:(id)sender
{
	ShadingMode = [[_surfacetype_matrix selectedCell] tag];
	kemoview_set_object_property_flags(SHADING_SWITCH, (int) ShadingMode);
	[_metalView UpdateImage];
}

- (IBAction)AxisSwitchAction:(id)sender;
{
	self.axisDrawFlag = kemoview_toggle_object_properties(AXIS_TOGGLE);
	[_metalView UpdateImage];
}

- (IBAction)CoastSwitchAction:(id)sender;
{
	self.coastLineDrawFlag = kemoview_toggle_object_properties(COASTLINE_SWITCH);
	[_metalView UpdateImage];
}
- (IBAction)SphGridSwitchAction:(id)sender;
{
	self.globeGridDrawFlag = kemoview_toggle_object_properties(SPHEREGRID_SWITCH);
	[_metalView UpdateImage];
}
- (IBAction)SphRadiusAction:(id)sender;
{
	kemoview_set_coastline_radius((double) coastlineRadius);
	[_metalView UpdateImage];
}

- (IBAction)ChooseColorModeAction:(id)sender
{
	MeshColorMode = [[_colormode_matrix selectedCell] tag];
	kemoview_set_mesh_color_mode((int) MeshColorMode);

	[_metalView UpdateImage];
}

- (IBAction)SetColorLoopCount:(id)pSender {
	kemoview_set_num_of_color_loop((int) ColorLoopCount);

	[_metalView UpdateImage];
}

- (IBAction) ShowNodeSizeValue:(id)pSender {
	kemoview_set_node_diamater((double) NodeSizeFactor, (int) NodeSizedigits);

	[_metalView UpdateImage];
}

- (IBAction) ToggleQuiltSwitch:(id)sender
{
    self.StereoFlag = [_resetview ToggleQuiltMode];
}

- (IBAction) SetViewtypeAction:(id)pSender{
	[self UpdateViewtype:[_viewtypeItem selectedTag]];
}

- (void) Set3DView
{
    [self SetViewTypeMenu:VIEW_3D];
    [_metalView setViewerType:VIEW_3D];
    kemoview_set_viewtype(VIEW_3D);
}

- (IBAction) UpdateViewByInpit:(id)sender;
{
    [_metalView UpdateImage];
};

- (IBAction) ResetviewAction:(id)sender;
{
	[self Set3DView];
	[_metalView Resetview];
}


-(IBAction) Toggleinfo: (id) sender
{
	fInfo = 1 - fInfo;
	[_metalView setInfo:fInfo];
}

-(IBAction) ToggleQuickhelp: (id) sender
{
	fDrawHelp = 1 - fDrawHelp;
	[_metalView setQuickHelp:fDrawHelp];
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
    [_metalView UpdateImage];
};

- (IBAction)FileStepLabelSwitchAction:(id)sender{
    self.fileStepDisplayFlag = kemoview_toggle_object_properties(FILE_STEP_LABEL_SWITCH);
    if(self.fileStepDisplayFlag > 0){
        self.timeDisplayFlag = 0;
        kemoview_set_object_property_flags(TIME_LABEL_SWITCH, (int) self.timeDisplayFlag);
    };
    [_metalView UpdateImage];
};


@end
