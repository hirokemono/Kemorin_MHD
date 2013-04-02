//
//  PsfController.m
//  Open_three_windows_kemo_3
//
//  Created by Hiroaki Matsui on 10/09/21.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "PsfController.h"


// ==================================
@implementation PsfController

@synthesize psfMoreOpenFlag;
@synthesize rgbaMapObject;
@synthesize colorMapObject;
@synthesize opacityMapObject;

@synthesize PsfWindowlabel;

@synthesize DrawPsfFlag;
@synthesize PsfMinimumValue;
@synthesize PsfMaximumValue;
@synthesize IsolineNumber;
@synthesize PSFOpacity;

@synthesize currentPSFID;
@synthesize currentPSFStep;

@synthesize EvolutionStartStep;

@synthesize PSFSelectedField;
@synthesize PSFSelectedComponent;

@synthesize psfPatchColorTag;
@synthesize psfLineColorTag;
@synthesize psfVectorColorTag;

@synthesize psfPatchDirectionTag;

@synthesize PSFSurfaceSwitch;
@synthesize PSFIsolineSwitch;
@synthesize PSFZerolineSwitch;
@synthesize PSFColorbarSwitch;

@synthesize PsfMinimumRange;
@synthesize PsfMaximumRange;

@synthesize PSFVectorMenuAcrive;
@synthesize DrawPSFVectorFlag;
@synthesize ScaleVector;
@synthesize PSFVectorIncrement;
- (id)init;
{
	self.PsfWindowlabel = [NSString stringWithFormat:@"PSF View"];
		
	LoadedPsfID =      [[NSMutableArray alloc] init];
	LoadedPsfFileHead =[[NSMutableArray alloc] init];

	PsfNumberOfComponent =[[NSMutableArray alloc] init];
	PsfFieldName =        [[NSMutableArray alloc] init];
	PsfMinimum =          [[NSMutableArray alloc] init];
	PsfMaximum =          [[NSMutableArray alloc] init];
	
	PsfPatchFlag =    [NSNumber alloc];
	PsfIsolineFlag =  [NSNumber alloc];
	PsfZerolineFlag = [NSNumber alloc];
	
	PsfDrawFieldId =     [NSNumber alloc];
	PsfDrawComponentId = [NSNumber alloc];
	
	PsfShadingFlag =     [NSNumber alloc];
	PsfPatchColor =      [NSNumber alloc];
	PsfIsolineColor =    [NSNumber alloc];
	PsfIsolineNumber =   [NSNumber alloc];
	
	self.IsolineNumber = 20;
	self.PSFOpacity = 1.0;
	self.psfMoreOpenFlag = 0;
	self.psfPatchDirectionTag = 0;
	return self;
}

- (id)dealloc
{
	
	[LoadedPsfID       dealloc];
	[LoadedPsfFileHead dealloc];

	[PsfNumberOfComponent dealloc];
	[PsfFieldName         dealloc];
	[PsfMinimum           dealloc];
	[PsfMaximum           dealloc];
	
	[PsfPatchFlag    dealloc];
	[PsfIsolineFlag  dealloc];
	[PsfZerolineFlag dealloc];
	
	[PsfDrawFieldId      dealloc];
	[PsfDrawComponentId dealloc];
	
	[PsfShadingFlag      dealloc];
	[PsfPatchColor       dealloc];
	[PsfIsolineColor     dealloc];
	[PsfIsolineNumber    dealloc];
	
    [super dealloc];
	
	return self;
}

- (void)awakeFromNib {
	self.rgbaMapObject =    [RGBAMapController alloc];
	self.colorMapObject =   [ColorMapController alloc];
	self.opacityMapObject = [OpacityMapController alloc];
}

- (id) CopyPsfDisplayFlagsFromC
{
	int i, iflag;
	double minmax;
	char name[4096];
	NSString *stname;
	NSNumber *stnum;
	
	PsfNumberOfField =  send_nfield_current_psf();
	PsfTotalComponent = send_ncomptot_current_psf();
	[PsfFieldName removeAllObjects];	
	[PsfNumberOfComponent removeAllObjects];
	[PsfMinimum removeAllObjects];
	[PsfMaximum removeAllObjects];
	for(i = 0; i < PsfNumberOfField; i++){
		send_current_psf_data_name(name,i);
		stname = [[NSString alloc] initWithUTF8String:name];
		[PsfFieldName      addObject:stname];
		[stname release];	

		iflag = send_ncomp_current_psf(i);
		stnum = [[NSNumber alloc] initWithInt:iflag];
		[PsfNumberOfComponent addObject:stnum];
		[stnum release];	
	}
	for(i = 0; i < PsfTotalComponent; i++){
		minmax = send_current_psf_data_min(i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[PsfMinimum      addObject:stnum];
		[stnum release];	
		
		minmax = send_current_psf_data_max(i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[PsfMaximum      addObject:stnum];
		[stnum release];	
	}
	return self;
}

- (void) UpdateCurrentPsfMenu{
	self.PSFSelectedField =     send_draw_field_current_psf();
	self.PSFSelectedComponent = send_draw_comp_id_current_psf();

	int iplotted = send_draw_component_current_psf();
	
	self.PSFSurfaceSwitch =  send_kemoview_psf_draw_flags(PSFSOLID_TOGGLE);
	self.PSFIsolineSwitch =  send_kemoview_psf_draw_flags(PSFGRID_TOGGLE);
	self.PSFZerolineSwitch = send_kemoview_psf_draw_flags(ZEROGRID_TOGGLE);
	self.PSFColorbarSwitch = send_kemoview_psf_draw_flags(COLORBAR_TOGGLE);
	self.PsfMinimumRange =   send_current_PSF_color_table_min();
	self.PsfMaximumRange =   send_current_PSF_color_table_max();
	self.PsfMinimumValue =   send_current_psf_data_min(iplotted);
	self.PsfMaximumValue =   send_current_psf_data_max(iplotted);
	self.IsolineNumber =     send_current_num_isoline();
	self.PSFOpacity =        send_current_PSF_maximum_opacity();
	
	self.DrawPSFVectorFlag = send_kemoview_psf_draw_flags(PSFVECT_TOGGLE);
	self.ScaleVector =       send_current_scale_vect();
	self.PSFVectorIncrement = send_current_increment_vect();
	
	self.psfPatchDirectionTag = send_kemoview_psf_draw_flags(PSF_POLYGON_SWITCH);
	
	self.psfLineColorTag =  send_current_isoline_color();
	self.psfPatchColorTag = send_current_psf_patch_color();
	
	self.psfVectorColorTag = send_current_vector_patch_color();
	
	[self CopyPsfDisplayFlagsFromC];
	[self SetPsfFieldMenu];
	[self SetPsfComponentMenu:self.PSFSelectedField];
	[_psfFieldMenu selectItemAtIndex:self.PSFSelectedField];
	[_psfComponentMenu selectItemAtIndex:self.PSFSelectedComponent];
	
	[colorMapObject SetColorTables];
	[opacityMapObject SetOpacityTables];
	
	[_psfPatchDirMatrix selectCellWithTag:self.psfPatchDirectionTag];
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
}

- (void) SetCurrentPsfMenu{
	int i, j, istep, ifmt;
	char file_head[4096];
	NSString *PsfNumberTxt;
	NSString *PsfFileHeader;

	PsfNumberOfdata = send_max_loaded_PSF();
	[LoadedPsfFileHead removeAllObjects];
	[LoadedPsfID removeAllObjects];
	for(i = 0; i < PsfNumberOfdata; i++){
		if(send_loaded_PSF_flag(i) > 0){
			set_to_current_PSF(i);
			istep = send_current_psf_full_path_header(file_head, &ifmt);
			PsfNumberTxt = [[NSString alloc] initWithFormat:@"%d: ",i+1];
			PsfFileHeader = [[[NSString alloc] initWithUTF8String:file_head] lastPathComponent];
			PsfFileHeader = [PsfNumberTxt stringByAppendingString:PsfFileHeader];
			self.currentPSFStep = istep;
			[LoadedPsfFileHead addObject:PsfFileHeader];
			[LoadedPsfID addObject:[[NSNumber alloc] initWithInt:i]];
		}
	}

	PsfNumberOfdata = send_num_loaded_PSF();
	[_currentPsfMenu removeAllItems];
	for(i = 0; i < PsfNumberOfdata; i++){
		[_currentPsfMenu addItemWithTitle:[LoadedPsfFileHead objectAtIndex: i]];
		j = [[LoadedPsfID objectAtIndex: i] intValue];
		if(j == send_current_PSF()) self.currentPSFID = i;
	};


	[self UpdateCurrentPsfMenu];
}

- (void) SetPsfFieldMenu{
	int i;
	[_psfFieldMenu removeAllItems];
	if(PsfNumberOfField < 1){
		[_psfFieldMenu addItemWithTitle:@"No field"];
	} else {
		for(i = 0; i < PsfNumberOfField; i++){
			[_psfFieldMenu addItemWithTitle:[PsfFieldName objectAtIndex: i]];
		};
	}
}

- (void) SetPsfComponentMenu:(int)isel{
	int iplotted;
	
	[_psfComponentMenu removeAllItems];
	// NSLog ([NSString stringWithFormat:@"component %@\n", [PsfNumberOfComponent objectAtIndex:isel]]);	
	
	if(PsfNumberOfField < 1){
		[_psfComponentMenu addItemWithTitle:@"No data"];
	} else {
		if([[PsfNumberOfComponent objectAtIndex:isel] intValue] == 1){
			[_psfComponentMenu addItemWithTitle:@"Scalar"];
		} else if([[PsfNumberOfComponent objectAtIndex:isel] intValue] == 6){
			[_psfComponentMenu addItemWithTitle:@"xx"];
			[_psfComponentMenu addItemWithTitle:@"xy"];
			[_psfComponentMenu addItemWithTitle:@"xz"];
			[_psfComponentMenu addItemWithTitle:@"yy"];
			[_psfComponentMenu addItemWithTitle:@"yz"];
			[_psfComponentMenu addItemWithTitle:@"zz"];
		}
		else if([[PsfNumberOfComponent objectAtIndex:isel] intValue] == 3){			
			NSInteger charalen = [[PsfFieldName objectAtIndex:isel] length];
			if(charalen > 4){
				NSString *stname = [[PsfFieldName objectAtIndex:isel] substringFromIndex:charalen-4];
				// NSLog ([NSString stringWithFormat:@"end is %@\n",stname ]);
				if([stname compare:@"_sph"] == NSOrderedSame){
					[_psfComponentMenu addItemWithTitle:@"r"];
					[_psfComponentMenu addItemWithTitle:@"θ"];
					[_psfComponentMenu addItemWithTitle:@"φ"];
				}else if([stname compare:@"_cyl"] == NSOrderedSame){
					[_psfComponentMenu addItemWithTitle:@"s"];
					[_psfComponentMenu addItemWithTitle:@"φ"];
					[_psfComponentMenu addItemWithTitle:@"z"];
				}else{
					[_psfComponentMenu addItemWithTitle:@"x"];
					[_psfComponentMenu addItemWithTitle:@"y"];
					[_psfComponentMenu addItemWithTitle:@"z"];
				}
			} else {
				[_psfComponentMenu addItemWithTitle:@"x"];
				[_psfComponentMenu addItemWithTitle:@"y"];
				[_psfComponentMenu addItemWithTitle:@"z"];
			}
		}

		if([[PsfNumberOfComponent objectAtIndex:isel] intValue] == 3){
			self.PSFVectorMenuAcrive = 1;
		} else {
			self.PSFVectorMenuAcrive = 0;
		};
		

		iplotted = send_draw_component_current_psf();
		self.PsfMinimumValue =   send_current_psf_data_min(iplotted);
		self.PsfMaximumValue =   send_current_psf_data_max(iplotted);
	}
	
	return;
}

- (void) SetPsfRanges{
	int iplotted;
   
	iplotted = send_draw_component_current_psf();
      
 	self.PsfMinimumValue = send_current_psf_data_min(iplotted);
	self.PsfMaximumValue = send_current_psf_data_max(iplotted);
	self.PsfMinimumRange = send_current_PSF_color_table_min();
	self.PsfMaximumRange = send_current_PSF_color_table_max();

      [self.rgbaMapObject updateColormapParameter];
      [self.colorMapObject InitColorTables];
      [self.colorMapObject SetColorTables];
      [self.opacityMapObject InitOpacityTables];
      [self.opacityMapObject SetOpacityTables];
}


- (void) DrawPsfFile:(NSString*) PsfOpenFilehead{
	int id_viewtype = send_iflag_view_type();

	self.currentPSFStep = [[PsfOpenFilehead pathExtension] intValue];
	self.PsfWindowlabel = [NSString stringWithFormat:@"PSF:%@",
						   [[PsfOpenFilehead stringByDeletingPathExtension] lastPathComponent]];
	[_kemoviewControl SetViewTypeMenu:id_viewtype];
	
	self.DrawPsfFlag = send_iflag_draw_current_psf();
	[self CopyPsfDisplayFlagsFromC];
	[self SetPsfFieldMenu];
	[self SetPsfComponentMenu:0];
	[self SetCurrentPsfMenu];
    [self SetPsfRanges];
	
	[_kemoviewer setViewerType:VIEW_3D];
	[_kemoviewer UpdateImage];
	
	int num_loaded =  send_num_loaded_PSF();
	int nlimit_load = send_nlimit_load_psf();
	if(num_loaded >= nlimit_load-1){
		self.psfMoreOpenFlag = 1;
	}else {
		self.psfMoreOpenFlag = 0;
	}
};

- (void) ChooseTextureFile{
	int iflag_fmt;

	NSArray *psfFileTypes = [NSArray arrayWithObjects:@"png",@"bmp",@"PNG",@"BMP",nil];
	NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
	[PsfOpenPanelObj setTitle:@"Choose Texture image data"];
    [PsfOpenPanelObj setAllowedFileTypes:psfFileTypes];
	NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
	
	if(PsfOpenInteger == NSOKButton){
		PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
		NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
		NSString *PsfOpenFileext =   [PsfOpenFilename pathExtension];
		NSString *PsfOpenFilehead =  [PsfOpenFilename stringByDeletingPathExtension];
		
		if([PsfOpenFileext isEqualToString:@"png"]
		   || [PsfOpenFileext isEqualToString:@"PNG"]){ iflag_fmt = SAVE_PNG;}
		else if([PsfOpenFileext isEqualToString:@"bmp"]
				|| [PsfOpenFileext isEqualToString:@"BMP"]){ iflag_fmt = SAVE_BMP;};
		
		set_texture_current_psf(iflag_fmt, [PsfOpenFilehead UTF8String]);
	};	
	return;
}

- (IBAction) OpenPsfFile:(id)pId{
	NSArray *psfFileTypes = [NSArray arrayWithObjects:@"udt",@"inp",@"gz",@"UDT",@"INP",@"GZ",nil];
	NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
	[PsfOpenPanelObj setTitle:@"Choose surface rendering data"];
    [PsfOpenPanelObj setAllowedFileTypes:psfFileTypes];
	NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
	int ierr;
	
	if(PsfOpenInteger == NSOKButton){
		PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
		NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
		NSString *PsfOpenFileext =   [PsfOpenFilename pathExtension];
		NSString *PsfOpenFilehead =  [PsfOpenFilename stringByDeletingPathExtension];
		// NSLog(@"PSF file directory = %@",PsfOpenDirectory);
		// NSLog(@"PSF file name =      %@",PsfOpenFilename);
		// NSLog(@"PSF file header =    %@",PsfOpenFilehead);
		// NSLog(@"self.PsfWindowlabel =    %@",self.PsfWindowlabel);
		
		if([PsfOpenFileext isEqualToString:@"gz"] || [PsfOpenFileext isEqualToString:@"GZ"]){
//			NSString *PsfOpenFileext_gz = [self.PsfOpenFilename pathExtension];
			PsfOpenFileext =    [PsfOpenFilehead pathExtension];
			PsfOpenFilehead =   [PsfOpenFilehead stringByDeletingPathExtension];
		};
		
		ierr = kemoview_open_data_glut([PsfOpenFilename UTF8String]);

		if(ierr == IFLAG_SURF_UDT || ierr ==IFLAG_SURF_UDT_GZ
		   || ierr == IFLAG_SURF_UCD || ierr ==IFLAG_SURF_UCD_GZ){
			[self DrawPsfFile:PsfOpenFilehead];
		};
	};	
}


- (IBAction) ClosePsfFile:(id)pId{
	close_psf_view();

	self.DrawPsfFlag = send_iflag_draw_current_psf();
	[self CopyPsfDisplayFlagsFromC];
	[_kemoviewer UpdateImage];

	int num_loaded = send_num_loaded_PSF();
	if(num_loaded > 0) [self SetCurrentPsfMenu];
};

- (IBAction) CurrentPsfAction:(id)sender
{	
	int id_current;
	
	id_current = [[LoadedPsfID objectAtIndex:self.currentPSFID] intValue];
	set_to_current_PSF(id_current);

	[self UpdateCurrentPsfMenu];
}

- (IBAction) PsfFieldAction:(id)sender
{	
	int isel = self.PSFSelectedField;
	[self SetPsfComponentMenu:isel];

	set_current_psf_field_flag(isel);
	
	[self SetPsfRanges];

	[_kemoviewer UpdateImage];
}

- (IBAction) PsfComponentAction:(id)sender
{	

	set_current_psf_component_flag((int) self.PSFSelectedComponent);
	
      [self SetPsfRanges];

	[_kemoviewer UpdateImage];
}


- (IBAction)PsfSurfSwitchAction:(id)sender;
{
	self.PSFSurfaceSwitch = kemoview_psf_draw_switch_select(PSFSOLID_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)PsfLineSwitchAction:(id)sender;
{
	self.PSFIsolineSwitch = kemoview_psf_draw_switch_select(PSFGRID_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)PsfZeroLineSwitchAction:(id)sender;
{
	self.PSFZerolineSwitch = kemoview_psf_draw_switch_select(ZEROGRID_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)PsfColorbarSwitchAction:(id)sender;
{
	self.PSFColorbarSwitch = kemoview_psf_draw_switch_select(COLORBAR_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePsfPatchColorAction:(id)sender;
{
	if(self.psfPatchColorTag == TEXTURED_SURFACE){
		[self ChooseTextureFile];
	};
	set_current_psf_patch_color(self.psfPatchColorTag);

	[_kemoviewer UpdateImage];
}
- (IBAction)ChoosePsfLineColorAction:(id)sender;
{
	set_current_isoline_color(self.psfLineColorTag);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePsfVectorColorAction:(id)sender;
{
	if(self.psfVectorColorTag == 0){
		kemoview_psf_draw_switch_select(RAINBOW_PSF_VECT);
	} else if (self.psfVectorColorTag == 1) {
		kemoview_psf_draw_switch_select(WHITE_PSF_VECT);
	} else if (self.psfVectorColorTag == 2) {
		[self ChooseTextureFile];
		set_current_psf_patch_color(TEXTURED_SURFACE);
	}
	
	[_kemoviewer UpdateImage];
}

- (IBAction) SetPSFDisplayrange:(id)pSender {
	
	set_current_PSF_linear_colormap(self.PsfMinimumRange,self.PsfMaximumRange);
	[_kemoviewer UpdateImage];
}
	
- (IBAction) ShowIsolineNumber:(id)pSender {
	set_current_n_isoline((int) self.IsolineNumber);

	[_kemoviewer UpdateImage];
}

- (IBAction)ShowPsfOpacity:(id)pSender; {
	set_current_PSF_constant_opacity((double) self.PSFOpacity);

	[_kemoviewer UpdateImage];
}

- (IBAction)DrawPSFVectorAction:(id)sender;
{
	self.DrawPSFVectorFlag = kemoview_psf_draw_switch_select(PSFVECT_TOGGLE);
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
	
	[_kemoviewer UpdateImage];
}

- (IBAction)SetReferenceVector:(id)pSender {
	set_current_scale_vect((double) self.ScaleVector);

	[_kemoviewer UpdateImage];
}

- (IBAction)SetVectorIncrement:(id)pSender {
	set_current_increment_vect(self.PSFVectorIncrement);
	
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePsfPatchDirection:(id)sender;
{
	self.psfPatchDirectionTag = [[_psfPatchDirMatrix selectedCell] tag];
	set_current_psf_polygon_mode(self.psfPatchDirectionTag);
	[_kemoviewer UpdateImage];
}

@end
