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
@synthesize IsolineWidth;
@synthesize IsolineDigit;
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
@synthesize psfTangentialVectorTag;

@synthesize PSFSurfaceSwitch;
@synthesize PSFIsolineSwitch;
@synthesize PSFZerolineSwitch;
@synthesize PSFLineSwitch;
@synthesize PSFColorbarSwitch;

@synthesize PsfMinimumRange;
@synthesize PsfMaximumRange;
@synthesize PsfMinimumDigit;
@synthesize PsfMaximumDigit;

@synthesize PSFVectorMenuAcrive;
@synthesize DrawPSFVectorFlag;
@synthesize ScaleVector;
@synthesize ScaleDigit;
@synthesize VectorThickness;
@synthesize VectorDigit;
@synthesize PSFVectorIncrement;
@synthesize PSFVectorIncDigit;
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
	
	self.IsolineWidth = 1.0;
	self.IsolineDigit = -3;
	self.IsolineNumber = 20;
	self.PSFOpacity = 1.0;
	self.psfMoreOpenFlag = 0;
	self.psfPatchDirectionTag = 0;
    self.psfTangentialVectorTag = 0;
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
    [_ElasticControl UpdateWindow:0];
}

- (id) CopyPsfDisplayFlagsFromC
{
	int i, iflag;
	double minmax;
	NSString *stname;
	NSNumber *stnum;
    struct kv_string *colorname;
	
	PsfNumberOfField =  kemoview_get_each_PSF_field_param(NUM_FIELD_FLAG);
	PsfTotalComponent = kemoview_get_each_PSF_field_param(NTOT_COMPONENT_FLAG);
	[PsfFieldName removeAllObjects];	
	[PsfNumberOfComponent removeAllObjects];
	[PsfMinimum removeAllObjects];
	[PsfMaximum removeAllObjects];
    
    colorname = kemoview_alloc_kvstring();
	for(i = 0; i < PsfNumberOfField; i++){
		kemoview_get_PSF_field_name(colorname,i);
		stname = [[NSString alloc] initWithUTF8String:colorname->string];
		[PsfFieldName      addObject:stname];
		[stname release];	
        
		iflag = kemoview_get_PSF_num_component(i);
		stnum = [[NSNumber alloc] initWithInt:iflag];
		[PsfNumberOfComponent addObject:stnum];
		[stnum release];	
	}
    kemoview_free_kvstring(colorname);
	for(i = 0; i < PsfTotalComponent; i++){
		minmax = kemoview_get_each_PSF_data_range(ISET_COLOR_MIN, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[PsfMinimum      addObject:stnum];
		[stnum release];	
		
		minmax = kemoview_get_each_PSF_data_range(ISET_COLOR_MAX, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[PsfMaximum      addObject:stnum];
		[stnum release];	
	}
	return self;
}

- (void) UpdateCurrentPsfMenu{
	double current_value;
	int i_digit;
	
	self.PSFSelectedField =     kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	self.PSFSelectedComponent = kemoview_get_each_PSF_field_param(COMPONENT_SEL_FLAG);
    
	int iplotted = kemoview_get_each_PSF_field_param(DRAW_ADDRESS_FLAG);
	
	self.PSFSurfaceSwitch =  kemoview_get_PSF_draw_flags(PSFSOLID_TOGGLE);
	self.PSFIsolineSwitch =  kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE);
	self.PSFZerolineSwitch = kemoview_get_PSF_draw_flags(ZEROGRID_TOGGLE);
	self.PSFColorbarSwitch = kemoview_get_PSF_draw_flags(COLORBAR_TOGGLE);
	self.PsfMinimumValue =   kemoview_get_each_PSF_data_range(ISET_COLOR_MIN, iplotted);
	self.PsfMaximumValue =   kemoview_get_each_PSF_data_range(ISET_COLOR_MAX, iplotted);
	self.IsolineNumber =     kemoview_get_PSF_color_param(ISET_NLINE);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;

	kemoview_get_each_PSF_color_w_exp(ISET_COLOR_MIN, &current_value, &i_digit);
	self.PsfMinimumRange =      (CGFloat) current_value;
	self.PsfMinimumDigit =      (CGFloat) i_digit;
	kemoview_get_each_PSF_color_w_exp(ISET_COLOR_MAX, &current_value, &i_digit);
	self.PsfMaximumRange =      (CGFloat) current_value;
	self.PsfMaximumDigit =      (CGFloat) i_digit;

	kemoview_get_each_PSF_color_w_exp(ISET_WIDTH, &current_value, &i_digit);
	self.IsolineWidth =      (CGFloat) current_value;
	self.IsolineDigit =      (CGFloat) i_digit;
	self.PSFOpacity =        kemoview_get_each_PSF_colormap_range(ISET_OPACITY_MAX);
	
	self.DrawPSFVectorFlag = kemoview_get_PSF_draw_flags(PSFVECT_TOGGLE);

	kemoview_get_each_PSF_color_w_exp(ISET_PSF_REFVECT, &current_value, &i_digit);
	self.ScaleVector =      (CGFloat) current_value;
	self.ScaleDigit =       (CGFloat) i_digit;

	kemoview_get_each_PSF_color_w_exp(ISET_VECTOR_INC, &current_value, &i_digit);
	self.PSFVectorIncrement = (CGFloat) current_value;
	self.PSFVectorIncDigit =  (CGFloat) i_digit;
	
	kemoview_get_each_PSF_color_w_exp(ISET_PSF_V_THICK, &current_value, &i_digit);
    self.VectorThickness = (CGFloat) current_value;
	self.VectorDigit =     (CGFloat) i_digit;
	
	self.psfPatchDirectionTag = kemoview_get_PSF_draw_flags(PSF_POLYGON_SWITCH);
	self.psfTangentialVectorTag = kemoview_get_PSF_draw_flags(PSFTANVEC_TOGGLE);
	
	self.psfLineColorTag =  kemoview_get_PSF_color_param(PSFGRID_TOGGLE);
	self.psfPatchColorTag = kemoview_get_PSF_color_param(PSFSOLID_TOGGLE);
	
	self.psfVectorColorTag = kemoview_get_PSF_color_param(ISET_VECTOR_COLOR);
	
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

- (void) ResetCurrentPsfParam{
	self.PSFSelectedField =     IZERO;
	self.PSFSelectedComponent = IZERO;
    
	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, self.PSFSelectedField);
    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, (int) self.PSFSelectedComponent);
    /*   
     int iplotted = kemoview_get_each_PSF_field_param(DRAW_ADDRESS_FLAG);
     
     self.PSFSurfaceSwitch =  kemoview_get_PSF_draw_flags(PSFSOLID_TOGGLE);
     self.PSFIsolineSwitch =  kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE);
     self.PSFZerolineSwitch = kemoview_get_PSF_draw_flags(ZEROGRID_TOGGLE);
     self.PSFColorbarSwitch = kemoview_get_PSF_draw_flags(COLORBAR_TOGGLE);
     self.PsfMinimumValue =   kemoview_get_each_PSF_data_range(ISET_COLOR_MIN, iplotted);
     self.PsfMaximumValue =   kemoview_get_each_PSF_data_range(ISET_COLOR_MAX, iplotted);
     self.IsolineNumber =     kemoview_get_PSF_color_param(ISET_NLINE);
	 self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;

	 kemoview_get_each_PSF_color_w_exp(ISET_COLOR_MIN, &current_value, &i_digit);
	 self.PsfMinimumRange =      (CGFloat) current_value;
	 self.PsfMinimumDigit =      (CGFloat) i_digit;
	 kemoview_get_each_PSF_color_w_exp(ISET_COLOR_MAX, &current_value, &i_digit);
	 self.PsfMaximumRange =      (CGFloat) current_value;
	 self.PsfMaximumDigit =      (CGFloat) i_digit;
	 
	 kemoview_get_each_PSF_color_w_exp(ISET_WIDTH, &current_value, &i_digit);
	 self.IsolineWidth =      (CGFloat) current_value;
	 self.IsolineDigit =      (CGFloat) i_digit;

	 self.PSFOpacity =        kemoview_get_each_PSF_colormap_range(ISET_OPACITY_MAX);
     
     self.DrawPSFVectorFlag = kemoview_get_PSF_draw_flags(PSFVECT_TOGGLE);

	 kemoview_get_each_PSF_color_w_exp(ISET_PSF_REFVECT, &current_value, &i_digit);
	 self.ScaleVector =      (CGFloat) current_value;
	 self.ScaleDigit =       (CGFloat) i_digit;

	 kemoview_get_each_PSF_color_w_exp(ISET_VECTOR_INC, &current_value, &i_digit);
	 self.PSFVectorIncrement = (CGFloat) current_value;
	 self.PSFVectorIncDigit =  (CGFloat) i_digit;
	 
     self.psfTangentialVectorTag = kemoview_get_PSF_draw_flags(PSFTANVEC_TOGGLE);
     
     self.psfPatchDirectionTag = kemoview_get_PSF_draw_flags(PSF_POLYGON_SWITCH);
     self.psfTangentialVectorTag = kemoview_get_PSF_draw_flags(PSFTANVEC_TOGGLE);
     
     self.psfLineColorTag =  kemoview_get_PSF_color_param(PSFGRID_TOGGLE);
     self.psfPatchColorTag = kemoview_get_PSF_color_param(PSFSOLID_TOGGLE);
     
     self.psfVectorColorTag = kemoview_get_PSF_color_param(ISET_VECTOR_COLOR);
     
     [self CopyPsfDisplayFlagsFromC];
     [self SetPsfFieldMenu];
     [_psfFieldMenu selectItemAtIndex:self.PSFSelectedField];
     [_psfComponentMenu selectItemAtIndex:self.PSFSelectedComponent];
     
     [colorMapObject SetColorTables];
     [opacityMapObject SetOpacityTables];
     
     [_psfPatchDirMatrix selectCellWithTag:self.psfPatchDirectionTag];
     
     if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
     else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
     */
}

- (void) SetCurrentPsfMenu{
	int i, j, istep, ifmt;
    struct kv_string *psf_filehead;
	NSString *PsfNumberTxt;
	NSString *PsfFileHeader;
    
	PsfNumberOfdata = kemoview_get_PSF_loaded_params(MAX_LOADED);
	[LoadedPsfFileHead removeAllObjects];
	[LoadedPsfID removeAllObjects];
	for(i = 0; i < PsfNumberOfdata; i++){
		if(kemoview_get_PSF_loaded_flag(i) > 0){
			kemoview_set_PSF_loaded_params(SET_CURRENT, i);

            psf_filehead = kemoview_alloc_kvstring();
			istep = kemoview_get_PSF_full_path_file_prefix(psf_filehead, &ifmt);
			PsfNumberTxt = [[NSString alloc] initWithFormat:@"%d: ",i+1];
			PsfFileHeader = [[[NSString alloc] initWithUTF8String:psf_filehead->string] lastPathComponent];
			PsfFileHeader = [PsfNumberTxt stringByAppendingString:PsfFileHeader];
			self.currentPSFStep = istep;
			[LoadedPsfFileHead addObject:PsfFileHeader];
			[LoadedPsfID addObject:[[NSNumber alloc] initWithInt:i]];
            
            kemoview_free_kvstring(psf_filehead);
		}
	}
    
	PsfNumberOfdata = kemoview_get_PSF_loaded_params(NUM_LOADED);
	[_currentPsfMenu removeAllItems];
	for(i = 0; i < PsfNumberOfdata; i++){
		[_currentPsfMenu addItemWithTitle:[LoadedPsfFileHead objectAtIndex: i]];
		j = [[LoadedPsfID objectAtIndex: i] intValue];
		if(j == kemoview_get_PSF_loaded_params(SET_CURRENT)) self.currentPSFID = i;
	};
    
    
	[self UpdateCurrentPsfMenu];
}

- (void) SetCurrentPSFFile
{
    struct kv_string *ucd_m = kemoview_alloc_kvstring();

    kemoview_get_PSF_full_path_file_name(ucd_m);
    NSString *str = [NSString stringWithCString:ucd_m->string encoding:NSUTF8StringEncoding];
    NSURL *urlReadPSF = [NSURL fileURLWithPath:str]; 
    [_psfPathControl setURL:urlReadPSF];

    kemoview_free_kvstring(ucd_m);
    return;
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
		
        
		iplotted = kemoview_get_each_PSF_field_param(DRAW_ADDRESS_FLAG);
		self.PsfMinimumValue = kemoview_get_each_PSF_data_range(ISET_COLOR_MIN, iplotted);
		self.PsfMaximumValue = kemoview_get_each_PSF_data_range(ISET_COLOR_MAX, iplotted);
	}
	
	return;
}

- (void) SetPsfRanges{
	double current_value;
	int i_digit;
	int iplotted;
    
	iplotted = kemoview_get_each_PSF_field_param(DRAW_ADDRESS_FLAG);
    
 	self.PsfMinimumValue = kemoview_get_each_PSF_data_range(ISET_COLOR_MIN, iplotted);
	self.PsfMaximumValue = kemoview_get_each_PSF_data_range(ISET_COLOR_MAX, iplotted);

	kemoview_get_each_PSF_color_w_exp(ISET_COLOR_MIN, &current_value, &i_digit);
	self.PsfMinimumRange =      (CGFloat) current_value;
	self.PsfMinimumDigit =      (CGFloat) i_digit;
	kemoview_get_each_PSF_color_w_exp(ISET_COLOR_MAX, &current_value, &i_digit);
	self.PsfMaximumRange =      (CGFloat) current_value;
	self.PsfMaximumDigit =      (CGFloat) i_digit;
	
    
    [self.rgbaMapObject updateColormapParameter];
    [self.colorMapObject InitColorTables];
    [self.colorMapObject SetColorTables];
    [self.opacityMapObject InitOpacityTables];
    [self.opacityMapObject SetOpacityTables];
}


- (void) DrawPsfFile:(NSString*) PsfOpenFilehead{
	int id_viewtype = kemoview_get_view_type_flag();
    
	self.currentPSFStep = [[PsfOpenFilehead pathExtension] intValue];
	self.PsfWindowlabel = [NSString stringWithFormat:@"PSF:%@",
						   [[PsfOpenFilehead stringByDeletingPathExtension] lastPathComponent]];
	[_kemoviewControl SetViewTypeMenu:id_viewtype];
	
	self.DrawPsfFlag = kemoview_get_PSF_loaded_params(DRAW_SWITCH);
    [_ElasticControl UpdateWindow:self.DrawPsfFlag];
	[self CopyPsfDisplayFlagsFromC];
	[self SetPsfFieldMenu];
	[self SetPsfComponentMenu:0];
	[self SetCurrentPsfMenu];
    [self SetPsfRanges];
	
    [_kemoviewControl Set3DView];
	[_kemoviewer UpdateImage];
	
	int num_loaded =  kemoview_get_PSF_loaded_params(NUM_LOADED);
	int nlimit_load = kemoview_get_PSF_maximum_load();
	if(num_loaded >= nlimit_load-1){
		self.psfMoreOpenFlag = 1;
	}else {
		self.psfMoreOpenFlag = 0;
	}
    
    [self SetCurrentPSFFile];
};

- (void) ReadTextureFile:(NSString *) PsfOpenFilename
{
    int width, height;
    int rowBytes, pixelBytes;
    unsigned char *pixels;
    
    
    NSData *img = [ [ NSData alloc ] initWithContentsOfFile: PsfOpenFilename];
    NSBitmapImageRep *imgRep = [NSBitmapImageRep imageRepWithData:img];
    if (img != nil) {
        width = [imgRep pixelsWide];
        height = [imgRep pixelsHigh];
//      int  bmpformat = [imgRep bitmapFormat];
//      int  bitParPix = [imgRep bitsPerPixel];
        rowBytes = [imgRep bytesPerRow];
        pixelBytes = rowBytes / width;
        pixels = (unsigned char *)[imgRep bitmapData];
        
        kemoview_set_PSF_by_rgba_texture(width, height, pixels);
    }
    [img release];
}

- (void) ReadPsfFile:(NSString *) PsfOpenFilename
{
    NSString *PsfOpenFileext =   [PsfOpenFilename pathExtension];
    NSString *PsfOpenFilehead =  [PsfOpenFilename stringByDeletingPathExtension];
    // NSLog(@"PSF file name =      %@",PsfOpenFilename);
    // NSLog(@"PSF file header =    %@",PsfOpenFilehead);
    // NSLog(@"self.PsfWindowlabel =    %@",self.PsfWindowlabel);
    
    if([PsfOpenFileext isEqualToString:@"gz"] || [PsfOpenFileext isEqualToString:@"GZ"]){
        //			NSString *PsfOpenFileext_gz = [self.PsfOpenFilename pathExtension];
        PsfOpenFileext =    [PsfOpenFilehead pathExtension];
        PsfOpenFilehead =   [PsfOpenFilehead stringByDeletingPathExtension];
    };
    
    struct kv_string *filename = kemoview_init_kvstring_by_string([PsfOpenFilename UTF8String]);
    int iflag_datatype = kemoview_open_data(filename);
    kemoview_free_kvstring(filename);
    
    if(iflag_datatype == IFLAG_SURFACES) [self DrawPsfFile:PsfOpenFilehead];
}

- (void) ChooseTextureFile{
    
    NSArray *psfFileTypes = [NSArray arrayWithObjects:@"png",@"bmp",@"PNG",@"BMP",nil];
    NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
    [PsfOpenPanelObj setTitle:@"Choose Texture image data"];
    [PsfOpenPanelObj setAllowedFileTypes:psfFileTypes];
    NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
    
    if(PsfOpenInteger == NSFileHandlingPanelOKButton){
        PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
        NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
        [self ReadTextureFile:PsfOpenFilename];
    };	
    return;
}

- (IBAction) OpenPsfFile:(id)pId{
	NSArray *psfFileTypes = [NSArray arrayWithObjects:
                             @"sfm",@"SFM",@"sdt",@"SDT",
                             @"udt",@"inp",@"vtk",@"vtd",@"gz",
                             @"UDT",@"INP",@"VTK",@"VTD",@"GZ",nil];
	NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
	[PsfOpenPanelObj setTitle:@"Choose surface rendering data"];
    [PsfOpenPanelObj setAllowedFileTypes:psfFileTypes];
    [PsfOpenPanelObj beginSheetModalForWindow:window 
                                 completionHandler:^(NSInteger PsfOpenInteger){
	
	if(PsfOpenInteger == NSFileHandlingPanelOKButton){
		NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
        PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
        // NSLog(@"PSF file directory = %@",PsfOpenDirectory);
        [self ReadPsfFile:PsfOpenFilename];
	};	
                                 }];
}


- (IBAction) ClosePsfFile:(id)pId{
    [self ResetCurrentPsfParam];
    int num_loaded = kemoview_close_PSF_view();
    self.DrawPsfFlag = kemoview_get_PSF_loaded_params(DRAW_SWITCH);
    [_ElasticControl UpdateWindow:self.DrawPsfFlag];
    
	if(num_loaded > 0){
        [self CopyPsfDisplayFlagsFromC];
        [self SetCurrentPsfMenu];
    };
    [_kemoviewControl Set3DView];
	[_kemoviewer UpdateImage];
};

- (IBAction) UpdatePsfAction:(id)sender
{
    [_kemoviewer UpdateImage];
};

- (IBAction) CurrentPsfAction:(id)sender
{	
	int id_current;
	
	id_current = [[LoadedPsfID objectAtIndex:self.currentPSFID] intValue];
	kemoview_set_PSF_loaded_params(SET_CURRENT, id_current);
    [self SetCurrentPSFFile];
	[self UpdateCurrentPsfMenu];
}

- (IBAction) PsfFieldAction:(id)sender
{	
	int isel = self.PSFSelectedField;
	[self SetPsfComponentMenu:isel];
    
	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, isel);
	
	[self SetPsfRanges];
    
	[_kemoviewer UpdateImage];
}

- (IBAction) PsfComponentAction:(id)sender
{	
    
	kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, (int) self.PSFSelectedComponent);
	
    [self SetPsfRanges];
    
	[_kemoviewer UpdateImage];
}


- (IBAction)PsfSurfSwitchAction:(id)sender;
{
	self.PSFSurfaceSwitch = kemoview_select_PSF_draw_switch(PSFSOLID_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)PsfLineSwitchAction:(id)sender;
{
	self.PSFIsolineSwitch = kemoview_select_PSF_draw_switch(PSFGRID_TOGGLE);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
	[self UpdateCurrentPsfMenu];
	[_kemoviewer UpdateImage];
}

- (IBAction)PsfZeroLineSwitchAction:(id)sender;
{
	self.PSFZerolineSwitch = kemoview_select_PSF_draw_switch(ZEROGRID_TOGGLE);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
	[self UpdateCurrentPsfMenu];
	[_kemoviewer UpdateImage];
}

- (IBAction)PsfColorbarSwitchAction:(id)sender;
{
	self.PSFColorbarSwitch = kemoview_select_PSF_draw_switch(COLORBAR_TOGGLE);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePsfPatchColorAction:(id)sender;
{
	if(self.psfPatchColorTag == TEXTURED_SURFACE){
		[self ChooseTextureFile];
	}
    else if(self.psfPatchColorTag == SINGLE_COLOR){
        [self SetPSFColorFromColorWell];
    };
	kemoview_set_PSF_color_param(PSFSOLID_TOGGLE, self.psfPatchColorTag);
    
	[_kemoviewer UpdateImage];
}
- (IBAction)ChoosePsfLineColorAction:(id)sender;
{
	kemoview_set_PSF_color_param(PSFGRID_TOGGLE, self.psfLineColorTag);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePsfVectorColorAction:(id)sender;
{
	kemoview_set_PSF_color_param(ISET_VECTOR_COLOR, (int) self.psfVectorColorTag);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChoosePsfVectorModeAction:(id)sender;
{
	if(self.psfTangentialVectorTag == 0){
		kemoview_set_PSF_tangential_vec_mode(FULL_COMPONENT);
	} else if (self.psfVectorColorTag == 1) {
		kemoview_set_PSF_tangential_vec_mode(TANGENTIAL_COMPONENT);
	}
	[_kemoviewer UpdateImage];
}

- (IBAction) SetPSFDisplayrange:(id)pSender {
	
	kemoview_set_PSF_linear_colormap(self.PsfMinimumRange, (int) self.PsfMinimumDigit,
									 self.PsfMaximumRange, (int) self.PsfMaximumDigit);
//	[_kemoviewer UpdateImage];
}

- (IBAction) ShowIsolineNumber:(id)pSender {
	kemoview_set_PSF_color_param(ISET_NLINE, (int) self.IsolineNumber);
//    [_kemoviewer UpdateImage];
}

- (IBAction) SetIsolineWidth:(id)pSender {
	kemoview_set_each_PSF_color_w_exp(ISET_WIDTH, (double) self.IsolineWidth, (int) self.IsolineDigit);
//	[_kemoviewer UpdateImage];
}

- (IBAction)DrawPSFVectorAction:(id)sender;
{
	self.DrawPSFVectorFlag = kemoview_select_PSF_draw_switch(PSFVECT_TOGGLE);
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
	
	[_kemoviewer UpdateImage];
}

- (IBAction)SetReferenceVector:(id)pSender {
	kemoview_set_each_PSF_color_w_exp(ISET_PSF_REFVECT, (double) self.ScaleVector, (int) self.ScaleDigit);
//	[_kemoviewer UpdateImage];
}

- (IBAction)SetVectorIncrement:(id)pSender {
	kemoview_set_each_PSF_color_w_exp(ISET_VECTOR_INC, (double) self.PSFVectorIncrement, (int) self.PSFVectorIncDigit);
//	[_kemoviewer UpdateImage];
}

- (IBAction)SetVectorThickness:(id)pSender {
    kemoview_set_each_PSF_color_w_exp(ISET_PSF_V_THICK, (double) self.VectorThickness, (int) self.VectorDigit);
//    [_kemoviewer UpdateImage];
}
    
- (IBAction)ChoosePsfPatchDirection:(id)sender;
{
	self.psfPatchDirectionTag = [[_psfPatchDirMatrix selectedCell] tag];
	kemoview_set_PSF_polygon_mode(self.psfPatchDirectionTag);
	[_kemoviewer UpdateImage];
}

- (void)SetPSFColorFromColorWell{
    CGFloat redBG, greenBG, blueBG, opacityBG;
    double rgba[4];
    NSColor *nsPSFPatchColor = [PSFPatchColorWell color];
    [nsPSFPatchColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
    
    rgba[0] = (double) redBG;
    rgba[1] = (double) greenBG;
    rgba[2] = (double) blueBG;
    rgba[3] = (double) opacityBG;
    self.PSFOpacity = opacityBG;
    
    kemoview_set_PSF_single_color(rgba);
}

- (IBAction)SetPSFPatchColorAction:(id)sender
{
    [self SetPSFColorFromColorWell];
    [_kemoviewer UpdateImage];
}

- (IBAction)SetPSFSingleOpacityAction:(id)sender
{
    kemoview_set_PSF_constant_opacity((double) self.PSFOpacity);

    NSColor *OriginalWellColor = [PSFPatchColorWell color];
    NSColor *NewWellColor = [OriginalWellColor colorWithAlphaComponent:self.PSFOpacity];
    [PSFPatchColorWell setColor:NewWellColor];
    
    [_kemoviewer UpdateImage];
};

@end
