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
    self.rgbaMapObject =    [RGBAMapController alloc];
    self.colorMapObject =   [[ColorMapController alloc] init];
    self.opacityMapObject = [OpacityMapController alloc];

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
    [_ElasticControl UpdateWindow:0];
}

- (void) CopyPsfDisplayFlagsFromC:(struct kemoviewer_type *) kemo_sgl
{
	int i;
	double minmax;
	NSString *stname;
	NSNumber *stnum;
    struct kv_string *colorname;
	
	PsfNumberOfField
        =  kemoview_get_each_PSF_field_param(kemo_sgl, NUM_FIELD_FLAG);
	PsfTotalComponent
        = kemoview_get_each_PSF_field_param(kemo_sgl, NTOT_COMPONENT_FLAG);
	[PsfFieldName removeAllObjects];	
	[PsfNumberOfComponent removeAllObjects];
	[PsfMinimum removeAllObjects];
	[PsfMaximum removeAllObjects];
    
    colorname = kemoview_alloc_kvstring();
	for(i = 0; i < PsfNumberOfField; i++){
		kemoview_get_PSF_field_name(kemo_sgl, colorname, i);
		stname = [[NSString alloc] initWithUTF8String:colorname->string];
		[PsfFieldName      addObject:stname];
		[stname release];	
        
		long iflag = kemoview_get_PSF_num_component(kemo_sgl, i);
		stnum = [[NSNumber alloc] initWithLong:iflag];
		[PsfNumberOfComponent addObject:stnum];
		[stnum release];	
	}
    kemoview_free_kvstring(colorname);
	for(i = 0; i < PsfTotalComponent; i++){
		minmax = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MIN, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[PsfMinimum      addObject:stnum];
		[stnum release];	
		
		minmax = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MAX, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[PsfMaximum      addObject:stnum];
		[stnum release];	
	}
	return;
}

- (void) UpdateCurrentPsfMenu:(struct kemoviewer_type *) kemo_sgl
{
	double current_value;
	int i_digit;
	
	self.PSFSelectedField
        = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
	self.PSFSelectedComponent
        = kemoview_get_each_PSF_field_param(kemo_sgl, COMPONENT_SEL_FLAG);
    
	int iplotted = kemoview_get_each_PSF_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
	
	self.PSFSurfaceSwitch 
        =  kemoview_get_PSF_draw_flags(kemo_sgl, PSFSOLID_TOGGLE);
	self.PSFIsolineSwitch 
        =  kemoview_get_PSF_draw_flags(kemo_sgl, PSFGRID_TOGGLE);
	self.PSFZerolineSwitch
        = kemoview_get_PSF_draw_flags(kemo_sgl, ZEROGRID_TOGGLE);
	self.PSFColorbarSwitch 
        = kemoview_get_PSF_draw_flags(kemo_sgl, COLORBAR_TOGGLE);
	self.PsfMinimumValue 
        = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MIN, iplotted);
	self.PsfMaximumValue 
        = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MAX, iplotted);
	self.IsolineNumber =     kemoview_get_PSF_color_param(kemo_sgl, ISET_NLINE);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;

	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                      &current_value, &i_digit);
	self.PsfMinimumRange =      (CGFloat) current_value;
	self.PsfMinimumDigit =      (CGFloat) i_digit;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                      &current_value, &i_digit);
	self.PsfMaximumRange =      (CGFloat) current_value;
	self.PsfMaximumDigit =      (CGFloat) i_digit;

	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_WIDTH,
                                      &current_value, &i_digit);
	self.IsolineWidth =      (CGFloat) current_value;
	self.IsolineDigit =      (CGFloat) i_digit;
	self.PSFOpacity
        = kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_OPACITY_MAX);
	
	self.DrawPSFVectorFlag
        = kemoview_get_PSF_draw_flags(kemo_sgl, PSFVECT_TOGGLE);

	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_REFVECT, 
                                      &current_value, &i_digit);
	self.ScaleVector =      (CGFloat) current_value;
	self.ScaleDigit =       (CGFloat) i_digit;

	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_VECTOR_INC,
                                      &current_value, &i_digit);
	self.PSFVectorIncrement = (CGFloat) current_value;
	self.PSFVectorIncDigit =  (CGFloat) i_digit;
	
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_V_THICK, 
                                      &current_value, &i_digit);
    self.VectorThickness = (CGFloat) current_value;
	self.VectorDigit =     (CGFloat) i_digit;
	
	self.psfPatchDirectionTag
        = kemoview_get_PSF_draw_flags(kemo_sgl, PSF_POLYGON_SWITCH);
	self.psfTangentialVectorTag 
        = kemoview_get_PSF_draw_flags(kemo_sgl, PSFTANVEC_TOGGLE);
	
	self.psfLineColorTag =  kemoview_get_PSF_color_param(kemo_sgl, PSFGRID_TOGGLE);
	self.psfPatchColorTag = kemoview_get_PSF_color_param(kemo_sgl, PSFSOLID_TOGGLE);
	
	self.psfVectorColorTag = kemoview_get_PSF_color_param(kemo_sgl, ISET_VECTOR_COLOR);
	
    [self CopyPsfDisplayFlagsFromC:kemo_sgl];
	[self SetPsfFieldMenu];
	[self SetPsfComponentMenu:self.PSFSelectedField
                     kemoview:kemo_sgl];
	[_psfFieldMenu selectItemAtIndex:self.PSFSelectedField];
	[_psfComponentMenu selectItemAtIndex:self.PSFSelectedComponent];
	
    [colorMapObject SetColorTables:kemo_sgl];
    [opacityMapObject SetOpacityTables:kemo_sgl];
	
	[_psfPatchDirMatrix selectCellWithTag:self.psfPatchDirectionTag];
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
}

- (void) ResetCurrentPsfParam:(struct kemoviewer_type *) kemo_sgl{
	self.PSFSelectedField =     IZERO;
	self.PSFSelectedComponent = IZERO;
    
	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, (int) self.PSFSelectedField,
                                      kemo_sgl);
    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, (int) self.PSFSelectedComponent,
                                      kemo_sgl);
    /*   
     int iplotted = kemoview_get_each_PSF_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
     
     self.PSFSurfaceSwitch =  kemoview_get_PSF_draw_flags(kemo_sgl, PSFSOLID_TOGGLE);
     self.PSFIsolineSwitch =  kemoview_get_PSF_draw_flags(kemo_sgl, PSFGRID_TOGGLE);
     self.PSFZerolineSwitch = kemoview_get_PSF_draw_flags(kemo_sgl, ZEROGRID_TOGGLE);
     self.PSFColorbarSwitch = kemoview_get_PSF_draw_flags(kemo_sgl, COLORBAR_TOGGLE);
     self.PsfMinimumValue =   kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MIN, iplotted);
     self.PsfMaximumValue =   kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MAX, iplotted);
     self.IsolineNumber =     kemoview_get_PSF_color_param(kemo_sgl, ISET_NLINE);
	 self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;

	 kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN, &current_value, &i_digit);
	 self.PsfMinimumRange =      (CGFloat) current_value;
	 self.PsfMinimumDigit =      (CGFloat) i_digit;
	 kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX, &current_value, &i_digit);
	 self.PsfMaximumRange =      (CGFloat) current_value;
	 self.PsfMaximumDigit =      (CGFloat) i_digit;
	 
	 kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_WIDTH, &current_value, &i_digit);
	 self.IsolineWidth =      (CGFloat) current_value;
	 self.IsolineDigit =      (CGFloat) i_digit;

	 self.PSFOpacity =        kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_OPACITY_MAX);
     
     self.DrawPSFVectorFlag = kemoview_get_PSF_draw_flags(kemo_sgl, PSFVECT_TOGGLE);

	 kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_REFVECT, &current_value, &i_digit);
	 self.ScaleVector =      (CGFloat) current_value;
	 self.ScaleDigit =       (CGFloat) i_digit;

	 kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_VECTOR_INC, &current_value, &i_digit);
	 self.PSFVectorIncrement = (CGFloat) current_value;
	 self.PSFVectorIncDigit =  (CGFloat) i_digit;
	 
     self.psfTangentialVectorTag = kemoview_get_PSF_draw_flags(kemo_sgl, PSFTANVEC_TOGGLE);
     
     self.psfPatchDirectionTag = kemoview_get_PSF_draw_flags(kemo_sgl, PSF_POLYGON_SWITCH);
     self.psfTangentialVectorTag = kemoview_get_PSF_draw_flags(kemo_sgl, PSFTANVEC_TOGGLE);
     
     self.psfLineColorTag =  kemoview_get_PSF_color_param(kemo_sgl, PSFGRID_TOGGLE);
     self.psfPatchColorTag = kemoview_get_PSF_color_param(kemo_sgl, PSFSOLID_TOGGLE);
     
     self.psfVectorColorTag = kemoview_get_PSF_color_param(kemo_sgl, ISET_VECTOR_COLOR);
     
     [self CopyPsfDisplayFlagsFromC:kemo_sgl];
     [self SetPsfFieldMenu];
     [_psfFieldMenu selectItemAtIndex:self.PSFSelectedField];
     [_psfComponentMenu selectItemAtIndex:self.PSFSelectedComponent];
     
     [colorMapObject SetColorTables:kemo_sgl];
     [opacityMapObject SetOpacityTables:kemo_sgl];
     
     [_psfPatchDirMatrix selectCellWithTag:self.psfPatchDirectionTag];
     
     if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
     else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
     */
}

- (void) SetCurrentPsfMenu:(struct kemoviewer_type *) kemo_sgl
{
	int i, j, istep, ifmt;
    struct kv_string *psf_filehead;
	NSString *PsfNumberTxt;
	NSString *PsfFileHeader;
    
	PsfNumberOfdata = kemoview_get_PSF_loaded_params(kemo_sgl, MAX_LOADED);
	[LoadedPsfFileHead removeAllObjects];
	[LoadedPsfID removeAllObjects];
	for(i = 0; i < PsfNumberOfdata; i++){
		if(kemoview_get_PSF_loaded_flag(kemo_sgl, i) > 0){
			kemoview_set_PSF_loaded_params(SET_CURRENT, i, kemo_sgl);

            psf_filehead = kemoview_alloc_kvstring();
			istep = kemoview_get_PSF_full_path_file_prefix(kemo_sgl, psf_filehead, &ifmt);
			PsfNumberTxt = [[NSString alloc] initWithFormat:@"%d: ",i+1];
			PsfFileHeader = [[[NSString alloc] initWithUTF8String:psf_filehead->string] lastPathComponent];
			PsfFileHeader = [PsfNumberTxt stringByAppendingString:PsfFileHeader];
			self.currentPSFStep = istep;
			[LoadedPsfFileHead addObject:PsfFileHeader];
			[LoadedPsfID addObject:[[NSNumber alloc] initWithInt:i]];
            
            kemoview_free_kvstring(psf_filehead);
		}
	}
    
	PsfNumberOfdata = kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);
	[_currentPsfMenu removeAllItems];
	for(i = 0; i < PsfNumberOfdata; i++){
		[_currentPsfMenu addItemWithTitle:[LoadedPsfFileHead objectAtIndex: i]];
		j = [[LoadedPsfID objectAtIndex: i] intValue];
		if(j == kemoview_get_PSF_loaded_params(kemo_sgl, SET_CURRENT)) self.currentPSFID = i;
	};
    [self UpdateCurrentPsfMenu:kemo_sgl];
}

- (void) SetCurrentPSFFile:(struct kemoviewer_type *) kemo_sgl
{
    struct kv_string *ucd_m = kemoview_alloc_kvstring();

    kemoview_get_PSF_full_path_file_name(kemo_sgl, ucd_m);
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

- (void) SetPsfComponentMenu:(NSInteger)isel
                    kemoview:(struct kemoviewer_type *) kemo_sgl
{
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
		
        
		iplotted = kemoview_get_each_PSF_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
		self.PsfMinimumValue 
            = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MIN, iplotted);
		self.PsfMaximumValue
            = kemoview_get_each_PSF_data_range(kemo_sgl, ISET_COLOR_MAX, iplotted);
	}
	
	return;
}

- (void) SetPsfRanges:(struct kemoviewer_type *) kemo_sgl{
	double current_value;
	int i_digit;
	int iplotted;
    
	iplotted = kemoview_get_each_PSF_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
    
 	self.PsfMinimumValue = kemoview_get_each_PSF_data_range(kemo_sgl, 
                                                            ISET_COLOR_MIN, iplotted);
	self.PsfMaximumValue = kemoview_get_each_PSF_data_range(kemo_sgl,
                                                            ISET_COLOR_MAX, iplotted);

	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                      &current_value, &i_digit);
	self.PsfMinimumRange =      (CGFloat) current_value;
	self.PsfMinimumDigit =      (CGFloat) i_digit;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_COLOR_MAX, 
                                      &current_value, &i_digit);
	self.PsfMaximumRange =      (CGFloat) current_value;
	self.PsfMaximumDigit =      (CGFloat) i_digit;
	
    
    [self.rgbaMapObject updateColormapParameter:kemo_sgl];
    [self.colorMapObject InitColorTables:kemo_sgl];
    [self.colorMapObject SetColorTables:kemo_sgl];
    [self.opacityMapObject InitOpacityTables:kemo_sgl];
    [self.opacityMapObject SetOpacityTables:kemo_sgl];
}


- (void) DrawPsfFile:(NSString*) PsfOpenFilehead
            kemoview:(struct kemoviewer_type *) kemo_sgl
{
	int id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
    
	self.currentPSFStep = [[PsfOpenFilehead pathExtension] intValue];
	self.PsfWindowlabel = [NSString stringWithFormat:@"PSF:%@",
						   [[PsfOpenFilehead stringByDeletingPathExtension] lastPathComponent]];
	[_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
	
	self.DrawPsfFlag = kemoview_get_PSF_loaded_params(kemo_sgl, DRAW_SWITCH);
    [_ElasticControl UpdateWindow:self.DrawPsfFlag];
    [self CopyPsfDisplayFlagsFromC:kemo_sgl];
	[self SetPsfFieldMenu];
	[self SetPsfComponentMenu:0
                     kemoview:kemo_sgl];
    [self SetCurrentPsfMenu:kemo_sgl];
    [self SetPsfRanges:kemo_sgl];
	
    [_kemoviewControl Set3DView:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
	
	int num_loaded =  kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);
	int nlimit_load = kemoview_get_PSF_maximum_load(kemo_sgl);
	if(num_loaded >= nlimit_load-1){
		self.psfMoreOpenFlag = 1;
	}else {
		self.psfMoreOpenFlag = 0;
	}
    [_kemoviewControl TimeLabelAvaiability];
    [_kemoviewControl FileStepLabelAvaiability];
    
    [self SetCurrentPSFFile:kemo_sgl];
};

- (void) ReadTextureFile:(NSString *) PsfOpenFilename
                kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSInteger width, height;
    NSInteger rowBytes, pixelBytes;
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
        
        kemoview_set_PSF_by_rgba_texture((int) width, (int) height, 
                                         pixels, kemo_sgl);
    }
    [img release];
}

- (void) ReadPsfFile:(NSString *) PsfOpenFilename
            kemoview:(struct kemoviewer_type *) kemo_sgl
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
    int iflag_datatype = kemoview_open_data(filename, kemo_sgl);
    kemoview_free_kvstring(filename);
    
    if(iflag_datatype == IFLAG_SURFACES) [self DrawPsfFile:PsfOpenFilehead
                                                  kemoview:kemo_sgl];
}

- (void) ChooseTextureFile:(struct kemoviewer_type *) kemo_sgl
{
    
    NSArray *psfFileTypes = [NSArray arrayWithObjects:@"png",@"bmp",@"PNG",@"BMP",nil];
    NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
    [PsfOpenPanelObj setTitle:@"Choose Texture image data"];
    [PsfOpenPanelObj setAllowedFileTypes:psfFileTypes];
    NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
    
    if(PsfOpenInteger == NSModalResponseOK){
        PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
        NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
        [self ReadTextureFile:PsfOpenFilename
                     kemoview:kemo_sgl];
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
	
	if(PsfOpenInteger == NSModalResponseOK){
		NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
        PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
        // NSLog(@"PSF file directory = %@",PsfOpenDirectory);
        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        [self ReadPsfFile:PsfOpenFilename
                 kemoview:kemo_sgl];
	};	
                                 }];
    [_kemoviewControl TimeLabelAvaiability];
    [_kemoviewControl FileStepLabelAvaiability];
}


- (IBAction) ClosePsfFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self ResetCurrentPsfParam:kemo_sgl];
    int num_loaded = kemoview_close_PSF_view(kemo_sgl);
    self.DrawPsfFlag = kemoview_get_PSF_loaded_params(kemo_sgl, DRAW_SWITCH);
    [_ElasticControl UpdateWindow:self.DrawPsfFlag];
    
	if(num_loaded > 0){
        [self CopyPsfDisplayFlagsFromC:kemo_sgl];
        [self SetCurrentPsfMenu:kemo_sgl];
    };
    [_kemoviewControl Set3DView:kemo_sgl];
    [_kemoviewControl TimeLabelAvaiability];
    [_kemoviewControl FileStepLabelAvaiability];
	[_metalView UpdateImage:kemo_sgl];
};

- (IBAction) UpdatePsfAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [_metalView UpdateImage:kemo_sgl];
};

- (IBAction) CurrentPsfAction:(id)sender
{	
	int id_current = [[LoadedPsfID objectAtIndex:self.currentPSFID] intValue];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_loaded_params(SET_CURRENT, id_current, kemo_sgl);
    [self SetCurrentPSFFile:kemo_sgl];
    [self UpdateCurrentPsfMenu:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) PsfFieldAction:(id)sender
{	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	[self SetPsfComponentMenu:self.PSFSelectedField
                     kemoview:kemo_sgl];
    kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, (int) self.PSFSelectedField,
                                      kemo_sgl);
	
    [self SetPsfRanges:kemo_sgl];
    
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) PsfComponentAction:(id)sender
{	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, (int) self.PSFSelectedComponent,
                                      kemo_sgl);
	
    [self SetPsfRanges:kemo_sgl];
    
	[_metalView UpdateImage:kemo_sgl];
}


- (IBAction)PsfSurfSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags(PSFSOLID_TOGGLE, self.PSFSurfaceSwitch ,
                                kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)PsfLineSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags(PSFGRID_TOGGLE, self.PSFIsolineSwitch, kemo_sgl);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
    [self UpdateCurrentPsfMenu:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)PsfZeroLineSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags(ZEROGRID_TOGGLE, (int) self.PSFZerolineSwitch, kemo_sgl);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
    [self UpdateCurrentPsfMenu:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)PsfColorbarSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags(COLORBAR_TOGGLE, (int) self.PSFColorbarSwitch, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChoosePsfPatchColorAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if(self.psfPatchColorTag == TEXTURED_SURFACE){
        kemoview_update_PSF_textured_id(kemo_sgl);
        [self ChooseTextureFile:kemo_sgl];
	}
    else if(self.psfPatchColorTag == SINGLE_COLOR){
        [self SetPSFColorFromColorWell:kemo_sgl];
    };
	kemoview_set_PSF_color_param(PSFSOLID_TOGGLE,
                                 (int) self.psfPatchColorTag,
                                 kemo_sgl);
    
	[_metalView UpdateImage:kemo_sgl];
}
- (IBAction)ChoosePsfLineColorAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_color_param(PSFGRID_TOGGLE, 
                                 (int) self.psfLineColorTag,
                                 kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChoosePsfVectorColorAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_color_param(ISET_VECTOR_COLOR, 
                                 (int) self.psfVectorColorTag,
                                 kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChoosePsfVectorModeAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if(self.psfTangentialVectorTag == 0){
		kemoview_set_PSF_tangential_vec_mode(FULL_COMPONENT, kemo_sgl);
	} else if (self.psfVectorColorTag == 1) {
		kemoview_set_PSF_tangential_vec_mode(TANGENTIAL_COMPONENT, kemo_sgl);
	}
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) SetPSFDisplayrange:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_linear_colormap(self.PsfMinimumRange, (int) self.PsfMinimumDigit,
									 self.PsfMaximumRange, (int) self.PsfMaximumDigit,
                                     kemo_sgl);
//	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowIsolineNumber:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_color_param(ISET_NLINE,
                                 (int) self.IsolineNumber,
                                 kemo_sgl);
//    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) SetIsolineWidth:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_PSF_color_w_exp(ISET_WIDTH, (double) self.IsolineWidth,
                                      (int) self.IsolineDigit, kemo_sgl);
//	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)DrawPSFVectorAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags(PSFVECT_TOGGLE, self.DrawPSFVectorFlag, kemo_sgl);
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetReferenceVector:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_PSF_color_w_exp(ISET_PSF_REFVECT, (double) self.ScaleVector, 
                                      (int) self.ScaleDigit, kemo_sgl);
//	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetVectorIncrement:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_PSF_color_w_exp(ISET_VECTOR_INC, (double) self.PSFVectorIncrement,
                                      (int) self.PSFVectorIncDigit, kemo_sgl);
//	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetVectorThickness:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_each_PSF_color_w_exp(ISET_PSF_V_THICK, (double) self.VectorThickness,
                                      (int) self.VectorDigit, kemo_sgl);
//    [_metalView UpdateImage:kemo_sgl];
}
    
- (IBAction)ChoosePsfPatchDirection:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	self.psfPatchDirectionTag = [[_psfPatchDirMatrix selectedCell] tag];
	kemoview_set_PSF_polygon_mode((int) self.psfPatchDirectionTag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (void)SetPSFColorFromColorWell:(struct kemoviewer_type *) kemo_sgl
{
    CGFloat redBG, greenBG, blueBG, opacityBG;
    double rgba[4];
    NSColor *nsPSFPatchColor = [PSFPatchColorWell color];
    [nsPSFPatchColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
    
    rgba[0] = (double) redBG;
    rgba[1] = (double) greenBG;
    rgba[2] = (double) blueBG;
    rgba[3] = (double) opacityBG;
    self.PSFOpacity = opacityBG;
    
    kemoview_set_PSF_single_color(rgba, kemo_sgl);
}

- (IBAction)SetPSFPatchColorAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetPSFColorFromColorWell:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetPSFSingleOpacityAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_constant_opacity((double) self.PSFOpacity, kemo_sgl);

    NSColor *OriginalWellColor = [PSFPatchColorWell color];
    NSColor *NewWellColor = [OriginalWellColor colorWithAlphaComponent:self.PSFOpacity];
    [PSFPatchColorWell setColor:NewWellColor];
    
    [_metalView UpdateImage:kemo_sgl];
};

@end
