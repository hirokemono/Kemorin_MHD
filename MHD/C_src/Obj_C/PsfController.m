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
@synthesize PsfWindowlabel;
@synthesize DrawPsfFlag;
@synthesize IsolineNumber;
@synthesize IsolineWidth;
@synthesize IsolineDigit;
@synthesize PSFOpacity;

@synthesize currentPSFID;
@synthesize currentPSFStep;

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

@synthesize PsfMinimumValue;
@synthesize PsfMaximumValue;
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

void SetDataRanges(int id_model, struct kemoviewer_type *kemo_sgl,
                   double *dataMin, double *dataMax, 
                   double *cmapMinValue, int *cmapMinDigit,
                   double *cmapMaxValue, int *cmapMaxDigit)
{
	int iplotted = kemoview_get_VIZ_field_param(kemo_sgl, id_model,
                                                DRAW_ADDRESS_FLAG);
 	*dataMin = kemoview_get_VIZ_data_range(kemo_sgl, id_model,
                                           ISET_COLOR_MIN, iplotted);
	*dataMax = kemoview_get_VIZ_data_range(kemo_sgl, id_model,
                                           ISET_COLOR_MAX, iplotted);
    
    kemoview_get_VIZ_color_w_exp(kemo_sgl, id_model, ISET_COLOR_MIN,
                                 cmapMinValue, cmapMinDigit);
    kemoview_get_VIZ_color_w_exp(kemo_sgl, id_model, ISET_COLOR_MAX,
                                 cmapMaxValue, cmapMaxDigit);
}


- (id)init;
{
    self.PsfWindowlabel = [NSString stringWithFormat:@"PSF View"];
    
	LoadedPsfID =      [[NSMutableArray alloc] init];
	LoadedPsfFileHead =[[NSMutableArray alloc] init];
    
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

- (void) setSelectedPSFComponentRanges:(struct kemoviewer_type *) kemo_sgl
{
    double dataMin, dataMax;
    double cmapMinValue, cmapMaxValue;
    int cmapMinDigit, cmapMaxDigit;
    SetDataRanges(SURFACE_RENDERING, kemo_sgl, &dataMin, &dataMax, 
                  &cmapMinValue, &cmapMinDigit, &cmapMaxValue, &cmapMaxDigit);
    self.PsfMinimumValue = (CGFloat) dataMin;
    self.PsfMaximumValue = (CGFloat) dataMax;
	self.PsfMinimumRange = (CGFloat) cmapMinValue;
	self.PsfMinimumDigit = (CGFloat) cmapMinDigit;
	self.PsfMaximumRange = (CGFloat) cmapMaxValue;
	self.PsfMaximumDigit = (CGFloat) cmapMaxDigit;
}

- (void) UpdateCurrentPsfMenu:(struct kemoviewer_type *) kemo_sgl
{
	double current_value;
	int i_digit;
	
	self.PSFSelectedField = kemoview_get_VIZ_field_param(kemo_sgl,
                                                         SURFACE_RENDERING,
                                                         FIELD_SEL_FLAG);
	self.PSFSelectedComponent = kemoview_get_VIZ_field_param(kemo_sgl,
                                                             SURFACE_RENDERING,
                                                             COMPONENT_SEL_FLAG);
    
	self.PSFSurfaceSwitch 
        =  kemoview_get_VIZ_draw_flags(kemo_sgl, SURFACE_RENDERING);
	self.PSFIsolineSwitch 
        =  kemoview_get_PSF_draw_flags(kemo_sgl, PSFGRID_TOGGLE);
	self.PSFZerolineSwitch
        = kemoview_get_PSF_draw_flags(kemo_sgl, ZEROGRID_TOGGLE);
	self.PSFColorbarSwitch 
        = kemoview_get_colorbar_draw_flag(kemo_sgl, SURFACE_RENDERING);
    
    [self setSelectedPSFComponentRanges:kemo_sgl];
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                 SURFACE_RENDERING, ISET_WIDTH,
                                 &current_value, &i_digit);
	self.IsolineWidth =      (CGFloat) current_value;
	self.IsolineDigit =      (CGFloat) i_digit;
	self.PSFOpacity = kemoview_get_VIZ_opacity_range(kemo_sgl,
                                                     SURFACE_RENDERING,
                                                     ISET_OPACITY_MAX);
	
	self.DrawPSFVectorFlag = kemoview_get_VIZ_vector_draw_flags(kemo_sgl,
                                                                SURFACE_RENDERING);

    kemoview_get_VIZ_vector_w_exp(kemo_sgl,
                                  SURFACE_RENDERING,
                                  ISET_PSF_REFVECT,
                                  &current_value, &i_digit);
	self.ScaleVector =      (CGFloat) current_value;
	self.ScaleDigit =       (CGFloat) i_digit;

    kemoview_get_VIZ_vector_w_exp(kemo_sgl,
                                  SURFACE_RENDERING,
                                  ISET_VECTOR_INC,
                                  &current_value, &i_digit);
	self.PSFVectorIncrement = (CGFloat) current_value;
	self.PSFVectorIncDigit =  (CGFloat) i_digit;
	
    kemoview_get_VIZ_vector_w_exp(kemo_sgl,
                                  SURFACE_RENDERING,
                                  ISET_PSF_V_THICK,
                                  &current_value, &i_digit);
    self.VectorThickness = (CGFloat) current_value;
	self.VectorDigit =     (CGFloat) i_digit;
	
	self.psfPatchDirectionTag
        = kemoview_get_PSF_draw_flags(kemo_sgl, PSF_POLYGON_SWITCH);
	self.psfTangentialVectorTag 
        = kemoview_get_PSF_draw_flags(kemo_sgl, PSFTANVEC_TOGGLE);
	
    self.psfPatchColorTag = kemoview_get_VIZ_patch_color_mode(kemo_sgl,
                                                              SURFACE_RENDERING);
	self.psfLineColorTag =  kemoview_get_PSF_color_param(kemo_sgl, PSFGRID_TOGGLE);

	self.psfVectorColorTag = kemoview_get_PSF_color_param(kemo_sgl, ISET_VECTOR_COLOR);
	
    [self SetFieldMenuItems:SURFACE_RENDERING
                   kemoview:kemo_sgl
                  fieldMenu:_psfFieldMenu];
	[self SetComponentMenuItems:self.PSFSelectedField
                    activeModel:SURFACE_RENDERING
                       kemoview:kemo_sgl
                  componentMenu:_psfComponentMenu];
    [self SetPsfSelectedRange:self.PSFSelectedField
                  activeModel:SURFACE_RENDERING
                     kemoview:kemo_sgl];
	[_psfFieldMenu selectItemAtIndex:self.PSFSelectedField];
	[_psfComponentMenu selectItemAtIndex:self.PSFSelectedComponent];
	
	[_psfPatchDirMatrix selectCellWithTag:self.psfPatchDirectionTag];
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
}

- (void) ResetCurrentPsfParam:(struct kemoviewer_type *) kemo_sgl{
	self.PSFSelectedField =     IZERO;
	self.PSFSelectedComponent = IZERO;
    
    kemoview_set_VIZ_field_param((int) self.PSFSelectedField,
                                 SURFACE_RENDERING,
                                 FIELD_SEL_FLAG,
                                 kemo_sgl);
    kemoview_set_VIZ_field_param((int) self.PSFSelectedComponent,
                                 SURFACE_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 kemo_sgl);
}

- (void) SetCurrentPsfMenu:(struct kemoviewer_type *) kemo_sgl
{
    int i_file_step;
	int i, j, ifmt;
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
            ifmt = kemoview_get_full_path_file_prefix_step(kemo_sgl, SURFACE_RENDERING,
                                                           psf_filehead, &i_file_step);
			PsfNumberTxt = [[NSString alloc] initWithFormat:@"%d: ",i+1];
			PsfFileHeader = [[[NSString alloc] initWithUTF8String:psf_filehead->string] lastPathComponent];
			PsfFileHeader = [PsfNumberTxt stringByAppendingString:PsfFileHeader];
			self.currentPSFStep = i_file_step;
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
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
}

- (void) SetFieldMenuItems:(int) id_model
                  kemoview:(struct kemoviewer_type *) kemo_sgl
                 fieldMenu:(NSPopUpButton *) psfFieldMenu
{
    NSString *stname;
    struct kv_string *colorname = kemoview_alloc_kvstring();

	[psfFieldMenu removeAllItems];
    
    int n_field = kemoview_get_VIZ_field_param(kemo_sgl,
                                               id_model,
                                               NUM_FIELD_FLAG);
    int i;
	if(n_field < 1){
		[psfFieldMenu addItemWithTitle:@"No field"];
	} else {
		for(i = 0; i < n_field; i++){
            kemoview_get_VIZ_field_name(kemo_sgl,
                                        id_model,
                                        colorname, i);
            stname = [[NSString alloc] initWithUTF8String:colorname->string];
			[psfFieldMenu addItemWithTitle:stname];
            [stname release];
		};
	}
    kemoview_free_kvstring(colorname);
}

- (void) SetComponentMenuItems:(NSInteger) isel
                   activeModel:(NSInteger) id_model
                      kemoview:(struct kemoviewer_type *) kemo_sgl
                 componentMenu:(NSPopUpButton *) psfComponentMenu
{
    
    [psfComponentMenu removeAllItems];
    
    int n_field = kemoview_get_VIZ_field_param(kemo_sgl, id_model,
                                               NUM_FIELD_FLAG);
    long num_comp = kemoview_get_VIZ_num_component(kemo_sgl, id_model,
                                                   (int) isel);
    if(n_field < 1){
        [psfComponentMenu addItemWithTitle:@"No data"];
    } else {
        struct kv_string *colorname = kemoview_alloc_kvstring();
        kemoview_get_VIZ_field_name(kemo_sgl, id_model,
                                    colorname, (int) isel);
        NSString *stname = [[NSString alloc] initWithUTF8String:colorname->string];
        
        if(num_comp == 1){
            [psfComponentMenu addItemWithTitle:@"Scalar"];
        } else if(num_comp == 6){
            [psfComponentMenu addItemWithTitle:@"xx"];
            [psfComponentMenu addItemWithTitle:@"xy"];
            [psfComponentMenu addItemWithTitle:@"xz"];
            [psfComponentMenu addItemWithTitle:@"yy"];
            [psfComponentMenu addItemWithTitle:@"yz"];
            [psfComponentMenu addItemWithTitle:@"zz"];
        }
        else if(num_comp == 3){
            NSInteger charalen = [stname length];
            if(charalen > 4){
                NSString *partname = [stname substringFromIndex:charalen-4];
                if([partname compare:@"_sph"] == NSOrderedSame){
                    [psfComponentMenu addItemWithTitle:@"r"];
                    [psfComponentMenu addItemWithTitle:@"θ"];
                    [psfComponentMenu addItemWithTitle:@"φ"];
                }else if([partname compare:@"_cyl"] == NSOrderedSame){
                    [psfComponentMenu addItemWithTitle:@"s"];
                    [psfComponentMenu addItemWithTitle:@"φ"];
                    [psfComponentMenu addItemWithTitle:@"z"];
                }else{
                    [psfComponentMenu addItemWithTitle:@"x"];
                    [psfComponentMenu addItemWithTitle:@"y"];
                    [psfComponentMenu addItemWithTitle:@"z"];
                }
            } else {
                [psfComponentMenu addItemWithTitle:@"x"];
                [psfComponentMenu addItemWithTitle:@"y"];
                [psfComponentMenu addItemWithTitle:@"z"];
            }
        }
        [stname release];
        kemoview_free_kvstring(colorname);
    }
    return;
}

- (void) SetPsfSelectedRange:(NSInteger) isel
                 activeModel:(NSInteger) id_model
                    kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int iplotted;
    int n_field = kemoview_get_VIZ_field_param(kemo_sgl, id_model,
                                               NUM_FIELD_FLAG);
    long num_comp = kemoview_get_VIZ_num_component(kemo_sgl, id_model,
                                                   (int) isel);
    if(n_field > 0){
		if(num_comp == 3){
			self.PSFVectorMenuAcrive = 1;
		} else {
			self.PSFVectorMenuAcrive = 0;
		};
		
        
		iplotted = kemoview_get_VIZ_field_param(kemo_sgl,
                                                SURFACE_RENDERING,
                                                DRAW_ADDRESS_FLAG);
		self.PsfMinimumValue = kemoview_get_VIZ_data_range(kemo_sgl,
                                                           SURFACE_RENDERING,
                                                           ISET_COLOR_MIN,
                                                           iplotted);
		self.PsfMaximumValue = kemoview_get_VIZ_data_range(kemo_sgl,
                                                           SURFACE_RENDERING,
                                                           ISET_COLOR_MAX,
                                                           iplotted);
	}
	return;
}

- (void) SetPsfRanges:(struct kemoviewer_type *) kemo_sgl
{
    [self setSelectedPSFComponentRanges:kemo_sgl];
    
	self.IsolineNumber =     kemoview_get_PSF_color_param(kemo_sgl, ISET_NLINE);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
    
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
}


- (void) DrawPsfFile:(NSString*) PsfOpenFilehead
            kemoview:(struct kemoviewer_type *) kemo_sgl
{
    [_controlTabView selectTabViewItemAtIndex:SURFACE_RENDERING];
	int id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
    
	self.currentPSFStep = [[PsfOpenFilehead pathExtension] intValue];
	self.PsfWindowlabel = [NSString stringWithFormat:@"PSF:%@",
						   [[PsfOpenFilehead stringByDeletingPathExtension] lastPathComponent]];
	[_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
	
	self.DrawPsfFlag = kemoview_get_PSF_loaded_params(kemo_sgl, DRAW_SWITCH);
    
    [self SetFieldMenuItems:SURFACE_RENDERING
                   kemoview:kemo_sgl
                  fieldMenu:_psfFieldMenu];
    [self SetComponentMenuItems:0
                    activeModel:SURFACE_RENDERING
                       kemoview:kemo_sgl
                  componentMenu:_psfComponentMenu];
    [self SetPsfSelectedRange:0
                  activeModel:SURFACE_RENDERING
                     kemoview:kemo_sgl];
    [self SetCurrentPsfMenu:kemo_sgl];
    [self SetPsfRanges:kemo_sgl];
	
    [_kemoviewControl Set3DView:kemo_sgl];
	
	int num_loaded =  kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);
	int nlimit_load = kemoview_get_PSF_maximum_load(kemo_sgl);
	if(num_loaded >= nlimit_load-1){
		self.psfMoreOpenFlag = 1;
	}else {
		self.psfMoreOpenFlag = 0;
	}
    [_kemoviewControl TimeLabelAvaiability];
    [_kemoviewControl FileStepLabelAvaiability];
    
    self.currentPSFStep = [_kemoviewControl SetCurrentPSFFile:SURFACE_RENDERING
                                                     kemoview:kemo_sgl
                                                     pathTree:_psfPathControl];
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

- (void) ChooseTextureFile:(struct kemoviewer_type *) kemo_sgl
{
    
    NSArray *psfFileTypes = [NSArray arrayWithObjects:@"png",@"bmp",@"PNG",@"BMP",nil];
    NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
    [PsfOpenPanelObj setTitle:@"Choose Texture image data"];
    [PsfOpenPanelObj setAllowedFileTypes:psfFileTypes];
    NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
    
    if(PsfOpenInteger == NSModalResponseOK){
//        NSString *PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
        NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
        [self ReadTextureFile:PsfOpenFilename
                     kemoview:kemo_sgl];
    };	
    return;
}

- (int) PSFColorbarSwitchStatus
{
    return (int) self.PSFColorbarSwitch;
}
- (void) setPSFColorbarSwitchStatus:(int) isel
{
    self.PSFColorbarSwitch = isel;
}



- (IBAction) OpenPsfFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
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
            // NSString *PsfOpenDirectory = [[PsfOpenPanelObj directoryURL] path];
            // NSLog(@"PSF file directory = %@",PsfOpenDirectory);
            NSString *PsfOpenFilename =  [[PsfOpenPanelObj URL] path];
            NSString *PsfOpenFileExtension = [PsfOpenFilename pathExtension];
            NSString *PsfOpenFileprefix =    [PsfOpenFilename stringByDeletingPathExtension];
            if([PsfOpenFileExtension isEqualToString:@"gz"]
               || [PsfOpenFileExtension isEqualToString:@"GZ"]){
                // NSString *PsfOpenFileext_gz = [self.PsfOpenFilename pathExtension];
                // PsfOpenFileext =    [PsfOpenFileprefix pathExtension];
                PsfOpenFileprefix =   [PsfOpenFileprefix stringByDeletingPathExtension];
            };
            // NSLog(@"PSF file name =      %@",PsfOpenFilename);
            // NSLog(@"PSF file header =    %@",PsfOpenFileprefix);
            // NSLog(@"self.PsfWindowlabel =    %@",self.PsfWindowlabel);
            
            struct kv_string *filename = kemoview_init_kvstring_by_string([PsfOpenFilename UTF8String]);
            int iflag_datatype = kemoview_open_data(filename, kemo_sgl);
            kemoview_free_kvstring(filename);
        
            if(iflag_datatype == IFLAG_SURFACES){
                [self DrawPsfFile:PsfOpenFileprefix
                         kemoview:kemo_sgl];
            };
        };	
    }];
    [_kemoviewControl TimeLabelAvaiability];
    [_kemoviewControl FileStepLabelAvaiability];
    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    [_metalView UpdateImage:kemo_sgl];
}


- (IBAction) ClosePsfFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self ResetCurrentPsfParam:kemo_sgl];
    int num_loaded = kemoview_close_PSF_view(kemo_sgl);
    self.DrawPsfFlag = kemoview_get_PSF_loaded_params(kemo_sgl, DRAW_SWITCH);

    NSInteger WindowExpandFlag = kemoview_check_all_VIZ_draw_flags(kemo_sgl);
    [_ElasticControl UpdateWindow:WindowExpandFlag];
    
	if(num_loaded > 0){
        [self SetCurrentPsfMenu:kemo_sgl];
    };
    [_kemoviewControl Set3DView:kemo_sgl];
    [_kemoviewControl TimeLabelAvaiability];
    [_kemoviewControl FileStepLabelAvaiability];
	[_metalView UpdateImage:kemo_sgl];
};

- (IBAction) CurrentPsfAction:(id)sender
{	
	int id_current = [[LoadedPsfID objectAtIndex:self.currentPSFID] intValue];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_loaded_params(SET_CURRENT, id_current, kemo_sgl);
    self.currentPSFStep = [_kemoviewControl SetCurrentPSFFile:SURFACE_RENDERING
                                                     kemoview:kemo_sgl
                                                     pathTree:_psfPathControl];
    
    [self UpdateCurrentPsfMenu:kemo_sgl];
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) PsfFieldAction:(id)sender
{	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	[self SetComponentMenuItems:self.PSFSelectedField
                    activeModel:SURFACE_RENDERING
                       kemoview:kemo_sgl
                  componentMenu:_psfComponentMenu];
    [self SetPsfSelectedRange:self.PSFSelectedField
                  activeModel:SURFACE_RENDERING
                     kemoview:kemo_sgl];
    kemoview_set_VIZ_field_param((int) self.PSFSelectedField,
                                 SURFACE_RENDERING,
                                 FIELD_SEL_FLAG,
                                 kemo_sgl);
	
    [self SetPsfRanges:kemo_sgl];
    
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) PsfComponentAction:(id)sender
{	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_field_param((int) self.PSFSelectedComponent,
                                 SURFACE_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 kemo_sgl);
	
    [self SetPsfRanges:kemo_sgl];
    
    [_rgbaMapObject UpdateColormapView:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}


- (IBAction)PsfSurfSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_draw_flag(SURFACE_RENDERING,
                               (int) self.PSFSurfaceSwitch,
                               kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)PsfLineSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags((int) self.PSFIsolineSwitch,
                                PSFGRID_TOGGLE, kemo_sgl);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
    [self UpdateCurrentPsfMenu:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)PsfZeroLineSwitchAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_PSF_draw_flags((int) self.PSFZerolineSwitch,
                                ZEROGRID_TOGGLE, kemo_sgl);
	self.PSFLineSwitch = self.PSFZerolineSwitch + self.PSFIsolineSwitch;
    [self UpdateCurrentPsfMenu:kemo_sgl];
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
        [self SetPSFColorFromColorWell:kemo_sgl
                             colorwell:PSFPatchColorWell];
    };
    kemoview_set_PSF_patch_color_mode((int) self.psfPatchColorTag,
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
	} else if (self.psfTangentialVectorTag == 1) {
		kemoview_set_PSF_tangential_vec_mode(TANGENTIAL_COMPONENT, kemo_sgl);
	}
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) SetPSFDisplayrange:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_linear_colormap(self.PsfMinimumRange, (int) self.PsfMinimumDigit,
                                 self.PsfMaximumRange, (int) self.PsfMaximumDigit,
                                 SURFACE_RENDERING, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowIsolineNumber:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_color_param(ISET_NLINE,
                                 (int) self.IsolineNumber,
                                 kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) SetIsolineWidth:(id)pSender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_color_value_w_exp(SURFACE_RENDERING, ISET_WIDTH,
                                       (double) self.IsolineWidth,
                                       (int) self.IsolineDigit,
                                       kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)DrawPSFVectorAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_vector_draw_flags((int) self.DrawPSFVectorFlag,
                                       SURFACE_RENDERING, kemo_sgl);
	
	if(self.DrawPSFVectorFlag == 0) {[_PSFVectorSwitchOutlet setTitle:@"Off"];}
	else{ [_PSFVectorSwitchOutlet setTitle:@"On"];};
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetReferenceVector:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_each_VIZ_vector_w_exp(ISET_PSF_REFVECT,
                                      (double) self.ScaleVector,
                                      (int) self.ScaleDigit,
                                      SURFACE_RENDERING, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetVectorIncrement:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_each_VIZ_vector_w_exp(ISET_VECTOR_INC,
                                      (double) self.PSFVectorIncrement,
                                      (int) self.PSFVectorIncDigit,
                                      SURFACE_RENDERING, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetVectorThickness:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_each_VIZ_vector_w_exp(ISET_PSF_V_THICK,
                                      (double) self.VectorThickness,
                                      (int) self.VectorDigit,
                                      SURFACE_RENDERING, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}
    
- (IBAction)ChoosePsfPatchDirection:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	self.psfPatchDirectionTag = [[_psfPatchDirMatrix selectedCell] tag];
	kemoview_set_PSF_polygon_mode((int) self.psfPatchDirectionTag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (void)SetPSFColorFromColorWell:(struct kemoviewer_type *) kemo_sgl
                       colorwell:(NSColorWell *) viewColorWell
{
    CGFloat redBG, greenBG, blueBG, opacityBG;
    double rgba[4];
    
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    
    NSColor *nsPSFPatchColor = [viewColorWell color];
    [nsPSFPatchColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
    
    rgba[0] = (double) redBG;
    rgba[1] = (double) greenBG;
    rgba[2] = (double) blueBG;
    rgba[3] = (double) opacityBG;
    self.PSFOpacity = opacityBG;
    
    kemoview_set_VIZ_single_color(rgba, id_model, kemo_sgl);
}
- (void)SetPSFSingleOpacity:(struct kemoviewer_type *) kemo_sgl
                    opacity:(CGFloat) const_opacity
                  colorwell:(NSColorWell *) viewColorWell
{
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    kemoview_set_constant_opacity((double) const_opacity,
                                  id_model, kemo_sgl);

    NSColor *OriginalWellColor = [PSFPatchColorWell color];
    NSColor *NewWellColor = [OriginalWellColor colorWithAlphaComponent:const_opacity];
    [viewColorWell setColor:NewWellColor];
};

- (IBAction)SetPSFPatchColorAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetPSFColorFromColorWell:kemo_sgl
                         colorwell:PSFPatchColorWell];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetPSFSingleOpacityAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetPSFSingleOpacity:kemo_sgl
                      opacity:self.PSFOpacity
                    colorwell:PSFPatchColorWell];
    [_metalView UpdateImage:kemo_sgl];
};

@end
