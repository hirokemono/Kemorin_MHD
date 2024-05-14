//
//  FlineController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "FlineController.h"
#include "kemoviewer.h"


@implementation FlineController

@synthesize FlineWindowlabel;

@synthesize DrawFlineFlag;
@synthesize FlineMinimumValue;
@synthesize FlineMaximumValue;
@synthesize FlineDisplayMinimum;
@synthesize FlineDisplayMaximum;
@synthesize FlineDisplayMinDigit;
@synthesize FlineDisplayMaxDigit;
@synthesize Flinetype;
@synthesize FlineThickFactor;
@synthesize FlineThickDigit;
@synthesize FlineNumCorners;

- (id)init;
{
	self.FlineThickFactor = 1;
	self.FlineThickDigit = -2;
	self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline View"];
	
	FlineNumberOfComponent =[[NSMutableArray alloc] init];
	FlineFieldName =        [[NSMutableArray alloc] init];
	FlineMinimum =          [[NSMutableArray alloc] init];
	FlineMaximum =          [[NSMutableArray alloc] init];
	
	FieldlineFlag =  [NSNumber alloc];
	
	FlineDrawFieldId =     [NSNumber alloc];
	FlineDrawComponentId = [NSNumber alloc];
	
	FieldlineColor =      [NSNumber alloc];
	
	FlineMinimumRange = [NSNumber alloc];
	FlineMaximumRange = [NSNumber alloc];
	
	return self;
}

- (id)dealloc
{
	
	[FlineNumberOfComponent dealloc];
	[FlineFieldName         dealloc];
	[FlineMinimum           dealloc];
	[FlineMaximum           dealloc];
	
	[FieldlineFlag    dealloc];
	
	[FlineDrawFieldId      dealloc];
	[FlineDrawComponentId  dealloc];
	
	[FieldlineColor      dealloc];

	[FlineMinimumRange dealloc];
	[FlineMaximumRange dealloc];
	
    [super dealloc];
	return self;
}

-(void) awakeFromNib
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    self.Flinetype =       kemoview_get_fline_field_param(kemo_sgl, LINETYPE_FLAG);
    self.FlineNumCorners = kemoview_get_fline_field_param(kemo_sgl, NUM_TUBE_CORNERS_FLAG);
    return;
}

- (id) CopyFlineDisplayFlagsFromC:(struct kemoviewer_type *) kemo_sgl
{
	int i, iflag;
	double minmax;
	double current_thick;
	int current_digit;
    struct kv_string *colorname = kemoview_alloc_kvstring();
    NSString *stname;
	NSNumber *stnum;
	
	FlineNumberOfField 
        =  kemoview_get_fline_field_param(kemo_sgl, NUM_FIELD_FLAG);
	FlineTotalComponent 
        = kemoview_get_fline_field_param(kemo_sgl, NTOT_COMPONENT_FLAG);
	
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_WIDTH, 
                                   &current_thick, &current_digit);
	self.FlineThickFactor = (CGFloat) current_thick;
	self.FlineThickDigit = (CGFloat) current_digit;


	[FlineFieldName removeAllObjects];	
	[FlineNumberOfComponent removeAllObjects];
	[FlineMinimum removeAllObjects];
	[FlineMaximum removeAllObjects];
	for(i = 0; i < FlineNumberOfField; i++){
		kemoview_get_fline_color_data_name(kemo_sgl, colorname,i);
		stname = [[NSString alloc] initWithUTF8String:colorname->string];
		[FlineFieldName      addObject:stname];
		[stname release];
		
		iflag = kemoview_get_fline_color_num_comps(kemo_sgl, i);
		stnum = [[NSNumber alloc] initWithInt:iflag];
		[FlineNumberOfComponent addObject:stnum];
		[stnum release];	
	}
    kemoview_free_kvstring(colorname);
    
	for(i = 0; i < FlineTotalComponent; i++){
		minmax = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MIN, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[FlineMinimum      addObject:stnum];
		[stnum release];	
		
		minmax = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MAX, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[FlineMaximum      addObject:stnum];
		[stnum release];	
	}

	for(i = 0; i < FlineNumberOfField; i++){
		NSLog(@"FlineNumberOfComponent = %d %@\n",i, [FlineNumberOfComponent objectAtIndex: i]);
		NSLog(@"FlineFieldName = %d %@\n",i, [FlineFieldName objectAtIndex: i]);
	}
	/*
	NSLog(@"FlineTotalComponent = %d\n",FlineTotalComponent);
	for(i = 0; i < FlineTotalComponent; i++){
		NSLog(@"FlineMinimum = %d %@\n",i, [FlineMinimum objectAtIndex: i]);
		NSLog(@"FlineMaximum = %d %@\n",i, [FlineMaximum objectAtIndex: i]);
	}
	*/

	[self SetFlineFieldMenu];
	[self SetFlineComponentMenu:0
                       kemoview:kemo_sgl];
	return self;
}

- (void) OpenFieldlineFile:(NSString*) fieldlineFilehead
                  kemoview:(struct kemoviewer_type *) kemo_sgl
{
	int id_viewtype;
	
	self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline:%@",
							 [[fieldlineFilehead lastPathComponent] stringByDeletingPathExtension]];

	self.DrawFlineFlag = kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH);
    [self CopyFlineDisplayFlagsFromC:kemo_sgl];
	//		self.EvolutionStartStep = [[FlineOpenFilehead pathExtension] intValue];
	//		self.EvolutionEndStep =    self.EvolutionStartStep;
	
	id_viewtype = kemoview_get_view_type_flag(kemo_sgl);
	[_kemoviewControl SetViewTypeMenu:id_viewtype
                             kemoview:kemo_sgl];
    [_kemoviewControl Set3DView:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
};

- (void) ReadFlineFile:(NSString *) FlineFileName
              kemoview:(struct kemoviewer_type *) kemo_sgl
{
    FlineOpenFileext =   [FlineFileName pathExtension];
    FlineOpenFilehead =  [FlineFileName stringByDeletingPathExtension];
    // NSLog(@"PSF file name =      %@",FlineFileName);
    // NSLog(@"PSF file header =    %@",FlineOpenFilehead);
    // NSLog(@"self.FlineWindowlabel = %@",self.FlineWindowlabel);
    
    if([FlineOpenFileext isEqualToString:@"gz"] || [FlineOpenFileext isEqualToString:@"GZ"]){
        FlineOpenFileext =    [FlineOpenFilehead pathExtension];
        FlineOpenFilehead =   [FlineOpenFilehead stringByDeletingPathExtension];
    };
    
    struct kv_string *filename = kemoview_init_kvstring_by_string([FlineFileName UTF8String]);
    int iflag_datatype =  kemoview_open_data(filename, kemo_sgl);
    kemoview_free_kvstring(filename);
    
    if(iflag_datatype == IFLAG_LINES) [self OpenFieldlineFile:(NSString *)FlineOpenFilehead
                                                     kemoview:kemo_sgl];
}

- (IBAction) UpdateFieldline:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	[_metalView UpdateImage:kemo_sgl];
    return;
};

- (IBAction) DrawFlineFile:(id)pId{
	NSArray *flineFileTypes = [NSArray arrayWithObjects:@"inp",@"vtk",@"gz",@"INP",@"VTK",@"GZ",nil];
	NSOpenPanel *flineOpenPanelObj	= [NSOpenPanel openPanel];
	[flineOpenPanelObj setTitle:@"Choose field line data"];
    [flineOpenPanelObj setAllowedFileTypes:flineFileTypes];
    [flineOpenPanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger FlineOpenInteger){
	if(FlineOpenInteger == NSModalResponseOK){
		FlineOpenDirectory = [[flineOpenPanelObj directoryURL] path];
		NSString *FlineOpenFilename =  [[flineOpenPanelObj URL] path];
		// NSLog(@"PSF file directory = %@",FlineOpenDirectory);
        struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
        [self ReadFlineFile:FlineOpenFilename
                   kemoview:kemo_sgl];
	};
                                   }];
}

- (IBAction) CloseFlineFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_close_fieldline_view(kemo_sgl);
	self.DrawFlineFlag = kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH);
    [self CopyFlineDisplayFlagsFromC:kemo_sgl];
	
	[_metalView UpdateImage:kemo_sgl];
	
}

- (IBAction) FlineFieldAction:(id)sender
{	
	int i_digit;
	double value;
	NSInteger isel = [_FlineFieldMenu indexOfSelectedItem];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetFlineComponentMenu:isel
                       kemoview:kemo_sgl];
    
	kemoview_set_fline_field_param(FIELD_SEL_FLAG, (int) isel,
                                   kemo_sgl);
	
    int iplotted
        = kemoview_get_fline_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
	self.FlineMinimumValue
        = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MIN, iplotted);
	self.FlineMaximumValue
        = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MAX, iplotted);
	
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &value, &i_digit);
	self.FlineDisplayMinimum =  (CGFloat) value;
	self.FlineDisplayMinDigit = (CGFloat) i_digit;
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                   &value, &i_digit);
	self.FlineDisplayMaximum =  (CGFloat) value;
	self.FlineDisplayMaxDigit = (CGFloat) i_digit;

	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) FlineComponentAction:(id)sender
{	
	int i_digit;
	double value;
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_fline_field_param(COMPONENT_SEL_FLAG, 
                                   (int) [_FlineComponentMenu indexOfSelectedItem],
                                   kemo_sgl);
	
	int iplotted
        = kemoview_get_fline_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
	self.FlineMinimumValue 
        = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MIN, iplotted);
	self.FlineMaximumValue 
        = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MAX, iplotted);

	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &value, &i_digit);
	self.FlineDisplayMinimum =  (CGFloat) value;
	self.FlineDisplayMinDigit = (CGFloat) i_digit;
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX, 
                                   &value, &i_digit);
	self.FlineDisplayMaximum =  (CGFloat) value;
	self.FlineDisplayMaxDigit = (CGFloat) i_digit;

	[_metalView UpdateImage:kemo_sgl];
}

- (void) SetFlineFieldMenu{
	int i;
	
	if(FlineNumberOfField > 0) [_FlineFieldMenu removeAllItems];
	if(FlineNumberOfField < 1){
		[_FlineFieldMenu addItemWithTitle:@"No Field"];
	} else {
		for(i = 0; i < FlineNumberOfField; i++){
			[_FlineFieldMenu addItemWithTitle:[FlineFieldName objectAtIndex: i]];
		};
	}
}

- (void) SetFlineComponentMenu:(NSInteger)isel
                      kemoview:(struct kemoviewer_type *) kemo_sgl
{
	int i_digit;
	double value;
	int iplotted;

	[_FlineComponentMenu removeAllItems];
	// NSLog ([NSString stringWithFormat:@"component %@\n", [FlineNumberOfComponent objectAtIndex:isel]]);	
	
	if (FlineNumberOfField < 1) {
		[_FlineComponentMenu addItemWithTitle:@"No Field"];
	} else {
		if([[FlineNumberOfComponent objectAtIndex:isel] intValue] == 1){
			[_FlineComponentMenu addItemWithTitle:@"Scalar"];
		}
		else if([[FlineNumberOfComponent objectAtIndex:isel] intValue] == 6){
			[_FlineComponentMenu addItemWithTitle:@"xx"];
			[_FlineComponentMenu addItemWithTitle:@"xy"];
			[_FlineComponentMenu addItemWithTitle:@"xz"];
			[_FlineComponentMenu addItemWithTitle:@"yy"];
			[_FlineComponentMenu addItemWithTitle:@"yz"];
			[_FlineComponentMenu addItemWithTitle:@"zz"];
		}
		else if([[FlineNumberOfComponent objectAtIndex:isel] intValue] == 3){
			NSInteger charalen = [[FlineFieldName objectAtIndex:isel] length];
			if(charalen > 4){
				NSString *stname = [[FlineFieldName objectAtIndex:isel] substringFromIndex:charalen-4];
				// NSLog ([NSString stringWithFormat:@"end is %@\n",stname ]);
				if([stname compare:@"_sph"] == NSOrderedSame){
					[_FlineComponentMenu addItemWithTitle:@"r"];
					[_FlineComponentMenu addItemWithTitle:@"θ"];
					[_FlineComponentMenu addItemWithTitle:@"φ"];
				} else if([stname compare:@"_cyl"] == NSOrderedSame){
					[_FlineComponentMenu addItemWithTitle:@"s"];
					[_FlineComponentMenu addItemWithTitle:@"φ"];
					[_FlineComponentMenu addItemWithTitle:@"z"];
				} else{
					[_FlineComponentMenu addItemWithTitle:@"x"];
					[_FlineComponentMenu addItemWithTitle:@"y"];
					[_FlineComponentMenu addItemWithTitle:@"z"];
				}
			} else {
				[_FlineComponentMenu addItemWithTitle:@"x"];
				[_FlineComponentMenu addItemWithTitle:@"y"];
				[_FlineComponentMenu addItemWithTitle:@"z"];
			}
		}

		iplotted
            = kemoview_get_fline_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
		self.FlineMinimumValue
            = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MIN, iplotted);
		self.FlineMaximumValue
            = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MAX, iplotted);

		kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                       &value, &i_digit);
		self.FlineDisplayMinimum =  (CGFloat) value;
		self.FlineDisplayMinDigit = (CGFloat) i_digit;
		kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                       &value, &i_digit);
		self.FlineDisplayMaximum =  (CGFloat) value;
		self.FlineDisplayMaxDigit = (CGFloat) i_digit;
	}
	return;
}

- (IBAction)ChooseFieldlineColorAction:(id)sender;
{
	NSInteger tag = [[FieldlineColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_fline_color_param(ISET_COLORMAP, (int) tag, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowFlineRange:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_fline_linear_colormap(self.FlineDisplayMinimum, 
                                       (int) self.FlineDisplayMinDigit,
									   self.FlineDisplayMaximum, 
                                       (int) self.FlineDisplayMaxDigit,
                                       kemo_sgl);
//	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseFieldlineTypeAction:(id)sender;
{
	self.Flinetype = [[_flinetype_matrix selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_fline_field_param(LINETYPE_FLAG,
                                   (int) self.Flinetype,
                                   kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFieldlineThicknessAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_fline_color_w_exp(ISET_WIDTH, (double) self.FlineThickFactor,
                                   (int) self.FlineThickDigit, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFieldTubeCornersAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_fline_field_param(NUM_TUBE_CORNERS_FLAG,
                                   (int) self.FlineNumCorners,
                                   kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

@end
