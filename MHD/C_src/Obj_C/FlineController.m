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

- (id) CopyFlineDisplayFlagsFromC
{
	int i, iflag;
	double minmax;
	double current_thick;
	int current_digit;
    struct kv_string *colorname = kemoview_alloc_kvstring();
    NSString *stname;
	NSNumber *stnum;
	
	FlineNumberOfField =  kemoview_get_fline_field_param(NUM_FIELD_FLAG);
	FlineTotalComponent = kemoview_get_fline_field_param(NTOT_COMPONENT_FLAG);
	
	kemoview_get_fline_color_w_exp(ISET_WIDTH, &current_thick, &current_digit);
	self.FlineThickFactor = (CGFloat) current_thick;
	self.FlineThickDigit = (CGFloat) current_digit;


	[FlineFieldName removeAllObjects];	
	[FlineNumberOfComponent removeAllObjects];
	[FlineMinimum removeAllObjects];
	[FlineMaximum removeAllObjects];
	for(i = 0; i < FlineNumberOfField; i++){
		kemoview_get_fline_color_data_name(colorname,i);
		stname = [[NSString alloc] initWithUTF8String:colorname->string];
		[FlineFieldName      addObject:stname];
		[stname release];
		
		iflag = kemoview_get_fline_color_num_comps(i);
		stnum = [[NSNumber alloc] initWithInt:iflag];
		[FlineNumberOfComponent addObject:stnum];
		[stnum release];	
	}
    kemoview_free_kvstring(colorname);
    
	for(i = 0; i < FlineTotalComponent; i++){
		minmax = kemoview_get_fline_data_range(ISET_COLOR_MIN, i);
		stnum = [[NSNumber alloc] initWithDouble:minmax];
		[FlineMinimum      addObject:stnum];
		[stnum release];	
		
		minmax = kemoview_get_fline_data_range(ISET_COLOR_MAX, i);
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
	[self SetFlineComponentMenu:0];
	return self;
}

- (void) OpenFieldlineFile:(NSString*) fieldlineFilehead{
	int id_viewtype;
	
	self.FlineWindowlabel = [NSString stringWithFormat:@"Fieldline:%@",
							 [[fieldlineFilehead lastPathComponent] stringByDeletingPathExtension]];

	self.DrawFlineFlag = kemoview_get_fline_parameters(DRAW_SWITCH);
	[self CopyFlineDisplayFlagsFromC];
	//		self.EvolutionStartStep = [[FlineOpenFilehead pathExtension] intValue];
	//		self.EvolutionEndStep =    self.EvolutionStartStep;
	
	id_viewtype = kemoview_get_view_type_flag();
	[_kemoviewControl SetViewTypeMenu:id_viewtype];
	
	
	[_kemoviewControl Set3DView];
	[_kemoviewer UpdateImage];
};

- (void) ReadFlineFile:(NSString *) FlineFileName
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
    int iflag_datatype =  kemoview_open_data(filename);
    kemoview_free_kvstring(filename);
    
    if(iflag_datatype == IFLAG_LINES) [self OpenFieldlineFile:(NSString *)FlineOpenFilehead];
}

- (IBAction) UpdateFieldline:(id)pId{
	[_kemoviewer UpdateImage];
    return;
};

- (IBAction) DrawFlineFile:(id)pId{
	NSArray *flineFileTypes = [NSArray arrayWithObjects:@"inp",@"vtk",@"gz",@"INP",@"VTK",@"GZ",nil];
	NSOpenPanel *flineOpenPanelObj	= [NSOpenPanel openPanel];
	[flineOpenPanelObj setTitle:@"Choose field line data"];
    [flineOpenPanelObj setAllowedFileTypes:flineFileTypes];
    [flineOpenPanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger FlineOpenInteger){
	if(FlineOpenInteger == NSFileHandlingPanelOKButton){
		FlineOpenDirectory = [[flineOpenPanelObj directoryURL] path];
		NSString *FlineOpenFilename =  [[flineOpenPanelObj URL] path];
		// NSLog(@"PSF file directory = %@",FlineOpenDirectory);
        [self ReadFlineFile:FlineOpenFilename];
	};
                                   }];
}

- (IBAction) CloseFlineFile:(id)pId{

	kemoview_close_fieldline_view();
	self.DrawFlineFlag = kemoview_get_fline_parameters(DRAW_SWITCH);
	[self CopyFlineDisplayFlagsFromC];
	
	[_kemoviewer UpdateImage];
	
}

- (IBAction) FlineFieldAction:(id)sender
{	
	int isel, iplotted;
	int i_digit;
	double value;
	isel = [_FlineFieldMenu indexOfSelectedItem];
	[self SetFlineComponentMenu:isel];
	
	kemoview_set_fline_field_param(FIELD_SEL_FLAG, isel);
	
	
	iplotted = kemoview_get_fline_field_param(DRAW_ADDRESS_FLAG);
	self.FlineMinimumValue =   kemoview_get_fline_data_range(ISET_COLOR_MIN, iplotted);
	self.FlineMaximumValue =   kemoview_get_fline_data_range(ISET_COLOR_MAX, iplotted);
	
	kemoview_get_fline_color_w_exp(ISET_COLOR_MIN, &value, &i_digit);
	self.FlineDisplayMinimum =  (CGFloat) value;
	self.FlineDisplayMinDigit = (CGFloat) i_digit;
	kemoview_get_fline_color_w_exp(ISET_COLOR_MAX, &value, &i_digit);
	self.FlineDisplayMaximum =  (CGFloat) value;
	self.FlineDisplayMaxDigit = (CGFloat) i_digit;

	[_kemoviewer UpdateImage];
}

- (IBAction) FlineComponentAction:(id)sender
{	
	int i_digit;
	double value;
	int iplotted;
	
	kemoview_set_fline_field_param(COMPONENT_SEL_FLAG, (int) [_FlineComponentMenu indexOfSelectedItem]);
	
	iplotted = kemoview_get_fline_field_param(DRAW_ADDRESS_FLAG);
	self.FlineMinimumValue =   kemoview_get_fline_data_range(ISET_COLOR_MIN, iplotted);
	self.FlineMaximumValue =   kemoview_get_fline_data_range(ISET_COLOR_MAX, iplotted);

	kemoview_get_fline_color_w_exp(ISET_COLOR_MIN, &value, &i_digit);
	self.FlineDisplayMinimum =  (CGFloat) value;
	self.FlineDisplayMinDigit = (CGFloat) i_digit;
	kemoview_get_fline_color_w_exp(ISET_COLOR_MAX, &value, &i_digit);
	self.FlineDisplayMaximum =  (CGFloat) value;
	self.FlineDisplayMaxDigit = (CGFloat) i_digit;

	[_kemoviewer UpdateImage];
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

- (void) SetFlineComponentMenu:(int)isel{
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

		iplotted = kemoview_get_fline_field_param(DRAW_ADDRESS_FLAG);
		self.FlineMinimumValue =   kemoview_get_fline_data_range(ISET_COLOR_MIN, iplotted);
		self.FlineMaximumValue =   kemoview_get_fline_data_range(ISET_COLOR_MAX, iplotted);

		kemoview_get_fline_color_w_exp(ISET_COLOR_MIN, &value, &i_digit);
		self.FlineDisplayMinimum =  (CGFloat) value;
		self.FlineDisplayMinDigit = (CGFloat) i_digit;
		kemoview_get_fline_color_w_exp(ISET_COLOR_MAX, &value, &i_digit);
		self.FlineDisplayMaximum =  (CGFloat) value;
		self.FlineDisplayMaxDigit = (CGFloat) i_digit;
	}
	return;
}

- (IBAction)ChooseFieldlineColorAction:(id)sender;
{
	NSInteger tag = [[FieldlineColorItem selectedCell] tag];
	kemoview_set_fline_color_param(ISET_COLORMAP, (int) tag);
	
	[_kemoviewer UpdateImage];
}

- (IBAction) ShowFlineRange:(id)pSender {
	kemoview_set_fline_linear_colormap(self.FlineDisplayMinimum, (int) self.FlineDisplayMinDigit,
									   self.FlineDisplayMaximum, (int) self.FlineDisplayMaxDigit);
//	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseFieldlineTypeAction:(id)sender;
{
	self.Flinetype = [[_flinetype_matrix selectedCell] tag];
	kemoview_set_fline_field_param(LINETYPE_FLAG, (int) self.Flinetype);
	
	[_kemoviewer UpdateImage];
}

- (IBAction)SetFieldlineThicknessAction:(id)sender;
{
	kemoview_set_fline_color_w_exp(ISET_WIDTH, (double) self.FlineThickFactor, (int) self.FlineThickDigit);
//	[_kemoviewer UpdateImage];
}

@end
