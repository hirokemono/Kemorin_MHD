//
//  TracerController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "TracerController.h"
#include "kemoviewer.h"


@implementation TracerController

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
    self.Flinetype = kemoview_get_line_type_flag(kemo_sgl);
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
	
	FlineNumberOfField =  kemoview_get_VIZ_field_param(kemo_sgl,
                                                       FIELDLINE_RENDERING,
                                                       NUM_FIELD_FLAG);
	FlineTotalComponent = kemoview_get_VIZ_field_param(kemo_sgl,
                                                       FIELDLINE_RENDERING,
                                                       NTOT_COMPONENT_FLAG);
	
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                 FIELDLINE_RENDERING, ISET_WIDTH,
                                 &current_thick, &current_digit);
	self.FlineThickFactor = (CGFloat) current_thick;
	self.FlineThickDigit = (CGFloat) current_digit;


	[FlineFieldName removeAllObjects];	
	[FlineNumberOfComponent removeAllObjects];
	for(i = 0; i < FlineNumberOfField; i++){
        kemoview_get_VIZ_field_name(kemo_sgl,
                                    FIELDLINE_RENDERING,
                                    colorname,i);
		stname = [[NSString alloc] initWithUTF8String:colorname->string];
		[FlineFieldName      addObject:stname];
		[stname release];
		
		iflag = kemoview_get_VIZ_num_component(kemo_sgl, FIELDLINE_RENDERING, i);
		stnum = [[NSNumber alloc] initWithInt:iflag];
		[FlineNumberOfComponent addObject:stnum];
		[stnum release];	
	}
    kemoview_free_kvstring(colorname);
    
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

	self.DrawFlineFlag = kemoview_get_VIZ_draw_flags(kemo_sgl, FIELDLINE_RENDERING);
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

- (IBAction) FlineFieldAction:(id)sender
{	
	int i_digit;
	double value;
	NSInteger isel = [_FlineFieldMenu indexOfSelectedItem];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetFlineComponentMenu:isel
                       kemoview:kemo_sgl];
    
    kemoview_set_VIZ_field_param(FIELDLINE_RENDERING,
                                 FIELD_SEL_FLAG,
                                 (int) isel, kemo_sgl);
	
    int iplotted = kemoview_get_VIZ_field_param(kemo_sgl,
                                                FIELDLINE_RENDERING,
                                                DRAW_ADDRESS_FLAG);
	self.FlineMinimumValue
        = kemoview_get_VIZ_data_range(kemo_sgl,
                                      FIELDLINE_RENDERING,
                                      ISET_COLOR_MIN, iplotted);
	self.FlineMaximumValue
        = kemoview_get_VIZ_data_range(kemo_sgl,
                                      FIELDLINE_RENDERING,
                                      ISET_COLOR_MAX,
                                      iplotted);
	
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                   FIELDLINE_RENDERING, ISET_COLOR_MIN,
                                   &value, &i_digit);
	self.FlineDisplayMinimum =  (CGFloat) value;
	self.FlineDisplayMinDigit = (CGFloat) i_digit;
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                   FIELDLINE_RENDERING, ISET_COLOR_MAX,
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
    kemoview_set_VIZ_field_param(FIELDLINE_RENDERING,
                                 COMPONENT_SEL_FLAG,
                                 (int) [_FlineComponentMenu indexOfSelectedItem],
                                 kemo_sgl);
	
	int iplotted = kemoview_get_VIZ_field_param(kemo_sgl,
                                                FIELDLINE_RENDERING,
                                                DRAW_ADDRESS_FLAG);
	self.FlineMinimumValue 
        = kemoview_get_VIZ_data_range(kemo_sgl,
                                      FIELDLINE_RENDERING,
                                      ISET_COLOR_MIN,
                                      iplotted);
	self.FlineMaximumValue 
        = kemoview_get_VIZ_data_range(kemo_sgl,
                                      FIELDLINE_RENDERING,
                                      ISET_COLOR_MAX,
                                      iplotted);

    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                   FIELDLINE_RENDERING, ISET_COLOR_MIN,
                                   &value, &i_digit);
	self.FlineDisplayMinimum =  (CGFloat) value;
	self.FlineDisplayMinDigit = (CGFloat) i_digit;
    kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                   FIELDLINE_RENDERING, ISET_COLOR_MAX,
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

		iplotted = kemoview_get_VIZ_field_param(kemo_sgl,
                                                FIELDLINE_RENDERING,
                                                DRAW_ADDRESS_FLAG);
		self.FlineMinimumValue
            = kemoview_get_VIZ_data_range(kemo_sgl,
                                          FIELDLINE_RENDERING,
                                          ISET_COLOR_MIN,
                                          iplotted);
		self.FlineMaximumValue
            = kemoview_get_VIZ_data_range(kemo_sgl,
                                          FIELDLINE_RENDERING,
                                          ISET_COLOR_MAX,
                                          iplotted);

        kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                     FIELDLINE_RENDERING,
                                     ISET_COLOR_MIN,
                                     &value, &i_digit);
		self.FlineDisplayMinimum =  (CGFloat) value;
		self.FlineDisplayMinDigit = (CGFloat) i_digit;
        kemoview_get_VIZ_color_w_exp(kemo_sgl,
                                     FIELDLINE_RENDERING,
                                     ISET_COLOR_MAX,
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
    kemoview_set_VIZ_patch_color_mode(FIELDLINE_RENDERING,
                                      (int) tag, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) ShowFlineRange:(id)pSender {
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_linear_colormap(self.FlineDisplayMinimum,
                                 (int) self.FlineDisplayMinDigit,
                                 self.FlineDisplayMaximum,
                                 (int) self.FlineDisplayMaxDigit,
                                 FIELDLINE_RENDERING,
                                kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseFieldlineTypeAction:(id)sender;
{
	self.Flinetype = [[_flinetype_matrix selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_line_type_flag((int) self.Flinetype, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetFieldlineThicknessAction:(id)sender;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_color_value_w_exp(FIELDLINE_RENDERING, ISET_WIDTH,
                                       (double) self.FlineThickFactor,
                                       (int) self.FlineThickDigit,
                                       kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

@end
