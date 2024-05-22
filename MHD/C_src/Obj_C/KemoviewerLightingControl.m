//
//  KemoviewerLightingControl.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2019/08/23.
//

#import "KemoviewerLightingControl.h"

@implementation LightTableController
@synthesize radialLightPosition;
@synthesize elevationLightPosition;
@synthesize azimuthLightPosition;
@synthesize idlightTableView;

@synthesize numLightTable;

@synthesize ambientMaterial;
@synthesize diffuseMaterial;
@synthesize specularMaterial;
@synthesize shinessMaterial;

@synthesize radialSliderValue;
@synthesize elevationSliderValue;
@synthesize azimuthSliderValue;
@synthesize lightCheckFlag;
- (void)awakeFromNib{
    int i;
    float r, t, p;
    
	self.radialLightPosition = [[NSMutableArray alloc]init];
	self.elevationLightPosition = [[NSMutableArray alloc]init];
	self.azimuthLightPosition = [[NSMutableArray alloc]init];
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
    
	self.numLightTable =  [[defaults stringForKey:@"numberOfLights"] intValue];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    self.lightCheckFlag = kemoview_get_view_integer(LIGHTING_CHECK, kemo_sgl);
;
    kemoview_init_phong_light_list(kemo_sgl);
    
    if(self.numLightTable <= 0){
        
        self.numLightTable = kemoview_get_num_light_position(kemo_sgl);

        self.ambientMaterial =  kemoview_get_material_parameter(kemo_sgl, AMBIENT_FLAG);
        self.diffuseMaterial =  kemoview_get_material_parameter(kemo_sgl, DIFFUSE_FLAG);
        self.specularMaterial = kemoview_get_material_parameter(kemo_sgl, SPECULAR_FLAG);
        self.shinessMaterial =  kemoview_get_material_parameter(kemo_sgl, SHINENESS_FLAG);
        
        for(i=0;i<self.numLightTable;i++){
            kemoview_get_each_light_rtp(kemo_sgl, i, &r, &t, &p);
            
            [self.radialLightPosition addObject:[NSNumber numberWithFloat:r]];
            [self.elevationLightPosition addObject:[NSNumber numberWithFloat:t]];
            [self.azimuthLightPosition addObject:[NSNumber numberWithFloat:p]];
        };
    } else {
        self.radialLightPosition =    [[defaults arrayForKey:@"lightsRadialPosition"] mutableCopy];
        self.elevationLightPosition = [[defaults arrayForKey:@"lightsElevationPosition"] mutableCopy];
        self.azimuthLightPosition =   [[defaults arrayForKey:@"lightsAzimuthalPosition"] mutableCopy];

        self.ambientMaterial =  [[defaults stringForKey:@"materialAmbient"] floatValue];
        self.diffuseMaterial =  [[defaults stringForKey:@"materialDiffuse"] floatValue];
        self.specularMaterial = [[defaults stringForKey:@"materialSpecular"] floatValue];
        self.shinessMaterial =  [[defaults stringForKey:@"materialShineness"] floatValue];

        kemoview_alloc_phong_light_list((int) self.numLightTable, kemo_sgl);
        kemoview_set_material_parameter(AMBIENT_FLAG, self.ambientMaterial, kemo_sgl);
        kemoview_set_material_parameter(DIFFUSE_FLAG, self.diffuseMaterial, kemo_sgl);
        kemoview_set_material_parameter(SPECULAR_FLAG, self.specularMaterial, kemo_sgl);
        kemoview_set_material_parameter(SHINENESS_FLAG, self.shinessMaterial, kemo_sgl);
    };
	
	[self InitLightTable];
	return;
};


- (IBAction)addAtSelectedRow:(id)pId{
	float r2, t2, p2;
	int isel = (int) [idlightTableView selectedRow];
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if ([idlightTableView selectedRow] >= 0) {
		r2 = [[self.radialLightPosition    objectAtIndex:isel  ] floatValue];
		t2 = [[self.elevationLightPosition objectAtIndex:isel  ] floatValue];
 		p2 = [[self.azimuthLightPosition   objectAtIndex:isel  ] floatValue];
        
		kemoview_add_phong_light_list(isel, r2, t2, p2, kemo_sgl);
        [self SetLightTable:kemo_sgl];
	}
    [_metalView UpdateImage:kemo_sgl];
	return;
};



- (IBAction)deleteSelectedRow:(id)pId{
	int i, n;
	
	NSIndexSet *SelectedList = [idlightTableView selectedRowIndexes];
	n = (int) [self.radialLightPosition count];
	if(n < 1) return;
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if ([idlightTableView numberOfSelectedRows] > 0) {
		for(i= n;i>0;i--){
			if([SelectedList containsIndex:i] == TRUE){
				kemoview_delete_phong_light_list(i, kemo_sgl);
			}
		};
	}
	
    [self SetLightTable:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
	return;
};

- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj{
	return  [self.radialLightPosition count];
};

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex{
	if ([[pTableColumn identifier] isEqualToString:@"Radius"]) {
		return [self.radialLightPosition objectAtIndex:pRowIndex];
	}
	if ([[pTableColumn identifier] isEqualToString:@"Elevation"]) {
		return [self.elevationLightPosition objectAtIndex:pRowIndex];
	}
	if ([[pTableColumn identifier] isEqualToString:@"Azimuth"]) {
		return [self.azimuthLightPosition objectAtIndex:pRowIndex];
	}
	
	NSLog(@"***ERROR** dropped through pTableColumn identifiers");
	return NULL;
};

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject
   forTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex{
	float r0, t0, p0;
	float r_in, r1, r2;
	
    int numberOfRaw = (int) [self.radialLightPosition count];
	
	r0 =  [[self.radialLightPosition objectAtIndex:pRowIndex] floatValue];
	t0 =  [[self.elevationLightPosition objectAtIndex:pRowIndex] floatValue];
	p0 =  [[self.azimuthLightPosition objectAtIndex:pRowIndex] floatValue];
	if(pRowIndex > 0){
		r1 =   [[self.radialLightPosition objectAtIndex:(pRowIndex-1)] floatValue];
	}
	if(pRowIndex < numberOfRaw-1){
		r2 =   [[self.radialLightPosition objectAtIndex:(pRowIndex+1)] floatValue];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Radius"]) {
		r_in = [(NSString *)pObject floatValue];
		if(numberOfRaw == 1){
			r0 = r_in;
		} else if(pRowIndex > 0 && r_in < r1){
			r0 = r1;
		} else if(pRowIndex < (numberOfRaw-1) && r_in > r2){
			r0 = r2;
		} else {
			r0 = r_in;
		};
		
		[self.radialLightPosition replaceObjectAtIndex:pRowIndex
										withObject:[[NSNumber alloc] initWithFloat:r0]];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Elevation"]) {
		t0 = [(NSString *)pObject floatValue];
		[self.elevationLightPosition replaceObjectAtIndex:pRowIndex
										withObject:[[NSNumber alloc] initWithFloat:t0]];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Azimuth"]) {
		p0 = [(NSString *)pObject floatValue];
		[self.azimuthLightPosition replaceObjectAtIndex:pRowIndex
											   withObject:[[NSNumber alloc] initWithFloat:p0]];
	}
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_light_position(pRowIndex, r0, t0, p0, kemo_sgl);
	self.radialSliderValue =    r0;
	self.elevationSliderValue = t0;
	self.azimuthSliderValue =   p0;

	
	[_metalView UpdateImage:kemo_sgl];
	[self SetLightTable:kemo_sgl];
} // end tableView:setObjectValue:forTableColumn:row:


- (void) ViewSelection:(NSTableView *)pTableViewObj
    objectValueForTableColumn:(NSTableColumn *)pTableColumn
                          row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}



- (void)InitLightTable{
	int i, n_ini;
	float r, t, p;
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	n_ini = kemoview_get_num_light_position(kemo_sgl);
	for(i=0;i<self.numLightTable;i++){
		r =  [[self.radialLightPosition objectAtIndex:i] floatValue];
		t =  [[self.elevationLightPosition objectAtIndex:i] floatValue];
		p =  [[self.azimuthLightPosition objectAtIndex:i] floatValue];
		if(i < n_ini){
			kemoview_set_each_light_position(i, r, t, p, kemo_sgl);
		} else {
			kemoview_add_phong_light_list(i, r, t, p, kemo_sgl);
		};
	};
	for(i=n_ini; i>self.numLightTable;i--){
		kemoview_delete_phong_light_list(i-1, kemo_sgl);
	};
	[self SetLightTable:kemo_sgl];
	return;
};
- (void)SetLightTable:(struct kemoviewer_type *) kemo_sgl
{
	int i;
	float r, t, p;
	//	double value, color, color;
	
	[radialLightPosition removeAllObjects];
	[elevationLightPosition removeAllObjects];
	[azimuthLightPosition removeAllObjects];
	self.numLightTable = kemoview_get_num_light_position(kemo_sgl);
	for(i=0;i<self.numLightTable;i++){
		kemoview_get_each_light_rtp(kemo_sgl, i, &r, &t, &p);
		[self.radialLightPosition addObject:[[NSNumber alloc ] initWithFloat:r] ];
		[self.elevationLightPosition addObject:[[NSNumber alloc ] initWithFloat:t] ];
		[self.azimuthLightPosition addObject:[[NSNumber alloc ] initWithFloat:p] ];
	}
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setInteger:((int) self.numLightTable) forKey:@"numberOfLights"];

	[defaults setObject:self.radialLightPosition forKey:@"lightsRadialPosition"];
	[defaults setObject:self.elevationLightPosition forKey:@"lightsElevationPosition"];
	[defaults setObject:self.azimuthLightPosition forKey:@"lightsAzimuthalPosition"];

	[_lightTableView reloadData];
	return;
};

- (IBAction)UpdateLightTable:(id)pID{
    float r, t, p;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    int i = (int) [_lightTableView selectedRow];
    kemoview_get_each_light_rtp(kemo_sgl, i, &r, &t, &p);
    self.radialSliderValue =    r;
    self.elevationSliderValue = t;
    self.azimuthSliderValue =   p;

	[self SetLightTable:[_kmv KemoViewPointer]];
	return;
};


- (IBAction)SetAmbientMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.ambientMaterial) forKey:@"materialAmbient"];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_material_parameter(AMBIENT_FLAG, self.ambientMaterial, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
	return;
};
- (IBAction)SetDiffuseMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.diffuseMaterial) forKey:@"materialDiffuse"];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_material_parameter(DIFFUSE_FLAG, self.diffuseMaterial, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
	return;
};
- (IBAction)SetSpecularMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.specularMaterial) forKey:@"materialSpecular"];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_material_parameter(SPECULAR_FLAG, self.specularMaterial, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
	return;
};
- (IBAction)SetShinenessMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.shinessMaterial) forKey:@"materialShineness"];
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_material_parameter(SHINENESS_FLAG, self.shinessMaterial, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
	return;
};

- (IBAction)SetRadialLightPositionAction:(id)sender{
	float r0, t0, p0, r1, r2;
	int isel = (int) [idlightTableView selectedRow];
	if(isel < 0 || isel >= self.numLightTable) return;
	
	t0 =  [[self.elevationLightPosition objectAtIndex:isel] floatValue];
	p0 =  [[self.azimuthLightPosition objectAtIndex:isel] floatValue];
	if(isel > 0){
		r1 =   [[self.radialLightPosition objectAtIndex:(isel-1)] floatValue];
	}
	if(isel < self.numLightTable-1){
		r2 =   [[self.radialLightPosition objectAtIndex:(isel+1)] floatValue];
	}
	
	if(self.numLightTable == 1){
		r0 = self.radialSliderValue;
	} else if(isel > 0 && self.radialSliderValue < r1){
		r0 = r1;
	} else if(isel < (self.numLightTable-1) && self.radialSliderValue > r2){
		r0 = r2;
	} else {
		r0 = self.radialSliderValue;
	};
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_light_position(isel, r0, t0, p0, kemo_sgl);
	[self.radialLightPosition replaceObjectAtIndex:isel
										withObject:[[NSNumber alloc] initWithFloat:r0]];
	self.elevationSliderValue = t0;
	self.azimuthSliderValue =   p0;
    
	[self SetLightTable:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
};
- (IBAction)SetelevationLightPositionAction:(id)sender{
	float r0, t0, p0;
	int isel = (int) [idlightTableView selectedRow];
	if(isel < 0 || isel >= self.numLightTable) return;
	
	r0 =  [[self.radialLightPosition objectAtIndex:isel] floatValue];
	t0 =  self.elevationSliderValue;
	p0 =  [[self.azimuthLightPosition objectAtIndex:isel] floatValue];
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_light_position(isel, r0, t0, p0, kemo_sgl);
	[self.elevationLightPosition replaceObjectAtIndex:isel
										   withObject:[[NSNumber alloc] initWithFloat:t0]];
	self.radialSliderValue =    r0;
	self.azimuthSliderValue =   p0;
	[self SetLightTable:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
};
- (IBAction)SetAzimuthLightPositionAction:(id)sender{
	float r0, t0, p0;
	int isel = (int) [idlightTableView selectedRow];
	if(isel < 0 || isel >= self.numLightTable) return;
	
	r0 =  [[self.radialLightPosition objectAtIndex:isel] floatValue];
	t0 =  [[self.elevationLightPosition objectAtIndex:isel] floatValue];
	p0 =  self.azimuthSliderValue;
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_each_light_position(isel, r0, t0, p0, kemo_sgl);
	[self.azimuthLightPosition replaceObjectAtIndex:isel
										 withObject:[[NSNumber alloc] initWithFloat:p0]];
	
	self.radialSliderValue =    r0;
	self.elevationSliderValue = t0;
	[self SetLightTable:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
};

- (IBAction)SetLightCheckAction:(id)sender
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetLightTable:kemo_sgl];
    kemoview_set_view_integer(LIGHTING_CHECK, self.lightCheckFlag, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
};

@end
