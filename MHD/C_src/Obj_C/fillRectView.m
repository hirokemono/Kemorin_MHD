#import "fillRectView.h"
#import "kemoviewer.h"

@implementation fillRectView


BOOL appearanceIsDark(NSAppearance * appearance)
{
    if (@available(macOS 10.14, *)) {
        NSAppearanceName basicAppearance = [appearance bestMatchFromAppearancesWithNames:@[
            NSAppearanceNameAqua,
            NSAppearanceNameDarkAqua
        ]];
        return [basicAppearance isEqualToString:NSAppearanceNameDarkAqua];
    } else {
        return NO;
    }
}

- (void)drawRect:(NSRect)frameRect
{
    int current_model = [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self setBoundsSize : NSMakeSize(165,275)];
    [self SetColorRectangles:current_model
                    kemoview:kemo_sgl];
    [self DrawColorMarks];
}
- (void)UpdateColorbar{
	[self setNeedsDisplay:YES];
}

- (void)SetColorRectangles:(int) current_model
                  kemoview:(struct kemoviewer_type *) kemo_sgl
{
    boxRect = NSMakeRect(54, 1, 52, 257);
    NSRect	rect1 = NSMakeRect(55, 2, 25, 2);
    NSRect	rect2 = NSMakeRect(80, 2, 25, 2);
	NSString *str;
	double value, color, opacity;
	double maxOpacity;
	double colorMin, colorMax;
	double dataMin, dataMax;
	double r, g, b, a;
	float ylabel;
    int		i, npoint;
    
    if(current_model == SURFACE_RENDERING){
        int n_loaded = kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);
        if(n_loaded < 1) return;
    }
    
    npoint = kemoview_get_viz_colormap_param(kemo_sgl,
                                             current_model,
                                             ISET_NUM_COLOR);
    kemoview_get_VIZ_color_RGB_value(kemo_sgl, current_model,
                                     IZERO, &colorMin, &color);
    kemoview_get_VIZ_color_RGB_value(kemo_sgl, current_model,
                                     (npoint-1), &colorMax, &color);
	npoint = kemoview_get_viz_colormap_param(kemo_sgl,
                                             current_model,
                                             ISET_NUM_OPACITY);
	kemoview_get_PSF_opacity_items(kemo_sgl,
                                   current_model,
                                   IZERO, &dataMin, &opacity);
	kemoview_get_PSF_opacity_items(kemo_sgl,
                                   current_model,
                                   (npoint-1), &dataMax, &opacity);
	if (dataMin > colorMin) {dataMin = colorMin;};
	if (dataMax < colorMax) {dataMax = colorMax;};

	maxOpacity = kemoview_get_VIZ_opacity_range(kemo_sgl,
                                                current_model,
                                                ISET_OPACITY_MAX);
	
    // Set rectList
    for(i = 0; i < RECTCOUNT; i++) {
        *(rectList1 + i) = rect1;
        *(rectList2 + i) = rect2;
        rect1.origin.y += rect1.size.height;
        rect2.origin.y += rect2.size.height;
    }
    
    // Set color
    for(i = 0; i < RECTCOUNT; i++) {
		value = dataMin
			+ ((double) i / ((double)RECTCOUNT-1)) * (dataMax-dataMin);
		kemoview_get_PSF_rgb_at_value(kemo_sgl, current_model, value,
                                      &r, &g, &b);
		a = kemoview_get_PSF_opacity_at_value(kemo_sgl, current_model, value);
		a = a / maxOpacity;

        colors[i] = [NSColor colorWithDeviceRed:r green:g blue:b alpha:1.0];
        color_w_opaciy[i] = [NSColor colorWithDeviceRed:r green:g blue:b alpha:a];
    };

	str = [NSString stringWithFormat:@"Color"];
	[self drawString:str x:105 y:265];
    int n_color = kemoview_get_viz_colormap_param(kemo_sgl,
                                                  current_model,
                                                  ISET_NUM_COLOR);
	for(i = 0; i<n_color; i++) {
        kemoview_get_VIZ_color_RGB_value(kemo_sgl, current_model,
                                         i, &value, &color);
		ylabel = 250 * (value-dataMin) / (dataMax - dataMin);
		str = [NSString stringWithFormat:@"%1.2e", value];
		[self drawString:str x:112 y:ylabel];
	}

	str = [NSString stringWithFormat:@"Opacity"];
	[self drawString:str x:3 y:265];
    int n_opacity = kemoview_get_viz_colormap_param(kemo_sgl,
                                                    current_model,
                                                    ISET_NUM_OPACITY);
	for(i = 0; i < n_opacity; i++) {
		kemoview_get_PSF_opacity_items(kemo_sgl,
                                       current_model,
                                       i, &value, &opacity);
		ylabel = 250 * (value-dataMin) / (dataMax - dataMin);
		str = [NSString stringWithFormat:@"%1.2e", value];
		[self drawString:str x:0 y:ylabel];
	}
    return;
};

- (void)DrawColorMarks
{
    // Call NSRectFillListWithColors
    NSRectFillListWithColors(rectList1, colors, RECTCOUNT);
    NSRectFillListWithColors(rectList2, color_w_opaciy, RECTCOUNT);
	NSFrameRectWithWidth(boxRect, 1.0);
    return;
}

- (void)drawString:(NSString*)string x:(double)x y:(double)y {
    NSDictionary* attr;
    if(appearanceIsDark(self.effectiveAppearance)){
        attr=[NSDictionary dictionaryWithObjectsAndKeys:
                            [NSFont systemFontOfSize:10.0f],NSFontAttributeName,
                            [NSColor whiteColor],NSForegroundColorAttributeName,
                            nil];
    }else{
        attr=[NSDictionary dictionaryWithObjectsAndKeys:
                            [NSFont systemFontOfSize:10.0f],NSFontAttributeName,
                            [NSColor blackColor],NSForegroundColorAttributeName,
                            nil];
    };
    
    NSPoint point;
    point.x = x;
    point.y = y;
    [string drawAtPoint:point withAttributes:attr];
}


@end
