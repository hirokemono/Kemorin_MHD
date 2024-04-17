#import "fillRectView.h"
#import "kemoviewer.h"

@implementation fillRectView

- (void)drawRect:(NSRect)frameRect
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self setBoundsSize : NSMakeSize(165,275)];
    [self SetColorRectangles:kemo_sgl];
    [self DrawColorMarks];
}
- (void)UpdateColorbar{
	[self setNeedsDisplay:YES];
}

- (void)SetColorRectangles:(struct kemoviewer_type *) kemo_sgl
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
    
    if(kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED) < 1) return;
	npoint = kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_COLOR);
	kemoview_get_PSF_color_items(kemo_sgl, IZERO, &colorMin, &color);
	kemoview_get_PSF_color_items(kemo_sgl, (npoint-1), &colorMax, &color);
	npoint = kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_OPACITY);
	kemoview_get_PSF_opacity_items(kemo_sgl, IZERO, &dataMin, &opacity);
	kemoview_get_PSF_opacity_items(kemo_sgl, (npoint-1), &dataMax, &opacity);
	if (dataMin > colorMin) {dataMin = colorMin;};
	if (dataMax < colorMax) {dataMax = colorMax;};

	maxOpacity = kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_OPACITY_MAX);
	
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
		kemoview_get_PSF_rgb_at_value(kemo_sgl, value, &r, &g, &b);
		a = kemoview_get_PSF_opacity_at_value(kemo_sgl, value);
		a = a / maxOpacity;

        colors[i] = [NSColor colorWithDeviceRed:r green:g blue:b alpha:1.0];
        color_w_opaciy[i] = [NSColor colorWithDeviceRed:r green:g blue:b alpha:a];
    };

	str = [NSString stringWithFormat:@"Color"];
	[self drawString:str x:105 y:265];
	for(i = 0; i < kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_COLOR); i++) {
		kemoview_get_PSF_color_items(kemo_sgl, i, &value, &color);
		ylabel = 250 * (value-dataMin) / (dataMax - dataMin);
		str = [NSString stringWithFormat:@"%1.2e", value];
		[self drawString:str x:112 y:ylabel];
	}

	str = [NSString stringWithFormat:@"Opacity"];
	[self drawString:str x:3 y:265];
	for(i = 0; i < kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_OPACITY); i++) {
		kemoview_get_PSF_opacity_items(kemo_sgl, i, &value, &opacity);
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
    NSDictionary* attr=[NSDictionary dictionaryWithObjectsAndKeys:
						[NSFont systemFontOfSize:10.0f],NSFontAttributeName,//フォントサイズ
						[NSColor blackColor],NSForegroundColorAttributeName,//フォント色
						nil];
    
    NSPoint point;
    point.x = x;
    point.y = y;
    [string drawAtPoint:point withAttributes:attr];
}


@end
