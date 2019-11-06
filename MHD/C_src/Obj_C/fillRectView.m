#import "fillRectView.h"
#import "kemoviewer.h"

@implementation fillRectView

- (void)drawRect:(NSRect)frameRect
{
	[self setBoundsSize : NSMakeSize(165,275) ];
	[self setNeedsDisplay:YES]; 
	[self DrawColorMarks:frameRect];
	[self DrawColorBarFlame:frameRect];
}
- (void)UpdateColorbar{
	[self drawRect:[self bounds]];
}


- (void)DrawColorMarks:(NSRect)frameRect
{
    NSRect	rect1 = NSMakeRect(55, 2, 25, 2);
    NSRect	rect2 = NSMakeRect(80, 2, 25, 2);
    int		rectCount = 128;
	double value, color, opacity;
	double dataMin, dataMax, maxOpacity;
	double colorMin, colorMax;
	double r, g, b, a;
    NSRect	rectList1[rectCount], rectList2[rectCount];;
    NSColor* colors[rectCount];
	NSColor* color_w_opaciy[rectCount];
	NSString *str;
	float ylabel;
    int		i, npoint;
    

    if(kemoview_get_PSF_loaded_params(NUM_LOADED) < 1) return;
	npoint = kemoview_get_PSF_color_param(ISET_NUM_COLOR);
	kemoview_get_PSF_color_items(IZERO, &colorMin, &color);
	kemoview_get_PSF_color_items(npoint-1, &colorMax, &color);
	npoint = kemoview_get_PSF_color_param(ISET_NUM_OPACITY);
	kemoview_get_PSF_opacity_items(IZERO, &dataMin, &opacity);
	kemoview_get_PSF_opacity_items(npoint-1, &dataMax, &opacity);
	if (dataMin > colorMin) {dataMin = colorMin;};
	if (dataMax < colorMax) {dataMax = colorMax;};

	maxOpacity = kemoview_get_each_PSF_colormap_range(ISET_OPACITY_MAX);
	
    // Set rectList
    for(i = 0; i < rectCount; i++) {
        *(rectList1 + i) = rect1;
        *(rectList2 + i) = rect2;
        rect1.origin.y += rect1.size.height;
        rect2.origin.y += rect2.size.height;
    }
    
    // Set color
    for(i = 0; i < rectCount; i++) {
		value = dataMin
			+ ((double) i / ((double)rectCount-1)) * (dataMax-dataMin);
		kemoview_get_PSF_rgb_at_value(value, &r, &g, &b);
		a = kemoview_get_PSF_opacity_at_value(value);
		a = a / maxOpacity;

        colors[i] = [NSColor colorWithDeviceRed:r green:g blue:b alpha:1.0];
        color_w_opaciy[i] = [NSColor colorWithDeviceRed:r green:g blue:b alpha:a];
    }
    
    // Call NSRectFillListWithColors
    NSRectFillListWithColors(rectList1, colors, rectCount);
    NSRectFillListWithColors(rectList2, color_w_opaciy, rectCount);

	str = [NSString stringWithFormat:@"Color"];
	[self drawString:str x:105 y:265];
	for(i = 0; i < kemoview_get_PSF_color_param(ISET_NUM_COLOR); i++) {
		kemoview_get_PSF_color_items(i, &value, &color);
		ylabel = 250 * (value-dataMin) / (dataMax - dataMin);
		str = [NSString stringWithFormat:@"%1.2e", value];
		[self drawString:str x:112 y:ylabel];
	}

	str = [NSString stringWithFormat:@"Opacity"];
	[self drawString:str x:3 y:265];
	for(i = 0; i < kemoview_get_PSF_color_param(ISET_NUM_OPACITY); i++) {
		kemoview_get_PSF_opacity_items(i, &value, &opacity);
		ylabel = 250 * (value-dataMin) / (dataMax - dataMin);
		str = [NSString stringWithFormat:@"%1.2e", value];
		[self drawString:str x:0 y:ylabel];
	}
}

- (void)DrawColorBarFlame:(NSRect)frameRect
{
    NSRect	rect = NSMakeRect(54, 1, 52, 257);
	NSFrameRectWithWidth(rect, 1.0);
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
