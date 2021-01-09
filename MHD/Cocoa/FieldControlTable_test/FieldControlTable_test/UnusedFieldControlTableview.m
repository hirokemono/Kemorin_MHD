/*
//  UnusedFieldControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "UnusedFieldControlTableview.h"

@implementation UnusedFieldControlTableview

@synthesize baseFieldDictionary;
@synthesize forceFieldDictionary;
@synthesize energyFieldDictionary;
@synthesize sgsFieldDictionary;

@synthesize baseFieldArray;
@synthesize forceFieldArray;
@synthesize energyFieldArray;
@synthesize sgsFieldArray;

@synthesize baseParentsDictionary;
@synthesize forceParentsDictionary;
@synthesize energyParentsDictionary;
@synthesize sgsParentsDictionary;

@synthesize FieldControlDictionary;
@synthesize FieldControlArray;
@synthesize unusedFieldTableView;

@synthesize key2;
@synthesize key1;
@synthesize key0;

@synthesize firstParent;
@synthesize secondParent;
@synthesize list;

-(void)linkToFieldclist:(struct all_field_ctl_c *) ref_all_fld_list
{
    mhd_ctl_m = link_to_mhd_ctl();
    all_fld_list = ref_all_fld_list;
}

-(void)initMutablearray
{
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    self.baseFieldArray = [[NSMutableArray alloc]init];    
    self.forceFieldArray = [[NSMutableArray alloc]init];    
    self.energyFieldArray = [[NSMutableArray alloc]init];    
    self.sgsFieldArray = [[NSMutableArray alloc]init];    

    self.FieldControlArray = [[NSMutableArray alloc]init];    
    
    self.key2 = [NSString stringWithCString:"Field_group" encoding:NSUTF8StringEncoding];    
    self.key1 = [NSString stringWithCString:"Field_name" encoding:NSUTF8StringEncoding];    
    self.key0 = [NSString stringWithCString:"ID" encoding:NSUTF8StringEncoding];    
}

-(void)createMutablearray
{
    int i;
    char *c1_out;
    
    /*
     for(i=0;i<all_fld_list->fld_list->ntot_fields;i++){
     printf("%d %s %d %d %d %d \n", i, all_fld_list->fld_list->field_name[i], 
     all_fld_list->iflag_use[i], all_fld_list->iflag_viz[i],
     all_fld_list->iflag_monitor[i], all_fld_list->iflag_quad[i]);
     }
     */
    
    [self.FieldControlArray removeAllObjects];
    [self.baseFieldArray removeAllObjects];
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=2;i<50;i++){
        if(all_fld_list->iflag_use[i] == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_list->fld_list->field_name[i] encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.baseFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Base",self.key2, num0,self.key0, data1,self.key1, nil];
            [self.baseFieldArray addObject:data1];
        };
    };
    self.baseParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Base",self.key2, self.baseFieldArray,self.key1, nil];
    [self.FieldControlArray addObject:self.baseParentsDictionary];
    
    [self.forceFieldArray removeAllObjects];
    for(i=50;i<100;i++){
        if(all_fld_list->iflag_use[i] == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_list->fld_list->field_name[i] encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.forceFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Force",self.key2, num0,self.key0, data1,self.key1, nil];
            [self.forceFieldArray addObject:data1];
        };
    };
    self.forceParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Energy",self.key2, self.forceFieldArray,self.key1, nil];
    [self.FieldControlArray addObject:self.forceParentsDictionary];

    [self.energyFieldArray removeAllObjects];
    for(i=100;i<170;i++){
        if(all_fld_list->iflag_use[i] == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_list->fld_list->field_name[i] encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.energyFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Energy",self.key2, num0,self.key0, data1,self.key1, nil];
            [self.energyFieldArray addObject:data1];
        };
    };
    self.energyParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Energy",self.key2, self.sgsFieldArray,self.key1, nil];
    [self.FieldControlArray addObject:self.energyParentsDictionary];

    [self.sgsFieldArray removeAllObjects];
    for(i=170;i<all_fld_list->fld_list->ntot_fields;i++){
        if(all_fld_list->iflag_use[i] == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_list->fld_list->field_name[i] encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.sgsFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"SGS",self.key2, num0,self.key0, data1,self.key1, nil];
            [self.sgsFieldArray addObject:data1];
        };
    };
    self.sgsParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"SGS",self.key2, self.sgsFieldArray,self.key1, nil];
    [self.FieldControlArray addObject:self.sgsParentsDictionary];
    free(c1_out);

    self.firstParent = [[NSMutableDictionary alloc] initWithObjectsAndKeys:@"Mac",self.key2, energyFieldArray,self.key1, nil];
    self.secondParent = [[NSMutableDictionary alloc] initWithObjectsAndKeys:@"PC",self.key2, sgsFieldArray,self.key1, nil];
    NSLog(@"self.firstParent   %@",self.firstParent);
    
    self.list = [NSMutableArray arrayWithObjects:firstParent,secondParent, nil];

}


-(void)createFieldView:(NSView *) unUsedFieldTableViewOutlet
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:unUsedFieldTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.unusedFieldTableView = [[NSOutlineView alloc] initWithFrame:unUsedFieldTableViewOutlet.bounds];
    NSTableColumn *tCol;
    NSButtonCell *cell;
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key2];
    [tCol setWidth:80.0];
    [[tCol headerCell] setStringValue:self.key2];
    [self.unusedFieldTableView addTableColumn:tCol];
    [self.unusedFieldTableView setOutlineTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:180.0];
    [[tCol headerCell] setStringValue:self.key1];
    [self.unusedFieldTableView addTableColumn:tCol];

    [self.unusedFieldTableView setUsesAlternatingRowBackgroundColors:YES];
    [self.unusedFieldTableView setGridStyleMask:NSTableViewSolidVerticalGridLineMask];
    [self.unusedFieldTableView setGridColor:[NSColor grayColor]];
    [self.unusedFieldTableView setRowHeight:23.0];
    [self.unusedFieldTableView setDelegate:self];
    [self.unusedFieldTableView setDataSource:self];
    [self.unusedFieldTableView setSelectionHighlightStyle:NSTableViewSelectionHighlightStyleRegular];
    [self.unusedFieldTableView setAutoresizesSubviews:YES];
    [self.unusedFieldTableView setAllowsMultipleSelection:YES];
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.unusedFieldTableView];
    [unUsedFieldTableViewOutlet addSubview:scrollView];
}

-(void) updateUnusedFieldTable
{
    [self.unusedFieldTableView reloadData];
}

- (void)addUsedField
{
    NSIndexSet *selectedRows = [self.unusedFieldTableView selectedRowIndexes];
    //    NSUInteger numberOfSelectedRows = [selectedRows count];
    NSUInteger isel;
    int index;
    
    NSLog(@"selectedRows %@ \n", selectedRows);
    isel = [selectedRows lastIndex];
    NSMutableIndexSet *field_Indices = [NSMutableIndexSet indexSet];
    while(isel != NSNotFound) {
        NSLog(@"index = %d", (int) isel);
        /*
        NSString *selectedID = [[self.FieldControlArray objectAtIndex:isel] objectForKey:self.key0];
        index =  [selectedID intValue];
        [field_Indices addIndex:index];
        all_fld_list->iflag_use[index] =     1;
        all_fld_list->iflag_viz[index] =     0;
        all_fld_list->iflag_monitor[index] = 0;
        all_fld_list->iflag_quad[index] =    0;
        add_field_wqflag_to_ctl(index, all_fld_list, mhd_ctl_m->model_ctl->fld_ctl);
        
        [self.FieldControlArray removeObjectAtIndex:isel];
         */
        isel = [selectedRows indexLessThanIndex:isel];
    }
    [self.unusedFieldTableView reloadData];
    //    NSLog(@"field_Indices   %@",field_Indices);
}

// OutlineView Datasource method implementation
- (BOOL)outlineView:(NSOutlineView *)pOutlineView isItemExpandable:(id)item
{
    if ([item isKindOfClass:[NSDictionary class]] || [item isKindOfClass:[NSArray class]]) return YES;
    return NO;
}

- (NSInteger)outlineView:(NSOutlineView *)pOutlineView numberOfChildrenOfItem:(id)item
{
    if (item == nil) { //item is nil when the outline view wants to inquire for root level items
        return [self.FieldControlArray count];
    }
    
    if ([item isKindOfClass:[NSDictionary class]]) {
        return [[item objectForKey:self.key1] count];
    }
    
    return 0;
}

- (id)outlineView:(NSOutlineView *)pOutlineView child:(NSInteger)index ofItem:(id)item
{
    if (item == nil) { //item is nil when the outline view wants to inquire for root level items
        return [self.FieldControlArray objectAtIndex:index];
    }
    
    if ([item isKindOfClass:[NSDictionary class]]) {
        return [[item objectForKey:self.key1] objectAtIndex:index];
    }
    
    return nil;
}

- (id)outlineView:(NSOutlineView *)pOutlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item

{
    if ([[tableColumn identifier] isEqualToString:self.key1]) {
        if ([item isKindOfClass:[NSDictionary class]]) {
            return [NSString stringWithFormat:@"%li fields",[[item objectForKey:self.key1] count]];
            
        }
        return item;
    } else {
        if ([item isKindOfClass:[NSDictionary class]]) {
            return [item objectForKey:self.key2];
        }
    }
    
    return nil;
}

- (BOOL)outlineView:(NSOutlineView *)pOutlineView shouldExpandItem:(id)item
{
    if ([[item objectForKey:self.key2] isEqualToString:@"Base"]) return YES;
    if ([[item objectForKey:self.key2] isEqualToString:@"Force"]) return YES;
    if ([[item objectForKey:self.key2] isEqualToString:@"Energy"]) return YES;
    if ([[item objectForKey:self.key2] isEqualToString:@"SGS"]) return YES;
    return NO;
}

- (BOOL)outlineView:(NSOutlineView *)pOutlineView shouldCollapseItem:(id)item
{
    return YES;
}


// TableView Datasource method implementation
/*
- (id)tableView:(NSTableView *)pTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.FieldControlArray count];
    return recordCount;
}
/*
- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    int index;
    NSString *selectedKey = [pTableColumn identifier];
    NSString *selectedID = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:self.key0];
    index =  [selectedID intValue];
    
    [[self.FieldControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    update_field_flag_wqflag_in_ctl(index, all_fld_list, mhd_ctl_m->model_ctl->fld_ctl);
    
    /*
     NSString *editedtext = [self.FieldControlArray objectAtIndex:pRowIndex];
     NSString *selectedItem = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
     
     NSLog(@"Mutablearray   %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
     NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
     
     NSLog(@"Mutablearray Selected  %@",selectedItem);
     
     NSLog(@"%d %d %@¥n", pRowIndex, index, editedtext);
     NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
    
    NSLog(@"Mutablearray again  %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
};
*/
@end
