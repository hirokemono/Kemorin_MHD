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

@synthesize key1;
@synthesize key0;

-(void)linkToFieldclist:(struct all_field_ctl_c **) ref_all_fld_table
{
    mhd_ctl_m = link_to_mhd_ctl();
    all_fld_tbl = ref_all_fld_table;
}

-(void)initMutablearray
{
    integerFormatter = [[NSNumberFormatter alloc] init];
    integerFormatter.minimumSignificantDigits = 0;
    
    self.FieldControlArray = [[NSMutableArray alloc]init];    
    
    self.key1 = [NSString stringWithCString:"Field_name" encoding:NSUTF8StringEncoding];    
    self.key0 = [NSString stringWithCString:"ID" encoding:NSUTF8StringEncoding];    
}

-(void)createMutablearray
{
    int i;
    char *c1_out;
    
    /*
     for(i=0;i<NUM_FIELD;i++){
     printf("%d %s %d %d %d %d \n", i, all_fld_tbl[i]->field_name, 
     all_fld_tbl[i]->iflag_use, all_fld_tbl[i]->iflag_viz,
     all_fld_tbl[i]->iflag_monitor, all_fld_tbl[i]->iflag_quad);
     }
     */
    
    [self.FieldControlArray removeAllObjects];
    [self.baseFieldArray removeAllObjects];
    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=2;i<50;i++){
        if(all_fld_tbl[i]->iflag_use == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_tbl[i]->field_name encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.baseFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, nil];
            [self.baseFieldArray addObject:self.baseFieldDictionary];
            self.baseParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Base",@"Parents", self.baseFieldArray,@"children", nil];
            
            [self.FieldControlArray addObject:self.baseFieldDictionary];
        };
    };
    
    [self.forceFieldArray removeAllObjects];
    for(i=50;i<100;i++){
        if(all_fld_tbl[i]->iflag_use == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_tbl[i]->field_name encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.forceFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, nil];
            [self.forceFieldArray addObject:self.forceFieldDictionary];
            self.forceParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Force",@"Parents", self.forceFieldArray,@"children", nil];

            [self.FieldControlArray addObject:self.forceFieldDictionary];
        };
    };

    [self.energyFieldArray removeAllObjects];
    for(i=100;i<170;i++){
        if(all_fld_tbl[i]->iflag_use == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_tbl[i]->field_name encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.energyFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, nil];
            [self.energyFieldArray addObject:self.energyFieldDictionary];
            self.energyParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"Energy",@"Parents", self.energyFieldArray,@"children", nil];

            [self.FieldControlArray addObject:self.energyFieldDictionary];
        };
    };

    [self.sgsFieldArray removeAllObjects];
    for(i=170;i<NUM_FIELD;i++){
        if(all_fld_tbl[i]->iflag_use == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_tbl[i]->field_name encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithInt:i];
            self.sgsFieldDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, nil];
            [self.sgsFieldArray addObject:self.sgsFieldDictionary];
            self.sgsParentsDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:@"SGS",@"Parents", self.sgsFieldArray,@"children", nil];

            [self.FieldControlArray addObject:self.sgsFieldDictionary];
        };
    };
    free(c1_out);
}


-(void)createFieldView:(NSView *) unUsedFieldTableViewOutlet
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:unUsedFieldTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.unusedFieldTableView = [[NSTableView alloc] initWithFrame:unUsedFieldTableViewOutlet.bounds];
    NSTableColumn *tCol;
    NSButtonCell *cell;
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key0];
    [tCol setWidth:40.0];
    [[tCol headerCell] setStringValue:self.key0];
    [self.unusedFieldTableView addTableColumn:tCol];
    
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
    
    isel = [selectedRows lastIndex];
    NSMutableIndexSet *field_Indices = [NSMutableIndexSet indexSet];
    while(isel != NSNotFound) {
        NSLog(@"index = %d", (int) isel);
        NSString *selectedID = [[self.FieldControlArray objectAtIndex:isel] objectForKey:self.key0];
        index =  [selectedID intValue];
        [field_Indices addIndex:index];
        all_fld_tbl[index]->iflag_use =     1;
        all_fld_tbl[index]->iflag_viz =     0;
        all_fld_tbl[index]->iflag_monitor = 0;
        all_fld_tbl[index]->iflag_quad =    0;
        add_field_wqflag_to_ctl(all_fld_tbl[index], mhd_ctl_m->model_ctl->fld_ctl);
        
        [self.FieldControlArray removeObjectAtIndex:isel];
        isel = [selectedRows indexLessThanIndex:isel];
    }
    [self.unusedFieldTableView reloadData];
    //    NSLog(@"field_Indices   %@",field_Indices);
}

// TableView Datasource method implementation
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.FieldControlArray count];
    return recordCount;
}

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
    
    /*
     NSString *editedtext = [self.FieldControlArray objectAtIndex:pRowIndex];
     NSString *selectedItem = [[self.FieldControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
     
     NSLog(@"Mutablearray   %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
     NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
     
     NSLog(@"Mutablearray Selected  %@",selectedItem);
     
     NSLog(@"%d %d %@¥n", pRowIndex, index, editedtext);
     NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
     */
    
    [[self.FieldControlArray objectAtIndex:pRowIndex] setObject:pObject forKey:selectedKey];
    update_field_flag_wqflag_in_ctl(all_fld_tbl[index], mhd_ctl_m->model_ctl->fld_ctl);
    
    //    NSLog(@"Mutablearray again  %@",[self.FieldControlArray objectAtIndex:pRowIndex]);
};
@end
