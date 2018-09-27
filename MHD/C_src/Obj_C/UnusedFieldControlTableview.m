/*
//  UnusedFieldControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
*/

#import <Foundation/Foundation.h>
#include "UnusedFieldControlTableview.h"

@implementation UnusedFieldControlTableview

@synthesize FieldTableView;

@synthesize unusedFieldControlDictionary;
@synthesize unusedFieldTableView;
@synthesize unusedFieldControlArray;

@synthesize key1;
@synthesize key2;
@synthesize key3;
@synthesize key4;
@synthesize key0;

-(void)linkToFieldclist
{
    load_field_w_qflag_from_ctl(mhd_ctl_m->model_ctl->fld_ctl, all_fld_tbl);
}

-(void)createMutablearray
{
    int i;
    char *c1_out, *c2_out;
    double r1_out;
    
    numberFormatter = [[NSNumberFormatter alloc] init];
    numberFormatter.minimumSignificantDigits = 2;
    
    for(i=0;i<NUM_FIELD;i++){
        printf("%d %s %d %d %d %d \n", i, all_fld_tbl[i]->field_name, 
               all_fld_tbl[i]->iflag_use, all_fld_tbl[i]->iflag_viz,
               all_fld_tbl[i]->iflag_monitor, all_fld_tbl[i]->iflag_quad);
    }
    
    self.unusedFieldControlArray = [[NSMutableArray alloc]init];    

    self.key1 = [NSString stringWithCString:"Field_name" encoding:NSUTF8StringEncoding];    
    self.key2 = [NSString stringWithCString:"Viz_flag" encoding:NSUTF8StringEncoding];    
    self.key3 = [NSString stringWithCString:"Monitor" encoding:NSUTF8StringEncoding];    
    self.key4 = [NSString stringWithCString:"Quadrature_field" encoding:NSUTF8StringEncoding];    
    self.key0 = [NSString stringWithCString:"ID" encoding:NSUTF8StringEncoding];    

    c1_out = (char *)calloc(KCHARA_C, sizeof(char));
    for(i=0;i<NUM_FIELD;i++){
        if(all_fld_tbl[i]->iflag_use == 0){
            NSString *data1 = [NSString stringWithCString:all_fld_tbl[i]->field_name encoding:NSUTF8StringEncoding];
            NSNumber *num0 = [[NSNumber alloc] initWithDouble:i];
            self.unusedFieldControlDictionary = [NSMutableDictionary dictionaryWithObjectsAndKeys:num0,self.key0, data1,self.key1, nil];
            [self.unusedFieldControlArray addObject:self.unusedFieldControlDictionary];
        };
    };
    free(c1_out);
}


-(void)createUnusedFieldView
{
    NSScrollView *scrollView = [[NSScrollView alloc] initWithFrame:unusedFieldTableViewOutlet.bounds];
    [scrollView setBorderType:NSBezelBorder];
    self.unusedFieldTableView = [[NSTableView alloc] initWithFrame:unusedFieldTableViewOutlet.bounds];
    NSTableColumn *tCol;
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key0];
    [tCol setWidth:40.0];
    [[tCol headerCell] setStringValue:self.key0];
    [self.unusedFieldTableView addTableColumn:tCol];
    
    tCol = [[NSTableColumn alloc] initWithIdentifier:self.key1];
    [tCol setWidth:80.0];
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
    
    [scrollView setHasVerticalScroller:YES];
    [scrollView setHasHorizontalScroller:YES];
    [scrollView setAutoresizesSubviews:YES];
    [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [scrollView setDocumentView:self.unusedFieldTableView];
    [unusedFieldTableViewOutlet addSubview:scrollView];
}

// TableView Datasource method implementation
- (void)awakeFromNib {
    [self createMutablearray];
    [self createUnusedFieldView];
} // end awakeFromNib

- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)pTableColumn 
            row:(NSInteger)pRowIndex
{
    // NSString *aString = [NSString stringWithFormat:@"%@, Row %ld",[pTableColumn identifier],(long)pRowIndex];
    NSString *aString;
    aString = [[self.unusedFieldControlArray objectAtIndex:pRowIndex] objectForKey:[pTableColumn identifier]];
    return aString;
}

// TableView Datasource method implementation
- (NSInteger)numberOfRowsInTableView:(NSTableView *)tableView
{
    //we have only one table in the screen and thus we are not checking the row count based on the target table view
    long recordCount = [self.unusedFieldControlArray count];
    return recordCount;
}

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}


- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(NSInteger)pRowIndex
{
    /*
    NSString *selectedKey = [pTableColumn identifier];
    NSString *editedtext = [self.unusedFieldControlArray objectAtIndex:pRowIndex];
    NSString *selectedItem = [[self.unusedFieldControlArray objectAtIndex:pRowIndex] objectForKey:selectedKey];
    NSLog(@"Mutablearray   %@",[self.unusedFieldControlArray objectAtIndex:pRowIndex]);
    NSLog(@"[pTableColumn identifier] %@¥n", selectedKey);
    
    NSLog(@"Mutablearray Selected  %@",selectedItem);
    
    NSLog(@"%d  %@¥n", pRowIndex, editedtext);
    NSLog(@"[setObjectValue] %@¥n", (NSString *)pObject);
*/
};

@end

