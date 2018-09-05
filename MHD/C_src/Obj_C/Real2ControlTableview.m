//
//  Real2ControlTableview.m
//  
//
//  Created by Hiroaki Matsui on 2018/09/03.
//

#import <Foundation/Foundation.h>
#include "Real2ControlTableview.h"

struct real2_clist * Real2CtlList;

struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
char buf[LENGTHBUF];      /* character buffer for reading line */


@implementation Real2TableController
@synthesize Title1;
@synthesize Title2;
// @synthesize TableRealData1;
// @synthesize TableRealData2;
@synthesize idReal2TableView;

- (void)awakeFromNib {
    int i;
    double data1, data2;

    mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
    alloc_SGS_MHD_control_c(mhd_ctl);
    read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
    struct PVR_ctl_list *pvr1 = &mhd_ctl->viz_c->pvr_ctl_list;
    Real2CtlList = pvr1->_next->_next->v_render_c->pvr_c->cmap_cbar_c->cmap_c->colortbl_list;
    
    TableRealData2 = [[NSMutableArray alloc]init];
    self.Title1 = [[NSString alloc]init];
    self.Title2 = [[NSString alloc]init];
    self.Title1 = [NSString stringWithCString:(Real2CtlList->r1_name) encoding:NSUTF8StringEncoding];
    self.Title2 = [NSString stringWithCString:(Real2CtlList->r2_name) encoding:NSUTF8StringEncoding];
    
    NSTableHeaderCell *tfCell1 = [[NSTableHeaderCell alloc] init];
    NSTableColumn *theColumn1;
    NSTableHeaderCell *tfCell2 = [[NSTableHeaderCell alloc] init];
    NSTableColumn *theColumn2;
    
    [[[_real2TableView tableColumns] objectAtIndex:0] setIdentifier:self.Title1];
    [tfCell1 setStringValue:self.Title1];
    [theColumn1 setHeaderCell:tfCell1];
    [[[_real2TableView tableColumns] objectAtIndex:0] setHeaderCell:tfCell1];
    
    [[[_real2TableView tableColumns] objectAtIndex:1] setIdentifier:self.Title2];
    [tfCell2 setStringValue:self.Title2];
    [theColumn2 setHeaderCell:tfCell2];
    [[[_real2TableView tableColumns] objectAtIndex:1] setHeaderCell:tfCell2];
    
    TableRealData1 = [[NSMutableArray alloc]init];
    TableRealData2 = [[NSMutableArray alloc]init];
    int NumTableItem = count_real2_clist(Real2CtlList);
    for(i=0;i<NumTableItem;i++){
        set_from_real2_clist_at_index(i, Real2CtlList, &data1, &data2);
        
        [TableRealData1 addObject:[NSNumber numberWithDouble:data1]];
        [TableRealData2 addObject:[NSNumber numberWithDouble:data2]];
    }

    [self SetReal2Tables];
} // end awakeFromNib

- (IBAction)addAtSelectedRow:(id)pId {
    double dinput1;
    double dinput2;
    long isel = [idReal2TableView selectedRow];
    
    if ([idReal2TableView selectedRow] > 0 && isel > 0) {
        dinput1 =   [[TableRealData1 objectAtIndex:isel-1] doubleValue];
        dinput2 =   [[TableRealData1 objectAtIndex:isel] doubleValue];
        add_real2_clist_between_value1(dinput1, dinput2, Real2CtlList);
        [self SetReal2Tables];
    }
}

- (IBAction)deleteSelectedRow:(id)pId
{
    int i;
    
    NSIndexSet *SelectedList = [idReal2TableView selectedRowIndexes];
    if([TableRealData1 count] < 3) return;
    
    if ([idReal2TableView numberOfSelectedRows] > 0) {
        for(i = (int) [TableRealData1 count]-1;i>1;i--){
            if([SelectedList containsIndex:i] == TRUE){
                del_real2_clist_by_index(i, Real2CtlList);
            }
        }
    }
    
    [self SetReal2Tables];
}



- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj {
    return (int) [TableRealData1 count];
} // end numberOfRowsInTableView

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
             row:(int)pRowIndex
{
    NSString *label1 = [NSString stringWithCString:(Real2CtlList->r1_name) encoding:NSUTF8StringEncoding];
    NSString *label2 = [NSString stringWithCString:(Real2CtlList->r2_name) encoding:NSUTF8StringEncoding];
    
    if ([[pTableColumn identifier] isEqualToString:label1]) {
        return [TableRealData1 objectAtIndex:pRowIndex];
    }
    
    if ([[pTableColumn identifier] isEqualToString:label2]) {
        return [TableRealData2 objectAtIndex:pRowIndex];
    }
    
    NSLog(@"***ERROR** dropped through pTableColumn identifiers");
    return NULL;
    
} // end tableView:objectValueForTableColumn:row:

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject forTableColumn
                 :(NSTableColumn *)pTableColumn row:(int)pRowIndex
{
    double data1, data2;
    double dinput1;
    double dinput2;
    NSString *label1 = [NSString stringWithCString:(Real2CtlList->r1_name) encoding:NSUTF8StringEncoding];
    NSString *label2 = [NSString stringWithCString:(Real2CtlList->r2_name) encoding:NSUTF8StringEncoding];
    
    long numberOfRaw = [TableRealData1 count];
    
    data1 = [[TableRealData1 objectAtIndex:pRowIndex] doubleValue];
    data2 = [[TableRealData2 objectAtIndex:pRowIndex] doubleValue];
    if(pRowIndex > 0){
        dinput1 =   [[TableRealData1 objectAtIndex:(pRowIndex-1)] doubleValue];
    } else {
        dinput1 =   [[TableRealData1 objectAtIndex:0] doubleValue];
    }
    if(pRowIndex < numberOfRaw-1){
        dinput2 =   [[TableRealData1 objectAtIndex:(pRowIndex+1)] doubleValue];
    } else{
        dinput2 =   [[TableRealData1 objectAtIndex:(numberOfRaw-1)] doubleValue];        
    }
    
    if ([[pTableColumn identifier] isEqualToString:label1]) {
        data1 = [(NSString *)pObject doubleValue];
        if(pRowIndex > 0 && data1 < dinput1)               data1 = dinput1;
        if(pRowIndex < (numberOfRaw-1) && data1 > dinput2) data1 = dinput2;
        
        [TableRealData1 replaceObjectAtIndex:pRowIndex
                                          withObject:[[NSNumber alloc] initWithDouble:data1]];
    }
    
    if ([[pTableColumn identifier] isEqualToString:label2]) {
        data2 = [(NSString *)pObject doubleValue];
        if(pRowIndex > 0 && data2 < dinput1)               data2 = dinput1;
        if(pRowIndex < (numberOfRaw-1) && data2 > dinput2) data2 = dinput2;
        
        [TableRealData2 replaceObjectAtIndex:pRowIndex
                                            withObject:[[NSNumber alloc] initWithDouble:data2]];
    }
    update_real2_clist_by_index(pRowIndex, data1, data2, Real2CtlList);

} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
    NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void)InitReal2Tables
{    
    [self SetReal2Tables];
}

- (void) SetReal2Tables
{
    int i;
    double data1, data2;

/*
    [TableRealData1 removeAllObjects];
    [TableRealData2 removeAllObjects];
 */
    TableRealData1 = [[NSMutableArray alloc]init];
    TableRealData2 = [[NSMutableArray alloc]init];
    NumTableItem = count_real2_clist(Real2CtlList);
    for(i=0;i<NumTableItem;i++){
        set_from_real2_clist_at_index(i, Real2CtlList, &data1, &data2);
        
        [TableRealData1 addObject:[[NSNumber alloc ] initWithDouble:data1] ];
        [TableRealData2 addObject:[[NSNumber alloc ] initWithDouble:data2] ];
    }
    [_real2TableView reloadData];
}

- (IBAction) UpdateReal2Tables:(id)pID
{

}
@end

