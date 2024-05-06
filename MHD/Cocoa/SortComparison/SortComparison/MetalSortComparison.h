/*
See LICENSE folder for this sample’s licensing information.

Abstract:
A class to manage all of the Metal objects this app creates.
*/

#include <stdlib.h>
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

NS_ASSUME_NONNULL_BEGIN

// The number of floats in each array, and the size of the arrays in bytes.
struct sortdata{
    unsigned long sortLength;
    unsigned long narrayP2;
    unsigned int nextP2;
    long nsize_buf;
 
    float *floatinput;
    int   *intinput;
    float *floatresult;
    int   *intresult;
};


@interface MetalSortComparison : NSObject
- (instancetype) initWithDevice: (id<MTLDevice>) device;

- (void) prepareSort:(struct sortdata *) _d;
- (void) sendSortCommand:(struct sortdata *) _d;

@end

NS_ASSUME_NONNULL_END
