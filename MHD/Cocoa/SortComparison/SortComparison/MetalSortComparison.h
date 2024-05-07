/*
See LICENSE folder for this sample’s licensing information.

Abstract:
A class to manage all of the Metal objects this app creates.
*/

#include <stdlib.h>
#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

#include "array_for_sorting_test.h"
#include "float_sorting_tests.h"

NS_ASSUME_NONNULL_BEGIN

@interface MetalSortComparison : NSObject
- (instancetype) initWithDevice: (id<MTLDevice>) device;

- (void) sendSortCommand:(struct sort_float_array *) rSort;

@end

NS_ASSUME_NONNULL_END
