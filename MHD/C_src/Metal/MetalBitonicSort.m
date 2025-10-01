/*
See LICENSE folder for this sample’s licensing information.

Abstract:
A class to manage all of the Metal objects this app creates.
*/

@import Metal;
@import MetalKit;
#import "MetalBitonicSort.h"

@implementation MetalBitonicSort
{
    id<MTLDevice> _mDevice;

    // The compute pipeline generated from the compute kernel in the .metal shader file.
    id<MTLComputePipelineState> _mBitonicSort;

    // The command queue used to pass commands to the device.
    id<MTLCommandQueue> _mCommandQueue;

    id<MTLBuffer> _sortFBuffer;
    id<MTLBuffer> _sortIBuffer;
    id<MTLBuffer> _n1Buffer;
    id<MTLBuffer> _n2Buffer;
}

- (instancetype) initWithDevice: (id<MTLDevice>) device
{
    self = [super init];
    if (self)
    {
        _mDevice = device;
        NSError* error = nil;

        // Load the shader files with a .metal file extension in the project
        id<MTLLibrary> defaultLibrary = [_mDevice newDefaultLibrary];
        if (defaultLibrary == nil)
        {
            NSLog(@"Failed to find the default library.");
            return nil;
        }

        id<MTLFunction> bitonicSort = [defaultLibrary newFunctionWithName:@"bitonicSort"];
        if (bitonicSort == nil)
        {
            NSLog(@"Failed to find the bitonicSort function.");
            return nil;
        }
        _mBitonicSort = [_mDevice newComputePipelineStateWithFunction: bitonicSort error:&error];
        if (_mBitonicSort == nil)
        {
            NSLog(@"Failed to created pipeline state object, error %@.", error);
            return nil;
        }
        _mCommandQueue = [_mDevice newCommandQueue];
        if (_mCommandQueue == nil)
        {
            NSLog(@"Failed to find the command queue.");
            return nil;
        }
    }

    return self;
}

- (void)encodeSortCommand:(id<MTLComputeCommandEncoder>)computeEncoder
                arraySize:(long) narrayP2
{

/*
    MTLSize gridSize = MTLSizeMake(sortLength, 1, 1);
    // Calculate a threadgroup size.
    NSUInteger threadGroupSize = _mBitonicSort.maxTotalThreadsPerThreadgroup;
    if (threadGroupSize > sortLength)
    {
        threadGroupSize = sortLength;
    }
    MTLSize threadgroupSize = MTLSizeMake(threadGroupSize, 1, 1);

    // Encode the compute command.
    [computeEncoder dispatchThreads:gridSize
              threadsPerThreadgroup:threadgroupSize];
*/
    long ngroup = narrayP2 / _mBitonicSort.threadExecutionWidth;
    MTLSize nThreadgroupsPerGrid =   MTLSizeMake(ngroup, 1, 1);
    MTLSize nThreadsPerThreadgroup = MTLSizeMake(_mBitonicSort.threadExecutionWidth, 1, 1);
    [computeEncoder dispatchThreadgroups:nThreadgroupsPerGrid
                   threadsPerThreadgroup:nThreadsPerThreadgroup];

}

- (void) bitonicSortCommand:(int) nArrayPower
                  sortArray:(float *) result
                 indexArray:(int *) index
{
    long narrayP2 = 1 << nArrayPower;

    _sortFBuffer = [_mDevice newBufferWithBytesNoCopy:result
                                               length:(narrayP2 * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    _sortIBuffer = [_mDevice newBufferWithBytesNoCopy:index
                                               length:(narrayP2 * sizeof(int))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];

    int nsend[2];
    int i, j;
    for(i=0;i<nArrayPower;i++){
        for(j=0;j<i+1;j++){
            nsend[0] = i;
            nsend[1] = j;

            // Create a command buffer to hold commands.
            id<MTLCommandBuffer> commandBuffer = [_mCommandQueue commandBuffer];
            assert(commandBuffer != nil);
            // Start a compute pass.
            id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
            assert(computeEncoder != nil);

            // Encode the pipeline state object and its parameters.
            [computeEncoder setComputePipelineState:_mBitonicSort];
            [computeEncoder setBuffer:_sortFBuffer offset:0 atIndex:0];
            [computeEncoder setBuffer:_sortIBuffer offset:0 atIndex:1];
            [computeEncoder setBytes:&nsend[0] length:sizeof(int) atIndex:2];
            [computeEncoder setBytes:&nsend[1] length:sizeof(int) atIndex:3];

            [self encodeSortCommand:computeEncoder
                          arraySize:narrayP2];

            [computeEncoder endEncoding];
            [commandBuffer commit];
            [commandBuffer waitUntilCompleted];
        }
    };
    return;
};

@end
