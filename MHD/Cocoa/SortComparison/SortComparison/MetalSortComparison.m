/*
See LICENSE folder for this sample’s licensing information.

Abstract:
A class to manage all of the Metal objects this app creates.
*/

@import Metal;
@import MetalKit;
#import "MetalSortComparison.h"

struct sortdata *_d;

@implementation MetalSortComparison
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



long prod_padding_4096chars(long num_bytes){
    long nsize = 1 + num_bytes / 4096;
    return (4096 * nsize);
};
static long prod_padding_1024floats(int ncomp_buf){
    long nsize = 4 * ncomp_buf;
    return prod_padding_4096chars(nsize);
};

void verifySort(struct sort_float_array *rSort)
{
    long nout = rSort->narrayP2;
    if(rSort->narrayP2 > 256) nout = 128;
    for(long i=0;i<nout;i++){
        printf("%ld %f: %ld %f: \n", i, rSort->org[i],
               rSort->idx[i], rSort->ra[i]);
    }
}


- (void)encodeSortCommand:(id<MTLComputeCommandEncoder>)computeEncoder
                     data:(struct sort_float_array *) rSort
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
    long ngroup = rSort->narrayP2 / _mBitonicSort.threadExecutionWidth;
    MTLSize nThreadgroupsPerGrid =   MTLSizeMake(ngroup, 1, 1);
    MTLSize nThreadsPerThreadgroup = MTLSizeMake(_mBitonicSort.threadExecutionWidth, 1, 1);
    [computeEncoder dispatchThreadgroups:nThreadgroupsPerGrid
                   threadsPerThreadgroup:nThreadsPerThreadgroup];

}

- (void) sendSortCommand:(struct sort_float_array *) rSort;
{
    struct timeval startwtime;
    struct timeval endwtime;
    long i;
    
    int *intinput;
    posix_memalign((void**)&intinput, 4096, rSort->narrayP2*sizeof(int));
    if(intinput == NULL){
        printf("malloc error for intinput\n");
        exit(0);
    };
    for (i=0;i<rSort->Narray;i++) {intinput[i] = (int) i;}
    for (i=rSort->Narray;i<rSort->narrayP2;i++) {intinput[i] = -1;}

    _sortFBuffer = [_mDevice newBufferWithBytesNoCopy:rSort->ra
                                               length:(rSort->narrayP2 * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    _sortIBuffer = [_mDevice newBufferWithBytesNoCopy:intinput
                                               length:(rSort->narrayP2 * sizeof(int))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];

    gettimeofday( &startwtime, NULL );
    int nsend[2];

    for(int i=0;i<rSort->nextP2;i++){
        for(int j=0;j<i+1;j++){
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
                               data:rSort];

            [computeEncoder endEncoding];
            [commandBuffer commit];
            [commandBuffer waitUntilCompleted];
        }
    };
    gettimeofday( &endwtime, NULL );
    for(i=0;i<rSort->narrayP2;i++) {rSort->idx[i] = intinput[i];}
    free(intinput);


    verifySort(rSort);
    check_sorted_Float(rSort->narrayP2, rSort->ra);

    double seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                               + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic sort by Metal wall clock time = %f\n", seq_time);
}
@end
