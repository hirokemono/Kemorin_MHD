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

- (void) prepareSort:(struct sortdata *) _d
{
    _d->sortLength = 1<<23;
    printf("sortLength %ld \n", _d->sortLength);

    _d->nextP2 =  1 + (int) log2((double) (_d->sortLength-1));
    posix_memalign((void**)&_d->floatresult, 4096, _d->sortLength*sizeof(float));
    if(_d->floatresult == NULL){
        printf("malloc error for floatresult\n");
        exit(0);
    };
    posix_memalign((void**)&_d->floatinput, 4096, _d->sortLength*sizeof(float));
    if(_d->floatinput == NULL){
        printf("malloc error for floatinput\n");
        exit(0);
    };
    posix_memalign((void**)&_d->intresult, 4096, _d->sortLength*sizeof(int));
    if(_d->intresult == NULL){
        printf("malloc error for intresult\n");
        exit(0);
    };
    posix_memalign((void**)&_d->intinput, 4096, _d->sortLength*sizeof(int));
    if(_d->intinput == NULL){
        printf("malloc error for intinput\n");
        exit(0);
    };
    for(long i=0;i<_d->sortLength;i++){
        _d->floatresult[i] = -i + ((float) _d->sortLength * 0.5);
        _d->intresult[i] =  (int) ( -i + _d->sortLength-1);
        _d->floatinput[i] = _d->floatresult[i];
        _d->intinput[i] =   _d->intinput[i];
    }
}

void verifySort(struct sortdata *_d)
{
    long nout = _d->sortLength;
    if(_d->sortLength > 256) nout = 128;
    for(long i=0;i<nout;i++){
        printf("%ld %f: %d %f: \n", i, _d->floatinput[i],
               _d->intresult[i], _d->floatresult[i]);
    }
}

int nsend[2];

- (void)encodeSortCommand:(id<MTLComputeCommandEncoder>)computeEncoder
                     data:(struct sortdata *) _d{

    // Encode the pipeline state object and its parameters.
    [computeEncoder setComputePipelineState:_mBitonicSort];
    [computeEncoder setBuffer:_sortFBuffer offset:0 atIndex:0];
    [computeEncoder setBuffer:_sortIBuffer offset:0 atIndex:1];
    [computeEncoder setBytes:&nsend[0] length:sizeof(int) atIndex:2];
    [computeEncoder setBytes:&nsend[1] length:sizeof(int) atIndex:3];
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
    long ngroup = _d->sortLength / _mBitonicSort.threadExecutionWidth;
    MTLSize nThreadgroupsPerGrid =   MTLSizeMake(ngroup, 1, 1);
    MTLSize nThreadsPerThreadgroup = MTLSizeMake(_mBitonicSort.threadExecutionWidth, 1, 1);
    [computeEncoder dispatchThreadgroups:nThreadgroupsPerGrid
                   threadsPerThreadgroup:nThreadsPerThreadgroup];

}

- (void) sendSortCommand:(struct sortdata *) _d
{

    _sortFBuffer = [_mDevice newBufferWithBytesNoCopy:_d->floatresult
                                               length:(_d->sortLength * sizeof(float))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    _sortIBuffer = [_mDevice newBufferWithBytesNoCopy:_d->intresult
                                               length:(_d->sortLength * sizeof(int))
                                              options:MTLResourceStorageModeShared
                                          deallocator:nil];
    
    struct timeval startwtime;
    struct timeval endwtime;
    gettimeofday( &startwtime, NULL );

    for(int i=0;i<_d->nextP2;i++){
        for(int j=0;j<i+1;j++){
            nsend[0] = i;
            nsend[1] = j;

            // Create a command buffer to hold commands.
            id<MTLCommandBuffer> commandBuffer = [_mCommandQueue commandBuffer];
            assert(commandBuffer != nil);
            // Start a compute pass.
            id<MTLComputeCommandEncoder> computeEncoder = [commandBuffer computeCommandEncoder];
            assert(computeEncoder != nil);
            [self encodeSortCommand:computeEncoder
                               data:_d];

            [computeEncoder endEncoding];
            [commandBuffer commit];
            [commandBuffer waitUntilCompleted];
        }
    };
    gettimeofday( &endwtime, NULL );


    verifySort(_d);
    check_sorted_Int(_d->sortLength,   _d->intresult);
    check_sorted_Float(_d->sortLength, _d->floatresult);

    double seq_time = (double)( ( endwtime.tv_usec - startwtime.tv_usec ) / 1.0e6
                               + endwtime.tv_sec - startwtime.tv_sec );
    printf("Bitonic sort by Metal wall clock time = %f\n", seq_time);
}
@end
