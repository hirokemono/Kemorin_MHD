/*
 bitonic_sort_pthread.c

 This file contains two different implementations of the bitonic sort
        imperative version :  OMPimp_int_BitonicSort(int nthreads, long num, int *ires, long *idx);
        imperative version :  OMPimp_long_BitonicSort(int nthreads, long num, long *lres, long *idx);

        imperative version :  OMPimp_double_BitonicSort(int nthreads, long num, double *dres, long *idx);
 

 The bitonic sort is also known as Batcher Sort. 
 For a reference of the algorithm, see the article titled 
 Sorting networks and their applications by K. E. Batcher in 1968 


 The following codes take references to the codes avaiable at 

 http://www.cag.lcs.mit.edu/streamit/results/bitonic/code/c/bitonic.c

 http://www.tools-of-computing.com/tc/CS/Sorts/bitonic_sort.htm

 http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm 
 */

#include "bitonic_sort_pthread.h"

/** Procedure bitonic_Int_Merge
 *  Same as serial, but uses pthreads.
 **/

static void * max_Int_Array_each_thread(void *args)
{
    int id = ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int lo = ((args_pthread *) args)->lo;
    int hi = ((args_pthread *) args)->hi;
    int *ires = ((args_pthread *) args)->ires;
    int *imax = ((args_pthread *) args)->imax;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    imax[id] = ires[start];
    for(int i=start+1;i<end;i++){
        if(ires[i] > imax[id]){imax[id] = ires[i];};
    };
    return 0;
}

static void * max_Long_Array_each_thread(void *args)
{
    int id = ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int lo = ((args_pthread *) args)->lo;
    int hi = ((args_pthread *) args)->hi;
    long *lres = ((args_pthread *) args)->lres;
    long *lmax = ((args_pthread *) args)->lmax;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    lmax[id] = lres[start];
    for(int i=start+1;i<end;i++){
        if(lres[i] > lmax[id]){lmax[id] = lres[i];};
    };
    return 0;
}

static void * max_Double_Array_each_thread(void *args)
{
    int id = ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int lo = ((args_pthread *) args)->lo;
    int hi = ((args_pthread *) args)->hi;
    double *dres = ((args_pthread *) args)->dres;
    double *dmax = ((args_pthread *) args)->dmax;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    dmax[id] = dres[start];
    for(int i=start+1;i<end;i++){
        if(dres[i] > dmax[id]){dmax[id] = dres[i];};
    };
    return 0;
}

static void * max_Float_Array_each_thread(void *args)
{
    int id = ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int lo = ((args_pthread *) args)->lo;
    int hi = ((args_pthread *) args)->hi;
    float *res =  ((args_pthread *) args)->res;
    long  *imax = ((args_pthread *) args)->idx;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    imax[id] = res[start];
    for(int i=start+1;i<end;i++){
        if(res[i] > imax[id]){imax[id] = res[i];};
    };
    return 0;
}


int max_Int_Array_pthreads(int nthreads, long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    int *imax = (int *) malloc (nthreads * sizeof(int));
    if (!imax) {fprintf (stderr, "Malloc failed for imax.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].lo = 0;
        args[i].hi = num;
        args[i].ires = ires;
        args[i].imax = imax;
        pthread_create(&thread_handles[i], NULL, max_Int_Array_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    return max_int_array((long) nthreads, imax);
}

int max_Long_Array_pthreads(int nthreads, long num, long *lres){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    long *lmax = (long *) malloc (nthreads * sizeof(long));
    if (!lmax) {fprintf (stderr, "Malloc failed for lmax.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].lo = 0;
        args[i].hi = num;
        args[i].lres = lres;
        args[i].lmax =  lmax;
        pthread_create(&thread_handles[i], NULL, max_Long_Array_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    
    return max_long_array((long) nthreads, lmax);
}

int max_Double_Array_pthreads(int nthreads, long num, double *dres){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    double *dmax = (double *) malloc (nthreads * sizeof(double));
    if (!dmax) {fprintf (stderr, "Malloc failed for dmax.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].lo = 0;
        args[i].hi = num;
        args[i].dres = dres;
        args[i].dmax = dmax;
        pthread_create(&thread_handles[i], NULL, max_Double_Array_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    
    return max_double_array((long) nthreads, dmax);
}

int max_Float_Array_pthreads(int nthreads, long num, float *res){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    float *rmax = (float *) malloc (nthreads * sizeof(float));
    if (!rmax) {fprintf (stderr, "Malloc failed for rmax.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].lo = 0;
        args[i].hi = num;
        args[i].res = res;
        args[i].rmax = rmax;
        pthread_create(&thread_handles[i], NULL, max_Float_Array_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {
        pthread_join(thread_handles[i], NULL);
    }
    free(args);
    
    return max_float_array((long) nthreads, rmax);
}


static void * flip_sign_Int_each_thread(void *args)
{
    int id =       ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int hi =       ((args_pthread *) args)->hi;
    int *ires =    ((args_pthread *) args)->ires;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    for(int i=start+1;i<end;i++){ires[i] = -ires[i];};
    return 0;
}

static void * flip_sign_Long_each_thread(void *args)
{
    int id =       ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int hi =       ((args_pthread *) args)->hi;
    long *lres =   ((args_pthread *) args)->lres;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    for(int i=start+1;i<end;i++){lres[i] = -lres[i];};
    return 0;
}

static void * flip_sign_Double_each_thread(void *args)
{
    int id =       ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int hi =       ((args_pthread *) args)->hi;
    double *dres = ((args_pthread *) args)->dres;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    for(int i=start+1;i<end;i++){dres[i] = -dres[i];};
    return 0;
}

static void * flip_sign_Float_each_thread(void *args)
{
    int id =       ((args_pthread *) args)->id;
    int nthreads = ((args_pthread *) args)->nthreads;
    int hi =       ((args_pthread *) args)->hi;
    float *res =   ((args_pthread *) args)->res;
    
    /* Get portion of array to process. */
    int start = hi * id /     nthreads; /* Thread is in charge of [start, start+n] elements */
    int end =   hi * (id+1) / nthreads; /* Thread is in charge of [start, start+n] elements */
    
//    printf("start %d %d %d \n",id, start, end);
    for(int i=start+1;i<end;i++){res[i] = -res[i];};
    return 0;
}



void flip_sign_Int_pthreads(int nthreads, long num, int *ires){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].hi = num;
        args[i].ires = ires;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_Int_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {pthread_join(thread_handles[i], NULL);}
    free(args);
    return;
}

void flip_sign_Long_pthreads(int nthreads, long num, long *lres){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].hi = num;
        args[i].lres = lres;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_Long_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {pthread_join(thread_handles[i], NULL);}
    free(args);
    return;
}

void flip_sign_Double_pthreads(int nthreads, long num, double *dres){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].hi = num;
        args[i].dres = dres;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_Double_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {pthread_join(thread_handles[i], NULL);}
    free(args);
    return;
}

void flip_sign_Float_pthreads(int nthreads, long num, float *res){
    int i;
    /* Allocate thread arguments. */
    args_pthread *args = (args_pthread *) malloc (nthreads * sizeof(args_pthread));
    if (!args) {fprintf (stderr, "Malloc failed for args_pthread.\n"); exit(1);}
    /* Thread-related variables. */
    long thread;
    pthread_t* thread_handles;
    
/* Initialize thread handles and barrier. */
    thread_handles = malloc (nthreads * sizeof(pthread_t));
    if (!thread_handles) {fprintf (stderr, "Malloc failed for thread_handles.\n"); exit(1);}
    
    for (i=0;i<nthreads;i++) {
        args[i].id = i;
        args[i].nthreads = nthreads;
        args[i].hi = num;
        args[i].res = res;
/* Create a thread. */
//        printf ("####### CREATING THREAD id = %d\n", args[i].id);
        pthread_create(&thread_handles[i], NULL, flip_sign_Float_each_thread, &args[i]);
    }
    
    for (i=1;i<nthreads;i++) {pthread_join(thread_handles[i], NULL);}
    free(args);
    return;
}




static void * bitonicMerge_Int_pthread(void *arg){
    long i;
    
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer = ((args_pthread *) arg)->layer;
    int *ires = ((args_pthread *) arg)->ires;
    long *idx = ((args_pthread *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (ires[i]>ires[i+k])){
                exchange(&ires[i], &ires[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonic_Int_Merge(lo,     k, iflag_ascend, ires, idx);
            bitonic_Int_Merge((lo+k), k, iflag_ascend, ires, idx);
            return 0;
        }
        
        pthread_t thread1;
        args_pthread arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.ires = ires;
        arg1.idx =  idx;
        
        pthread_t thread2;
        args_pthread arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.ires = ires;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, bitonicMerge_Int_pthread, &arg1 );
        pthread_create( &thread2, NULL, bitonicMerge_Int_pthread, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}


static void * bitonicMerge_Long_pthread(void *arg){
    long i;
    
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer =  ((args_pthread *) arg)->layer;
    long *lres = ((args_pthread *) arg)->lres;
    long *idx =  ((args_pthread *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (lres[i]>lres[i+k])){
                exchange_long(&lres[i], &lres[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonic_Long_Merge(lo,     k, iflag_ascend, lres, idx);
            bitonic_Long_Merge((lo+k), k, iflag_ascend, lres, idx);
            return 0;
        }
        
        pthread_t thread1;
        args_pthread arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.lres = lres;
        arg1.idx =  idx;
        
        pthread_t thread2;
        args_pthread arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.lres = lres;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, bitonicMerge_Long_pthread, &arg1 );
        pthread_create( &thread2, NULL, bitonicMerge_Long_pthread, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}

static void * bitonicMerge_Double_pthread(void *arg){
    long i;
    
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer =  ((args_pthread *) arg)->layer;
    double *dres = ((args_pthread *) arg)->dres;
    long *idx =  ((args_pthread *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (dres[i]>dres[i+k])){
                exchange_double(&dres[i], &dres[i+k]);
                exchange_long(&idx[i], &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonic_Double_Merge(lo,     k, iflag_ascend, dres, idx);
            bitonic_Double_Merge((lo+k), k, iflag_ascend, dres, idx);
            return 0;
        }
        
        pthread_t thread1;
        args_pthread arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.dres = dres;
        arg1.idx =  idx;
        
        pthread_t thread2;
        args_pthread arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.dres = dres;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, bitonicMerge_Double_pthread, &arg1 );
        pthread_create( &thread2, NULL, bitonicMerge_Double_pthread, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}

static void * bitonicMerge_Float_pthread(void *arg){
    long i;
    
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer =  ((args_pthread *) arg)->layer;
    float *res = ((args_pthread *) arg)->res;
    long *idx =  ((args_pthread *) arg)->idx;
    
    if( hi > 1 ){
        long k = hi / 2;
        for(i=lo;i<(lo+k);++i){
            if (iflag_ascend == (res[i]>res[i+k])){
                exchange_float(&res[i], &res[i+k]);
                exchange_long(&idx[i],   &idx[i+k]);
            };
        }
        if( layer <= 0 ){
            bitonic_Float_Merge(lo,     k, iflag_ascend, res, idx);
            bitonic_Float_Merge((lo+k), k, iflag_ascend, res, idx);
            return 0;
        }
        
        pthread_t thread1;
        args_pthread arg1;
        arg1.lo = lo;
        arg1.hi = k;
        arg1.iflag_ascend = iflag_ascend;
        arg1.maxlayers =    maxlayers;
        arg1.layer = layer - 1;
        arg1.res = res;
        arg1.idx =  idx;
        
        pthread_t thread2;
        args_pthread arg2;
        arg2.lo = lo + k;
        arg2.hi = k;
        arg2.iflag_ascend = iflag_ascend;
        arg2.maxlayers =    maxlayers;
        arg2.layer = layer - 1;
        arg2.res = res;
        arg2.idx =  idx;
        
        pthread_create( &thread1, NULL, bitonicMerge_Float_pthread, &arg1 );
        pthread_create( &thread2, NULL, bitonicMerge_Float_pthread, &arg2 );
        
        pthread_join(thread1, NULL);
        pthread_join(thread2, NULL);
    }
    return 0;
}





/** function rec_Int_BitonicSort_pthread() 
    first produces a bitonic sequence by recursively sorting 
    its two halves in opposite sorting orders, and then
    calls bitonic_Int_Merge to make them in the same order 

    Uses pthreads
 **/

static void * rec_Int_BitonicSort_pthread(void *arg){
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer = ((args_pthread *) arg)->layer;
    int *ires = ((args_pthread *) arg)->ires;
    long *idx = ((args_pthread *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_int_c(&ires[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_int_sign(k, &ires[lo+k]);
            quicksort_int_c(&ires[lo+k], &idx[lo+k], 0, (k-1));
            flip_int_sign(k, &ires[lo+k]);
        }
        else{
            args_pthread arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.ires = ires;
            arg1.idx =  idx;
            
            args_pthread arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.ires = ires;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, rec_Int_BitonicSort_pthread, &arg1);
            pthread_create(&thread2, NULL, rec_Int_BitonicSort_pthread, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        args_pthread arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.ires = ires;
        arg3.idx =  idx;
        bitonicMerge_Int_pthread(&arg3);
    }
    return 0;
}

static void * rec_Long_BitonicSort_pthread(void *arg){
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer = ((args_pthread *) arg)->layer;
    long *lres = ((args_pthread *) arg)->lres;
    long *idx =  ((args_pthread *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_int8_c(&lres[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_long_sign(k, &lres[lo+k]);
            quicksort_int8_c(&lres[lo+k], &idx[lo+k], 0, (k-1));
            flip_long_sign(k, &lres[lo+k]);
        }
        else{
            args_pthread arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.lres = lres;
            arg1.idx =  idx;
            
            args_pthread arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.lres = lres;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, rec_Long_BitonicSort_pthread, &arg1);
            pthread_create(&thread2, NULL, rec_Long_BitonicSort_pthread, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        args_pthread arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.lres = lres;
        arg3.idx =  idx;
        bitonicMerge_Long_pthread(&arg3);
    }
    return 0;
}

static void * rec_Double_BitonicSort_pthread(void *arg){
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer = ((args_pthread *) arg)->layer;
    double *dres = ((args_pthread *) arg)->dres;
    long *idx =  ((args_pthread *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_double_c(&dres[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_double_sign(k, &dres[lo+k]);
            quicksort_double_c(&dres[lo+k], &idx[lo+k], 0, (k-1));
            flip_double_sign(k, &dres[lo+k]);
        }
        else{
            args_pthread arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.dres = dres;
            arg1.idx =  idx;
            
            args_pthread arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.dres = dres;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, rec_Double_BitonicSort_pthread, &arg1);
            pthread_create(&thread2, NULL, rec_Double_BitonicSort_pthread, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        args_pthread arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.dres = dres;
        arg3.idx =  idx;
        bitonicMerge_Double_pthread(&arg3);
    }
    return 0;
}

static void * rec_Float_BitonicSort_pthread(void *arg){
    long lo = ((args_pthread *) arg)->lo;
    long hi = ((args_pthread *) arg)->hi;
    int iflag_ascend = ((args_pthread *) arg)->iflag_ascend;
    int maxlayers =    ((args_pthread *) arg)->maxlayers;
    int layer = ((args_pthread *) arg)->layer;
    float *res = ((args_pthread *) arg)->res;
    long *idx =  ((args_pthread *) arg)->idx;
    if ( hi > 1 ) {
        long k = hi / 2;
        if(layer >= maxlayers) {
            quicksort_real_c(&res[lo  ], &idx[lo  ], 0, (k-1));
            
            flip_float_sign(k, &res[lo+k]);
            quicksort_real_c(&res[lo+k], &idx[lo+k], 0, (k-1));
            flip_float_sign(k, &res[lo+k]);
        }
        else{
            args_pthread arg1;
            pthread_t thread1;
            arg1.lo = lo;
            arg1.hi = k;
            arg1.iflag_ascend = ASCENDING;
            arg1.maxlayers =    maxlayers;
            arg1.layer = layer + 1;
            arg1.res = res;
            arg1.idx =  idx;
            
            args_pthread arg2;
            pthread_t thread2;
            arg2.lo = lo + k;
            arg2.hi = k;
            arg2.iflag_ascend = DESCENDING;
            arg2.maxlayers =    maxlayers;
            arg2.layer = layer + 1;
            arg2.res = res;
            arg2.idx =  idx;
            
            pthread_create(&thread1, NULL, rec_Float_BitonicSort_pthread, &arg1);
            pthread_create(&thread2, NULL, rec_Float_BitonicSort_pthread, &arg2);
            
            pthread_join(thread1, NULL);
            pthread_join(thread2, NULL);
        }
        args_pthread arg3;
        arg3.lo = lo;
        arg3.hi = hi;
        arg3.iflag_ascend = iflag_ascend;
        arg3.maxlayers =    maxlayers;
        arg3.layer = maxlayers - layer;
        arg3.res = res;
        arg3.idx = idx;
        bitonicMerge_Float_pthread(&arg3);
    }
    return 0;
}



/** function sort() 
   Caller of rec_Int_BitonicSort for sorting the entire array of length N 
   in ASCENDING order
 **/
void bitonicsort_Int_Pthread(int nthreads, long num, int *ires, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    args_pthread arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.ires = ires;
    arg.idx = idx;
    
    rec_Int_BitonicSort_pthread(&arg);
}

void bitonicsort_Long_Pthread(int nthreads, long num, long *lres, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    args_pthread arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.lres = lres;
    arg.idx = idx;
    
    rec_Long_BitonicSort_pthread(&arg);
}

void bitonicsort_Double_Pthread(int nthreads, long num, double *dres, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    args_pthread arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.dres = dres;
    arg.idx = idx;
    
    rec_Double_BitonicSort_pthread(&arg);
}

void bitonicsort_Float_Pthread(int nthreads, long num, float *res, long *idx){
    int threadlayers = 0;
    if(nthreads != 0 && nthreads != 1) {
        threadlayers = 1 + (int) log2((double) (nthreads-1));
    }
    
    args_pthread arg;
    arg.lo = 0;
    arg.hi = (int) num;
    arg.iflag_ascend = ASCENDING;
    arg.maxlayers = threadlayers;
    arg.layer = 0;
    arg.res = res;
    arg.idx = idx;
    
    rec_Float_BitonicSort_pthread(&arg);
}

