!>@file   m_sph_trans_comm_table.f90
!!@brief  module m_sph_trans_comm_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!>@brief  Communication tables for spherical transform
!!
!!@verbatim
!!      subroutine allocate_sph_comm_stack
!!      subroutine allocate_sph_comm_item(nnod_rtp, nnod_rtm,           &
!!     &          nnod_rlm, nnod_rj)
!!
!!      subroutine deallocate_sph_comm_item
!!
!!      subroutine allocate_sph_comm_stack_rtp
!!      subroutine allocate_sph_comm_stack_rtm
!!      subroutine allocate_sph_comm_stack_rlm
!!      subroutine allocate_sph_comm_stack_rj
!!
!!      subroutine allocate_sph_comm_item_rtp(nnod_rtp)
!!      subroutine allocate_sph_comm_item_rtm(nnod_rtm)
!!      subroutine allocate_sph_comm_item_rlm(nnod_rlm)
!!      subroutine allocate_sph_comm_item_rj(nnod_rj)
!!
!!      subroutine deallocate_sph_comm_item_rtm
!!      subroutine deallocate_sph_comm_item_rlm
!!      subroutine deallocate_sph_comm_item_rj
!!@endverbatim
!!
!!@n @param nnod_rtp
!!      number of data points for @f$ f(r,\theta,\phi) @f$
!!@n @param nnod_rtm
!!      number of data points for @f$ f(r,\theta,m) @f$
!!@n @param nnod_rlm
!!      number of data points for @f$ f(r,l,m) @f$
!!@n @param nnod_rj 
!!      number of data points for @f$ f(r,j) @f$
!!
!!@n @param numnod             Number of data points
!!@n @param ntot_item          Number of data for communication
!!@n @param item_sr(ntot_item) Communication table
!!@n @param irev_sr(numnod)  
!!                 Communication table id for local data pointa
!!                (if data point does not need communication, 0 is set)
!
      module m_sph_trans_comm_table
!
      use m_precision
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>        Communication table for @f$ f(r,t,p) @f$ 
      type(sph_comm_tbl), save :: comm_rtp1
!>        Communication table for @f$ f(r,t,m) @f$ 
      type(sph_comm_tbl), save :: comm_rtm1
!>        Communication table for @f$ f(r,l,m) @f$ 
      type(sph_comm_tbl), save :: comm_rlm1
!comm_rlm1%irev_sr
!
!>        Communication table for @f$ f(r,j) @f$ 
      type(sph_comm_tbl), save :: comm_rj1
!
!
!>      number of domain to communicate from @f$ f(r,l,m) @f$ 
!      integer(kind = kint) :: nneib_domain_rlm
!>      total number of data points to communicate
!!      from @f$ f(r,l,m) @f$ 
!      integer(kind = kint) :: ntot_item_sr_rlm
!>      integer flag for transfering data within same process
!!      from @f$ f(r,l,m) @f$ 
!      integer(kind = kint) :: iflag_self_rlm
!>      process IDs to communicate from @f$ f(r,l,m) @f$ 
!      integer(kind = kint), allocatable :: id_domain_rlm(:)
!>      end point for communication to each process
!!      from @f$ f(r,l,m) @f$ 
!      integer(kind = kint), allocatable :: istack_sr_rlm(:)
!>      local data id to communicate from @f$ f(r,l,m) @f$
!      integer(kind = kint), allocatable :: item_sr_rlm(:)
!>      communication table id for local point @f$ f(r,l,m) @f$
!      integer(kind = kint), allocatable :: irev_sr_rlm(:)
!
!>      number of domain to communicate from @f$ f(r,j) @f$ 
      integer(kind = kint) :: nneib_domain_rj
!>      total number of data points to communicate
!!      from @f$ f(r,j) @f$ 
      integer(kind = kint) :: ntot_item_sr_rj
!>      integer flag for transfering data within same process
!!      from @f$ f(r,j) @f$ 
      integer(kind = kint) :: iflag_self_rj
!>      process IDs to communicate from @f$ f(r,j) @f$ 
      integer(kind = kint), allocatable :: id_domain_rj(:)
!>      end point for communication to each process
!!      from @f$ f(r,j) @f$ 
      integer(kind = kint), allocatable :: istack_sr_rj(:)
!>      local data id to communicate from @f$ f(r,j) @f$
      integer(kind = kint), allocatable :: item_sr_rj(:)
!>      communication table id for local point @f$ f(r,j) @f$
      integer(kind = kint), allocatable :: irev_sr_rj(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack
!
!
      call alloc_type_sph_comm_stack(comm_rtp1)
      call alloc_type_sph_comm_stack(comm_rtm1)
      call alloc_type_sph_comm_stack(comm_rlm1)
      call allocate_sph_comm_stack_rj
!
      end subroutine allocate_sph_comm_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item(nnod_rtp, nnod_rtm,             &
     &          nnod_rlm, nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
!
!
      call alloc_type_sph_comm_item(nnod_rtp, comm_rtp1)
      call alloc_type_sph_comm_item(nnod_rtm, comm_rtm1)
      call alloc_type_sph_comm_item(nnod_rlm, comm_rlm1)
      call allocate_sph_comm_item_rj(nnod_rj)
!
      end subroutine allocate_sph_comm_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item
!
!
      call dealloc_type_sph_comm_item(comm_rtp1)
      call dealloc_type_sph_comm_item(comm_rtm1)
      call dealloc_type_sph_comm_item(comm_rlm1)
      call deallocate_sph_comm_item_rj
!
      end subroutine deallocate_sph_comm_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_stack_rj
!
!
      allocate( id_domain_rj(nneib_domain_rj) )
      allocate( istack_sr_rj(0:nneib_domain_rj) )
      if(nneib_domain_rj .gt. 0) id_domain_rj =  0
      istack_sr_rj =  0
      iflag_self_rj = 0
!
      end subroutine allocate_sph_comm_stack_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_comm_item_rj(nnod_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj
!
!
      allocate( item_sr_rj(ntot_item_sr_rj) )
      allocate( irev_sr_rj(nnod_rj) )
      if(ntot_item_sr_rj .gt. 0) item_sr_rj = 0
      if(nnod_rj .gt. 0) irev_sr_rj = 0
!
      end subroutine allocate_sph_comm_item_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_comm_item_rj
!
      deallocate( item_sr_rj, irev_sr_rj )
      deallocate( id_domain_rj )
      deallocate( istack_sr_rj )
!
      end subroutine deallocate_sph_comm_item_rj
!
! -----------------------------------------------------------------------
!
      end module m_sph_trans_comm_table
