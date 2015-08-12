!>@file   m_combained_filter_IO.f90
!!@brief  module m_combained_filter_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Array for filter data IO
!!
!!@verbatim
!!      subroutine allocate_num_filtering_IO
!!      subroutine allocate_inod_filter_comb_IO
!!      subroutine allocate_3d_filter_data_IO
!!      subroutine deallocate_num_filtering_IO
!!      subroutine deallocate_inod_filter_comb_IO
!!      subroutine deallocate_3d_filter_data_IO
!!@endverbatim
!
      module m_combained_filter_IO
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: ngrp_nod_filter_IO
      character(len=kchara), allocatable :: grp_name_filter_IO(:)
      integer(kind = kint), allocatable :: num_nod_filter_IO(:)
      integer(kind = kint), allocatable :: istack_nod_filter_IO(:)
!
      integer(kind = kint) :: ntot_nod_filter_IO
      integer(kind = kint), allocatable :: inod_filter_IO(:)
      integer(kind = kint), allocatable :: num_near_nod_filter_IO(:)
      integer(kind = kint), allocatable :: istack_near_nod_filter_IO(:)
!
      integer(kind = kint) :: ntot_near_nod_filter_IO
      integer(kind = kint), allocatable :: inod_near_nod_IO(:)
      real(kind = kreal), allocatable :: filter_func_IO(:)
      real(kind = kreal), allocatable :: filter_weight_IO(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_num_filtering_IO
!
      allocate(grp_name_filter_IO(ngrp_nod_filter_IO))
      allocate(num_nod_filter_IO(ngrp_nod_filter_IO))
      allocate(istack_nod_filter_IO(0:ngrp_nod_filter_IO))
!
      if (ngrp_nod_filter_IO .gt. 0) then
        num_nod_filter_IO = 0
        istack_nod_filter_IO = 0
      end if
!
      end subroutine allocate_num_filtering_IO
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_inod_filter_comb_IO
!
      allocate(inod_filter_IO(ntot_nod_filter_IO))
      allocate(num_near_nod_filter_IO(ntot_nod_filter_IO))
      allocate(istack_near_nod_filter_IO(0:ntot_nod_filter_IO))
!
      if (ntot_nod_filter_IO .gt. 0) then
        inod_filter_IO = 0
        num_near_nod_filter_IO = 0
        istack_near_nod_filter_IO = 0
      end if
!
      end subroutine allocate_inod_filter_comb_IO
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_3d_filter_data_IO
!
!
      allocate(inod_near_nod_IO(ntot_near_nod_filter_IO))
      allocate(filter_func_IO(ntot_near_nod_filter_IO))
      allocate(filter_weight_IO(ntot_near_nod_filter_IO))
!
      if (ntot_near_nod_filter_IO .gt. 0) then
        inod_near_nod_IO = 0
        filter_func_IO = 0.0d0
        filter_weight_IO = 0.0d0
      end if
!
      end subroutine allocate_3d_filter_data_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_num_filtering_IO
!
      deallocate(grp_name_filter_IO)
      deallocate(num_nod_filter_IO)
      deallocate(istack_nod_filter_IO)
!
      end subroutine deallocate_num_filtering_IO
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_inod_filter_comb_IO
!
      deallocate(inod_filter_IO)
      deallocate(num_near_nod_filter_IO)
      deallocate(istack_near_nod_filter_IO)
!
      end subroutine deallocate_inod_filter_comb_IO
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_3d_filter_data_IO
!
      deallocate(inod_near_nod_IO)
      deallocate(filter_func_IO)
      deallocate(filter_weight_IO)
!
      end subroutine deallocate_3d_filter_data_IO
!
!  ---------------------------------------------------------------------
!
      end module m_combained_filter_IO
