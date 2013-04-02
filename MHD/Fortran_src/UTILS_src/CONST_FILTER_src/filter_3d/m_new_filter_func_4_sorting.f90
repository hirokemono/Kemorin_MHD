!m_new_filter_func_4_sorting.f90
!      module m_new_filter_func_4_sorting
!
      module m_new_filter_func_4_sorting
!
!     Written by H. Matsui on Apr., 2008
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: intnod_w_fliter2
!
      integer(kind = kint), allocatable :: inod_filter_new_2(:)
      real(kind = kreal), allocatable :: whole_filter_func2(:)
      real(kind = kreal), allocatable :: whole_filter_weight2(:)
!
      integer(kind = kint) :: ntot_nod_near_w_filter2
      integer(kind = kint) :: nmax_nod_near_w_filter2
      integer(kind = kint) :: nmin_nod_near_w_filter2
      integer(kind = kint), allocatable :: nnod_near_nod_w_filter2(:)
      integer(kind = kint), allocatable :: istack_near_nod_w_filter2(:)
      integer(kind = kint), allocatable :: inod_near_nod_w_filter2(:)
!
      integer(kind = kint), allocatable :: i_exp_level_w_filter2(:)
!
!
      real(kind = kreal), allocatable :: fluid_filter_func2(:)
      real(kind = kreal), allocatable :: fluid_filter_weight2(:)
!
      integer(kind = kint) :: ntot_nod_near_f_filter2
      integer(kind = kint) :: nmax_nod_near_f_filter2
      integer(kind = kint) :: nmin_nod_near_f_filter2
      integer(kind = kint), allocatable :: nnod_near_nod_f_filter2(:)
      integer(kind = kint), allocatable :: istack_near_nod_f_filter2(:)
      integer(kind = kint), allocatable :: inod_near_nod_f_filter2(:)
!
      integer(kind = kint), allocatable :: i_exp_level_f_filter2(:)
!
!      subroutine allocate_whole_filter_stack2
!      subroutine allocate_whole_filter_coefs2
!      subroutine allocate_fluid_filter_stack2
!      subroutine allocate_fluid_filter_coefs2
!
!      subroutine deallocate_whole_filter_coefs2
!      subroutine deallocate_fluid_filter_coefs2
!      subroutine deallocate_whole_filter_func2
!      subroutine deallocate_fluid_filter_func2
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_whole_filter_stack2
!
!
      allocate( inod_filter_new_2(intnod_w_fliter2) )
      allocate( nnod_near_nod_w_filter2(intnod_w_fliter2) )
      allocate( istack_near_nod_w_filter2(0:intnod_w_fliter2) )
      allocate( i_exp_level_w_filter2(intnod_w_fliter2) )
!
      inod_filter_new_2 = 0
      nnod_near_nod_w_filter2 = 0
      istack_near_nod_w_filter2 = 0
      i_exp_level_w_filter2 = 0
!
      end subroutine allocate_whole_filter_stack2
!
! ----------------------------------------------------------------------
!
      subroutine allocate_whole_filter_coefs2
!
      allocate( inod_near_nod_w_filter2(ntot_nod_near_w_filter2) )
      allocate( whole_filter_func2(ntot_nod_near_w_filter2) )
      allocate( whole_filter_weight2(ntot_nod_near_w_filter2) )
!
      inod_near_nod_w_filter2 = 0
      whole_filter_func2 =  0.0d0
      whole_filter_weight2 = 0.0d0
!
      end subroutine allocate_whole_filter_coefs2
!
! ----------------------------------------------------------------------
!
      subroutine allocate_fluid_filter_stack2
!
!
      allocate( nnod_near_nod_f_filter2(intnod_w_fliter2) )
      allocate( istack_near_nod_f_filter2(0:intnod_w_fliter2) )
      allocate( i_exp_level_f_filter2(intnod_w_fliter2) )
!
      nnod_near_nod_f_filter2 = 0
      istack_near_nod_f_filter2 = 0
      i_exp_level_f_filter2 = 0
!
      end subroutine allocate_fluid_filter_stack2
!
! ----------------------------------------------------------------------
!
      subroutine allocate_fluid_filter_coefs2
!
      allocate( inod_near_nod_f_filter2(ntot_nod_near_f_filter2) )
      allocate( fluid_filter_func2(ntot_nod_near_f_filter2) )
      allocate( fluid_filter_weight2(ntot_nod_near_f_filter2) )
!
      inod_near_nod_f_filter2 = 0
      fluid_filter_func2 =  0.0d0
      fluid_filter_weight2 = 0.0d0
!
      end subroutine allocate_fluid_filter_coefs2
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_whole_filter_coefs2
!
!
      call deallocate_whole_filter_func2
!
      deallocate( inod_filter_new_2 )
      deallocate( nnod_near_nod_w_filter2 )
      deallocate( istack_near_nod_w_filter2 )
      deallocate( i_exp_level_w_filter2 )
!
      end subroutine deallocate_whole_filter_coefs2
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_fluid_filter_coefs2
!
!
      call deallocate_fluid_filter_func2
!
      deallocate( nnod_near_nod_f_filter2 )
      deallocate( istack_near_nod_f_filter2 )
      deallocate( i_exp_level_f_filter2 )
!
      end subroutine deallocate_fluid_filter_coefs2
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_whole_filter_func2
!
      deallocate( inod_near_nod_w_filter2 )
      deallocate( whole_filter_func2 )
      deallocate( whole_filter_weight2 )
!
      end subroutine deallocate_whole_filter_func2
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_fluid_filter_func2
!
      deallocate( inod_near_nod_f_filter2 )
      deallocate( fluid_filter_func2 )
      deallocate( fluid_filter_weight2 )
!
      end subroutine deallocate_fluid_filter_func2
!
! ----------------------------------------------------------------------
!
      end module m_new_filter_func_4_sorting
