!m_filter_func_4_sorting.f90
!      module m_filter_func_4_sorting
!
      module m_filter_func_4_sorting
!
!     Written by H. Matsui on Apr., 2008
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: whole_filter_func(:)
      real(kind = kreal), allocatable :: whole_filter_weight(:)
!
      integer(kind = kint) :: ntot_nod_near_w_filter
      integer(kind = kint) :: nmax_nod_near_w_filter
      integer(kind = kint) :: nmin_nod_near_w_filter
      integer(kind = kint), allocatable :: nnod_near_nod_w_filter(:)
      integer(kind = kint), allocatable :: istack_near_nod_w_filter(:)
      integer(kind = kint), allocatable :: inod_near_nod_w_filter(:)
!
      integer(kind = kint), allocatable :: i_exp_level_w_filter(:)
!
!
      real(kind = kreal), allocatable :: fluid_filter_func(:)
      real(kind = kreal), allocatable :: fluid_filter_weight(:)
!
      integer(kind = kint) :: ntot_nod_near_f_filter
      integer(kind = kint) :: nmax_nod_near_f_filter
      integer(kind = kint) :: nmin_nod_near_f_filter
      integer(kind = kint), allocatable :: nnod_near_nod_f_filter(:)
      integer(kind = kint), allocatable :: istack_near_nod_f_filter(:)
      integer(kind = kint), allocatable :: inod_near_nod_f_filter(:)
!
      integer(kind = kint), allocatable :: i_exp_level_f_filter(:)
!
!      subroutine allocate_whole_filter_stack(intnod_w_fliter)
!      subroutine allocate_whole_filter_coefs
!      subroutine allocate_fluid_filter_stack(intnod_w_fliter)
!      subroutine allocate_fluid_filter_coefs
!
!      subroutine deallocate_whole_filter_coefs
!      subroutine deallocate_whole_filter_func
!      subroutine deallocate_fluid_filter_coefs
!      subroutine deallocate_fluid_filter_func
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_whole_filter_stack(intnod_w_fliter)
!
      integer(kind = kint), intent(in) :: intnod_w_fliter
!
!
      allocate( nnod_near_nod_w_filter(intnod_w_fliter) )
      allocate( istack_near_nod_w_filter(0:intnod_w_fliter) )
      allocate( i_exp_level_w_filter(intnod_w_fliter) )
!
      if(intnod_w_fliter .gt. 0) then
        nnod_near_nod_w_filter =   0
        i_exp_level_w_filter =     0
      end if
      istack_near_nod_w_filter = 0
!
      end subroutine allocate_whole_filter_stack
!
! ----------------------------------------------------------------------
!
      subroutine allocate_whole_filter_coefs
!
!
      allocate( inod_near_nod_w_filter(ntot_nod_near_w_filter) )
      allocate( whole_filter_func(ntot_nod_near_w_filter) )
      allocate( whole_filter_weight(ntot_nod_near_w_filter) )
!
      if(ntot_nod_near_w_filter .gt. 0) then
        inod_near_nod_w_filter = 0
        whole_filter_func =  0.0d0
        whole_filter_weight = 0.0d0
      end if
!
      end subroutine allocate_whole_filter_coefs
!
! ----------------------------------------------------------------------
!
      subroutine allocate_fluid_filter_stack(intnod_w_fliter)
!
      integer(kind = kint), intent(in) :: intnod_w_fliter
!
!
      allocate( nnod_near_nod_f_filter(intnod_w_fliter) )
      allocate( istack_near_nod_f_filter(0:intnod_w_fliter) )
      allocate( i_exp_level_f_filter(intnod_w_fliter) )
!
      if(intnod_w_fliter .gt. 0) then
        nnod_near_nod_f_filter =   0
        i_exp_level_f_filter =     0
      end if
      istack_near_nod_f_filter = 0
!
      end subroutine allocate_fluid_filter_stack
!
! ----------------------------------------------------------------------
!
      subroutine allocate_fluid_filter_coefs
!
      allocate( inod_near_nod_f_filter(ntot_nod_near_f_filter) )
      allocate( fluid_filter_func(ntot_nod_near_f_filter) )
      allocate( fluid_filter_weight(ntot_nod_near_f_filter) )
!
      if(ntot_nod_near_f_filter .gt. 0) then
        inod_near_nod_f_filter = 0
        fluid_filter_func =  0.0d0
        fluid_filter_weight = 0.0d0
      end if
!
      end subroutine allocate_fluid_filter_coefs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_whole_filter_coefs
!
!
      call deallocate_whole_filter_func
!
      deallocate( nnod_near_nod_w_filter )
      deallocate( istack_near_nod_w_filter )
      deallocate( i_exp_level_w_filter )
!
      end subroutine deallocate_whole_filter_coefs
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_fluid_filter_coefs
!
!
      call deallocate_fluid_filter_func
!
      deallocate( nnod_near_nod_f_filter )
      deallocate( istack_near_nod_f_filter )
      deallocate( i_exp_level_f_filter )
!
      end subroutine deallocate_fluid_filter_coefs
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_whole_filter_func
!
      deallocate( inod_near_nod_w_filter )
      deallocate( whole_filter_func )
      deallocate( whole_filter_weight )
!
      end subroutine deallocate_whole_filter_func
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_fluid_filter_func
!
      deallocate( inod_near_nod_f_filter )
      deallocate( fluid_filter_func )
      deallocate( fluid_filter_weight )
!
      end subroutine deallocate_fluid_filter_func
!
! ----------------------------------------------------------------------
!
      end module m_filter_func_4_sorting
