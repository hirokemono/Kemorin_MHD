!>@file   t_spheric_rtm_data.f90
!!@brief  module t_spheric_rtm_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine alloc_type_spheric_param_rtm(sph_rtm)
!!      subroutine alloc_type_sph_1d_index_rtm(sph_rtm)
!!      subroutine alloc_rtm_param_smp(sph_rtm)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine dealloc_type_spheric_param_rtm(sph_rtm)
!!      subroutine dealloc_type_sph_1d_index_rtm(sph_rtm)
!!      subroutine dealloc_rtm_param_smp(sph_rtm)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!
!!      subroutine check_type_spheric_param_rtm(my_rank, sph_rtm)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!@endverbatim
!!
!!@n @param  my_rank     Running rank ID
!!
      module t_spheric_rtm_data
!
      use m_precision
      use m_spheric_constants
!
      implicit none
!
!
!>        structure of index table for @f$ f(r,\theta,m) @f$
      type sph_rtm_grid
!>        Start address for @f$ m = 0 @f$ for @f$ f(r,\theta,m) @f$
        integer (kind=kint) :: ist_rtm_order_zero = 0
!>        Start address for @f$ l=1, m=-1 @f$ for @f$ f(r,\theta,m) @f$
        integer (kind=kint) :: ist_rtm_order_1s =   0
!>        Start address for @f$ l=1, m= 1 @f$ for @f$ f(r,\theta,m) @f$
        integer (kind=kint) :: ist_rtm_order_1c =   0
!
!>        number of global 1d data points for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: nidx_global_rtm(3)
!>        1d subdomain ID for @f$ f(r,\theta,m) @f$ (start from 0)
        integer(kind = kint) :: irank_sph_rtm(3)
!
!>        number of data points for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: nnod_rtm
!>        number of 1d data points for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: nidx_rtm(3)
!>        number of increments for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: istep_rtm(3)
!>        1d start address of global data for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: ist_rtm(3)
!>        1d end address of global data for @f$ f(r,\theta,m) @f$
        integer(kind = kint) :: ied_rtm(3)
!
!>        SMP stack for spectr data @f$ f(r,t,m) @f$
      integer(kind = kint), pointer :: istack_inod_rtm_smp(:)
!
!>        SMP stacks for indexing @f$ r@f$
      integer(kind = kint), pointer :: istack_rtm_kr_smp(:)
!>        SMP stacks for indexing @f$ t @f$
      integer(kind = kint), pointer :: istack_rtm_lt_smp(:)
!>        SMP stacks for indexing @f$ m @f$
      integer(kind = kint), pointer :: istack_rtm_m_smp(:)
!
!>        SMP stacks for indexing @f$ r, t@f$
      integer(kind = kint), pointer :: istack_rtm_rt_smp(:)
!
!>        Maximum SMP number for indexing @f$ r, t, m@f$
        integer(kind = kint)  ::  maxidx_rtm_smp(3) = (/0,0,0/)
!
!>        global address for each direction @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_global_rtm(:,:)
!
!>        radial global address for @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtm_r(:)
!>        meridional global address for @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtm_t(:)
!>        Zonal wave number for @f$ f(r,\theta,m) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtm_m(:,:)
!
!>        1d radius data for @f$ f(r,\theta,m) @f$
        real(kind = kreal), pointer :: radius_1d_rtm_r(:)
!>        1 / radius_1d_rtm_r
        real(kind = kreal), pointer :: a_r_1d_rtm_r(:)
      end type sph_rtm_grid
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rtm(sph_rtm)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      allocate(sph_rtm%idx_global_rtm(sph_rtm%nnod_rtm,3))
!
      if(sph_rtm%nnod_rtm .gt. 0) sph_rtm%idx_global_rtm = 0
!
      end subroutine alloc_type_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rtm(sph_rtm)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      integer(kind = kint) :: num
!
      num = sph_rtm%nidx_rtm(1)
      allocate(sph_rtm%idx_gl_1d_rtm_r(num))
      allocate(sph_rtm%radius_1d_rtm_r(num))
      allocate(sph_rtm%a_r_1d_rtm_r(num))
      num = sph_rtm%nidx_rtm(2)
      allocate(sph_rtm%idx_gl_1d_rtm_t(num))
      num = sph_rtm%nidx_rtm(3)
      allocate(sph_rtm%idx_gl_1d_rtm_m(num,2))
!
      if(sph_rtm%nidx_rtm(3) .gt. 0) sph_rtm%idx_gl_1d_rtm_m = 0
      if(sph_rtm%nidx_rtm(2) .gt. 0) sph_rtm%idx_gl_1d_rtm_t = 0
      if(sph_rtm%nidx_rtm(1) .gt. 0) then
        sph_rtm%idx_gl_1d_rtm_r = 0
        sph_rtm%radius_1d_rtm_r = 0.0d0
        sph_rtm%a_r_1d_rtm_r = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_rtm_param_smp(sph_rtm)
!
      use m_machine_parameter
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      allocate(sph_rtm%istack_inod_rtm_smp(0:np_smp))
!
      allocate(sph_rtm%istack_rtm_kr_smp(0:np_smp))
      allocate(sph_rtm%istack_rtm_lt_smp(0:np_smp))
      allocate(sph_rtm%istack_rtm_m_smp(0:np_smp))
!
      allocate(sph_rtm%istack_rtm_rt_smp(0:np_smp))
!
      sph_rtm%istack_inod_rtm_smp = 0
!
      sph_rtm%istack_rtm_kr_smp = 0
      sph_rtm%istack_rtm_lt_smp = 0
      sph_rtm%istack_rtm_m_smp = 0
!
      sph_rtm%istack_rtm_rt_smp = 0
!
      end subroutine alloc_rtm_param_smp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rtm(sph_rtm)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
      deallocate(sph_rtm%idx_global_rtm)
!
      end subroutine dealloc_type_spheric_param_rtm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rtm(sph_rtm)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      deallocate(sph_rtm%radius_1d_rtm_r)
      deallocate(sph_rtm%a_r_1d_rtm_r)
      deallocate(sph_rtm%idx_gl_1d_rtm_r)
      deallocate(sph_rtm%idx_gl_1d_rtm_t)
      deallocate(sph_rtm%idx_gl_1d_rtm_m)
!
      end subroutine dealloc_type_sph_1d_index_rtm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_rtm_param_smp(sph_rtm)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      deallocate(sph_rtm%istack_inod_rtm_smp)
      deallocate(sph_rtm%istack_rtm_kr_smp)
      deallocate(sph_rtm%istack_rtm_lt_smp, sph_rtm%istack_rtm_m_smp)
      deallocate(sph_rtm%istack_rtm_rt_smp)
!
      end subroutine dealloc_rtm_param_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rtm(my_rank, sph_rtm)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rtm_grid), intent(in) :: sph_rtm
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'irank_sph_rtm ', sph_rtm%irank_sph_rtm(1:3)
      write(my_rank+50,*) 'nidx_rtm ', sph_rtm%nidx_rtm(1:3)
      write(my_rank+50,*) 'nnod_rtm ', sph_rtm%nnod_rtm
!
      write(my_rank+50,*) 'i, idx_global_rtm(r,t,p)'
      do i = 1, sph_rtm%nnod_rtm
        write(my_rank+50,*) i, sph_rtm%idx_global_rtm(i,1:3)
      end do
!
      end subroutine check_type_spheric_param_rtm
!
! -----------------------------------------------------------------------
!
      end module t_spheric_rtm_data
