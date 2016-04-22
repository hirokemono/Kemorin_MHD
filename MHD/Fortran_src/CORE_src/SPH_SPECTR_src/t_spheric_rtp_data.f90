!>@file   t_spheric_rtp_data.f90
!!@brief  module t_spheric_rtp_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine alloc_type_spheric_param_rtp(sph_rtp)
!!      subroutine alloc_type_sph_1d_index_rtp(sph_rtp)
!!      subroutine alloc_rtp_param_smp(sph_rtp)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!
!!      subroutine dealloc_type_spheric_param_rtp(sph_rtp)
!!      subroutine dealloc_type_sph_1d_index_rtp(sph_rtp)
!!      subroutine dealloc_rtp_param_smp(sph_rtp)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!
!!      subroutine check_type_spheric_param_rtp(my_rank, sph_rtp)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!@endverbatim
!!
!!@n @param  my_rank     Running rank ID
!!
      module t_spheric_rtp_data
!
      use m_precision
      use m_spheric_constants
!
      implicit none
!
!>        structure of index table for @f$ f(r,\theta,\phi) @f$
      type sph_rtp_grid
!>        number of global 1d data points for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: nidx_global_rtp(3)
!>        1d subdomain ID for @f$ f(r,\theta,\phi) @f$ (start from 0)
        integer(kind = kint) :: irank_sph_rtp(3)
!
!>        number of data points for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: nnod_rtp
!
!>        number of 1d data points for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: nidx_rtp(3)
!>        number of increments for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: istep_rtp(3)
!>        1d start address of global data for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: ist_rtp(3)
!>        1d end address of global data for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) :: ied_rtp(3)
!
!>        SMP stack for spectr data @f$ f(r,t,p) @f$
      integer(kind = kint), pointer :: istack_inod_rtp_smp(:)
!
!>        SMP stacks for indexing @f$ r@f$
      integer(kind = kint), pointer :: istack_rtp_kr_smp(:)
!>        SMP stacks for indexing @f$ t @f$
      integer(kind = kint), pointer :: istack_rtp_lt_smp(:)
!>        SMP stacks for indexing @f$ p @f$
      integer(kind = kint), pointer :: istack_rtp_mp_smp(:)
!
!>        SMP stacks for indexing @f$ r, t@f$
      integer(kind = kint), pointer :: istack_rtp_rt_smp(:)
!
!>        Maximum SMP number for spectr data @f$ f(r,t,p) @f$
      integer(kind = kint)  ::  maxirt_rtp_smp =  0
!
!>        global address for each direction @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_global_rtp(:,:)
!
!>        radial global address for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtp_r(:)
!>        meridional global address for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtp_t(:)
!>        zonal global address for @f$ f(r,\theta,\phi) @f$
        integer(kind = kint), pointer :: idx_gl_1d_rtp_p(:,:)
!
!>        1d radius data for @f$ f(r,\theta,\phi) @f$
        real(kind = kreal), pointer :: radius_1d_rtp_r(:)
!>        1 / radius_1d_rtp_r
        real(kind = kreal), pointer :: a_r_1d_rtp_r(:)
      end type sph_rtp_grid
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rtp(sph_rtp)
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      allocate(sph_rtp%idx_global_rtp(sph_rtp%nnod_rtp,3))
      if(sph_rtp%nnod_rtp .gt. 0) sph_rtp%idx_global_rtp = 0
!
      end subroutine alloc_type_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rtp(sph_rtp)
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      integer(kind = kint) :: num
!
      num = sph_rtp%nidx_rtp(1)
      allocate(sph_rtp%idx_gl_1d_rtp_r(num))
      allocate(sph_rtp%radius_1d_rtp_r(num))
      allocate(sph_rtp%a_r_1d_rtp_r(num))
      num = sph_rtp%nidx_rtp(2)
      allocate(sph_rtp%idx_gl_1d_rtp_t(num))
      num = sph_rtp%nidx_rtp(3)
      allocate(sph_rtp%idx_gl_1d_rtp_p(num,2))
!
      if(sph_rtp%nidx_rtp(3) .gt. 0) sph_rtp%idx_gl_1d_rtp_p = 0
      if(sph_rtp%nidx_rtp(2) .gt. 0) sph_rtp%idx_gl_1d_rtp_t = 0
      if(sph_rtp%nidx_rtp(1) .gt. 0) then
        sph_rtp%idx_gl_1d_rtp_r = 0
        sph_rtp%radius_1d_rtp_r = 0.0d0
        sph_rtp%a_r_1d_rtp_r = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_rtp_param_smp(sph_rtp)
!
      use m_machine_parameter
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      allocate(sph_rtp%istack_inod_rtp_smp(0:np_smp))
!
      allocate(sph_rtp%istack_rtp_kr_smp(0:np_smp))
      allocate(sph_rtp%istack_rtp_lt_smp(0:np_smp))
      allocate(sph_rtp%istack_rtp_mp_smp(0:np_smp))
!
      allocate(sph_rtp%istack_rtp_rt_smp(0:np_smp))
!
      sph_rtp%istack_inod_rtp_smp = 0
!
      sph_rtp%istack_rtp_kr_smp = 0
      sph_rtp%istack_rtp_lt_smp = 0
      sph_rtp%istack_rtp_mp_smp = 0
!
      sph_rtp%istack_rtp_rt_smp = 0
!
      end subroutine alloc_rtp_param_smp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rtp(sph_rtp)
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      deallocate(sph_rtp%idx_global_rtp)
!
      end subroutine dealloc_type_spheric_param_rtp
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rtp(sph_rtp)
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      deallocate(sph_rtp%radius_1d_rtp_r)
      deallocate(sph_rtp%a_r_1d_rtp_r)
      deallocate(sph_rtp%idx_gl_1d_rtp_r)
      deallocate(sph_rtp%idx_gl_1d_rtp_t)
      deallocate(sph_rtp%idx_gl_1d_rtp_p)
!
      end subroutine dealloc_type_sph_1d_index_rtp
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_rtp_param_smp(sph_rtp)
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
!
      deallocate(sph_rtp%istack_inod_rtp_smp)
      deallocate(sph_rtp%istack_rtp_kr_smp)
      deallocate(sph_rtp%istack_rtp_lt_smp, sph_rtp%istack_rtp_mp_smp)
      deallocate(sph_rtp%istack_rtp_rt_smp)
!
      end subroutine dealloc_rtp_param_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rtp(my_rank, sph_rtp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint) :: i
!
!
      write(my_rank+50,*) 'irank_sph_rtp ', sph_rtp%irank_sph_rtp(1:3)
      write(my_rank+50,*) 'nidx_rtp ', sph_rtp%nidx_rtp(1:3)
      write(my_rank+50,*) 'nnod_rtp ', sph_rtp%nnod_rtp
!
      write(my_rank+50,*)  'i, idx_global_rtp(r,t,p)'
      do i = 1, sph_rtp%nnod_rtp
        write(my_rank+50,*) i, sph_rtp%idx_global_rtp(i,1:3)
      end do
!
      end subroutine check_type_spheric_param_rtp
!
! -----------------------------------------------------------------------
!
      end module t_spheric_rtp_data
