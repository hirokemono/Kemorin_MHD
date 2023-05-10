!>@file   t_circle_transform.f90
!!@brief  module t_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine alloc_circle_transform(ltr, circ_spec)
!!      subroutine dealloc_circle_transform(circ_spec)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!
      module t_circle_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
!
      type circle_transform_spetr
!>        Truncation level for spherical transform at equator
        integer(kind = kint) :: ltr_circle
!>        end address of SMP parallelization for scalar Fourier transform
        integer(kind = kint), allocatable :: istack_circfft_smp(:)
!
!>        Radius for specific circle
        real(kind = kreal) :: r_circle
!>        @f$ 1/ r @f$ for specific circle
        real(kind = kreal) :: ar_circle
!>        @f$ 1/ r^{2} @f$ for specific circle
        real(kind = kreal) :: ar2_circle
!
!>        colatitude for specific circle
        real(kind = kreal) :: theta_circle
!
!>        global sphrical harmonics corfs on circle
        real(kind = kreal), allocatable :: d_circ_gl(:,:)
!>        Local sphrical harmonics corfs on circle
        real(kind = kreal), allocatable :: d_circ_lc(:,:)
!
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_mag(:,:)
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_phase(:,:)
      end type circle_transform_spetr
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_circle_transform(ltr, circ_spec)
!
      integer(kind = kint), intent(in) :: ltr
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      circ_spec%ltr_circle =  ltr
!
      allocate(circ_spec%istack_circfft_smp(0:np_smp))
      circ_spec%istack_circfft_smp(0) =        0
      circ_spec%istack_circfft_smp(1:np_smp) = 1
!
      end subroutine alloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_circle_transform                            &
     &         (my_rank, d_circle, circ_spec)
!
      integer, intent(in) :: my_rank
      type(phys_data), intent(in) :: d_circle
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
      integer(kind = kint) :: ltr, ntot_comp
!
!
      ntot_comp = d_circle%ntot_phys
      ltr =       circ_spec%ltr_circle
      allocate(circ_spec%d_circ_gl(-ltr:ltr, ntot_comp))
      allocate(circ_spec%d_circ_lc(-ltr:ltr, ntot_comp))
!
      allocate(circ_spec%vrtm_mag(0:ltr,ntot_comp))
      allocate(circ_spec%vrtm_phase(0:ltr,ntot_comp))
!
!
      if((ltr*ntot_comp) .le. 0) return
!
!$omp parallel workshare
      circ_spec%d_circ_gl(-ltr:ltr, 1:ntot_comp) = 0.0d0
      circ_spec%d_circ_lc(-ltr:ltr, 1:ntot_comp) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel workshare
      circ_spec%vrtm_mag(0:ltr,1:ntot_comp) =   0.0d0
      circ_spec%vrtm_phase(0:ltr,1:ntot_comp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_work_circle_transform
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_transform(circ_spec)
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      deallocate(circ_spec%istack_circfft_smp)
!
      end subroutine dealloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_circle_transform(circ_spec)
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      if(allocated(circ_spec%vrtm_mag) .eqv. .FALSE.) return
      deallocate(circ_spec%vrtm_mag, circ_spec%vrtm_phase)
      deallocate(circ_spec%d_circ_gl, circ_spec%d_circ_lc)
!
      end subroutine dealloc_work_circle_transform
!
! ----------------------------------------------------------------------
!
     end module t_circle_transform
