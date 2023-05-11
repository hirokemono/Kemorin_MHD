!>@file   t_circle_transform.f90
!!@brief  module t_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine init_legendre_on_circle(sph, comms_sph, trans_p,     &
!!     &                                   circ_spec, SR_sig, SR_r)
!!        type(sph_grids), intent(in) ::  sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine alloc_circle_transform(ltr, circ_spec)
!!      subroutine alloc_legendre_on_circ_rj(sph_rj, circ_spec)
!!      subroutine alloc_work_circle_transform                          &
!!     &         (my_rank, d_circle, circ_spec)
!!        integer, intent(in) :: my_rank
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: d_circle
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
!!      subroutine dealloc_circle_transform(circ_spec)
!!      subroutine dealloc_legendre_on_circ_rj(circ_spec)
!!      subroutine dealloc_work_circle_transform(circ_spec)
!!        type(circle_transform_spetr), intent(inout) :: circ_spec
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
!>        Legendre polynomial of the circle
        real(kind = kreal), allocatable :: P_circ(:)
!>        difference of the Legendre polynomial of the circle
        real(kind = kreal), allocatable :: dPdt_circ(:)
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
      subroutine init_legendre_on_circle(sph, comms_sph, trans_p,       &
     &                                   circ_spec, SR_sig, SR_r)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_solver_SR
      use const_equator_legendres_rj
!
      type(sph_grids), intent(in) ::  sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call alloc_legendre_on_circ_rj(sph%sph_rj, circ_spec)
      call s_const_equator_legendres_rj(circ_spec%theta_circle,         &
     &    sph%sph_params, sph%sph_rj, sph%sph_rlm, sph%sph_rtm,         &
     &    comms_sph, trans_p, circ_spec%P_circ, circ_spec%dPdt_circ,    &
     &    SR_sig, SR_r)
!
      if(iflag_debug .gt. 0) then
        call check_legendre_on_circ_rj(sph%sph_rj, circ_spec)
      end if
!
      end subroutine init_legendre_on_circle
!
! ----------------------------------------------------------------------
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
      subroutine alloc_legendre_on_circ_rj(sph_rj, circ_spec)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      allocate(circ_spec%P_circ(sph_rj%nidx_rj(2)))
      allocate(circ_spec%dPdt_circ(sph_rj%nidx_rj(2)))
!$omp parallel workshare
      circ_spec%P_circ(1:sph_rj%nidx_rj(2)) =    0.0d0
      circ_spec%dPdt_circ(1:sph_rj%nidx_rj(2)) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_legendre_on_circ_rj
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
      subroutine dealloc_legendre_on_circ_rj(circ_spec)
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      deallocate(circ_spec%P_circ, circ_spec%dPdt_circ)
!
      end subroutine dealloc_legendre_on_circ_rj
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
! ----------------------------------------------------------------------
!
      subroutine check_legendre_on_circ_rj(sph_rj, circ_spec)
!
      use calypso_mpi
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(circle_transform_spetr), intent(in) :: circ_spec
!
      integer(kind = kint) :: ip, j
!
      do ip = 1, nprocs
        call calypso_mpi_barrier
        if(ip-1 .ne. my_rank) cycle
        open(80,file='eq_leg.dat', position='APPEND')
        if(ip.eq. 1) then
           write(80,*) 'my_rank, j_local, j, l, m, Pvec_1, Pvec_2',     &
     &                circ_spec%theta_circle
        end if
        do j = 1, sph_rj%nidx_rj(2)
          write(80,*) my_rank, j, sph_rj%idx_gl_1d_rj_j(j,1:3),         &
     &              circ_spec%P_circ(j), circ_spec%dPdt_circ(j)
        end do
        close(80)
      end do
!
      end subroutine check_legendre_on_circ_rj
!
! ----------------------------------------------------------------------
!
     end module t_circle_transform
