!>@file   t_field_on_circle.f90
!!@brief  module t_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine alloc_mul_fields_on_circle(num_circle, mul_circle)
!!      subroutine dealloc_mul_fields_on_circle(mul_circle)
!!        integer(kind = kint), intent(in) :: num_circle
!!        type(mul_fields_on_circle), intent(inout) :: mul_circle
!!      subroutine set_control_circles_def(circ_ctls, mul_circle)
!!        type(data_on_circles_ctl), intent(in) :: circ_ctls
!!        type(mul_fields_on_circle), intent(inout) :: mul_circle
!!
!!      subroutine init_circle_point_global(sph, trans_p, cdat)
!!      subroutine dealloc_circle_point_global(my_rank, cdat)
!!        integer, intent(in) :: my_rank
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(sph_grids), intent(in) ::  sph
!!        type(circle_fld_maker), intent(inout) :: cdat
!!
!!      subroutine init_legendre_on_circle(colat_circle, sph, comms_sph,&
!!     &          trans_p, leg_crc, SR_sig, SR_r)
!!        type(sph_grids), intent(in) ::  sph
!!        real(kind = kreal), intent(in) :: colat_circle
!!        type(leg_circle), intent(inout) :: leg_crc
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module t_field_on_circle
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_phys_address
      use t_circle_transform
      use t_fields_on_circle
      use t_FFT_selector
!
      implicit none
!
!
      type leg_circle
!>        Legendre polynomial of the circle
        real(kind = kreal), allocatable :: P_circ(:)
!>        difference of the Legendre polynomial of the circle
        real(kind = kreal), allocatable :: dPdt_circ(:)
!
        integer(kind = kint), allocatable :: ipol_circle_trns(:)
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
      end type leg_circle
!
      type circle_fld_maker
        type(circle_transform_spetr) :: circ_spec
!>        Structure to make fields on circle
        type(fields_on_circle) :: circle
!>         Structure of field data on circle
        type(phys_data) :: d_circle
!>        Legendre polynomials at specific latitude
        type(leg_circle) :: leg_crc
!>        Working structure for Fourier transform at mid-depth equator
!!@n      (Save attribute is necessary for Hitachi compiler for SR16000)
        type(working_FFTs) :: WK_circle_fft
      end type circle_fld_maker
!
      type mul_fields_on_circle
        integer(kind = kint) :: num_circles = 0
!>         Structure of field data on circle
        type(circle_fld_maker), allocatable :: cdat(:)
      end type mul_fields_on_circle
!
      private :: set_circle_point_global
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_mul_fields_on_circle(num_circle, mul_circle)
!
      integer(kind = kint), intent(in) :: num_circle
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      mul_circle%num_circles = max(num_circle, 0)
      allocate(mul_circle%cdat(mul_circle%num_circles))
!
      end subroutine alloc_mul_fields_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mul_fields_on_circle(mul_circle)
!
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      deallocate(mul_circle%cdat)
!
      end subroutine dealloc_mul_fields_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine set_control_circles_def(circ_ctls, mul_circle)
!
      use t_ctl_data_circles
!
      type(data_on_circles_ctl), intent(in) :: circ_ctls
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      integer(kind = kint) :: i
!
      call alloc_mul_fields_on_circle(circ_ctls%num_circ_ctl,           &
     &                                mul_circle)
!
      do i = 1, mul_circle%num_circles
        call set_control_circle_def(circ_ctls%meq_ctl(i),               &
     &                              mul_circle%cdat(i)%circle)
      end do
!
      end subroutine set_control_circles_def
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_circle_point_global(sph, trans_p, cdat)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_work_4_sph_trans
!
      type(sph_grids), intent(in) ::  sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(circle_fld_maker), intent(inout) :: cdat
!
!
      call alloc_circle_field(my_rank,                                  &
     &    sph%sph_rtp%nidx_rtp(3), sph%sph_rj%nidx_global_rj(2),        &
     &    cdat%circle, cdat%d_circle)
      call alloc_circle_transform(sph%sph_params%l_truncation,          &
     &                            cdat%circ_spec)
      call initialize_circle_transform(trans_p%iflag_FFT,               &
     &    cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
      call set_circle_point_global                                      &
     &   (sph%sph_rj%nidx_rj(1), sph%sph_rj%radius_1d_rj_r,             &
     &    cdat%circ_spec, cdat%circle)
!
      end subroutine init_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine init_legendre_on_circle(colat_circle, sph, comms_sph,  &
     &          trans_p, leg_crc, SR_sig, SR_r)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_solver_SR
      use const_equator_legendres_rj
!
      type(sph_grids), intent(in) ::  sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      real(kind = kreal), intent(in) :: colat_circle
!
      type(leg_circle), intent(inout) :: leg_crc
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!      integer(kind = kint) :: ip, j
!
      allocate(leg_crc%P_circ(sph%sph_rj%nidx_rj(2)))
      allocate(leg_crc%dPdt_circ(sph%sph_rj%nidx_rj(2)))
!$omp parallel workshare
      leg_crc%P_circ(1:sph%sph_rj%nidx_rj(2)) =    0.0d0
      leg_crc%dPdt_circ(1:sph%sph_rj%nidx_rj(2)) = 0.0d0
!$omp end parallel workshare
!
      call s_const_equator_legendres_rj(colat_circle,                   &
     &    sph%sph_params, sph%sph_rj, sph%sph_rlm, sph%sph_rtm,         &
     &    comms_sph, trans_p, leg_crc%P_circ, leg_crc%dPdt_circ,        &
     &    SR_sig, SR_r)
!
!      do ip = 1, nprocs
!        call calypso_mpi_barrier
!        if(ip-1 .ne. my_rank) cycle
!        open(80,file='eq_leg.dat', position='APPEND')
!        if(ip.eq. 1) then
!           write(80,*)                                                &
!     &      'my_rank, j_local, j, l, m, Pvec_1, Pvec_2', leg_crc%colat
!        end if
!        do j = 1, sph%sph_rj%nidx_rj(2)
!          write(80,*) my_rank, j, sph%sph_rj%idx_gl_1d_rj_j(j,1:3),   &
!     &              leg_crc%P_circ(j), leg_crc%dPdt_circ(j)
!        end do
!       close(80)
!      end do
!
      end subroutine init_legendre_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine set_circle_transfer_address(nod_fld, rj_fld, leg_crc)
!
      type(phys_data), intent(in) :: nod_fld, rj_fld
      type(leg_circle), intent(inout) :: leg_crc
!
      logical, allocatable :: flag_use(:)
      integer(kind = kint) :: i_fld, j_fld
!
      allocate(leg_crc%ipol_circle_trns(1:rj_fld%num_phys))
!$omp parallel workshare
      leg_crc%ipol_circle_trns(1:rj_fld%num_phys) = 0
!$omp end parallel workshare
!
      allocate(flag_use(1:rj_fld%num_phys))
!$omp parallel workshare
      flag_use(1:rj_fld%num_phys) = .FALSE.
!$omp end parallel workshare
      do i_fld = 1, nod_fld%num_phys_viz
        do j_fld = 1, rj_fld%num_phys
          if(flag_use(j_fld)) cycle
          if(rj_fld%phys_name(j_fld)                                    &
     &         .eq. nod_fld%phys_name(i_fld)) then
            leg_crc%ipol_circle_trns(i_fld)                             &
     &                      = rj_fld%istack_component(j_fld-1) + 1 
            flag_use(j_fld) = .TRUE.
            exit
          end if
        end do
      end do
      deallocate(flag_use)
!
      end subroutine set_circle_transfer_address
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_point_global(my_rank, cdat)
!
      integer, intent(in) :: my_rank
      type(circle_fld_maker), intent(inout) :: cdat
!
!
      call dealloc_circle_field(my_rank, cdat%circle, cdat%d_circle)
      call dealloc_circle_transform(cdat%circ_spec)
!
      end subroutine dealloc_circle_point_global
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_circle_point_global                                &
     &         (nri, radius_1d_rj_r, circ_spec, circle)
!
      integer(kind = kint), intent(in) ::  nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      type(circle_transform_spetr), intent(in) :: circ_spec
!
      type(fields_on_circle), intent(inout) :: circle
!
      integer(kind = kint) :: kr
!
!
      circle%kr_gl_rcirc_in =  izero
      circle%kr_gl_rcirc_out = izero
      do kr = 1, nri - 1
        if(radius_1d_rj_r(kr) .eq. circ_spec%r_circle) then
          circle%kr_gl_rcirc_in =  kr
          circle%kr_gl_rcirc_out = izero
          circle%coef_gl_rcirc_in =  one
          circle%coef_gl_rcirc_out = zero
          exit
        end if
        if(radius_1d_rj_r(kr) .lt. circ_spec%r_circle                   &
     &      .and. radius_1d_rj_r(kr+1) .gt. circ_spec%r_circle) then
          circle%kr_gl_rcirc_in =  kr
          circle%kr_gl_rcirc_out = kr + 1
          circle%coef_gl_rcirc_in                                       &
     &                   = (radius_1d_rj_r(kr+1) - circ_spec%r_circle)  &
     &                    / (radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr))
          circle%coef_gl_rcirc_out = one - circle%coef_gl_rcirc_in
          exit
        end if
      end do
!
      end subroutine set_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine initialize_circle_transform                            &
     &          (iflag_FFT, circle, circ_spec, WK_circle_fft)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(fields_on_circle), intent(in) :: circle
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint) :: l, m, mm, j
!
!
      circ_spec%r_circle                                                &
     &      = sqrt(circle%s_circle**2 + circle%z_circle**2)
      circ_spec%theta_circle                                            &
     &      = acos(circle%z_circle / circ_spec%r_circle)
!
      circ_spec%ar_circle = one / circ_spec%r_circle
      circ_spec%ar2_circle = circ_spec%ar_circle**2
!
      if(my_rank .gt. 0) return
!
      write(*,*) 'np_smp', np_smp
      write(*,*) 'istack_circfft_smp', circ_spec%istack_circfft_smp
      write(*,*) 'mphi_circle', circle%mphi_circle
      call initialize_FFT_select                                        &
     &   (my_rank, iflag_FFT, np_smp, circ_spec%istack_circfft_smp,     &
     &    circle%mphi_circle, WK_circle_fft)
!
      end subroutine initialize_circle_transform
!
! ----------------------------------------------------------------------
!
      end module t_field_on_circle
