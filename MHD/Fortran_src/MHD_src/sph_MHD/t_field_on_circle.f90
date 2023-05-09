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
!!      subroutine set_control_circles_def(circ_ctls, nod_fld,          &
!!     &                                   mul_circle)
!!        type(data_on_circles_ctl), intent(in) :: circ_ctls
!!        type(mul_fields_on_circle), intent(inout) :: mul_circle
!!
!!      subroutine sph_transfer_on_circle                               &
!!     &         (iflag_FFT, sph_rj, rj_fld, cdat)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(circle_fld_maker), intent(inout) :: cdat
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
      use t_sph_trans_arrays_MHD
      use t_circle_transform
      use t_fields_on_circle
      use t_FFT_selector
!
      implicit none
!
!
      type leg_circle
!>        Colatitude of the circle
        real(kind = kreal) :: colat
!>        Legendre polynomial of the circle
        real(kind = kreal), allocatable :: P_circ(:)
!>        difference of the Legendre polynomial of the circle
        real(kind = kreal), allocatable :: dPdt_circ(:)
      end type leg_circle
!
      type circle_fld_maker
        type(circle_transform_spetr) :: circ_spec
!>        Structure to make fields on circle
        type(fields_on_circle) :: circle
!>         Structure of field data on circle
        type(phys_data) :: d_circle
!>        Working structure for Fourier transform at mid-depth equator
!!@n      (Save attribute is necessary for Hitachi compiler for SR16000)
        type(working_FFTs) :: WK_circle_fft
!>        Address list for circle data
        type(phys_address) :: iphys_circle
!>        Address list for transform
        type(address_4_sph_trans) :: trns_dbench
!
!>        Legendre polynomials at specific latitude
        type(leg_circle) :: leg_crc
      end type circle_fld_maker
!
      type mul_fields_on_circle
        integer(kind = kint) :: num_circles = 0
        type(fields_on_circle), allocatable :: circle(:)
!>         Structure of field data on circle
        type(phys_data), allocatable :: d_circles(:)
!>        Legendre polynomials at specific latitude
        type(leg_circle), allocatable :: leg_crc(:)
      end type mul_fields_on_circle
!
      private :: collect_spectr_for_circle, set_circle_point_global
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
      allocate(mul_circle%circle(mul_circle%num_circles))
      allocate(mul_circle%d_circles(mul_circle%num_circles))
      allocate(mul_circle%leg_crc(mul_circle%num_circles))
!
      end subroutine alloc_mul_fields_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mul_fields_on_circle(mul_circle)
!
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      deallocate(mul_circle%circle, mul_circle%d_circles)
      deallocate(mul_circle%leg_crc)
!
      end subroutine dealloc_mul_fields_on_circle
!
! ----------------------------------------------------------------------
!
      subroutine set_control_circles_def(circ_ctls, nod_fld,            &
     &                                   mul_circle)
!
      use t_ctl_data_circles
!
      type(data_on_circles_ctl), intent(in) :: circ_ctls
      type(phys_data), intent(in) :: nod_fld
      type(mul_fields_on_circle), intent(inout) :: mul_circle
!
      integer(kind = kint) :: i
!
      call alloc_mul_fields_on_circle(circ_ctls%num_circ_ctl,           &
     &                                mul_circle)
!
      do i = 1, mul_circle%num_circles
        call set_control_circle_def(circ_ctls%meq_ctl(i),               &
     &                              mul_circle%circle(i))
        call dup_phys_name(nod_fld, mul_circle%d_circles(i))
      end do
!
      end subroutine set_control_circles_def
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sph_transfer_on_circle                                 &
     &         (iflag_FFT, sph_rj, rj_fld, cdat)
!
      use calypso_mpi
      use m_phys_constants
!
      use t_spheric_rj_data
      use t_phys_data
!
      use circle_transform_single
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(circle_fld_maker), intent(inout) :: cdat
!
      integer(kind = kint) :: ifld, icomp, m, nd
!
!
      call collect_spectr_for_circle(sph_rj%nidx_rj(2),                 &
     &    sph_rj%nidx_global_rj, sph_rj%idx_gl_1d_rj_j,                 &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%phys_name, rj_fld%d_fld,      &
     &    cdat%d_circle, cdat%circle)
!
!    spherical transfer
!
      if(my_rank .gt. 0) return
!
      do ifld = 1, cdat%d_circle%num_phys_viz
        icomp =  cdat%d_circle%istack_component(ifld-1) + 1
        if(cdat%d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call circle_transfer_sym_tensor(iflag_FFT, icomp,             &
     &        cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
        else if(cdat%d_circle%num_component(ifld) .eq. n_vector) then
          call circle_transfer_vector(iflag_FFT, icomp,                 &
     &        cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
        else
          call circle_transfer_scalar(iflag_FFT, icomp,                 &
     &        cdat%circle, cdat%circ_spec, cdat%WK_circle_fft)
        end if
!
        do nd = 1, cdat%d_circle%num_component(ifld)
          do m = 1, cdat%circle%mphi_circle
            cdat%d_circle%d_fld(m,icomp+nd-1)                           &
     &         = cdat%circle%v_rtp_circle(m,nd)
          end do
        end do
      end do
!
      end subroutine sph_transfer_on_circle
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_circle_point_global(sph, trans_p, cdat)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_work_4_sph_trans
      use circle_transform_single
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
      use circle_transform_single
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
      integer(kind = kint) :: ip, j
!
      allocate(leg_crc%P_circ(sph%sph_rj%nidx_rj(2)))
      allocate(leg_crc%dPdt_circ(sph%sph_rj%nidx_rj(2)))
!$omp parallel workshare
      leg_crc%P_circ(1:sph%sph_rj%nidx_rj(2)) =    0.0d0
      leg_crc%dPdt_circ(1:sph%sph_rj%nidx_rj(2)) = 0.0d0
!$omp end parallel workshare
!
!
      leg_crc%colat = colat_circle
      call s_const_equator_legendres_rj(leg_crc%colat,                  &
     &    sph%sph_params, sph%sph_rj, sph%sph_rlm, sph%sph_rtm,         &
     &    comms_sph, trans_p, leg_crc%P_circ, leg_crc%dPdt_circ,        &
     &    SR_sig, SR_r)
!
      do ip = 1, nprocs
        call calypso_mpi_barrier
        if(ip-1 .ne. my_rank) cycle

        open(80,file='eq_leg.dat', position='APPEND')
        if(ip.eq. 1) then
           write(80,*)                                                 &
     &      'my_rank, j_local, j, l, m, Pvec_1, Pvec_2'
        end if
        do j = 1, sph%sph_rj%nidx_rj(2)
          write(80,*) my_rank, j, sph%sph_rj%idx_gl_1d_rj_j(j,1:3),    &
     &              leg_crc%P_circ(j), leg_crc%dPdt_circ(j)
        end do
       close(80)
      end do
!
      end subroutine init_legendre_on_circle
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
      subroutine collect_spectr_for_circle                              &
     &         (jmax, nidx_global_rj, idx_gl_1d_rj_j, nnod_rj,          &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,         &
     &          phys_name_rj, d_rj, d_circle, circle)
!
      use calypso_mpi
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: nidx_global_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(jmax,3)
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      character (len=kchara), intent(in) :: phys_name_rj(num_phys_rj)
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
      type(phys_data), intent(in) :: d_circle
!
      type(fields_on_circle), intent(inout) :: circle
!
      integer(kind = kint) :: j, j_gl, i_in, i_ot, ncomp
      integer(kind = kint) :: ist_comp, jst_comp, nd, ifld, jfld
      integer(kind = kint_gl) :: num64
!
!
!    pickup spectrum for circle point
!
      do ifld = 1, d_circle%num_phys_viz
        ist_comp = d_circle%istack_component(ifld-1)
        do jfld = 1, num_phys_rj
          if(d_circle%phys_name(ifld) .eq. phys_name_rj(jfld)) then
            jst_comp = istack_phys_comp_rj(jfld-1)
            ncomp = istack_phys_comp_rj(jfld)                           &
     &             - istack_phys_comp_rj(jfld-1)
            if(iflag_debug .gt. 0) write(*,*)                           &
     &              trim(d_circle%phys_name(ifld)), ifld, jfld, ncomp
            do nd = 1, ncomp
              do j = 1, jmax
                j_gl = idx_gl_1d_rj_j(j,1)
                i_in = j + (circle%kr_gl_rcirc_in-1) *  jmax
                i_ot = j + (circle%kr_gl_rcirc_out-1) * jmax
!
                circle%d_rj_circ_lc(j_gl,ist_comp+nd)                   &
     &            = circle%coef_gl_rcirc_in * d_rj(i_in,jst_comp+nd)    &
     &             + circle%coef_gl_rcirc_out * d_rj(i_ot,jst_comp+nd)
              end do
            end do
            exit
          end if
        end do
      end do
!
!    collect data to rank 0
!
      num64 = d_circle%ntot_phys * (nidx_global_rj(2) + 1)
      if(my_rank .eq. 0) circle%d_rj_circle =   zero
      call calypso_mpi_reduce_real                                      &
     &   (circle%d_rj_circ_lc(0,1), circle%d_rj_circle(0,1), num64,     &
     &    MPI_SUM, 0)
!
      end subroutine collect_spectr_for_circle
!
! ----------------------------------------------------------------------
!
      end module t_field_on_circle
