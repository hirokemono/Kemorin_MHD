!>@file   pickup_sph_rms_spectr.f90
!!@brief      module pickup_sph_rms_spectr
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Select mean square data to output
!!
!!@verbatim
!!      subroutine allocate_work_pick_rms_sph(nri, jmax)
!!      subroutine init_sph_rms_4_monitor                               &
!!     &         (sph_params, sph_rj, pwr, pick_list, pick_rms)
!!
!!      subroutine pickup_sph_rms_4_monitor                             &
!!     &         (sph_rj, leg, ipol, rj_fld, pwr, pick_rms)
!!      subroutine pickup_sph_rms_vol_monitor                           &
!!     &        (kg_st, kg_ed, sph_rj, leg, ipol, rj_fld, pwr, pick_rms)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(picked_spectrum_data), intent(inout) :: pick_rms
!!@endverbatim
!
      module pickup_sph_rms_spectr
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_phys_address
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_pickup_sph_spectr_data
!
      use pickup_sph_spectr
!
      implicit  none
!
      real(kind = kreal), allocatable :: rms_sph_rj(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_v(:,:)
!
      private :: rms_sph_rj, rms_sph_v
!
      private :: set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_work_pick_rms_sph(nri, jmax)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
!
      allocate( rms_sph_rj(0:nri,jmax,3) )
      allocate( rms_sph_v(jmax,3) )
!
      rms_sph_rj =  0.0d0
      rms_sph_v =  0.0d0
!
      end subroutine allocate_work_pick_rms_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_pick_rms_sph
!
!
      deallocate( rms_sph_rj, rms_sph_v)
!
      end subroutine deallocate_work_pick_rms_sph
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_rms_4_monitor                                 &
     &         (sph_params, sph_rj, pwr, pick_list, pick_rms)
!
      use pickup_sph_coefs
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: iflag_center = 0
!
!
      call init_sph_radial_monitor_list(sph_rj, pick_rms, iflag_center)
!
      call const_picked_sph_address(iflag_center,                       &
     &    sph_params%l_truncation, sph_rj, pick_list, pick_rms)
!
      call set_sph_rms_labels_4_monitor(pwr, pick_rms)
!
      end subroutine init_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_4_monitor                               &
     &         (sph_rj, leg, ipol, rj_fld, pwr, pick_rms)
!
      use calypso_mpi
      use cal_rms_by_sph_spectr
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: i_fld, j_fld, j, icomp, ncomp
      integer(kind = kint) :: ist_fld, jst_rms
      integer(kind = kint) :: inum, knum, kr
      integer(kind = kint) :: ipick
      integer(kind = kint_gl) :: num
!
!
!$omp parallel do
      do inum = 1, pick_rms%num_sph_mode*pick_rms%num_layer
        pick_rms%d_rj_lc(1:pwr%ntot_comp_sq,inum) = zero
      end do
!$omp end parallel do
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld =   pwr%id_field(j_fld)
        ncomp =   pwr%num_comp_sq(j_fld)
        ist_fld = rj_fld%istack_component(i_fld-1)
        jst_rms = pwr%istack_comp_sq(j_fld-1)
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ipol, ncomp, leg%g_sph_rj, (ist_fld+1),             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, rms_sph_rj)
!
!$omp parallel do private(icomp,j,kr,inum,knum)
        do inum = 1, pick_rms%num_sph_mode
          j = pick_rms%idx_lc(inum)
          if(j .gt. izero) then
            do knum = 1, pick_rms%num_layer
              kr = pick_rms%id_radius(knum)
              ipick = knum + (inum-1) * pick_rms%num_layer
              do icomp = 1, ncomp
                pick_rms%d_rj_lc(jst_rms+icomp,ipick)                   &
     &            = rms_sph_rj(kr,j,icomp) * sph_rj%a_r_1d_rj_r(kr)**2
              end do
            end do
          end if
        end do
!$omp end parallel do
      end do
!
      num = pwr%ntot_comp_sq                                            &
     &     * pick_rms%num_layer * pick_rms%num_sph_mode
      call calypso_mpi_allreduce_real                                   &
     &   (pick_rms%d_rj_lc, pick_rms%d_rj_gl, num, MPI_SUM)
!
      end subroutine pickup_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_vol_monitor                             &
     &        (kg_st, kg_ed, sph_rj, leg, ipol, rj_fld, pwr, pick_rms)
!
      use calypso_mpi
      use t_spheric_rj_data
      use cal_rms_by_sph_spectr
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: i_fld, j_fld, j, icomp, ncomp
      integer(kind = kint) :: ist_fld, jst_rms, inum
      integer(kind = kint_gl) :: num
      real(kind = kreal) :: avol
!
!
      pick_rms%d_rj_lc = 0.0d0

      if(kg_st .eq. 0) then
        avol = three / (sph_rj%radius_1d_rj_r(kg_ed)**3)
      else
        avol = three / (sph_rj%radius_1d_rj_r(kg_ed)**3                 &
     &                - sph_rj%radius_1d_rj_r(kg_st)**3 )
      end if
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld =   pwr%id_field(j_fld)
        ncomp =   pwr%num_comp_sq(j_fld)
        ist_fld = rj_fld%istack_component(i_fld-1)
        jst_rms = pwr%istack_comp_sq(j_fld-1)
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ipol, ncomp, leg%g_sph_rj, (ist_fld+1),             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, rms_sph_rj)
        call radial_integration(kg_st, kg_ed, sph_rj%nidx_rj(1),        &
     &      sph_rj%radius_1d_rj_r, sph_rj%nidx_rj(2),                   &
     &      rms_sph_rj, rms_sph_v)
!
        do icomp = 1, ncomp
          do inum = 1, pick_rms%num_sph_mode
            j = pick_rms%idx_lc(inum)
            if(j .gt. izero) then
              pick_rms%d_rj_lc(jst_rms+icomp,inum)                      &
     &                         = avol * rms_sph_v(j,icomp)
            end if
          end do
        end do
      end do
!
      num = pwr%ntot_comp_sq * pick_rms%num_sph_mode
      call calypso_mpi_allreduce_real                                   &
     &   (pick_rms%d_rj_lc(1,1), pick_rms%d_rj_gl(1,1), num, MPI_SUM)
!
      end subroutine pickup_sph_rms_vol_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels_4_monitor(pwr, pick_rms)
!
      use m_phys_labels
      use add_direction_labels
      use sph_mean_spectr_header_IO
!
      type(sph_mean_squares), intent(in) :: pwr
      type(picked_spectrum_data), intent(inout) :: pick_rms
!
      integer(kind = kint) :: i_fld, ist, ncomp
!
!
      do i_fld = 1, pwr%num_fld_sq
        ist =   pwr%istack_comp_sq(i_fld-1)
        ncomp = pwr%num_comp_sq(i_fld)
        call set_sph_rms_labels(ncomp, pwr%pwr_name(i_fld),             &
     &      pick_rms%spectr_name(ist+1:ist+ncomp))
      end do
      pick_rms%ntot_comp_rj = pwr%ntot_comp_sq
!
      end subroutine set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_rms_spectr
