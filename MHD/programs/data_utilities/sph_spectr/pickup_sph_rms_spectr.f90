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
!!      subroutine init_sph_rms_4_monitor(l_truncation, sph_rj, pwr)
!!
!!      subroutine pickup_sph_rms_4_monitor                             &
!!     &         (sph_rj, leg, ipol, rj_fld, pwr)
!!      subroutine pickup_sph_rms_vol_monitor                           &
!!     &         (kg_st, kg_ed, sph_rj, leg, ipol, rj_fld, pwr)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!@endverbatim
!
      module pickup_sph_rms_spectr
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_pickup_sph_rms_data
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_phys_address
      use t_spheric_rj_data
      use t_rms_4_sph_spectr
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
      subroutine init_sph_rms_4_monitor(l_truncation, sph_rj, pwr)
!
      use m_pickup_sph_spectr_data
!
      integer(kind = kint), intent(in) ::l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_mean_squares), intent(in) :: pwr
!
!
      pickup_sph_rms_head = pickup_sph_head
      call allocate_iflag_pick_sph(l_truncation)
!
      call count_picked_sph_adrress(l_truncation,                       &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    num_pick_sph_rms_mode)
!
      call allocate_pick_sph_rms
!
      call set_picked_sph_address(l_truncation, sph_rj,                 &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    num_pick_sph_rms_mode, idx_pick_sph_rms_gl,                   &
     &    idx_pick_sph_rms_lc)
!
      call deallocate_iflag_pick_sph
      call deallocate_pick_sph_mode
!
      call set_sph_rms_labels_4_monitor(pwr)
!
      end subroutine init_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_4_monitor                               &
     &         (sph_rj, leg, ipol, rj_fld, pwr)
!
      use calypso_mpi
      use m_schmidt_poly_on_rtm
      use m_pickup_sph_spectr_data
      use cal_rms_by_sph_spectr
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: i_fld, j_fld, j, icomp, ncomp
      integer(kind = kint) :: ist_fld, jst_rms
      integer(kind = kint) :: inum, knum, kr
      integer(kind = kint) :: ipick, num
!
!
!$omp parallel do
      do inum = 1, num_pick_sph_rms_mode*pick1%num_layer
        d_rms_pick_sph_lc(1:pwr%ntot_comp_sq,inum) = zero
      end do
!$omp end parallel do
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld =   pwr%id_field(j_fld)
        ncomp =   pwr%num_comp_sq(j_fld)
        ist_fld = rj_fld%istack_component(i_fld-1)
        jst_rms = pwr%istack_comp_sq(j_fld-1)
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ipol, ncomp, (ist_fld+1), leg%g_sph_rj,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, rms_sph_rj)
!
!$omp parallel do private(icomp,j,kr,inum,knum)
        do inum = 1, num_pick_sph_rms_mode
          j = idx_pick_sph_rms_lc(inum)
          if(j .gt. izero) then
            do knum = 1, pick1%num_layer
              kr = pick1%id_radius(knum)
              ipick = knum + (inum-1) * pick1%num_layer
              do icomp = 1, ncomp
                d_rms_pick_sph_lc(jst_rms+icomp,ipick)                  &
     &            = rms_sph_rj(kr,j,icomp) * sph_rj%a_r_1d_rj_r(kr)**2
              end do
            end do
          end if
        end do
!$omp end parallel do
      end do
!
      num = pwr%ntot_comp_sq * pick1%num_layer * num_pick_sph_rms_mode
      call MPI_allREDUCE(d_rms_pick_sph_lc(1,1),                        &
     &    d_rms_pick_sph_gl(1,1), num, CALYPSO_REAL, MPI_SUM,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_vol_monitor                             &
     &         (kg_st, kg_ed, sph_rj, leg, ipol, rj_fld, pwr)
!
      use calypso_mpi
      use t_spheric_rj_data
      use m_schmidt_poly_on_rtm
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
      integer(kind = kint) :: i_fld, j_fld, j, icomp, ncomp
      integer(kind = kint) :: ist_fld, jst_rms, num, inum
      real(kind = kreal) :: avol
!
!
      d_rms_pick_sph_lc = 0.0d0

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
     &     (sph_rj, ipol, ncomp, (ist_fld+1), leg%g_sph_rj,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld, rms_sph_rj)
        call radial_integration(kg_st, kg_ed, sph_rj%nidx_rj(1),        &
     &      sph_rj%radius_1d_rj_r, sph_rj%nidx_rj(2),                   &
     &      rms_sph_rj, rms_sph_v)
!
        do icomp = 1, ncomp
          do inum = 1, num_pick_sph_rms_mode
            j = idx_pick_sph_rms_lc(inum)
            if(j .gt. izero) then
              d_rms_pick_sph_lc(jst_rms+icomp,inum)                     &
     &                         = avol * rms_sph_v(j,icomp)
            end if
          end do
        end do
      end do
!
      num = pwr%ntot_comp_sq * num_pick_sph_rms_mode
      call MPI_allREDUCE(d_rms_pick_sph_lc(1,1),                        &
     &    d_rms_pick_sph_gl(1,1), num, CALYPSO_REAL, MPI_SUM,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_rms_vol_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels_4_monitor(pwr)
!
      use m_phys_labels
      use add_direction_labels
!
      type(sph_mean_squares), intent(in) :: pwr
      integer(kind = kint) :: i_fld, ist
!
!
      do i_fld = 1, pwr%num_fld_sq
        ist = pwr%istack_comp_sq(i_fld-1)
          if      (pwr%pwr_name(i_fld) .eq. fhd_velo) then
            write(rms_pick_sph_name(ist+1),'(a)') 'K_ene_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'K_ene_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'K_ene'
!
          else if (pwr%pwr_name(i_fld) .eq. fhd_magne) then
            write(rms_pick_sph_name(ist+1),'(a)') 'M_ene_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'M_ene_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'M_ene'
!
          else if (pwr%pwr_name(i_fld) .eq. fhd_filter_v) then
            write(rms_pick_sph_name(ist+1),'(a)') 'filter_KE_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'filter_KE_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'filter_KE'
!
          else if (pwr%pwr_name(i_fld) .eq. fhd_filter_b) then
            write(rms_pick_sph_name(ist+1),'(a)') 'filter_ME_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'filter_ME_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'filter_ME'
!
          else if (pwr%num_comp_sq(i_fld) .eq. 1) then
            write(rms_pick_sph_name(ist+1),'(a)')                       &
     &                      trim(pwr%pwr_name(i_fld))
!
          else if (pwr%num_comp_sq(i_fld) .eq. 3) then
            call add_vector_power_sph_label(pwr%pwr_name(i_fld),        &
     &          rms_pick_sph_name(ist+1), rms_pick_sph_name(ist+2),     &
     &          rms_pick_sph_name(ist+3))
            write(rms_pick_sph_name(ist+3),'(a)') pwr%pwr_name(i_fld)
          else if (pwr%num_comp_sq(i_fld) .eq. 6) then
            call add_tensor_direction_label_rtp(pwr%pwr_name(i_fld),    &
     &          rms_pick_sph_name(ist+1), rms_pick_sph_name(ist+2),     &
     &          rms_pick_sph_name(ist+3), rms_pick_sph_name(ist+4),     &
     &          rms_pick_sph_name(ist+5), rms_pick_sph_name(ist+6))
          end if
      end do
      ncomp_pick_sph_rms = pwr%ntot_comp_sq
!
      end subroutine set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_rms_spectr
