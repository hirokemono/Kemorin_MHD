!>@file   pickup_sph_rms_spectr.f90
!!@brief      module pickup_sph_rms_spectr
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Select mean square data to output
!!
!!@verbatim
!!      subroutine init_sph_rms_4_monitor
!!
!!      subroutine pickup_sph_rms_4_monitor(rj_fld)
!!      subroutine pickup_sph_rms_vol_monitor(rj_fld)
!!@endverbatim
!
      module pickup_sph_rms_spectr
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_pickup_sph_rms_data
      use m_rms_4_sph_spectr
      use t_phys_data
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
      subroutine allocate_work_pick_rms_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
!
      nri = nidx_rj(1)
      jmax = nidx_rj(2)
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
      subroutine init_sph_rms_4_monitor
!
      use m_pickup_sph_spectr_data
!
!
      pickup_sph_rms_head = pickup_sph_head
!
      call count_picked_sph_adrress                                     &
     &   (num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    ntot_pick_sph_rms_mode)
!
      call allocate_pick_sph_rms
      call allocate_iflag_pick_sph(l_truncation)
!
      call set_picked_sph_address                                       &
     &   (num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    ntot_pick_sph_rms_mode, num_pick_sph_rms_mode,                &
     &    idx_pick_sph_rms_gl, idx_pick_sph_rms_lc)
!
      call deallocate_iflag_pick_sph
      call deallocate_pick_sph_mode
!
      call set_sph_rms_labels_4_monitor
!
      end subroutine init_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_4_monitor(rj_fld)
!
      use calypso_mpi
      use m_pickup_sph_spectr_data
      use cal_rms_by_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_fld, j, icomp, ncomp
      integer(kind = kint) :: ist_fld, jst_rms
      integer(kind = kint) :: inum, knum, kr
      integer(kind = kint) :: ipick, num
!
!
!$omp parallel do
      do inum = 1, num_pick_sph_rms_mode*num_pick_layer
        d_rms_pick_sph_lc(1:ntot_rms_rj,inum) = zero
      end do
!$omp end parallel do
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        ncomp = num_rms_comp_rj(j_fld)
        ist_fld =  rj_fld%istack_component(i_fld-1)
        jst_rms = istack_rms_comp_rj(j_fld-1)
        call cal_rms_sph_spec_one_field(ncomp, (ist_fld+1),             &
     &      nidx_rj(1), nidx_rj(2), rj_fld%ntot_phys, rj_fld%d_fld,     &
     &      rms_sph_rj)
!
!$omp parallel do private(icomp,j,kr,inum,knum)
        do inum = 1, num_pick_sph_rms_mode
          j = idx_pick_sph_rms_lc(inum)
          if(j .gt. izero) then
            do knum = 1, num_pick_layer
              kr = id_pick_layer(knum)
              ipick = knum + (inum-1) * num_pick_layer
              do icomp = 1, ncomp
                d_rms_pick_sph_lc(jst_rms+icomp,ipick)                  &
     &                 = rms_sph_rj(kr,j,icomp) * a_r_1d_rj_r(kr)**2
              end do
            end do
          end if
        end do
!$omp end parallel do
      end do
!
      num = ntot_rms_rj*num_pick_layer*num_pick_sph_rms_mode
      call MPI_allREDUCE(d_rms_pick_sph_lc(1,1),                        &
     &    d_rms_pick_sph_gl(1,1), num, CALYPSO_REAL, MPI_SUM,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_rms_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_rms_vol_monitor(kg_st, kg_ed, rj_fld)
!
      use calypso_mpi
      use cal_rms_by_sph_spectr
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_fld, j, icomp, ncomp
      integer(kind = kint) :: ist_fld, jst_rms, num, inum
      real(kind = kreal) :: avol
!
!
      d_rms_pick_sph_lc = 0.0d0

      if(kg_st .eq. 0) then
        avol = three / (radius_1d_rj_r(kg_ed)**3)
      else
        avol = three / (radius_1d_rj_r(kg_ed)**3                        &
     &                - radius_1d_rj_r(kg_st)**3 )
      end if
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        ncomp = num_rms_comp_rj(j_fld)
        ist_fld =  rj_fld%istack_component(i_fld-1)
        jst_rms = istack_rms_comp_rj(j_fld-1)
        call cal_rms_sph_spec_one_field(ncomp, (ist_fld+1),             &
     &      nidx_rj(1), nidx_rj(2), rj_fld%ntot_phys, rj_fld%d_fld,     &
     &      rms_sph_rj)
        call radial_integration(kg_st, kg_ed, nidx_rj(1),               &
     &      radius_1d_rj_r, nidx_rj(2), rms_sph_rj, rms_sph_v)
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
      num = ntot_rms_rj*num_pick_sph_rms_mode
      call MPI_allREDUCE(d_rms_pick_sph_lc(1,1),                        &
     &    d_rms_pick_sph_gl(1,1), num, CALYPSO_REAL, MPI_SUM,           &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_rms_vol_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_rms_labels_4_monitor
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint) :: i_fld, ist
!
!
      do i_fld = 1, num_rms_rj
        ist = istack_rms_comp_rj(i_fld-1)
          if ( rms_name_rj(i_fld) .eq. fhd_velo) then
            write(rms_pick_sph_name(ist+1),'(a)') 'K_ene_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'K_ene_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'K_ene'
!
          else if (rms_name_rj(i_fld) .eq. fhd_magne) then
            write(rms_pick_sph_name(ist+1),'(a)') 'M_ene_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'M_ene_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'M_ene'
!
          else if (rms_name_rj(i_fld) .eq. fhd_filter_v) then
            write(rms_pick_sph_name(ist+1),'(a)') 'filter_KE_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'filter_KE_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'filter_KE'
!
          else if (rms_name_rj(i_fld) .eq. fhd_filter_b) then
            write(rms_pick_sph_name(ist+1),'(a)') 'filter_ME_pol'
            write(rms_pick_sph_name(ist+2),'(a)') 'filter_ME_tor'
            write(rms_pick_sph_name(ist+3),'(a)') 'filter_ME'
!
          else if (num_rms_comp_rj(i_fld) .eq. 1) then
            write(rms_pick_sph_name(ist+1),'(a)')                       &
     &                      trim(rms_name_rj(i_fld))
!
          else if (num_rms_comp_rj(i_fld) .eq. 3) then
            call add_vector_power_sph_label(rms_name_rj(i_fld),         &
     &          rms_pick_sph_name(ist+1), rms_pick_sph_name(ist+2),     &
     &          rms_pick_sph_name(ist+3))
            write(rms_pick_sph_name(ist+3),'(a)') rms_name_rj(i_fld)
          else if (num_rms_comp_rj(i_fld) .eq. 6) then
            call add_tensor_direction_label_rtp(rms_name_rj(i_fld),     &
     &          rms_pick_sph_name(ist+1), rms_pick_sph_name(ist+2),     &
     &          rms_pick_sph_name(ist+3), rms_pick_sph_name(ist+4),     &
     &          rms_pick_sph_name(ist+5), rms_pick_sph_name(ist+6))
          end if
      end do
      ncomp_pick_sph_rms = ntot_rms_rj
!
      end subroutine set_sph_rms_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_rms_spectr
