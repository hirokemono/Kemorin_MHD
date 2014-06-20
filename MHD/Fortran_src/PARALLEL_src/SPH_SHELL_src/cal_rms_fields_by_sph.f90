!>@file   cal_rms_fields_by_sph.f90
!!@brief      module cal_rms_fields_by_sph
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine init_rms_4_sph_spectr
!!
!!      subroutine cal_rms_sph_spec_rms_whole
!!      subroutine cal_rms_sph_outer_core
!!      subroutine cal_rms_sph_inner_core
!!@endverbatim
!
      module cal_rms_fields_by_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: r_int_sph_rms_data, cal_average_for_sph_rms
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr
!
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use sum_sph_rms_data
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      num_rms_rj = 0
      do i_fld = 1, num_phys_rj
        num_rms_rj = num_rms_rj + iflag_monitor_rj(i_fld)
      end do
!
      call allocate_rms_name_sph_spec
!
      j_fld = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
          j_fld = j_fld + 1
          ifield_rms_rj(j_fld) =   i_fld
          num_rms_comp_rj(j_fld) = num_phys_comp_rj(i_fld)
          istack_rms_comp_rj(j_fld) = istack_rms_comp_rj(j_fld-1)       &
     &                              + num_phys_comp_rj(i_fld)
          rms_name_rj(j_fld) =     phys_name_rj(i_fld)
        end if
      end do
      ntot_rms_rj = istack_rms_comp_rj(num_rms_rj)
!
      call allocate_rms_4_sph_spectr
      call set_sum_table_4_sph_spectr
!
      end subroutine init_rms_4_sph_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_rms_whole
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
      use sum_sph_rms_data
!
!
      if(ntot_rms_rj .eq. 0) return
      call cal_average_sph_spectr(ione, nidx_rj(1))
      call cal_rms_sph_spec_local
!
      call r_int_sph_rms_data(ione, nidx_rj(1))
      call cal_average_for_sph_rms(ione, nidx_rj(1))
!
      end subroutine cal_rms_sph_spec_rms_whole
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_inner_core
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
!
      if(ntot_rms_rj .eq. 0) return
      call cal_average_sph_spectr(izero, nidx_rj(1))
      call cal_rms_sph_spec_local
!
      call r_int_sph_rms_data(izero, nlayer_ICB)
      call cal_average_for_sph_rms(izero, nlayer_ICB)
!
      end subroutine cal_rms_sph_inner_core
!
! ----------------------------------------------------------------------
!
      subroutine cal_rms_sph_outer_core
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
!
      if(ntot_rms_rj .eq. 0) return
      call cal_average_sph_spectr(nlayer_ICB, nlayer_CMB)
      call cal_rms_sph_spec_local
!
      if(iflag_debug.gt.0)  write(*,*) 'r_int_sph_rms_data'
      call r_int_sph_rms_data(nlayer_ICB, nlayer_CMB)
      if(iflag_debug.gt.0)  write(*,*) 'cal_average_for_sph_rms'
      call cal_average_for_sph_rms(nlayer_ICB, nlayer_CMB)
!
      end subroutine cal_rms_sph_outer_core
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_average_sph_spectr(kg_st, kg_ed)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
!
      integer(kind = kint) :: i_fld, j_fld, icomp_st, jcomp_st
      integer(kind = kint) :: num
!
      real(kind = kreal) :: avol
!
!
      call clear_ave_sph_spectr
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_st = istack_phys_comp_rj(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) +  1
        if (num_phys_comp_rj(i_fld) .eq. n_scalar) then
          call cal_ave_scalar_sph_spectr(nidx_rj(1), d_rj(1,icomp_st),  &
     &        ave_sph_lc(1,jcomp_st))
        else if (num_phys_comp_rj(i_fld) .eq. n_vector) then
          call cal_ave_vector_sph_spectr(nidx_rj(1), d_rj(1,icomp_st),  &
     &        ave_sph_lc(1,jcomp_st))
        end if
      end do
!
      num = ntot_rms_rj * nidx_rj(1)
      call MPI_allREDUCE (ave_sph_lc(1,1), ave_sph(1,1), num,           &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .gt. 0) return
!
      if(kg_st .eq. 0) then
        avol = three / (radius_1d_rj_r(kg_ed)**3)
      else
        avol = three / (radius_1d_rj_r(kg_ed)**3                        &
     &                - radius_1d_rj_r(kg_st)**3 )
      end if
!
      call radial_integration(ione, nidx_rj(1), kg_st, kg_ed,           &
     &    radius_1d_rj_r, ntot_rms_rj, ave_sph(1,1), ave_sph_vol(1) )
!
      call averaging_4_sph_ave_int(avol)
!
      end subroutine cal_average_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine cal_rms_sph_spec_local
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
!
      integer(kind = kint) :: i_fld, j_fld, icomp_st, jcomp_st
      integer(kind = kint) :: num
!
!
      call clear_rms_sph_spectr
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_st = istack_phys_comp_rj(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) +  1
        if (num_phys_comp_rj(i_fld) .eq. n_scalar) then
          call cal_rms_each_scalar_sph_spec                             &
     &       (d_rj(1,icomp_st), rms_sph_dat(1,1,jcomp_st))
        else if (num_phys_comp_rj(i_fld) .eq. n_vector) then
          call cal_rms_each_vector_sph_spec                             &
     &       (d_rj(1,icomp_st), rms_sph_dat(1,1,jcomp_st))
!
          if (   icomp_st .eq. ipol%i_velo                              &
     &      .or. icomp_st .eq. ipol%i_magne                             &
     &      .or. icomp_st .eq. ipol%i_filter_velo                       &
     &      .or. icomp_st .eq. ipol%i_filter_magne) then
            call set_sph_energies_by_rms(rms_sph_dat(1,1,jcomp_st) )
          end if
!
        end if
      end do
!
      call sum_sph_layerd_rms
!
      end subroutine cal_rms_sph_spec_local
!
! -----------------------------------------------------------------------
!
      subroutine r_int_sph_rms_data(kg_st, kg_ed)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use radial_int_for_sph_spec
      use sum_sph_rms_data
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
      integer(kind = kint) :: ltr1
!
!
      call radial_integration(nidx_rj(2), nidx_rj(1), kg_st, kg_ed,     &
     &    radius_1d_rj_r, ntot_rms_rj, rms_sph_dat, rms_sph_vol_dat)
!
      ltr1 = l_truncation + 1
      call radial_integration(ltr1, nidx_rj(1), kg_st, kg_ed,           &
     &    radius_1d_rj_r, ntot_rms_rj, rms_sph_l(0,1,1),                &
     &    rms_sph_vol_l(0,1) )
      call radial_integration(ltr1, nidx_rj(1), kg_st, kg_ed,           &
     &    radius_1d_rj_r, ntot_rms_rj, rms_sph_m(0,1,1),                &
     &    rms_sph_vol_m(0,1) )
      call radial_integration(ltr1, nidx_rj(1), kg_st, kg_ed,           &
     &    radius_1d_rj_r, ntot_rms_rj, rms_sph_lm(0,1,1),               &
     &    rms_sph_vol_lm(0,1) )
!
      call sum_sph_rms_all_modes(l_truncation, ione, ntot_rms_rj,       &
     &    rms_sph_vol_l(0,1), rms_sph_vol(1) )
!
!
      end subroutine r_int_sph_rms_data
!
! -----------------------------------------------------------------------
!
      subroutine cal_average_for_sph_rms(kg_st, kg_ed)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_phys_constants
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_sph_phys_address
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
!
      real(kind = kreal) :: avol
!
!
      if(kg_st .eq. 0) then
        avol = three / (radius_1d_rj_r(kg_ed)**3)
      else
        avol = three / (radius_1d_rj_r(kg_ed)**3                        &
     &                - radius_1d_rj_r(kg_st)**3 )
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'surf_ave_4_each_sph_rms'
      call surf_ave_4_each_sph_rms
      if(iflag_debug.gt.0)  write(*,*) 'vol_ave_4_each_sph_rms'
      call vol_ave_4_each_sph_rms(avol)
!
      if(my_rank .gt. 0) return
!
      call surf_ave_4_sph_rms_int
      call vol_ave_4_rms_sph(avol)
!
      end subroutine cal_average_for_sph_rms
!
! -----------------------------------------------------------------------
!
!
      end module cal_rms_fields_by_sph
