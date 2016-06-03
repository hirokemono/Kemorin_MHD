!>@file   cal_rms_fields_by_sph.f90
!!@brief      module cal_rms_fields_by_sph
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine init_rms_4_sph_spectr(l_truncation, sph_rj, rj_fld)
!!
!!      subroutine cal_mean_squre_in_shell(kr_st, kr_ed, l_truncation,  &
!!     &          sph_rj, rj_fld, g_sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!
      module cal_rms_fields_by_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_rms_4_sph_spectr(l_truncation, sph_rj, rj_fld)
!
      use calypso_mpi
      use m_rms_4_sph_spectr
!
      use sum_sph_rms_data
      use volume_average_4_sph
      use quicksort
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_fld
      integer(kind = kint) :: k, knum
!
!
      num_rms_rj = 0
      do i_fld = 1, rj_fld%num_phys
        num_rms_rj = num_rms_rj + rj_fld%iflag_monitor(i_fld)
      end do
!
      call allocate_rms_name_sph_spec
!
      j_fld = 0
      do i_fld = 1, rj_fld%num_phys
        if(rj_fld%iflag_monitor(i_fld) .gt. 0) then
          j_fld = j_fld + 1
          ifield_rms_rj(j_fld) =   i_fld
          num_rms_comp_rj(j_fld) = rj_fld%num_component(i_fld)
          istack_rms_comp_rj(j_fld) = istack_rms_comp_rj(j_fld-1)       &
     &                              + rj_fld%num_component(i_fld)
          rms_name_rj(j_fld) =     rj_fld%phys_name(i_fld)
        end if
      end do
      ntot_rms_rj = istack_rms_comp_rj(num_rms_rj)
!
      call quicksort_int(nri_rms, kr_for_rms, ione, nri_rms)
!
      call allocate_rms_4_sph_spectr(my_rank, l_truncation)
      call allocate_ave_4_sph_spectr                                    &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(1))
      call set_sum_table_4_sph_spectr                                   &
     &   (l_truncation, sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j)
!
!

      do knum = 1, nri_rms
        k = kr_for_rms(knum)
        if(k .le. 0) then
          r_for_rms(knum) = 0.0d0
        else
          r_for_rms(knum) = sph_rj%radius_1d_rj_r(k)
        end if
      end do
!
      end subroutine init_rms_4_sph_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_mean_squre_in_shell(kr_st, kr_ed, l_truncation,    &
     &          sph_rj, rj_fld, g_sph_rj)
!
      use calypso_mpi
      use m_rms_4_sph_spectr
!
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      real(kind = kreal) :: avol
!
!
      if(ntot_rms_rj .eq. 0) return

      if(iflag_debug .gt. 0) write(*,*) 'cal_one_over_volume'
      call cal_one_over_volume(kr_st, kr_ed,                            &
     &   sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, avol)
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_rms'
      call sum_sph_layerd_rms(kr_st, kr_ed,                             &
     &    l_truncation, sph_rj, g_sph_rj, rj_fld)
!
      if(my_rank .eq. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'surf_ave_4_sph_rms_int'
        call surf_ave_4_sph_rms_int                                     &
     &     (l_truncation, sph_rj%nidx_rj(1), sph_rj%a_r_1d_rj_r)
        call vol_ave_4_rms_sph(l_truncation, avol)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_average_sph'
      call cal_volume_average_sph(kr_st, kr_ed, avol, sph_rj, rj_fld)
!
      end subroutine cal_mean_squre_in_shell
!
! ----------------------------------------------------------------------
!
      end module cal_rms_fields_by_sph
