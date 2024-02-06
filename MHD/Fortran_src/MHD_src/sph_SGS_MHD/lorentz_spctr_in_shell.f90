!>@file   lorentz_spctr_in_shell.f90
!!@brief      module lorentz_spctr_in_shell
!!
!!@author H. Matsui
!!@date Programmed in  Dec., 2023
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine cal_lorentz_spctr_in_shell(sph_params, sph_rj,       &
!!     &          ipol, ipol_LES, rj_fld, leg, pwr, WK_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module lorentz_spctr_in_shell
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use t_spheric_parameter
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_sum_sph_rms_data
      use t_rms_4_sph_spectr
      use t_sph_volume_mean_square
!
      implicit none
!
      private :: sum_sph_layerd_lorentz_wk
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_lorentz_spctr_in_shell(sph_params, sph_rj,         &
     &          ipol, ipol_LES, rj_fld, leg, pwr, WK_pwr)
!
      use calypso_mpi
!
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_data
      use cal_rms_fields_by_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(pwr%ntot_comp_sq .eq. 0) return
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_sph_layerd_lorentz_wk'
      call sum_sph_layerd_lorentz_wk(sph_params%l_truncation, sph_rj,   &
     &    pwr, ipol, ipol_LES, rj_fld, leg%g_sph_rj, WK_pwr)
!
      if(iflag_debug .gt. 0) write(*,*) 'global_sum_sph_layerd_square'
      call global_sum_sph_layerd_square                                 &
     &    (sph_params%l_truncation, WK_pwr, pwr)
      call global_sum_sph_volume_square                                 &
     &    (sph_params%l_truncation, pwr%ntot_comp_sq, WK_pwr,           &
     &     pwr%num_vol_spectr, pwr%v_spectr)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_average_sph'
      call cal_volume_average_sph(sph_rj, rj_fld, pwr)
!
      call sum_mean_square_on_sphere(sph_params, pwr)
      call sum_mean_square_on_volume(sph_params, pwr%ntot_comp_sq,      &
     &    pwr%num_vol_spectr, pwr%v_spectr)
!
      end subroutine cal_lorentz_spctr_in_shell
!
! ----------------------------------------------------------------------
!
      subroutine sum_sph_layerd_lorentz_wk(l_truncation, sph_rj, pwr,   &
     &          ipol, ipol_LES, rj_fld, g_sph_rj, WK_lor_mode)
!
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_volume_mean_square
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
      use radial_int_for_sph_spec
      use sum_sph_rms_by_degree
      use sum_sph_rms_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
      integer(kind = kint), intent(in) :: l_truncation
      real(kind = kreal), intent(in)                                    &
     &                     :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(sph_mean_square_work), intent(inout) :: WK_lor_mode
!
      integer(kind = kint) :: i_fld, icomp_st, icomp_ref
      integer(kind = kint) :: j_fld, jcomp_st, ncomp_rj
!
!$omp parallel workshare
      WK_lor_mode%shl_l_local =  zero
      WK_lor_mode%shl_m_local =  zero
      WK_lor_mode%shl_lm_local = zero
      WK_lor_mode%vol_l_local =  zero
      WK_lor_mode%vol_m_local =  zero
      WK_lor_mode%vol_lm_local = zero
!$omp end parallel workshare
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld =    pwr%id_field(j_fld)
        icomp_st = rj_fld%istack_component(i_fld-1) + 1
        jcomp_st = pwr%istack_comp_sq(j_fld-1) + 1
        ncomp_rj = pwr%istack_comp_sq(j_fld)                            &
     &            - pwr%istack_comp_sq(j_fld-1)
        if(      icomp_st .eq. ipo%forces%i_induction                   &
     &      .or. icomp_st .eq. ipo%forces_by_sym_sym%i_induction        &
     &      .or. icomp_st .eq. ipo%forces_by_asym_asym%i_induction      &
     &      .or. icomp_st .eq. ipo%forces_by_sym_asym%i_induction       &
     &      .or. icomp_st .eq. ipo%forces_by_asym_sym%i_induction       &
     &      .or. icomp_st .eq. ipol_LES%SGS_term%i_SGS_induction        &
     &   ) then
          icomp_ref = ipol%base%i_magne
        else
          icomp_ref = ipol%base%i_velo
        end if
!
        call cal_rms_sph_spec_one_field                                 &
     &     (sph_rj, ncomp_rj, g_sph_rj, icomp_ref, icomp_st,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,             &
     &      WK_lor_mode%shl_rj(0,1,1))
        call sum_each_sph_layerd_pwr(l_truncation, sph_rj, pwr,         &
     &                               ncomp_rj, jcomp_st, WK_lor_mode)
      end do
!
      end subroutine sum_sph_layerd_lorentz_wk
!
! -----------------------------------------------------------------------
!
      end module lorentz_spctr_in_shell
