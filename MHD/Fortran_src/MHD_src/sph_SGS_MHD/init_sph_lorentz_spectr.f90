!>@file   init_sph_lorentz_spectr.f90
!!@brief      module init_sph_lorentz_spectr
!!
!!@author H. Matsui
!!@date Programmed in  Dec., 2023
!
!> @brief evaluate mean square data from spectr data
!!
!!@verbatim
!!      subroutine s_init_sph_lorentz_spectr(sph_params, sph_rj,        &
!!     &          ipol, ipol_LES, rj_fld, pwr, WK_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module init_sph_lorentz_spectr
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
      private :: set_field_list_Lorentz_spectr
      private :: check_Lorentz_gravity_intertia, check_SGS_forces
      private :: check_forces_for_work_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_init_sph_lorentz_spectr(sph_params, sph_rj,          &
     &          ipol, ipol_LES, rj_fld, pwr, WK_pwr)
!
      use calypso_mpi
      use init_sph_spec_radial_param
      use cal_ave_4_rms_vector_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: i, k
!
!
      call set_all_layer_sph_spectr(sph_params, sph_rj, pwr)
!
      call set_layers_4_sph_spectr(sph_rj, pwr)
!
      call set_field_list_Lorentz_spectr(sph_params, sph_rj,            &
     &    ipol, ipol_LES, rj_fld, pwr, WK_pwr)
!
      do i = 1, pwr%num_vol_spectr
        call init_sph_vol_spectr_r_param(sph_params, sph_rj,            &
     &                                   pwr%v_spectr(i))
        call cal_one_over_volume( pwr%v_spectr(i)%r_inside,             &
     &      pwr%v_spectr(i)%r_outside, pwr%v_spectr(i)%avol)
      end do
!
      if(iflag_debug .gt. 0) then
!      if(my_rank .eq. 0) then
        write(*,*) 'spectr layer data:', pwr%nri_rms
        do k = 1, pwr%nri_rms
          write(*,*) k, pwr%r_4_rms(k,1), pwr%kr_4_rms(k,1:2),          &
     &            sph_rj%radius_1d_rj_r(pwr%kr_4_rms(k,1:2)),           &
     &            pwr%c_gl_itp(k)
        end do
!
        write(*,*) 'volume mean square file area:'
        do i = 1, pwr%num_vol_spectr
          write(*,*) i, pwr%v_spectr(i)%iflag_volume_rms_spec,          &
     &                  trim(pwr%v_spectr(i)%fhead_rms_v),              &
     &                  pwr%v_spectr(i)%avol
        end do
        write(*,*) 'volume average file area:'
        do i = 1, pwr%num_vol_spectr
          write(*,*) i, pwr%v_spectr(i)%iflag_volume_ave_sph,           &
     &                  trim(pwr%v_spectr(i)%fhead_ave)
        end do
        write(*,*) 'Integration area:'
        do i = 1, pwr%num_vol_spectr
          write(*,*) i,                                                 &
     &     pwr%v_spectr(i)%r_inside,  pwr%v_spectr(i)%r_outside,        &
     &     pwr%v_spectr(i)%kr_inside(1:2), pwr%v_spectr(i)%c_inter_in,  &
     &     pwr%v_spectr(i)%kr_outside(1:2), pwr%v_spectr(i)%c_inter_out
        end do
      end if
!
      end subroutine s_init_sph_lorentz_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      logical function check_Lorentz_gravity_intertia(ipol_start,       &
     &                                                ipol_force)
      use t_base_force_labels
!
      integer(kind = kint), intent(in) :: ipol_start
      type(base_force_address), intent(in) :: ipol_force
!
      check_Lorentz_gravity_intertia = .TRUE.
      if(     ipol_start .eq. ipol_force%i_lorentz                      &
     &   .or. ipol_start .eq. ipol_force%i_buoyancy                     &
     &   .or. ipol_start .eq. ipol_force%i_comp_buo                     &
     &   .or. ipol_start .eq. ipol_force%i_m_advect                     &
     &   .or. ipol_start .eq. ipol_force%i_induction                    &
     &  ) return
      
      check_Lorentz_gravity_intertia = .FALSE.
!
      end function check_Lorentz_gravity_intertia
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_forces(ipol_start, ipol_SGS_term)
      use t_SGS_term_labels
!
      integer(kind = kint), intent(in) :: ipol_start
      type(SGS_term_address), intent(in) :: ipol_SGS_term
!
      check_SGS_forces = .TRUE.
      if(     ipol_start .eq. ipol_SGS_term%i_SGS_Lorentz               &
     &   .or. ipol_start .eq. ipol_SGS_term%i_SGS_inertia               &
     &   .or. ipol_start .eq. ipol_SGS_term%i_SGS_buoyancy              &
     &   .or. ipol_start .eq. ipol_SGS_term%i_SGS_comp_buo              &
     &   .or. ipol_start .eq. ipol_SGS_term%i_SGS_induction             &
     &  ) return
      
      check_SGS_forces = .FALSE.
!
      end function check_SGS_forces
!
! ----------------------------------------------------------------------
!
      logical function check_forces_for_work_spectr(ipol_start,         &
     &                                              ipol, ipol_LES)
      use t_SGS_model_addresses
!
      integer(kind = kint), intent(in) :: ipol_start
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      check_forces_for_work_spectr = .TRUE.
      if(     ipol_start .eq. ipol%prod_fld%i_dipole_Lorentz            &
!
     &   .or. check_Lorentz_gravity_intertia(ipol_start, ipol%forces)   &
!
     &   .or. check_Lorentz_gravity_intertia(ipol_start,                &
     &                                       ipol%forces_by_sym_sym)    &
     &   .or. check_Lorentz_gravity_intertia(ipol_start,                &
     &                                       ipol%forces_by_asym_asym)  &
     &   .or. check_Lorentz_gravity_intertia(ipol_start,                &
     &                                       ipol%forces_by_sym_asym)   &
     &   .or. check_Lorentz_gravity_intertia(ipol_start,                &
     &                                       ipol%forces_by_asym_sym)   &
!
     &   .or. check_Lorentz_gravity_intertia(ipol_start,                &
     &                                       ipol_LES%force_by_filter)  &
     &   .or. check_SGS_forces(ipol_start, ipol_LES%SGS_term)           &
        ) return
!
      check_forces_for_work_spectr = .FALSE.
!
      end function check_forces_for_work_spectr
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_list_Lorentz_spectr(sph_params, sph_rj,      &
     &          ipol, ipol_LES, rj_fld, pwr, WK_pwr)
!
      use m_base_force_labels
      use sum_sph_rms_data
      use volume_average_4_sph
      use cal_ave_4_rms_vector_sph
      use sum_sph_rms_by_degree
      use set_parallel_file_name
      use init_rms_4_sph_spectr
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: i_fld, icomp_st, j_fld
!
!
      j_fld = 0
      do i_fld = 1, rj_fld%num_phys
        if(rj_fld%flag_monitor(i_fld)) then
          icomp_st = rj_fld%istack_component(i_fld-1) + 1
          if(check_forces_for_work_spectr(icomp_st, ipol, ipol_LES))    &
     &                                      j_fld = j_fld + 1
        end if
      end do
!
      call alloc_rms_name_sph_spec(j_fld, pwr)
!
      j_fld =  0
      do i_fld = 1, rj_fld%num_phys
        if(rj_fld%flag_monitor(i_fld)) then
          icomp_st = rj_fld%istack_component(i_fld-1) + 1
          if(check_forces_for_work_spectr(icomp_st, ipol, ipol_LES))    &
     &     then
            j_fld = j_fld + 1
            pwr%id_field(j_fld) =   i_fld
            pwr%num_comp_sq(j_fld) =    rj_fld%num_component(i_fld)
            pwr%istack_comp_sq(j_fld) = pwr%istack_comp_sq(j_fld-1)     &
     &                              + rj_fld%num_component(i_fld)
            pwr%pwr_name(j_fld) =     rj_fld%phys_name(i_fld)
          end if
        end if
      end do
!
      call set_domains_4_spectr_output(sph_rj, pwr)
      call alloc_rms_4_sph_spectr                                       &
     &   (my_rank, sph_params%l_truncation, pwr)
      call alloc_ave_4_sph_spectr                                       &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(1), pwr)
      call allocate_rms_sph_local_data                                  &
     &   (sph_params%l_truncation, sph_rj%nidx_rj,                      &
     &    pwr%num_vol_spectr, pwr%nri_rms, pwr%ntot_comp_sq, WK_pwr)
!
      call set_sum_table_4_sph_spectr(sph_params%l_truncation,          &
     &    sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                        &
     &    WK_pwr%num_mode_sum_l,  WK_pwr%num_mode_sum_m,                &
     &    WK_pwr%num_mode_sum_lm, WK_pwr%istack_mode_sum_l,             &
     &    WK_pwr%istack_mode_sum_m, WK_pwr%istack_mode_sum_lm,          &
     &    WK_pwr%item_mode_sum_l, WK_pwr%item_mode_sum_m,               &
     &    WK_pwr%item_mode_sum_lm)
!
      end subroutine set_field_list_Lorentz_spectr
!
! ----------------------------------------------------------------------
!
      end module init_sph_lorentz_spectr
