!>@file  time_step_file_IO.f90
!!       module time_step_file_IO
!!
!!@author H. Matsui and H.Okuda
!!@date   Programmed in July 2000 (ver 1.1)
!!@n      Modified in Aug., 2007
!
!> @brief Labels of fields
!!
!!@verbatim
!!      subroutine write_monitor_labels                                 &
!!     &         (id_ave, id_msq, nod_fld)
!!        type(phys_data), intent(in) :: nod_fld
!!@endverbatim
!
      module time_step_file_IO
!
      use m_precision
!
      implicit none
!
      private :: sym_tensor_label_4_step
      private :: vector_label_4_step, scalar_label_4_step
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_monitor_labels                                   &
     &         (id_ave, id_msq, nod_fld)
!
      use t_phys_data
      use m_phys_labels
      use m_volume_average_labels
      use write_field_labels
!
      type(phys_data), intent(in) :: nod_fld
      integer (kind=kint), intent(in) :: id_ave, id_msq
!
      character(len=kchara) :: vector_label(3)
      integer (kind=kint) :: i
!
!
      call write_one_label(id_msq,       fhd_t_step)
      call write_one_label(id_ave, fhd_t_step)
      call write_one_label(id_msq,       fhd_time)
      call write_one_label(id_ave, fhd_time)
!
      do i = 1, nod_fld%num_phys
        if (nod_fld%iflag_monitor(i) .eq. 1) then
          if ( nod_fld%phys_name(i) .eq. fhd_velo ) then
            call write_one_label(id_msq, e_hd_k_ene)
            call write_one_label(id_msq, e_hd_div_v)
!
            call set_vector_label(fhd_velo, vector_label)
            call write_vector_label(id_ave, vector_label)
            call write_one_label(id_ave, e_hd_div_v)
            call write_vector_label(id_ave, e_hd_lvec)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_magne) then
            call write_three_labels                                     &
     &         (id_msq, e_hd_m_ene, e_hd_m_ene_cd, e_hd_div_b)
!
            call set_vector_label(fhd_magne, vector_label)
            call write_vector_label(id_ave, vector_label)
            call write_vector_label(id_ave, e_hd_bvec_cd)
            call write_one_label(id_ave, e_hd_div_b)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_vecp) then
            call write_one_label(id_msq,       e_hd_div_a)
            call write_one_label(id_ave, e_hd_div_a)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_vort) then
            call vector_label_4_step                                    &
     &         (id_ave, id_msq, nod_fld%phys_name(i), fhd_vort)
            call write_one_label(id_msq, e_hd_rms_w)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_current) then
            call vector_label_4_step                                    &
     &         (id_ave, id_msq, nod_fld%phys_name(i), fhd_current)
            call vector_label_4_step                                    &
     &         (id_ave, id_msq, e_hd_sq_j_cd, e_hd_sq_j_cd)
            call write_one_label(id_msq, e_hd_rms_j)
            call write_one_label(id_msq, e_hd_rms_j_cd)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_filter_velo ) then
            call write_one_label(id_msq, e_hd_fil_k_ene)
            call write_one_label(id_msq, e_hd_fil_div_v)
!
            call set_vector_label(fhd_filter_velo, vector_label)
            call write_vector_label(id_ave, vector_label)
            call write_one_label(id_ave, e_hd_fil_div_v)
            call write_vector_label(id_ave, e_hd_fil_lvec)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_filter_magne ) then
            call write_three_labels(id_msq,                      &
     &            e_hd_fil_m_ene, e_hd_fil_m_ene_cd, e_hd_fil_div_b)
!
            call set_vector_label(fhd_filter_magne, vector_label)
            call write_vector_label(id_ave, vector_label)
            call write_vector_label(id_ave, e_hd_fil_bvec_cd)
            call write_one_label(id_ave, e_hd_fil_div_b)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_filter_vecp ) then
            call write_one_label(id_msq,       e_hd_fil_div_a)
            call write_one_label(id_ave, e_hd_fil_div_a)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_induct_t ) then
            call write_one_label(id_msq, fhd_induct_t)
            call set_asym_tensor_label(fhd_induct_t, vector_label)
            call write_vector_label(id_ave, vector_label)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_SGS_induct_t ) then
            call write_one_label(id_msq, fhd_SGS_induct_t)
            call set_asym_tensor_label(fhd_SGS_induct_t, vector_label)
            call write_vector_label(id_ave, vector_label)
!
!    Old field label... Shold be deleted...
          else if ( nod_fld%phys_name(i) .eq. fhd_buoyancy_work ) then
            call write_one_label(id_msq, fhd_buoyancy_work)
            call write_one_label(id_ave, fhd_buoyancy_work)
          end if
!
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_temp)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_part_temp)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_light)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_part_light)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_press)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_mag_potential)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_entropy)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_per_entropy)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_density)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_per_density)
!
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_heat_source)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_light_source)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_entropy_source)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_e_field)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_poynting)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_buoyancy_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_comp_buo_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_filter_buo_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Lorentz_work)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_work_agst_Lorentz)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_mag_tension_work)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_mag_ene_gen)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_grad_v_1)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_grad_v_2)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_grad_v_3)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_filter_vort)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_filter_current)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_grad_filter_temp)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_grad_filter_comp)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_w_filter_velo)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_w_filter_vort)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_w_filter_vecp)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_w_filter_magne)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_w_filter_current)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_w_filter_grad_temp)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_w_filter_grad_comp)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_d_filter_velo)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_d_filter_vort)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_d_filter_vecp)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_d_filter_magne)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_d_filter_current)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_d_filter_grad_temp)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_d_filter_grad_comp)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_press_grad)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_mag_tension)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_inertia)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i),  fhd_div_m_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_div_maxwell_t)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_div_induct_t)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_vp_induct)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_mag_stretch)
!
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_vecp_diffuse)
          call vector_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_mag_diffuse)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_viscous)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_vis_ene_diffuse)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_mag_ene_diffuse)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_thermal_diffusion)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_c_diffuse)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Lorentz)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Coriolis)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_buoyancy)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_comp_buo)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_filter_buo)
!
          call sym_tensor_label_4_step                                  &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_mom_flux)
          call sym_tensor_label_4_step                                  &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_maxwell_t)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_h_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_ph_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_c_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_pc_flux)
!
!
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_heat_advect)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_part_h_advect)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_div_h_flux)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_div_ph_flux)
          call scalar_label_4_step (id_ave, id_msq,                     &
     &        nod_fld%phys_name(i), fhd_temp_generation)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_part_temp_gen)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_composit_advect)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_part_c_advect)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_div_c_flux)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_div_pc_flux)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_part_comp_gen)
!
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_filter_temp)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_filter_part_temp)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_filter_comp)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_w_filter_temp)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_w_filter_comp)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_d_filter_temp)
          call scalar_label_4_step                                      &
     &       (id_ave, id_msq, nod_fld%phys_name(i), fhd_d_filter_comp)
!
          call sym_tensor_label_4_step(id_ave, id_msq,                  &
     &        nod_fld%phys_name(i), fhd_SGS_m_flux)
          call sym_tensor_label_4_step(id_ave, id_msq,                  &
     &        nod_fld%phys_name(i), fhd_SGS_maxwell_t)
          call sym_tensor_label_4_step(id_ave, id_msq,                  &
     &        nod_fld%phys_name(i), fhd_mom_flux_w_sgs)
          call sym_tensor_label_4_step(id_ave, id_msq,                  &
     &        nod_fld%phys_name(i), fhd_maxwell_t_w_sgs)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_h_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_c_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_c_flux)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_wide_SGS_h_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_wide_SGS_c_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_wide_SGS_inertia)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_wide_SGS_Lorentz)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_wide_SGS_vp_induct)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_dbl_SGS_h_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_dbl_SGS_c_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_dbl_SGS_inertia)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_dbl_SGS_Lorentz)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_dbl_SGS_vp_induct)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_div_SGS_m_flux)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_inertia)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_Lorentz)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_induction)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_vp_induct)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_buoyancy)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_comp_buo)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_rot_inertia)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_rot_Lorentz)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_geostrophic)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_h_flux_w_sgs)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_c_flux_w_sgs)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_inertia_w_sgs)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Lorentz_w_sgs)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_vp_induct_w_sgs)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_mag_induct_w_sgs)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_div_SGS_h_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_div_SGS_c_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_div_inertia)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_div_Lorentz)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_temp_gen)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_m_ene_gen)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_Lorentz_work)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Reynolds_work)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_buo_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_comp_buo_flux)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_h_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_c_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_m_flux)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_Lorentz)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_induction)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_buoyancy)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Csim_SGS_comp_buo)
!
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_div_m_flux_true)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_Lorentz_true)
          call vector_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_mag_induct_true)
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_div_h_flux_true)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_div_c_flux_true)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_Lorentz_wk_true)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_Reynolds_work_true)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_temp_gen_true)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_comp_gen_true)
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_SGS_m_ene_gen_true)
!
        else
!
          if ( nod_fld%phys_name(i) .eq. fhd_velo) then
            call write_one_label(id_msq,       e_hd_div_v)
            call write_one_label(id_ave, e_hd_div_v)
          else if ( nod_fld%phys_name(i) .eq. fhd_magne) then
            call write_one_label(id_msq,       e_hd_div_b)
            call write_one_label(id_ave, e_hd_div_b)
          else if ( nod_fld%phys_name(i) .eq. fhd_vecp) then
            call write_one_label(id_msq,       e_hd_div_a)
            call write_one_label(id_ave, e_hd_div_a)
!
          else if ( nod_fld%phys_name(i) .eq. fhd_filter_velo) then
            call write_one_label(id_msq,       e_hd_fil_div_v)
            call write_one_label(id_ave, e_hd_fil_div_v)
          else if ( nod_fld%phys_name(i) .eq. fhd_filter_vecp) then
            call write_one_label(id_msq,       e_hd_fil_div_a)
            call write_one_label(id_ave, e_hd_fil_div_a)
          else if ( nod_fld%phys_name(i) .eq. fhd_filter_magne) then
            call write_one_label(id_msq,       e_hd_fil_div_b)
            call write_one_label(id_ave, e_hd_fil_div_b)
          end if
!
          call scalar_label_4_step(id_ave, id_msq,                      &
     &        nod_fld%phys_name(i), fhd_mag_potential)
        end if
      end do
!
      call write_one_label(id_msq, e_hd_volume)
!
      write(id_msq,*)      
      write(id_ave,*)
!
      end subroutine write_monitor_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine vector_label_4_step                                    &
     &         (id_ave, id_msq, phys_name, vector_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      character(len=kchara), intent(in) :: phys_name, vector_name
      character(len=kchara) :: vector_label(3)
!
!
      if (phys_name .ne. vector_name)  return
!
      call set_vector_label(vector_name, vector_label)
      call write_one_label(id_msq, vector_name)
      call write_vector_label(id_ave, vector_label)
!
      end subroutine vector_label_4_step
!
! ----------------------------------------------------------------------
!
      subroutine scalar_label_4_step                                    &
     &         (id_ave, id_msq, phys_name, scalar_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      character(len=kchara), intent(in) :: phys_name, scalar_name
!
!
      if (phys_name .ne. scalar_name)  return
!
      call write_one_label(id_msq, scalar_name)
      call write_one_label(id_ave, scalar_name)
!
      end subroutine scalar_label_4_step
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_label_4_step                                &
     &         (id_ave, id_msq, phys_name, tensor_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      character(len=kchara), intent(in) :: phys_name, tensor_name
      integer (kind=kint), intent(in) :: id_ave, id_msq
      character(len=kchara) :: tensor_label(6)
!
!
      if (phys_name .ne. tensor_name)  return
!
      call set_sym_tensor_label(tensor_name, tensor_label)
      call write_one_label(id_msq, tensor_name)
      call write_sym_tensor_label(id_ave, tensor_label)
!
      end subroutine sym_tensor_label_4_step
!
! ----------------------------------------------------------------------
!
      end module time_step_file_IO
