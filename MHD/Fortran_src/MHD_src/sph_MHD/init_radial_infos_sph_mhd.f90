!>@file   init_radial_infos_sph_mhd.f90
!!@brief  module init_radial_infos_sph_mhd
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine init_r_infos_sph_mhd_evo(bc_IO, sph_grps, MHD_BC,    &
!!     &          ipol, sph, r_2nd, omega_sph, MHD_prop, sph_MHD_bc)
!!      subroutine init_reference_fields(sph, ipol, r_2nd,              &
!!     &          refs, rj_fld, MHD_prop, sph_MHD_bc)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(inout) :: r_2nd
!!        type(sph_rotation), intent(inout) :: omega_sph
!!        type(radial_reference_field), intent(inout) :: refs
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine set_delta_r_4_sph_mhd(sph_params, sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!@endverbatim
!!
!!@n @param r_hot        radius at highest temperature point
!!@n @param r_cold       radius at lowest temperature point
!!@n @param temp_hot     temperature at highest temperature point
!!@n @param temp_cold    temperature at lowest temperature point
!!@n @param rotate(3)    rotation vector
!
      module init_radial_infos_sph_mhd
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_spheric_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_group
      use t_poloidal_rotation
      use t_radial_reference_field
      use t_fdm_coefs
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_boundary_data_sph_MHD
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_physical_property
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_r_infos_sph_mhd_evo(bc_IO, sph_grps, MHD_BC,      &
     &          ipol, sph, r_2nd, r_n2e_3rd, r_e2n_1st,                 &
     &          omega_sph, MHD_prop, sph_MHD_bc)
!
      use second_fdm_node_coefs
      use third_fdm_node_to_ele
      use first_fdm_ele_to_node
      use material_property
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
!
      type(fdm_matrices), intent(inout) :: r_2nd
      type(fdm_matrices), intent(inout) :: r_n2e_3rd
      type(fdm_matrices), intent(inout) :: r_e2n_1st
!
      type(sph_rotation), intent(inout) :: omega_sph
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
      real(kind = kreal), allocatable :: h_rho(:)
!
!
      allocate(h_rho(sph%sph_rj%nidx_rj(1)))
      h_rho(:) = zero
      call init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph, MHD_prop, &
     &                          omega_sph, h_rho, sph_MHD_bc)
      deallocate(h_rho)
!
!
      if (iflag_debug.gt.0) write(*,*) 'const_second_fdm_coefs'
      call const_second_fdm_coefs(sph%sph_params, sph%sph_rj, r_2nd)
!
      if (iflag_debug.gt.0) write(*,*) 'const_first_fdm_ele_to_node'
      call const_first_fdm_ele_to_node(sph%sph_rj, r_e2n_1st)
      if (iflag_debug.gt.0) write(*,*) 'const_third_fdm_node_to_ele'
      call const_third_fdm_node_to_ele(sph%sph_rj, r_n2e_3rd)
!
      call init_radius_variations_sph_mhd &
     &   (bc_IO, sph_grps, MHD_BC, sph, r_2nd, MHD_prop, &
     &                          omega_sph, sph_MHD_bc)
!
      if(iflag_debug.gt.0) write(*,*)' set_material_property'
      call set_material_property                                        &
     &   (sph%sph_params%radius_CMB, sph%sph_params%radius_ICB,         &
     &    ipol, MHD_prop)
!
      end subroutine init_r_infos_sph_mhd_evo
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph,     &
     &                                MHD_prop, omega_sph, h_rho,       &
     &                                sph_MHD_bc)
!
      use set_bc_sph_mhd
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      real(kind = kreal), intent(in) :: h_rho(sph%sph_rj%nidx_rj(1))
!
      type(sph_rotation), intent(inout) :: omega_sph
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
      type(phys_data) :: radial_variation
      type(sph_radial_interpolate) :: r_itp
      integer(kind = kint) :: icou_ref = 0
      integer(kind = kint) :: k
      real(kind = kreal) :: r_in, r_out, rho_in, rho_out
      real(kind = kreal) :: beta, N_p, xi_0, p_idx
      real(kind = kreal) :: c_0, c_1, xi_r, dxi_dr
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_delta_r_4_sph_mhd'
      call set_delta_r_4_sph_mhd(sph%sph_params, sph%sph_rj)
!
!*  ----------  rotation of earth  ---------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &                write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(sph%sph_rlm, sph%sph_rj,                 &
     &    MHD_prop%fl_prop, omega_sph)
!
!*  ---------- boundary conditions  ---------------
      if(iflag_debug.gt.0) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd                                             &
     &   (bc_IO, sph%sph_params, sph%sph_rj, sph_grps%radial_rj_grp,    &
     &    MHD_prop, MHD_BC, h_rho, sph_MHD_bc)
!
      if(iflag_debug .ge. iflag_full_msg) then
        call check_bc_sph_mhd(MHD_prop, sph_MHD_bc)
      end if
!
      end subroutine init_r_infos_sph_mhd
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine init_radius_variations_sph_mhd                         &
     &         (bc_IO, sph_grps, MHD_BC, sph, r_2nd,                    &
     &          MHD_prop, omega_sph, sph_MHD_bc)
!
      use set_bc_sph_mhd
      use t_sph_radial_interpolate
      use radial_interpolation
      use const_diffusive_profile
      use cal_sph_exp_1st_diff
      use field_file_IO
      use calypso_mpi_real
      use transfer_to_long_integers
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
      type(sph_rotation), intent(inout) :: omega_sph
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
      type(phys_data) :: radial_variation
      integer(kind = kint) :: icou_ref = 0
      integer(kind = kint) :: k
!
!
      icou_ref = 1
      if(MHD_prop%flag_ref_density_valiation)  icou_ref = icou_ref + 2
      if(MHD_prop%flag_viscous_variation)      icou_ref = icou_ref + 2
      if(MHD_prop%flag_mag_diffuse_variation)  icou_ref = icou_ref + 2
      if(MHD_prop%flag_term_diffuse_variation) icou_ref = icou_ref + 2
      if(MHD_prop%flag_comp_diffuse_variation) icou_ref = icou_ref + 2
      radial_variation%num_phys =  icou_ref
      radial_variation%ntot_phys = icou_ref
      call alloc_phys_name(radial_variation)
      call alloc_phys_data((sph%sph_rj%nidx_rj(1)+1), radial_variation)
      write(*,*) 'radial_variation%num_phys', radial_variation%num_phys
      write(*,*) 'radial_variation%ntot_phys', radial_variation%ntot_phys
!
      icou_ref = 1
      radial_variation%phys_name(icou_ref) = 'radius'
      radial_variation%d_fld(1,1) = 0.0d0
      do k = 1, sph%sph_rj%nidx_rj(1)
        radial_variation%d_fld(k+1,1) = sph%sph_rj%radius_1d_rj_r(k)
      end do
!
      if(MHD_prop%flag_ref_density_valiation) then
        write(*,*) 'flag_ref_density_valiation ON'
        MHD_prop%fl_prop%ir_rho =       icou_ref + 1
        MHD_prop%fl_prop%ir_drho_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%fl_prop%ir_rho)             &
    &                           = 'density'
        radial_variation%phys_name(MHD_prop%fl_prop%ir_drho_norm)       &
    &                           = 'normalized_drho_dr'
        icou_ref = icou_ref + 2
      end if
!
      if(MHD_prop%flag_viscous_variation) then
        write(*,*) 'flag_ref_density_valiation ON'
        MHD_prop%fl_prop%ir_nu =       icou_ref + 1
        MHD_prop%fl_prop%ir_dnu_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%fl_prop%ir_nu)              &
    &                           = 'viscousity'
        radial_variation%phys_name(MHD_prop%fl_prop%ir_dnu_norm)        &
    &                           = 'normalized_dnu_dr'
        icou_ref = icou_ref + 2
      end if
      if(MHD_prop%flag_mag_diffuse_variation) then
        write(*,*) 'flag_mag_diffuse_variation ON'
        MHD_prop%cd_prop%ir_eta =       icou_ref + 1
        MHD_prop%cd_prop%ir_deta_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%cd_prop%ir_eta)             &
    &                           = 'magnetic_diffusivity'
        radial_variation%phys_name(MHD_prop%cd_prop%ir_deta_norm)       &
    &                           = 'normalized_deta_dr'
        icou_ref = icou_ref + 2
      end if
      if(MHD_prop%flag_term_diffuse_variation) then
        write(*,*) 'flag_term_diffuse_variation ON'
        MHD_prop%ht_prop%ir_kappa =       icou_ref + 1
        MHD_prop%ht_prop%ir_dkappa_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%ht_prop%ir_kappa)           &
    &                           = 'thermal_diffusivity'
        radial_variation%phys_name(MHD_prop%ht_prop%ir_dkappa_norm)     &
    &                           = 'normalized_dkappa_T_dr'
        icou_ref = icou_ref + 2
      end if
      if(MHD_prop%flag_comp_diffuse_variation) then
        write(*,*) 'flag_comp_diffuse_variation ON'
        MHD_prop%cp_prop%ir_kappa =       icou_ref + 1
        MHD_prop%cp_prop%ir_dkappa_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%cp_prop%ir_kappa)           &
    &                           = 'compositional_diffusivity'
        radial_variation%phys_name(MHD_prop%cp_prop%ir_dkappa_norm)     &
    &                           = 'normalized_dkappa_C_dr'
        icou_ref = icou_ref + 2
      end if
!
      write(*,*) 'MHD_prop%fl_prop%ir_rho', MHD_prop%fl_prop%ir_rho
      write(*,*) 'MHD_prop%fl_prop%ir_drho_norm', MHD_prop%fl_prop%ir_drho_norm
      write(*,*) 'MHD_prop%fl_prop%ir_nu', MHD_prop%fl_prop%ir_nu
      write(*,*) 'MHD_prop%fl_prop%ir_dnu_norm', MHD_prop%fl_prop%ir_dnu_norm
      write(*,*) 'MHD_prop%cd_prop%ir_eta', MHD_prop%cd_prop%ir_eta
      write(*,*) 'MHD_prop%cd_prop%ir_deta_norm', MHD_prop%cd_prop%ir_deta_norm
      write(*,*) 'MHD_prop%ht_prop%ir_kappa', MHD_prop%ht_prop%ir_kappa
      write(*,*) 'MHD_prop%ht_prop%ir_dkappa_norm', MHD_prop%ht_prop%ir_dkappa_norm
      write(*,*) 'MHD_prop%cp_prop%ir_kappa', MHD_prop%cp_prop%ir_kappa
      write(*,*) 'MHD_prop%cp_prop%ir_dkappa_norm', MHD_prop%cp_prop%ir_dkappa_norm
      do icou_ref = 1, radial_variation%num_phys
        write(*,*) icou_ref, trim(radial_variation%phys_name(icou_ref))
      end do
!
      call set_radial_density_sph_mhd         &
     &   (sph, r_2nd, MHD_prop, radial_variation)
!
      end subroutine init_radius_variations_sph_mhd
!
!  -------------------------------------------------------------------
!
      subroutine set_radial_density_sph_mhd         &
     &         (sph, r_2nd, MHD_prop, radial_variation)
!
      use set_bc_sph_mhd
      use t_sph_radial_interpolate
      use radial_interpolation
      use const_diffusive_profile
      use cal_sph_exp_1st_diff
      use field_file_IO
      use calypso_mpi_real
      use transfer_to_long_integers
      use m_base_field_labels
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
      type(phys_data), intent(inout) :: radial_variation
!
      type(sph_radial_interpolate) :: r_itp
      type(time_data) :: t_IO
      type(field_IO) :: fld_IO
      integer(kind = kint) :: iend
      integer(kind = kint) :: icou, i_r, i_den
      integer(kind = kint) :: icou_ref = 0
      integer(kind = kint) :: k
      real(kind = kreal) :: r_in, r_out, rho_in, rho_out
      real(kind = kreal) :: beta, N_p, xi_0, p_idx
      real(kind = kreal) :: c_0, c_1, xi_r, dxi_dr
      character(len=kchara), parameter :: radius_nume = 'radius'
!
      if(my_rank .eq. 0) then
!
      p_idx =   MHD_prop%polytrope_param%polytrope_idx
      if(p_idx .le. 0.0d0) then
        if(MHD_prop%polytrope_param%num_density_list .le. 0) then
            call read_and_alloc_step_field                              &
     &         (MHD_prop%polytrope_param%density_file_name,             &
     &          my_rank, t_IO, fld_IO, iend)
!
          i_r =   find_address_from_field_IO(radius_nume, fld_IO)
          i_den = find_address_from_field_IO(density%name, fld_IO)
          write(*,*) 'i_den', i_den, i_r
          call alloc_density_variation_list(fld_IO%nnod_IO,             &
     &                                      MHD_prop%polytrope_param)
          MHD_prop%polytrope_param%density_radius(1:fld_IO%nnod_IO)     &
     &                            = fld_IO%d_IO(1:fld_IO%nnod_IO,i_r)
          MHD_prop%polytrope_param%density_list(1:fld_IO%nnod_IO)       &
     &                            = fld_IO%d_IO(1:fld_IO%nnod_IO,i_den)
        end if
!
        call alloc_org_radius_interpolate                               &
     &     (MHD_prop%polytrope_param%num_density_list, r_itp)
        call alloc_radial_interpolate(radial_variation%n_point, r_itp)
        call alloc_original_sph_data                                    &
     &     (MHD_prop%polytrope_param%num_density_list, r_itp)

        r_itp%source_radius(1:r_itp%nri_source)                         &
     &    = MHD_prop%polytrope_param%density_radius(1:r_itp%nri_source)
        call cal_radial_interpolation_coef                              &
     &     (r_itp%nri_source, r_itp%source_radius,                      &
     &      radial_variation%n_point, radial_variation%d_fld(1,1),      &
     &      r_itp%kr_inner_source, r_itp%kr_outer_source,               &
     &      r_itp%k_old2new_in, r_itp%k_old2new_out,                    &
     &      r_itp%coef_old2new_in)
!        call check_sph_radial_interpolate                              &
!     &     (r_itp%nri_source, r_itp%source_radius,                     &
!     &      radial_variation%n_point, radial_variation%d_fld(1,1),     &
!     &      r_itp)
        call interpolate_radial_field(radial_variation%n_point,         &
     &      r_itp%k_old2new_in, r_itp%k_old2new_out,                    &
     &      r_itp%coef_old2new_in, ione,                                &
     &      r_itp%nri_source, MHD_prop%polytrope_param%density_list(1), &
     &      radial_variation%d_fld(1,MHD_prop%fl_prop%ir_rho))
        call dealloc_original_sph_data(r_itp)
        call dealloc_radial_interpolate(r_itp)
        call dealloc_org_radius_interpolate(r_itp)
!
        call cal_sph_nod_gradient_1d(ione, sph%sph_rj%nidx_rj(1),       &
     &      sph%sph_rj%nidx_rj(1), r_2nd%fdm(1)%dmat,                   &
     &      radial_variation%d_fld(2,MHD_prop%fl_prop%ir_rho),          &
     &      radial_variation%d_fld(2,MHD_prop%fl_prop%ir_drho_norm))
        radial_variation%d_fld(1,MHD_prop%fl_prop%ir_drho_norm) = zero
        k = sph%sph_rj%nidx_rj(1) + 1
        radial_variation%d_fld(k,MHD_prop%fl_prop%ir_drho_norm) = zero
      else
        r_in =    MHD_prop%polytrope_param%rho_bottom(1)
        r_out =   MHD_prop%polytrope_param%rho_top(1)
        rho_in =  MHD_prop%polytrope_param%rho_bottom(2)
        rho_out = MHD_prop%polytrope_param%rho_top(2)
        beta = r_in / r_out
        N_p = log(rho_in / rho_out)
        xi_0 = (one + beta) / (one + beta * exp(N_p / p_idx))
        c_0 = (two * xi_0 - beta - one) / (one - beta)
        c_1 = (one + beta) * (one - xi_0) / ((one - beta)**2)
!
        do k = 1, sph%sph_rj%nidx_rj(1)
          xi_r =   c_0 + c_1 * sph%sph_rj%ar_1d_rj(k,1)
          dxi_dr =     - c_1 * sph%sph_rj%ar_1d_rj(k,2)
!
          radial_variation%d_fld(k+1,MHD_prop%fl_prop%ir_rho)           &
     &          = xi_r**p_idx
          radial_variation%d_fld(k+1,MHD_prop%fl_prop%ir_drho_norm)     &
!     &        = p_idx * xi_r**(p_idx-1.0d0) * dxi_dr / xi_r**p_idx
     &          = p_idx * dxi_dr / xi_r
        end do
      end if
      end if
!
      call calypso_mpi_bcast_real                                       &
     &   (radial_variation%d_fld(1,MHD_prop%fl_prop%ir_rho),            &
     &    cast_long(radial_variation%n_point), 0)
      call calypso_mpi_bcast_real                                       &
     &   (radial_variation%d_fld(1,MHD_prop%fl_prop%ir_drho_norm),      &
     &    cast_long(radial_variation%n_point), 0)
!
      do k = 1, sph%sph_rj%nidx_rj(1)
        write(*,*) k, sph%sph_rj%radius_1d_rj_r(k),                     &
     &      sph%sph_rj%ar_1d_rj(k,1),                                   &
     &      radial_variation%d_fld(k+1,MHD_prop%fl_prop%ir_rho),        &
     &      radial_variation%d_fld(k+1,MHD_prop%fl_prop%ir_drho_norm)
      end do
!
      end subroutine set_radial_density_sph_mhd
!
!  -------------------------------------------------------------------
!
      integer(kind = kint) function find_address_from_field_IO          &
     &                                          (target_name, fld_IO)
!
      character(len = kchara), intent(in) :: target_name
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: i_field, icou, i
!
      i_field = 0
      icou = 0
      do i = 1, fld_IO%num_field_IO
        if(fld_IO%fld_name(i) .eq. target_name) then
          i_field = icou + 1
          exit
        end if
        icou = icou + fld_IO%num_comp_IO(i)
      end do
      if(i_field .le. 0) write(*,*) trim(target_name),                  &
     &                           ' cannot be found...'
      find_address_from_field_IO = i_field
!
      end function find_address_from_field_IO
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_delta_r_4_sph_mhd(sph_params, sph_rj)
!
      use set_radius_func_noequi
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_shell_parameters), intent(in) :: sph_params
!
!   Choose radial grid mode
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_dr_for_nonequi'
      call allocate_dr_rj_noequi(sph_rj%nidx_rj(1))
      call set_dr_for_nonequi(sph_params%nlayer_CMB,                    &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
!*
      end subroutine set_delta_r_4_sph_mhd
!
!  -------------------------------------------------------------------
!
      subroutine init_reference_fields(sph, ipol, r_2nd,                &
     &          refs, rj_fld, MHD_prop, sph_MHD_bc)
!
      use sph_mhd_rst_IO_control
      use reference_sources_from_d_rj
      use init_reference_scalar
      use init_external_magne_sph
      use radial_reference_field_IO
      use m_base_field_labels
!
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
!
      type(radial_reference_field), intent(inout) :: refs
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara), parameter                                  &
     &            :: tmat_name = 'reference_Temperature'
      character(len=kchara), parameter                                  &
     &            :: cmat_name = 'reference_Composition'
      logical :: flag_write_ref
!
!
      call init_reft_rj_data(sph%sph_rj, ipol, refs)
!
      write(*,*) my_rank, 'refs%ref_field%n_point',   refs%ref_field%n_point
      write(*,*) my_rank, 'refs%ref_field%num_phys',  refs%ref_field%num_phys
      write(*,*) my_rank, 'refs%ref_field%ntot_phys', refs%ref_field%ntot_phys
      write(*,*) my_rank, 'refs%ref_field%phys_name: ', refs%ref_field%phys_name
      write(*,*) my_rank, 'refs%ref_field%d_fld: ',   &
    &    size(refs%ref_field%d_fld,1), size(refs%ref_field%d_fld,2)
      write(*,*) my_rank, 'refs%ref_field%d_fld: ', refs%ref_field%d_fld(:,1)
     call calypso_mpi_barrier
     call calypso_mpi_abort(222, 'Tako')
!
      call cal_ref_sources_from_d_rj(sph, ipol, rj_fld, refs)
      call load_sph_reference_fields(refs)
      call overwrite_sources_by_reference(sph%sph_rj, refs%iref_base,   &
     &    ipol%base, refs%ref_field, rj_fld)
!
!
      flag_write_ref = .FALSE.
      call s_init_reference_scalar                                      &
     &   (MHD_prop%takepito_T, sph%sph_params, sph%sph_rj,              &
     &    r_2nd, MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T,                 &
     &    sph_MHD_bc%fdm2_center, tmat_name, MHD_prop%ref_param_T,      &
     &    refs%iref_radius, temperature%name,                           &
     &    refs%iref_base%i_temp, refs%iref_grad%i_grad_temp,            &
     &    refs%iref_base%i_heat_source, refs%r_itp, refs%ref_fld_IO,    &
     &    refs%ref_field, sph_MHD_bc%bcs_T, flag_write_ref)
!
      call s_init_reference_scalar                                      &
     &   (MHD_prop%takepito_C, sph%sph_params, sph%sph_rj,              &
     &    r_2nd, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C,                 &
     &    sph_MHD_bc%fdm2_center, cmat_name, MHD_prop%ref_param_C,      &
     &    refs%iref_radius, composition%name,                           &
     &    refs%iref_base%i_light, refs%iref_grad%i_grad_composit,       &
     &    refs%iref_base%i_light_source, refs%r_itp, refs%ref_fld_IO,   &
     &    refs%ref_field, sph_MHD_bc%bcs_C, flag_write_ref)
!
      call init_sph_contant_ext_magne(MHD_prop%cd_prop, sph%sph_rj,     &
     &    refs%iref_cmp, ipol%base, refs%ref_field, rj_fld,             &
     &    flag_write_ref)
!
      call calypso_mpi_barrier
!
      if(flag_write_ref .eqv. .FALSE.) return
      call set_default_reference_file_name(refs)
      call output_reference_field(refs)
!
      end subroutine init_reference_fields
!
!  -------------------------------------------------------------------
!
      end module init_radial_infos_sph_mhd
