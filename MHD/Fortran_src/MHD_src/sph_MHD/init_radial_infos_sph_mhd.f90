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
!!      subroutine init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph,   &
!!     &                                MHD_prop, omega_sph, sph_MHD_bc)
!!      subroutine init_reference_scalars(sph, ipol, r_2nd,             &
!!     &          ref_temp, ref_comp, rj_fld, MHD_prop, sph_MHD_bc)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(inout) :: r_2nd
!!        type(sph_rotation), intent(inout) :: omega_sph
!!        type(reference_field), intent(inout) :: ref_temp, ref_comp
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!        type(phys_data), intent(inout) :: rj_fld
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
      use t_radial_reference_temp
      use t_fdm_coefs
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_boundary_data_sph_MHD
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_radial_references
!
      implicit none
!
      private :: set_delta_r_4_sph_mhd, init_reference_scalar
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_r_infos_sph_mhd_evo(bc_IO, sph_grps, MHD_BC,      &
     &          ipol, sph, r_2nd, omega_sph, MHD_prop, sph_MHD_bc)
!
      use calypso_mpi
      use second_fdm_node_coefs
      use material_property
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
!
      type(fdm_matrices), intent(inout) :: r_2nd
      type(sph_rotation), intent(inout) :: omega_sph
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      call init_r_infos_sph_mhd(bc_IO, sph_grps, MHD_BC, sph, MHD_prop, &
     &                          omega_sph, sph_MHD_bc)
!
      if (iflag_debug.gt.0) write(*,*) 'const_second_fdm_coefs'
      call const_second_fdm_coefs(sph%sph_params, sph%sph_rj, r_2nd)
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
     &                                MHD_prop, omega_sph, sph_MHD_bc)
!
      use set_bc_sph_mhd
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_group_data), intent(in) :: sph_grps
      type(MHD_BC_lists), intent(in) :: MHD_BC
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_rotation), intent(inout) :: omega_sph
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
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
!*  ---------- boudary conditions  ---------------
      if(iflag_debug.gt.0) write(*,*) 's_set_bc_sph_mhd'
      call s_set_bc_sph_mhd                                             &
     &   (bc_IO, sph%sph_params, sph%sph_rj, sph_grps%radial_rj_grp,    &
     &    MHD_prop, MHD_BC, sph_MHD_bc)
!
      end subroutine init_r_infos_sph_mhd
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
      subroutine init_reference_scalars(sph, ipol, r_2nd,               &
     &          ref_temp, ref_comp, rj_fld, MHD_prop, sph_MHD_bc)
!
      use t_physical_property
!
      type(phys_address), intent(in) :: ipol
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
!
      type(reference_field), intent(inout) :: ref_temp, ref_comp
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara) :: mat_name
!
!
      write(mat_name,'(a)') 'reference_Temperature'
      call init_reference_scalar(MHD_prop%takepito_T,                   &
     &    sph%sph_params, sph%sph_rj, r_2nd, rj_fld, MHD_prop%ht_prop,  &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center, mat_name,        &
     &    ipol%base%i_heat_source, MHD_prop%ref_param_T,                &
     &    ref_temp, sph_MHD_bc%bcs_T)
!
      write(mat_name,'(a)') 'reference_Composition'
      call init_reference_scalar(MHD_prop%takepito_C,                   &
     &    sph%sph_params, sph%sph_rj, r_2nd, rj_fld, MHD_prop%cp_prop,  &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center, mat_name,        &
     &    ipol%base%i_light_source, MHD_prop%ref_param_C,               &
     &    ref_comp, sph_MHD_bc%bcs_C)
!
      end subroutine init_reference_scalars
!
!  -------------------------------------------------------------------
!
      subroutine init_reference_scalar(takepiro, sph_params, sph_rj,    &
     &          r_2nd, rj_fld, sc_prop, sph_bc_S, fdm2_center,          &
     &          mat_name, i_source, ref_param, reftemp, bcs_S)
!
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_reference_scalar_param
      use t_sph_matrix
      use set_reference_sph_mhd
      use set_reference_temp_sph
      use const_r_mat_4_scalar_sph
!
      character(len=kchara), intent(in) :: mat_name
      integer(kind = kint), intent(in) :: i_source
      type(takepiro_model_param), intent(in) :: takepiro
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(phys_data), intent(in) :: rj_fld
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      type(reference_scalar_param), intent(inout) :: ref_param
      type(reference_field), intent(inout) :: reftemp
      type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!
      type(band_matrix_type) :: band_s00_poisson
!
!      Set reference temperature and adjust boundary conditions
!
      if(iflag_debug .gt. 0) write(*,*) 'alloc_reft_rj_data'
      call alloc_reft_rj_data(sph_rj%nidx_rj(1), reftemp)
!
      if (ref_param%iflag_reference .eq. id_sphere_ref_temp) then
        if(iflag_debug .gt. 0) write(*,*) 'set_ref_temp_sph_mhd'
        call set_ref_temp_sph_mhd                                       &
     &   (ref_param%low_value, ref_param%depth_top,                     &
     &    ref_param%high_value, ref_param%depth_bottom,                 &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, sph_rj%ar_1d_rj,       &
     &    reftemp%t_rj)
      else if(ref_param%iflag_reference .eq. id_takepiro_temp) then
        call set_stratified_sph_mhd(takepiro%stratified_sigma,          &
     &    takepiro%stratified_width, takepiro%stratified_outer_r,       &
     &    sph_rj%nidx_rj, sph_params%radius_ICB, sph_params%radius_CMB, &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_rj%radius_1d_rj_r, reftemp%t_rj)
      else if(ref_param%iflag_reference                                 &
     &                             .eq. id_numerical_solution) then
        call const_r_mat00_scalar_sph                                   &
     &     (mat_name, sc_prop%ICB_diffusie_reduction,                   &
     &      sph_params, sph_rj, r_2nd, sph_bc_S, fdm2_center,           &
     &      band_s00_poisson)
        call const_diffusive_profiles                                   &
     &     (sph_rj, sph_bc_S, bcs_S, fdm2_center, r_2nd,                &
     &      band_s00_poisson, i_source, rj_fld, reftemp%t_rj)
        call dealloc_band_matrix(band_s00_poisson)
      else
        call no_ref_temp_sph_mhd(ref_param%depth_top,                   &
     &      ref_param%depth_bottom, sph_rj%nidx_rj(1),                  &
     &      sph_params%radius_ICB, sph_params%radius_CMB, reftemp%t_rj)
      end if
!
      if (ref_param%iflag_reference .eq. id_sphere_ref_temp             &
     & .or. ref_param%iflag_reference .eq. id_takepiro_temp             &
     & .or. ref_param%iflag_reference .eq. id_numerical_solution) then
        call adjust_sph_temp_bc_by_reftemp                              &
     &     (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(1),               &
     &      reftemp%t_rj, sph_bc_S, bcs_S%ICB_Sspec, bcs_S%CMB_Sspec,   &
     &      bcs_S%ICB_Sevo, bcs_S%CMB_Sevo)
      end if
!
!      call set_reftemp_4_sph                                           &
!     &   (sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,            &
!     &    sph_rj%nidx_rj, reftemp%t_rj, i_ref, i_gref,                 &
!     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine init_reference_scalar
!
! -----------------------------------------------------------------------
!
      end module init_radial_infos_sph_mhd
