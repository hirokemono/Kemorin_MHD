!
!     module allocate_array_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!!      subroutine allocate_array_FEM_MHD(SGS_par, mesh, MHD_prop,      &
!!     &          iphys, iphys_LES, nod_fld, iref_base, iref_grad,      &
!!     &          ref_fld, Csims_FEM_MHD, SGS_MHD_wk, fem_sq, label_sim)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(inout) :: iphys
!!        type(SGS_model_addresses), intent(inout) :: iphys_LES
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module allocate_array_MHD
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_control_parameter
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_SGS_model_addresses
      use t_phys_data
      use t_SGS_control_parameter
      use t_physical_property
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_work_FEM_SGS_MHD
!
      implicit none
!
      private :: count_int_vol_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_array_FEM_MHD(SGS_par, mesh, MHD_prop,        &
     &          iphys, iphys_LES, nod_fld, iref_base, iref_grad,        &
     &          ref_fld, Csims_FEM_MHD, SGS_MHD_wk, fem_sq, label_sim)
!
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_mesh_data
      use t_work_FEM_integration
      use t_material_property
      use t_FEM_SGS_model_coefs
      use t_SGS_model_coefs
      use t_FEM_MHD_mean_square
!
      use set_control_field_data
      use count_sgs_components
      use dependency_FEM_SGS_MHD
      use set_mean_square_array
      use init_reference_field_data
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(inout) :: iphys
      type(SGS_model_addresses), intent(inout) :: iphys_LES
      type(phys_data), intent(inout) :: nod_fld
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(phys_data), intent(inout) :: ref_fld
!
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      character(len=kchara), intent(inout) :: label_sim
!
!
      label_sim = 'GeoFEM_MHD'
!
      if (iflag_debug.ge.1) write(*,*) 'alloc_finite_elem_mat'
      call alloc_finite_elem_mat(mesh, SGS_MHD_wk%rhs_mat)
      call alloc_fem_int_base_type(mesh, SGS_MHD_wk%fem_int)
      call alloc_mass_mat_fluid(mesh%node, SGS_MHD_wk%mk_MHD)
      call alloc_mass_mat_conduct(mesh%node, SGS_MHD_wk%mk_MHD)
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call alloc_int_vol_data(mesh%ele%numele, mesh%node%max_nod_smp,   &
     &   SGS_par%model_p, nod_fld, SGS_MHD_wk%mhd_fem_wk)
      call count_int_vol_data                                           &
         (SGS_par%model_p, MHD_prop%cd_prop, SGS_MHD_wk%mhd_fem_wk)
      call alloc_int_vol_dvx(mesh%ele%numele, SGS_MHD_wk%mhd_fem_wk)
      call set_SGS_ele_fld_addresses(MHD_prop%cd_prop, SGS_par%model_p, &
     &                               SGS_MHD_wk%mhd_fem_wk)
!
!  allocation for field values
      if (iflag_debug.ge.1)  write(*,*) 'set_FEM_SGS_MHD_field_data'
      call set_FEM_SGS_MHD_field_data                                   &
     &   (SGS_par%model_p, SGS_par%commute_p, mesh%node, mesh%ele,      &
     &    MHD_prop, nod_fld, iphys, iphys_LES, SGS_MHD_wk%ele_fld,      &
     &    SGS_MHD_wk%iphys_ele_base, SGS_MHD_wk%iphys_ele_fil)
      if (iflag_debug.ge.1)  write(*,*) 'initialize_ele_field_data'
!
      if ( iflag_debug.ge.1 ) write(*,*) 'init_FEM_MHD_mean_square'
      call init_FEM_MHD_mean_square(nod_fld, iphys, iphys_LES, fem_sq)
!
!
      call s_init_reference_field_data(mesh%node, iphys,                &
     &                                 iref_base, iref_grad, ref_fld)
!
      end subroutine allocate_array_FEM_MHD
!
! ----------------------------------------------------------------------
!
      subroutine count_int_vol_data(SGS_param, cd_prop, mhd_fem_wk)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in)  :: cd_prop
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      mhd_fem_wk%n_dvx = 0
      if ( SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none        &
     &   .or. SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none    &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none       &
     &   .or. SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 18
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 18
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 18
        end if
!
      else if(SGS_param%iflag_SGS .ne. id_SGS_none) then
        if (  SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none        &
     &   .or. SGS_param%SGS_momentum%iflag_SGS_flux .ne. id_SGS_none    &
     &   .or. SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none       &
     &   .or. SGS_param%iflag_SGS_uxb .ne.    id_SGS_none ) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        end if
!
        if ( SGS_param%iflag_SGS_lorentz .ne. id_SGS_none) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none               &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        end if
      end if
!
       end subroutine count_int_vol_data
!
! ----------------------------------------------------------------------
!
      end module allocate_array_MHD
