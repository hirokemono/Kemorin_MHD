!
!     module allocate_array_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!!      subroutine allocate_array(SGS_par, mesh, MHD_prop,              &
!!     &          iphys, nod_fld, iphys_ele, ele_fld, iphys_elediff,    &
!!     &          mk_MHD, mhd_fem_wk, rhs_mat, fem_int, fem_sq,         &
!!     &          label_sim)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(inout) :: iphys, iphys_ele
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!        type(SGS_terms_address), intent(inout) :: iphys_elediff
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_element_integration), intent(inout) :: fem_int
!!        type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
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
      use t_phys_data
      use t_SGS_control_parameter
      use t_physical_property
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
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
      subroutine allocate_array(SGS_par, mesh, MHD_prop,                &
     &          iphys, nod_fld, iphys_ele, ele_fld, iphys_elediff,      &
     &          mk_MHD, mhd_fem_wk, rhs_mat, fem_int, fem_sq,           &
     &          label_sim)
!
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_mesh_data
      use t_work_FEM_integration
      use t_material_property
      use t_SGS_model_coefs
      use t_FEM_MHD_mean_square
!
      use set_field_address
      use count_sgs_components
      use node_monitor_IO
      use dependency_FEM_SGS_MHD
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(inout) :: iphys, iphys_ele
      type(phys_data), intent(inout) :: nod_fld, ele_fld
      type(SGS_terms_address), intent(inout) :: iphys_elediff
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_element_integration), intent(inout) :: fem_int
      type(lumped_mass_mat_layerd), intent(inout) :: mk_MHD
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      character(len=kchara), intent(inout) :: label_sim
!
!
      label_sim = 'GeoFEM_MHD'
!
      if (iflag_debug.ge.1) write(*,*) 'alloc_finite_elem_mat'
      call alloc_finite_elem_mat(mesh, rhs_mat)
      call alloc_fem_int_base_type(mesh, fem_int)
      call alloc_mass_mat_fluid(mesh%node%numnod, mk_MHD)
      call alloc_mass_mat_conduct(mesh%node%numnod, mk_MHD)
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call alloc_int_vol_data(mesh%ele%numele, mesh%node%max_nod_smp,   &
     &   SGS_par%model_p, nod_fld, mhd_fem_wk)
      call count_int_vol_data                                           &
         (SGS_par%model_p, MHD_prop%cd_prop, mhd_fem_wk)
      call alloc_int_vol_dvx(mesh%ele%numele, mhd_fem_wk)
      call set_SGS_ele_fld_addresses                                    &
     &   (MHD_prop%cd_prop, SGS_par%model_p, iphys_elediff)
!
!  allocation for field values
      if (iflag_debug.ge.1)  write(*,*) 'set_FEM_SGS_MHD_field_data'
      call set_FEM_SGS_MHD_field_data                                   &
     &   (SGS_par%model_p, SGS_par%commute_p, mesh%node, mesh%ele,      &
     &    MHD_prop, iphys, nod_fld, iphys_ele, ele_fld)
      if (iflag_debug.ge.1)  write(*,*) 'initialize_ele_field_data'
!
      if ( iflag_debug.ge.1 ) write(*,*) 'set_mean_square_values'
      call init_FEM_MHD_mean_square(nod_fld, fem_sq)
!
      end subroutine allocate_array
!
! ----------------------------------------------------------------------
!
      subroutine count_int_vol_data(SGS_param, cd_prop, mhd_fem_wk)
!
      use m_phys_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(conductive_property), intent(in)  :: cd_prop
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      mhd_fem_wk%n_dvx = 0
      if ( SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        if (  SGS_param%iflag_SGS_h_flux .ne. id_SGS_none               &
     &   .or. SGS_param%iflag_SGS_m_flux .ne. id_SGS_none               &
     &   .or. SGS_param%iflag_SGS_c_flux .ne. id_SGS_none               &
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
        if (  SGS_param%iflag_SGS_h_flux .ne. id_SGS_none               &
     &   .or. SGS_param%iflag_SGS_m_flux .ne. id_SGS_none               &
     &   .or. SGS_param%iflag_SGS_c_flux .ne. id_SGS_none               &
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
