!
!     module allocate_array_MHD
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on July, 2006
!        Modified by H. Matsui on May, 2007
!
!!      subroutine allocate_array(node, ele, iphys, nod_fld,            &
!!     &          iphys_elediff, m_lump, mhd_fem_wk, fem_wk,            &
!!     &          f_l, f_nl, label_sim)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_terms_address), intent(inout) :: iphys_elediff
!!        type(lumped_mass_matrices), intent(inout) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      module allocate_array_MHD
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_phys_address
      use t_phys_data
      use t_SGS_control_parameter
      use t_time_stepping_parameter
      use t_MHD_finite_element_mat
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
      subroutine allocate_array(node, ele, iphys, nod_fld,              &
     &          iphys_elediff, m_lump, mhd_fem_wk, fem_wk,              &
     &          f_l, f_nl, label_sim)
!
      use m_control_parameter
      use m_element_phys_data
      use m_phys_constants
      use m_mean_square_values
!
      use t_geometry_data
      use t_finite_element_mat
      use t_FEM_phys_data
      use t_material_property
      use t_SGS_model_coefs
!
      use count_sgs_components
      use node_monitor_IO
      use check_dependency_for_MHD
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_terms_address), intent(inout) :: iphys_elediff
      type(lumped_mass_matrices), intent(inout) :: m_lump
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!
      character(len=kchara), intent(inout) :: label_sim
!
!
      label_sim = 'GeoFEM_MHD'
!
      if (iflag_debug.ge.1) write(*,*) 'alloc_finite_elem_mat'
      call alloc_finite_elem_mat                                        &
     &   (node, ele, m_lump, fem_wk, f_l, f_nl)
      call alloc_mass_mat_fluid(node%numnod, mhd_fem_wk)
      call alloc_mass_mat_conduct(node%numnod, mhd_fem_wk)
!
      if (iflag_debug.ge.1) write(*,*) 'allocate_int_vol_data'
      call alloc_int_vol_data                                           &
     &  (ele%numele, node%max_nod_smp, SGS_param1, nod_fld, mhd_fem_wk)
      call count_int_vol_data(SGS_param1, evo_magne, mhd_fem_wk)
      call alloc_int_vol_dvx(ele%numele, mhd_fem_wk)
      call set_SGS_ele_fld_addresses(SGS_param1, iphys_elediff)
!
!  allocation for field values
      if (iflag_debug.ge.1)  write(*,*) 'set_FEM_MHD_field_data'
      call set_FEM_MHD_field_data(SGS_param1, node, iphys, nod_fld)
      if (iflag_debug.ge.1)  write(*,*) 'initialize_ele_field_data'
      call initialize_ele_field_data(ele%numele)
!
      if ( iflag_debug.ge.1 ) write(*,*) 'set_mean_square_values'
      call count_mean_square_values(nod_fld)
      call set_mean_square_values(nod_fld)
!
      end subroutine allocate_array
!
! ----------------------------------------------------------------------
!
      subroutine count_int_vol_data(SGS_param, evo_magne, mhd_fem_wk)
!
      use m_phys_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(time_evolution_params), intent(in) :: evo_magne
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
     &     .and. evo_magne%iflag_scheme .gt. id_no_evolution) then
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
     &     .and. evo_magne%iflag_scheme .gt. id_no_evolution) then
         mhd_fem_wk%n_dvx = mhd_fem_wk%n_dvx + 9
        end if
      end if
!
       end subroutine count_int_vol_data
!
! ----------------------------------------------------------------------
!
      end module allocate_array_MHD
