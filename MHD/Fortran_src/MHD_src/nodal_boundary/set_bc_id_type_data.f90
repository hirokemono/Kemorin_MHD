!
!      module set_bc_id_type_data
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine s_set_bc_id_type_data(mesh, group, MHD_mesh, nodal_bc)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(mesh_groups),         intent(in) :: group
!        type(mesh_data_MHD),       intent(in) :: MHD_mesh
!        type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      module set_bc_id_type_data
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_bc_id_type_data(mesh, group, MHD_mesh, nodal_bc)
!
      use m_control_parameter
      use m_machine_parameter
      use m_bc_data_list
      use m_boundary_condition_IDs
      use t_mesh_data
      use t_geometry_data_MHD
      use t_nodal_bc_data
      use count_num_nod_bc_MHD
      use set_bc_phys_id_type
!
!
      type(mesh_geometry),       intent(in) :: mesh
      type(mesh_groups),         intent(in) :: group
      type(mesh_data_MHD),       intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
!
!
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call count_num_bc_velo(group%nod_grp, nodal_bc%velocity,        &
     &      nodal_bc%sgs_velo, nodal_bc%rotation, nodal_bc%free_plane,  &
     &      nodal_bc%free_sphere, nodal_bc%no_radial_v)
        call count_num_bc_scl_w_SGS(group%nod_grp, press_nod,           &
     &      nodal_bc%press, nodal_bc%sgs_press)
!
!
        if ( iflag_debug .eq.1) write(*,*) 'allocate boundary 4 v'
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%velocity)
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%sgs_velo)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%no_radial_v)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%free_plane)
        call alloc_rotate_nod_bc_type(mesh%node%numnod, nodal_bc%rotation)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%free_sphere)
!
        if ( iflag_debug .eq.1) write(*,*) 'allocate boundary 4 P'
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%press)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%sgs_press)
!
        if (iflag_debug .eq. 1)  write(*,*)  'set boundary id 4 v'
        call set_bc_velo_id_type(mesh, MHD_mesh, group%nod_grp,         &
     &      nodal_bc)
        if (iflag_debug .eq. 1)  write(*,*)  'set boundary id 4 P'
        call set_bc_press_id_type(mesh, MHD_mesh, group%nod_grp,        &
     &      nodal_bc)
      end if
!
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
        call count_num_bc_scl_w_SGS(group%nod_grp, temp_nod,            &
     &      nodal_bc%temp, nodal_bc%sgs_temp)
!
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%temp)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%sgs_temp)
!
        call set_bc_temp_id_type(mesh, MHD_mesh, group%nod_grp,         &
     &      nodal_bc)
      end if
!
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call count_num_bc_scl_w_SGS(group%nod_grp, light_nod,           &
     &    nodal_bc%composition, nodal_bc%sgs_comp)
!
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%composition)
!
        call set_bc_d_scalar_id_type(mesh, MHD_mesh, group%nod_grp,     &
     &      nodal_bc)
      end if
!
!
      if (iflag_t_evo_4_magne .gt. id_no_evolution                      &
     &  .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call count_num_bc_magne(iflag_bc_fixed, group%nod_grp,          &
     &      magne_nod, nodal_bc%magne)
        call count_num_bc_vect(iflag_bc_sgs, group%nod_grp,             &
     &      magne_nod, nodal_bc%sgs_magne)
        call count_num_bc_vect(iflag_bc_fixed, group%nod_grp,           &
     &      current_nod, nodal_bc%current)
        call count_num_bc_magp(group%nod_grp, nodal_bc%magne_p,         &
     &    nodal_bc%sgs_mag_p)
!
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%magne)
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%sgs_magne)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%magne_p)
        call alloc_scalar_nod_bc_type(mesh%node%numnod, nodal_bc%sgs_mag_p)
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%current)
!
        if (iflag_debug .eq. 1)  write(*,*)  'set boundary ID 4 magne'
        call set_bc_magne_id_type(mesh, group%nod_grp, nodal_bc)
        if (iflag_debug .eq. 1) write(*,*)  'set boundary ID 4 magne_p'
        call set_bc_m_potential_id_type(mesh, MHD_mesh, group%nod_grp,  &
     &      nodal_bc)
        if (iflag_debug .eq. 1) write(*,*)  'set boundary ID 4 current'
        call set_bc_current_id_type(group%nod_grp, nodal_bc)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call count_num_bc_vect(iflag_bc_fixed, group%nod_grp,           &
     &      a_potential_nod, nodal_bc%vector_p)
        call count_num_bc_vect(iflag_bc_sgs, group%nod_grp,             &
     &      a_potential_nod, nodal_bc%sgs_vect_p)
!
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%vector_p)
        call alloc_vector_nod_bc_type(mesh%node%numnod, nodal_bc%sgs_vect_p)
!
        if (iflag_debug .eq. 1) write(*,*)  'set boundary ID 4 vect_p'
        call set_bc_vect_p_id_type(mesh, group%nod_grp, nodal_bc)
      end if
!
      end subroutine s_set_bc_id_type_data
!
!-----------------------------------------------------------------------
!
      end module set_bc_id_type_data
