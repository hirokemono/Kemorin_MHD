!set_nodal_bc_id_data.f90
!     module set_nodal_bc_id_data
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on Aug., 2007
!
!!      subroutine set_bc_fields                                        &
!!     &         (evo_T, evo_C, mesh, fl_prop, cd_prop,                 &
!!     &          iphys, nod_fld, nod_bcs)
!!      subroutine set_boundary_velo(node, i_velo, nod_fld)
!!      subroutine set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!!      subroutine delete_field_by_fixed_v_bc(Vnod_bcs, i_field, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(mesh_groups), intent(in) ::   group
!!        type(node_data), intent(in) :: node
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(nodal_boundarty_conditions), intent(inout) :: nod_bcs
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module set_nodal_bc_id_data
!
      use m_precision
      use m_constants
!
      use t_time_stepping_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_bc_data_MHD
      use t_boundary_field_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_fields                                          &
     &         (evo_T, evo_C, mesh, fl_prop, cd_prop,                   &
     &          iphys, nod_fld, nod_bcs)
!
      use m_machine_parameter
      use m_physical_property
!
      use m_boundary_condition_IDs
      use m_bc_data_list
!
      use count_num_nod_bc_MHD
      use set_nodal_boundary
      use set_boundary_scalars
!
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(mesh_geometry), intent(in) :: mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
!
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
      type(nodal_boundarty_conditions), intent(inout) :: nod_bcs
!
!
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        if ( iflag_debug .eq.1) write(*,*)  'set boundary values 4 v'
        call set_boundary_velo                                          &
     &     (mesh%node, nod_bcs%Vnod_bcs, iphys%i_velo, nod_fld)
      end if
!
      if (evo_T%iflag_scheme .gt. id_no_evolution) then
        if (ref_param_T1%iflag_reference .ne. id_no_ref_temp) then
          call set_fixed_bc_per_scalar                                  &
     &       (mesh%node%numnod, nod_fld%ntot_phys,                      &
     &        iphys%i_ref_t, nod_fld%d_fld, nod_bcs%Tnod_bcs%nod_bc_s)
        end if
!
        call set_boundary_scalar                                        &
     &     (nod_bcs%Tnod_bcs%nod_bc_s, iphys%i_temp, nod_fld)
      end if
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        if (ref_param_C1%iflag_reference .ne. id_no_ref_temp) then
          call set_fixed_bc_per_scalar                                  &
     &       (mesh%node%numnod, nod_fld%ntot_phys,                      &
     &        iphys%i_ref_c, nod_fld%d_fld, nod_bcs%Cnod_bcs%nod_bc_s)
        end if
!
        call set_boundary_scalar                                        &
     &     (nod_bcs%Cnod_bcs%nod_bc_s, iphys%i_light, nod_fld)
      end if
!
      if    (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution             &
     &  .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (iflag_debug.eq.1)  write(*,*) 'set_boundary_vect magne'
        call set_boundary_vect                                          &
     &     (nod_bcs%Bnod_bcs%nod_bc_b, iphys%i_magne, nod_fld)
        if (iflag_debug.eq.1) write(*,*) 'set boundary value 4 magne'
        call set_boundary_scalar                                        &
     &     (nod_bcs%Bnod_bcs%nod_bc_f, iphys%i_m_phi, nod_fld)
        if (iflag_debug.eq.1) write(*,*) 'set_boundary_vect current'
        call set_boundary_vect                                          &
     &     (nod_bcs%Bnod_bcs%nod_bc_j, iphys%i_current, nod_fld)
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (iflag_debug .eq.1) write(*,*) 'set_boundary_vect vect_p'
        call set_boundary_vect                                          &
     &     (nod_bcs%Bnod_bcs%nod_bc_a, iphys%i_vecp, nod_fld)
      end if
!
      end subroutine set_bc_fields
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo(node, Vnod_bcs, i_velo, nod_fld)
!
      use t_bc_data_velo
!
      use set_nodal_bc_4_velo
      use set_boundary_scalars
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_velo
      type(node_data), intent(in) :: node
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_data), intent(inout) :: nod_fld
!
!
!     set fixed velocity
!
      call set_boundary_vect(Vnod_bcs%nod_bc_v, i_velo, nod_fld)
!
!   set rotation boundary
      call set_boundary_rot_vect                                        &
     &   (node, Vnod_bcs%nod_bc_rot, i_velo, nod_fld)
!
!   boundary condition for special case
!     ( please write every time!!)
      call set_boundary_specific_vect                                   &
     &   (node, Vnod_bcs%nod_bc_vsp, i_velo, nod_fld)
!
!
      call delete_radial_vector_on_bc                                   &
     &   (node, Vnod_bcs%nod_bc_vr0, i_velo, nod_fld)
!
      end subroutine set_boundary_velo
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_velo_4_rhs(node, Vnod_bcs, f_l, f_nl)
!
      use t_finite_element_mat
      use t_bc_data_velo
!
      use set_boundary_scalars
      use set_fixed_boundaries
!
      type(node_data), intent(in) :: node
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      call delete_vector_ffs_on_bc(node, Vnod_bcs%nod_bc_v, f_l, f_nl)
      call delete_vector_ffs_rot_bc                                     &
     &   (node, Vnod_bcs%nod_bc_rot, f_l, f_nl)
      call set_vector_ffs_special_bc(node, Vnod_bcs%nod_bc_vsp, f_l)
!
      end subroutine set_boundary_velo_4_rhs
!
!  ---------------------------------------------------------------------
!
      subroutine delete_field_by_fixed_v_bc(Vnod_bcs, i_field, nod_fld)
!
      use t_bc_data_velo
      use set_boundary_scalars
      use set_fixed_boundaries
!
      integer(kind = kint), intent(in) :: i_field
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(phys_data), intent(inout) :: nod_fld
!
!
      call delete_vector_on_bc(Vnod_bcs%nod_bc_v, i_field, nod_fld)
      call delete_vector_by_rot_v_bc                                    &
     &   (Vnod_bcs%nod_bc_rot, i_field, nod_fld)
      call delete_vector_by_fixed_t_bc                                  &
     &   (Vnod_bcs%nod_bc_vsp, i_field, nod_fld)
!
      end subroutine delete_field_by_fixed_v_bc
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_bc_id_data
