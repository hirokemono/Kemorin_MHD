!t_bc_data_MHD.f90
!     module t_bc_data_MHD
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_bc_id_data(dt, IO_bc, mesh, group, MHD_mesh,     &
!!     &          MHD_prop, MHD_BC, nodal_bc)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(IO_boundary),          intent(in) :: IO_bc
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(mesh_groups),         intent(in) :: group
!!        type(mesh_data_MHD),       intent(in) :: MHD_mesh
!!        type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
      module t_bc_data_MHD
!
      use m_precision
      use m_machine_parameter
      use t_bc_data_velo
      use t_bc_data_magne
      use t_bc_data_temp
!
      implicit  none
!
!
      type nodal_boundarty_conditions
        type(nodal_bcs_4_momentum_type) :: Vnod_bcs
!
        type(nodal_bcs_4_induction_type) :: Bnod_bcs
!
        type(nodal_bcs_4_scalar_type) :: Tnod_bcs
!
        type(nodal_bcs_4_scalar_type) :: Cnod_bcs
      end type nodal_boundarty_conditions
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_id_data(dt, IO_bc, mesh, group, MHD_mesh,       &
     &          MHD_prop, MHD_BC, nodal_bc)
!
      use m_boundary_condition_IDs
      use t_mesh_data
      use t_geometry_data_MHD
      use t_control_parameter
      use t_nodal_bc_data
      use t_boundary_field_IO
      use t_bc_data_list
!
!
      real(kind = kreal), intent(in) :: dt
!
      type(IO_boundary),    intent(in) :: IO_bc
      type(mesh_geometry),  intent(in) :: mesh
      type(mesh_groups),    intent(in) :: group
      type(mesh_data_MHD),  intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(nodal_boundarty_conditions), intent(inout) :: nodal_bc
!
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 v'
        call set_bc_velo_id(IO_bc, mesh%node, mesh%ele,                 &
     &      MHD_mesh%fluid, group%nod_grp, MHD_BC%velo_BC%nod_BC,       &
     &      nodal_bc%Vnod_bcs)
        if ( iflag_debug .eq.1) write(*,*)  'set boundary id 4 P'
        call set_bc_press_id(dt, IO_bc, mesh%node, mesh%ele,            &
     &      MHD_mesh%fluid, group%nod_grp, MHD_prop%fl_prop,            &
     &      MHD_BC%press_BC%nod_BC, nodal_bc%Vnod_bcs)
      end if
!
      if (MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call set_bc_temp_id(IO_bc, mesh%node, mesh%ele, MHD_mesh%fluid, &
     &      group%nod_grp, MHD_BC%temp_BC%nod_BC, nodal_bc%Tnod_bcs)
      end if
!
!
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call set_bc_temp_id(IO_bc, mesh%node, mesh%ele, MHD_mesh%fluid, &
     &      group%nod_grp, MHD_BC%light_BC%nod_BC, nodal_bc%Cnod_bcs)
      end if
!
!
      if    (MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution    &
     &  .or. MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)   &
     & then
        if (iflag_debug.eq.1) write(*,*)  'set boundary ID 4 magne'
        call set_bc_magne_id(IO_bc, mesh%node, mesh%ele,                &
     &     group%nod_grp, MHD_BC%magne_BC%nod_BC, nodal_bc%Bnod_bcs)
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 magne_p'
        call set_bc_current_id(IO_bc, mesh%node, mesh%ele,              &
     &      group%nod_grp, MHD_BC%current_BC%nod_BC,                    &
     &     nodal_bc%Bnod_bcs)
        if (iflag_debug.eq.1)  write(*,*) 'set boundary ID 4 current'
        call set_bc_m_potential_id(IO_bc, mesh%node, mesh%ele,          &
     &      MHD_mesh%conduct, MHD_mesh%insulate,                        &
     &      group%nod_grp, MHD_BC%e_potential_BC%nod_BC,                &
     &      nodal_bc%Bnod_bcs)
      end if
!
      if (MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if (iflag_debug .eq.1) write(*,*) 'set boundary ID 4 vect_p'
        call set_bc_vect_p_id(IO_bc, mesh%node, mesh%ele,               &
     &      group%nod_grp, MHD_BC%a_potential_BC%nod_BC,                &
     &      nodal_bc%Bnod_bcs)
      end if
!
      end subroutine set_bc_id_data
!
!-----------------------------------------------------------------------
!
      end module t_bc_data_MHD
