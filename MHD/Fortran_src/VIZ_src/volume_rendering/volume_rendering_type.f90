!volume_rendering_type.f90
!      module volume_rendering_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_pvr_type(ierr, fem,                    &
!     &          sf_mesh_psf, fld_nod)
!      subroutine visualize_pvr_type(istep_pvr,                         &
!     &          fem, sf_mesh_psf, jac_3d, fld_nod)
!        integer(kind = kint), intent(in) :: istep_pvr
!        integer(kind = kint), intent(inout) :: ierror
!        type(mesh_data), intent(in) :: fem
!        type(surface_geometry), intent(in) :: sf_mesh_psf
!        type(phys_data), intent(in) :: fld_nod
!        type(jacobians_3d), intent(in) :: jac_3d
!
!      subroutine pvr_init_type(ierr, fem, surf, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_data), intent(in) :: surf
!        type(edge_data), intent(in) :: edge
!        type(phys_data), intent(in) :: fld_nod
!
!      subroutine pvr_main_type(istep_pvr, fem, surf, jac_3d, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(surface_data), intent(in) :: surf
!        type(phys_data), intent(in) :: fld_nod
!        type(jacobians_3d), intent(in) :: jac_3d
!        integer(kind = kint), intent(in) :: istep_pvr
!
      module volume_rendering_type
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
      use calypso_mpi
!
      use t_mesh_data
      use t_surface_data
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_pvr_type(ierr, fem,                     &
     &          sf_mesh_psf, fld_nod)
!
      use m_control_data_pvrs
      use m_control_params_4_pvr
!
      integer(kind = kint), intent(inout) :: ierr
!
      type(mesh_data), intent(in) :: fem
      type(surface_geometry), intent(in) :: sf_mesh_psf
      type(phys_data), intent(in) :: fld_nod
!
!
      num_pvr = num_pvr_ctl
      if (num_pvr .gt. 0) then
        call pvr_init_type(ierr, fem, sf_mesh_psf%surf, fld_nod)
      end if
      call calypso_MPI_barrier
!
      end subroutine init_visualize_pvr_type
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_pvr_type(istep_pvr,                          &
     &          fem, sf_mesh_psf, jac_3d, fld_nod)
!
      use t_jacobian_3d
      use m_control_params_4_pvr
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(mesh_data), intent(in) :: fem
      type(surface_geometry), intent(in) :: sf_mesh_psf
      type(phys_data), intent(in) :: fld_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      if (num_pvr.gt.0 .and. istep_pvr.gt.0) then
        call pvr_main_type(istep_pvr, fem, sf_mesh_psf%surf,            &
     &     jac_3d, fld_nod)
      end if
!
      end subroutine visualize_pvr_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine pvr_init_type(ierr, fem, surf, fld_nod)
!
      use volume_rendering
!
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: fld_nod
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      call pvr_init(fem%mesh%node%numnod, fem%mesh%ele%numele,          &
     &    surf%numsurf, surf%nnod_4_surf, fem%mesh%node%istack_nod_smp, &
     &    fem%mesh%node%xx, fem%mesh%ele%e_multi, surf%ie_surf,         &
     &    surf%isf_4_ele, surf%iele_4_surf, fem%group%ele_grp%num_grp,  &
     &    fem%group%ele_grp%num_item, fem%group%ele_grp%grp_name,       &
     &    fem%group%ele_grp%istack_grp, fem%group%ele_grp%item_grp,     &
     &    fld_nod%num_phys, fld_nod%phys_name, ierr)
!
      end subroutine pvr_init_type
!
!  ---------------------------------------------------------------------
!
      subroutine pvr_main_type(istep_pvr, fem, surf, jac_3d, fld_nod)
!
      use t_jacobian_3d
!
      use volume_rendering
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(phys_data), intent(in) :: fld_nod
      type(jacobians_3d), intent(in) :: jac_3d
!
!
      call pvr_main                                                     &
     &   (istep_pvr, fem%mesh%node%numnod, fem%mesh%ele%numele,         &
     &    surf%numsurf, fem%mesh%ele%nnod_4_ele, surf%nnod_4_surf,      &
     &    fem%mesh%node%istack_nod_smp, fem%mesh%ele%istack_ele_smp,    &
     &    fem%mesh%node%xx, fem%mesh%node%rr, fem%mesh%node%a_r,        &
     &    fem%mesh%node%ss, fem%mesh%node%a_s, fem%mesh%ele%ie,         &
     &     fem%mesh%ele%a_vol_ele, fem%mesh%ele%e_multi,                &
     &    surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,               &
     &    jac_3d%ntot_int, jac_3d%dnx, jac_3d%xjac,                     &
     &    fld_nod%num_phys, fld_nod%ntot_phys,                          &
     &    fld_nod%istack_component, fld_nod%d_fld)
!
      end subroutine pvr_main_type
!
!  ---------------------------------------------------------------------
!
      end module volume_rendering_type
