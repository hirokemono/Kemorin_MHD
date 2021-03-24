!>@file   cal_volume_node_MHD.f90
!!@brief  module cal_volume_node_MHD
!!
!!@author H. Matsui and H. Okuda
!!@date   programmed in July 2000 (ver 1.1)
!>        Modified in Aug., 2006 (ver 1.2)
!>        Modified in Aug., 2007 (ver 1.2)
!!
!>@brief  Initialize Jacobians and volumes for FEM MHD
!!
!!@verbatim
!!      subroutine const_MHD_jacobian_and_volumes                       &
!!     &         (SGS_param, ifld_msq, MHD_BC, mesh, group, layer_tbl,  &
!!     &          spfs, jacs, MHD_mesh, fem_msq)
!!      subroutine finalize_MHD_jac_and_volumes(mesh, spfs, jacs)
!!        type(mean_square_address), intent(in) :: ifld_msq
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(shape_finctions_at_points), intent(inout) :: spfs
!!        type(jacobians_type), intent(inout) :: jacs
!!        type(layering_tbl), intent(inout) :: layer_tbl
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(mean_square_values), intent(inout) :: fem_msq
!!@endverbatim
!
      module cal_volume_node_MHD
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_boundary
      use t_geometry_data_MHD
!
      implicit none
!
      real(kind=kreal) :: vol_fl_local
      private :: vol_fl_local
!
      private :: cal_volume_4_area
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_MHD_jacobian_and_volumes                         &
     &         (SGS_param, ifld_msq, MHD_BC, mesh, group, layer_tbl,    &
     &          spfs, jacs, MHD_mesh, fem_msq)
!
      use t_bc_data_list
      use t_mean_square_values
      use t_jacobians
      use t_layering_ele_list
      use t_shape_functions
!
      use const_jacobians_3d
      use int_volume_of_domain
      use cal_layered_volumes
      use sum_volume_of_domain
      use const_bc_infty_surf_type
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(mean_square_address), intent(in) :: ifld_msq
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
      type(layering_tbl), intent(inout) :: layer_tbl
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      call const_bc_infinity_surf_grp(MHD_BC%infty_BC,                  &
     &    group%surf_grp, group%infty_grp)
!
!    Construct Jacobians
!
      call sel_max_int_point_by_etype(mesh%ele%nnod_4_ele, jacs%g_FEM)
      call initialize_FEM_integration                                   &
     &   (jacs%g_FEM, spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
!
      call alloc_vol_shape_func                                         &
     &   (mesh%ele%nnod_4_ele, jacs%g_FEM, spfs%spf_3d)
      call const_jacobians_element(my_rank, nprocs,                     &
     &    mesh%node, mesh%ele, group%surf_grp, group%infty_grp,         &
     &    spfs%spf_3d, jacs)
      call dealloc_vol_shape_func(spfs%spf_3d)
!
!    Construct volumes
!
      call allocate_volume_4_smp
!
      if (iflag_debug.eq.1) write(*,*) 's_int_volume_of_domain'
      call s_int_volume_of_domain                                       &
     &   (mesh%ele, jacs%g_FEM, jacs%jac_3d)
!
!     ---  lead total volume of each area
!
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_fluid'
      call cal_volume_4_area(mesh%ele, MHD_mesh%fluid)
!
      if (MHD_mesh%fluid%istack_ele_fld_smp(np_smp)                     &
     &   .eq. MHD_mesh%fluid%istack_ele_fld_smp(0)) then
        fem_msq%rms_local(ifld_msq%ivol) = vol_local
      else
        fem_msq%rms_local(ifld_msq%ivol) = vol_fl_local
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_conduct'
      call cal_volume_4_area(mesh%ele, MHD_mesh%conduct)
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_insulate'
      call cal_volume_4_area(mesh%ele, MHD_mesh%insulate)
!
       if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
         if (iflag_debug.eq.1) write(*,*) 's_cal_layered_volumes'
         call s_cal_layered_volumes(mesh%ele, layer_tbl)
       end if
!
!       call s_int_volume_insulate_core(mesh%ele, inner_core)
!
      call deallocate_volume_4_smp
      call dealloc_dxi_dx_element(mesh%ele, jacs)
!
!
      if (iflag_debug.eq.1) then
        write(*,*) 'volume:       ', mesh%ele%volume
        write(*,*) 'vol_fluid:    ', MHD_mesh%fluid%volume
        write(*,*) 'vol_conduct:  ', MHD_mesh%conduct%volume
        write(*,*) 'vol_insulate: ', MHD_mesh%insulate%volume
      end if
!
      end subroutine const_MHD_jacobian_and_volumes
!
!  ---------------------------------------------------------------------
!
      subroutine finalize_MHD_jac_and_volumes(mesh, spfs, jacs)
!
      use t_jacobians
!
      use const_jacobians_3d
!
      type(mesh_geometry), intent(in) :: mesh
      type(shape_finctions_at_points), intent(inout) :: spfs
      type(jacobians_type), intent(inout) :: jacs
!
!
      call dealloc_jacobians_surface(mesh%surf, jacs)
      call dealloc_jacobians_element(mesh%ele, jacs)
      call finalize_FEM_integration                                     &
     &   (jacs%g_FEM, spfs%spf_3d, spfs%spf_2d, spfs%spf_1d)
!
      end subroutine finalize_MHD_jac_and_volumes
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_area(ele, area)
!
      use calypso_mpi_real
      use sum_volume_of_domain
!
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(inout) :: area
!
!
      call sum_4_volume(ele%numele, ele%interior_ele,                   &
     &    area%istack_ele_fld_smp, ele%volume_ele, vol_fl_local)
!
      call calypso_mpi_allreduce_one_real(vol_fl_local, area%volume,    &
     &                                    MPI_SUM)
!
      if (area%volume .eq. 0.0d0) then
        area%a_volume = 1.0d30
      else
        area%a_volume = 1.0d0 / area%volume
      end if
!
      end subroutine cal_volume_4_area
!
!  ---------------------------------------------------------------------
!
      end module cal_volume_node_MHD
