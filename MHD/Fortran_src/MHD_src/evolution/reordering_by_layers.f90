!reordering_by_layers.f90
!     module reordering_by_layers
!
!      Written by H.Matsui
!      Modified by H. Matsui on Sep., 2007
!      Modified by H. Matsui on Feb., 2008
!
!!      subroutine reordering_by_layers_snap                            &
!!     &         (FEM_prm, SGS_par, ele, group, MHD_mesh)
!!      subroutine reordering_by_layers_MHD                             &
!!     &         (FEM_prm, SGS_par, ele, group, MHD_mesh, MG_interpolate)
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(element_data), intent(inout) :: ele
!!        type(mesh_groups), intent(inout) ::   group
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(MG_itp_table), intent(inout)                             &
!!       &               :: MG_interpolate(num_MG_level)
!!
!!.......................................................................
!!
!!     subroutine for element re-ordering
!!      by insulator, conductor, and fluid
!!
!!.......................................................................
!
      module reordering_by_layers
!
      use m_precision
      use m_machine_parameter
!
      use m_work_4_MHD_layering
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
!
      implicit none
!
      private :: s_reordering_by_layers
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine reordering_by_layers_snap                              &
     &         (FEM_prm, SGS_par, ele, group, MHD_mesh)
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(element_data), intent(inout) :: ele
      type(mesh_groups), intent(inout) ::   group
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      call allocate_lists_4_layer(ele%numele)
      call s_reordering_by_layers(FEM_prm, SGS_par,                     &
     &    ele, group%ele_grp, group%surf_grp, MHD_mesh)
      call deallocate_lists_4_layer
!
      end subroutine reordering_by_layers_snap
!
! -----------------------------------------------------------------------
!
      subroutine reordering_by_layers_MHD                               &
     &         (FEM_prm, SGS_par, ele, group, MHD_mesh, MG_interpolate)
!
      use m_iccg_parameter
      use m_work_4_MHD_layering
      use m_type_AMG_data
      use m_type_AMG_mesh
      use t_interpolate_table
!
      use reordering_MG_ele_by_layers
      use skip_comment_f
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(element_data), intent(inout) :: ele
      type(mesh_groups), intent(inout) ::   group
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(MG_itp_table), intent(inout) :: MG_interpolate(num_MG_level)
!
!
      call allocate_lists_4_layer(ele%numele)
      call s_reordering_by_layers(FEM_prm, SGS_par,                     &
     &    ele, group%ele_grp, group%surf_grp, MHD_mesh)
!
!   ordereing of element parameters for AMG (for first grid)
!
      if(cmp_no_case(method_4_solver, 'MGCG')) then
        call reordering_ele_interpolate_type(ele%numele,                &
     &     old2newele_layer, MG_interpolate(1)%f2c%tbl_org)
        call s_reordering_MG_ele_by_layers(FEM_prm, MG_interpolate)
      end if
!
      call deallocate_lists_4_layer
!
      end subroutine reordering_by_layers_MHD
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers                                 &
     &         (FEM_prm, SGS_par, ele, ele_grp, sf_grp, MHD_mesh)
!
      use calypso_mpi
!
      use const_layering_table
      use reordering_element_MHD
      use reordering_element_size
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(element_data), intent(inout) :: ele
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: sf_grp
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      call marking_by_layers                                            &
     &   (ele%numele, ele_grp%num_grp, ele_grp%num_item,                &
     &    ele_grp%istack_grp, ele_grp%grp_name, ele_grp%item_grp,       &
     &    mat_flag_mhd(1), FEM_prm)
!
!  set list vector for ordering
!
      call const_table_by_layers(ele%numele, mat_flag_mhd(1),           &
     &  MHD_mesh%fluid%iele_start_fld, MHD_mesh%conduct%iele_start_fld, &
     &  MHD_mesh%insulate%iele_start_fld, MHD_mesh%fluid%iele_end_fld,  &
     &  MHD_mesh%conduct%iele_end_fld, MHD_mesh%insulate%iele_end_fld,  &
     &  new2oldele_layer(1), old2newele_layer(1) )
!
      if (iflag_debug .gt. iflag_minimum_msg) then
       write(*,*) 'iele_fl_start, iele_fl_end',                         &
     &           MHD_mesh%fluid%iele_start_fld,                         &
     &           MHD_mesh%fluid%iele_end_fld
       write(*,*) 'iele_cd_start, iele_cd_end',                         &
     &           MHD_mesh%conduct%iele_start_fld,                       &
     &           MHD_mesh%conduct%iele_end_fld
       write(*,*) 'iele_ins_start, iele_ins_end',                       &
     &           MHD_mesh%insulate%iele_start_fld,                      &
     &           MHD_mesh%insulate%iele_end_fld
      end if
!
!   ordereing of connectivity, element group, and surface group
!
      call reordering_element_info(ele, ele_grp, sf_grp, MHD_mesh)
!
!   ordereing of element parameters for SGS model
!
      call reordering_ele_size(SGS_par%model_p, ele%numele)
!
      end subroutine s_reordering_by_layers
!
! -----------------------------------------------------------------------
!
      end module reordering_by_layers
