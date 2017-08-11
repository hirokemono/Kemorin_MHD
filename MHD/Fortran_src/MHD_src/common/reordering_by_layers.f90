!
!      module reordering_by_layers
!
!      Written by H. Matsui on Feb., 2008
!
!!      subroutine reordering_by_layers_snap                            &
!!     &         (FEM_prm, SGS_par, ele, group, MHD_mesh, FEM_elens)
!!      subroutine reordering_by_layers_MHD                             &
!!     &         (SGS_par, MGCG_WK, MGCG_FEM, MGCG_MHD_FEM, FEM_prm,    &
!!     &          ele, group, MHD_mesh, MG_interpolate, FEM_elens)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MGCG_data), intent(in) :: MGCG_WK
!!        type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!!        z!!        type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
!!        type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
!!        type(element_data), intent(inout) :: ele
!!        type(mesh_groups), intent(inout) ::   group
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!!        type(MG_itp_table), intent(inout)                             &
!!       &     :: MG_interpolate(MGCG_WK%num_MG_level)
!!        type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      module reordering_by_layers
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_work_4_MHD_layering
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
      use t_filter_elength
!
      implicit  none
!
!
      type(work_4_make_layering), save, private :: WK_layer_e
!
      private :: s_reordering_by_layers
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine reordering_by_layers_snap                              &
     &         (FEM_prm, SGS_par, ele, group, MHD_mesh, FEM_elens)
!
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(element_data), intent(inout) :: ele
      type(mesh_groups), intent(inout) ::   group
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
!
      call alloc_lists_4_layer(ele%numele, WK_layer_e)
      call s_reordering_by_layers                                       &
     &   (FEM_prm, SGS_par, ele, group%ele_grp, group%surf_grp,         &
     &    MHD_mesh, FEM_elens, WK_layer_e)
      call dealloc_lists_4_layer(WK_layer_e)
!
      end subroutine reordering_by_layers_snap
!
! -----------------------------------------------------------------------
!
      subroutine reordering_by_layers_MHD                               &
     &         (SGS_par, MGCG_WK, MGCG_FEM, MGCG_MHD_FEM, FEM_prm,      &
     &          ele, group, MHD_mesh, MG_interpolate, FEM_elens)
!
      use t_interpolate_table
      use t_MGCG_data
      use t_MGCG_data_4_MHD
!
      use skip_comment_f
      use reordering_MG_ele_by_layers
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(MGCG_data), intent(in) :: MGCG_WK
!
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
      type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
      type(FEM_MHD_paremeters), intent(inout) :: FEM_prm
      type(element_data), intent(inout) :: ele
      type(mesh_groups), intent(inout) ::   group
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
      type(MG_itp_table), intent(inout)                                 &
     &     :: MG_interpolate(MGCG_WK%num_MG_level)
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      call alloc_lists_4_layer(ele%numele, WK_layer_e)
      call s_reordering_by_layers                                       &
     &   (FEM_prm, SGS_par, ele, group%ele_grp, group%surf_grp,         &
     &    MHD_mesh, FEM_elens, WK_layer_e)
!
!   ordereing of element parameters for AMG (for first grid)
!
      if(cmp_no_case(FEM_PRM%CG11_param%METHOD, 'MGCG')) then
        call reordering_ele_interpolate_type(ele%numele,                &
     &     WK_layer_e%old2newele_layer, MG_interpolate(1)%f2c%tbl_org)
        call s_reordering_MG_ele_by_layers                              &
     &     (MGCG_WK, MGCG_FEM, MGCG_MHD_FEM, FEM_prm,                   &
     &      MG_interpolate, WK_layer_e)
      end if
!
      call dealloc_lists_4_layer(WK_layer_e)
!
      end subroutine reordering_by_layers_MHD
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers                                 &
     &         (FEM_prm, SGS_par, ele, ele_grp, sf_grp, MHD_mesh,       &
     &          FEM_elens, WK_layer)
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
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(work_4_make_layering), intent(inout) :: WK_layer
!
!
      call marking_by_layers                                            &
     &   (ele%numele, ele_grp%num_grp, ele_grp%num_item,                &
     &    ele_grp%istack_grp, ele_grp%grp_name, ele_grp%item_grp,       &
     &    WK_layer%mat_flag_mhd(1), FEM_prm)
!
!  set list vector for ordering
!
      call const_table_by_layers(ele%numele, WK_layer%mat_flag_mhd(1),  &
     &  MHD_mesh%fluid%iele_start_fld, MHD_mesh%conduct%iele_start_fld, &
     &  MHD_mesh%insulate%iele_start_fld, MHD_mesh%fluid%iele_end_fld,  &
     &  MHD_mesh%conduct%iele_end_fld, MHD_mesh%insulate%iele_end_fld,  &
     &  WK_layer%new2oldele_layer(1), WK_layer%old2newele_layer(1) )
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
      call reordering_element_info                                      &
     &   (WK_layer, ele, ele_grp, sf_grp, MHD_mesh)
!
!   ordereing of element parameters for SGS model
!
      call reordering_ele_size(SGS_par%model_p, ele%numele,             &
     &    WK_layer%old2newele_layer, FEM_elens)
!
      end subroutine s_reordering_by_layers
!
! -----------------------------------------------------------------------
!
      end module reordering_by_layers
