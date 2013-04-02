!reordering_MG_ele_by_layers.f90
!     module reordering_MG_ele_by_layers
!
!      Written by H. Matsui on Dec., 2008
!
!      subroutine s_reordering_MG_ele_by_layers
!      subroutine reordering_element_first_MG
!
      module reordering_MG_ele_by_layers
!
      use m_precision
!
      implicit none
!
      private :: reordering_ele_types_by_layer
      private :: reordering_ele_interpolate_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
     subroutine s_reordering_MG_ele_by_layers
!
      use m_type_AMG_mesh
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      integer(kind = kint) :: i_level, i_level_1, iflag_last_level
!
!
      iflag_last_level = 0
      do i_level = 1, num_MG_level-1
        if ( MG_mesh(i_level)%mesh%ele%numele .gt. 0) then
          i_level_1 = i_level + 1
!
          call reordering_ele_types_by_layer(iflag_last_level,          &
     &        MG_mesh(i_level), MG_MHD_mesh(i_level),                   &
     &        MG_itp(i_level_1)%f2c, MG_itp(i_level)%c2f )
!
        end if
      end do
!
      if ( MG_mesh(num_MG_level)%mesh%ele%numele .gt. 0) then
        i_level = num_MG_level
        iflag_last_level = num_MG_level
        call reordering_ele_types_by_layer(iflag_last_level,            &
     &      MG_mesh(i_level), MG_MHD_mesh(i_level),                     &
     &      MG_itp(i_level)%f2c, MG_itp(i_level)%c2f )
      end if
!
      end subroutine s_reordering_MG_ele_by_layers
!
! -----------------------------------------------------------------------
!
      subroutine reordering_element_first_MG
!
      use m_geometry_parameter
      use m_work_4_MHD_layering
      use m_type_AMG_data
      use m_type_AMG_mesh
      use t_interpolate_table
!
!
      call reordering_ele_interpolate_type(numele, old2newele_layer,    &
     &    MG_itp(1)%f2c%tbl_org )
!
      end subroutine reordering_element_first_MG
!
! -----------------------------------------------------------------------
!
      subroutine reordering_ele_types_by_layer(iflag_last_level,        &
     &          MG_mesh, MG_MHD_mesh, f2c_table, c2f_table)
!
      use m_parallel_var_dof
      use t_mesh_data
      use t_geometry_data_MHD
      use t_interpolate_table
      use m_work_4_MHD_layering
!
      use const_layering_table
      use reordering_element_MHD
!
      integer(kind = kint), intent(in) :: iflag_last_level
!
      type(mesh_data), intent(inout) ::         MG_mesh
      type(mesh_data_MHD), intent(inout) ::    MG_MHD_mesh
      type(interpolate_table), intent(inout) :: f2c_table
      type(interpolate_table), intent(inout) :: c2f_table
!
!
      call alloc_ele_connect_org_type(MG_mesh%mesh%ele, MG_MHD_mesh)
!
!
      call marking_by_layers(MG_mesh%mesh%ele%numele,                   &
     &    MG_mesh%group%ele_grp%num_grp,                                &
     &    MG_mesh%group%ele_grp%num_item,                               &
     &    MG_mesh%group%ele_grp%istack_grp,                             &
     &    MG_mesh%group%ele_grp%grp_name,                               &
     &    MG_mesh%group%ele_grp%item_grp, mat_flag_mhd )
!
!  set list vector for ordering
!
      call const_table_by_layers(MG_mesh%mesh%ele%numele, mat_flag_mhd, &
     &    MG_MHD_mesh%fluid%iele_start_fld,                             &
     &    MG_MHD_mesh%conduct%iele_start_fld,                           &
     &    MG_MHD_mesh%insulate%iele_start_fld,                          &
     &    MG_MHD_mesh%fluid%iele_end_fld,                               &
     &    MG_MHD_mesh%conduct%iele_end_fld,                             &
     &    MG_MHD_mesh%insulate%iele_end_fld,                            &
     &    new2oldele_layer, old2newele_layer )
!
      if (my_rank .eq. 0 ) then
       write(*,*) 'iele_fl_start, iele_fl_end',                         &
     &           MG_MHD_mesh%fluid%iele_start_fld,                      &
     &           MG_MHD_mesh%fluid%iele_end_fld
       write(*,*) 'iele_cd_start, iele_cd_end',                         &
     &           MG_MHD_mesh%conduct%iele_start_fld,                    &
     &           MG_MHD_mesh%conduct%iele_end_fld
       write(*,*) 'iele_ins_start, iele_ins_end',                       &
     &           MG_MHD_mesh%insulate%iele_start_fld,                   &
     &           MG_MHD_mesh%insulate%iele_end_fld
      end if
!
!   ordereing of connectivity, element group, and surface group
!
      call reordering_element_connect(MG_mesh%mesh%ele%numele,          &
     &    MG_mesh%mesh%ele%nnod_4_ele, new2oldele_layer,                &
     &    MG_MHD_mesh%iele_global_org, MG_mesh%mesh%ele%iele_global,    &
     &    MG_MHD_mesh%ie_org, MG_mesh%mesh%ele%ie )
!
      call reordering_element_group(MG_mesh%mesh%ele%numele,            &
     &    old2newele_layer, MG_mesh%group%ele_grp%num_item,             &
     &    MG_mesh%group%ele_grp%item_grp )
!
      call reordering_surface_group(MG_mesh%mesh%ele%numele,            &
     &    old2newele_layer,  MG_mesh%group%surf_grp%num_item,           &
     &    MG_mesh%group%surf_grp%item_sf_grp )
!
!    ordering interpolate tables
!
      if (iflag_last_level .eq. 0) then
        call reordering_ele_interpolate_type(MG_mesh%mesh%ele%numele,   &
     &      old2newele_layer, f2c_table%tbl_org)
      end if
!
      call reordering_ele_interpolate_type(MG_mesh%mesh%ele%numele,     &
     &    old2newele_layer, c2f_table%tbl_org)
!
      end subroutine reordering_ele_types_by_layer
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine reordering_ele_interpolate_type(numele,                &
     &          old2newele_layer, tbl_org)
!
      use t_interpolate_table_orgin
      use reordering_element_MHD
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: old2newele_layer(numele)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
!
      call reordering_element_group(numele, old2newele_layer,           &
     &    tbl_org%ntot_table_org, tbl_org%iele_org_4_org)
!
      end subroutine reordering_ele_interpolate_type
!
! -----------------------------------------------------------------------
!
      end module reordering_MG_ele_by_layers
