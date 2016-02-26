!reordering_by_layers.f90
!     module reordering_by_layers
!
!      Written by H.Matsui
!      Modified by H. Matsui on Sep., 2007
!      Modified by H. Matsui on Feb., 2008
!
!      subroutine reordering_by_layers_snap
!      subroutine reordering_by_layers_MHD
!
!.......................................................................
!
!     subroutine for element re-ordering
!      by insulator, conductor, and fluid
!
!.......................................................................
!
      module reordering_by_layers
!
      use m_precision
      use m_machine_parameter
!
      use m_work_4_MHD_layering
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
      subroutine reordering_by_layers_snap
!
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
!
!
      call allocate_lists_4_layer(ele1%numele)
      call s_reordering_by_layers(ele1, ele_grp1, sf_grp1,              &
     &                            fluid1, conduct1, insulate1)
      call deallocate_lists_4_layer
!
      end subroutine reordering_by_layers_snap
!
! -----------------------------------------------------------------------
!
      subroutine reordering_by_layers_MHD
!
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_iccg_parameter
      use m_work_4_MHD_layering
      use m_type_AMG_data
      use m_type_AMG_mesh
      use t_geometry_data
      use t_interpolate_table
!
      use reordering_MG_ele_by_layers
      use skip_comment_f
!
!
      call allocate_lists_4_layer(ele1%numele)
      call s_reordering_by_layers(ele1, ele_grp1, sf_grp1,              &
     &                            fluid1, conduct1, insulate1)
!
!   ordereing of element parameters for AMG (for first grid)
!
      if(cmp_no_case(method_4_solver, 'MGCG')) then
        call reordering_ele_interpolate_type(ele1%numele,               &
     &      old2newele_layer, MG_itp(1)%f2c%tbl_org )
        call s_reordering_MG_ele_by_layers
      end if
!
      call deallocate_lists_4_layer
!
      end subroutine reordering_by_layers_MHD
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers(ele, ele_grp, sf_grp,           &
     &          fluid, conduct, insulate)
!
      use calypso_mpi
      use m_control_parameter
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_group_data
!
      use const_layering_table
      use reordering_element_MHD
      use reordering_element_size
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: sf_grp
      type(field_geometry_data), intent(inout) :: fluid, conduct
      type(field_geometry_data), intent(inout) :: insulate
!
!
!      if (my_rank .eq. 0 ) then
!        write(*,*) 'num_fl_ele_grp         ', num_fl_ele_grp
!        if ( num_fl_ele_grp .gt.0 ) then
!          write(*,*) 'fl_ele_grp_name        ', fl_ele_grp_name
!        end if
!        write(*,*) 'num_cd_ele_grp         ', num_cd_ele_grp
!        if ( num_cd_ele_grp .gt.0 ) then
!          write(*,*) 'cd_ele_grp_name        ', cd_ele_grp_name
!        end if
!        write(*,*) 'num_ins_ele_grp         ', num_ins_ele_grp
!        if ( num_ins_ele_grp .gt.0 ) then
!          write(*,*) 'ins_ele_grp_name        ', ins_ele_grp_name
!        end if
!        write(*,*) 'num_in_core_ele_grp   ', num_in_core_ele_grp
!        if ( num_in_core_ele_grp .gt.0 ) then
!          write(*,*) 'in_core_ele_grp_name  ', in_core_ele_grp_name
!        end if
!      end if
!
!
      call marking_by_layers                                            &
     &   (ele%numele, ele_grp%num_grp, ele_grp%num_item,                &
     &    ele_grp%istack_grp, ele_grp%grp_name, ele_grp%item_grp,       &
     &    mat_flag_mhd(1) )
!
!  set list vector for ordering
!
      call const_table_by_layers(ele%numele, mat_flag_mhd(1),           &
     &    fluid%iele_start_fld,    conduct%iele_start_fld,              &
     &    insulate%iele_start_fld, fluid%iele_end_fld,                  &
     &    conduct%iele_end_fld,    insulate%iele_end_fld,               &
     &    new2oldele_layer(1), old2newele_layer(1) )
!
      if (iflag_debug .gt. iflag_minimum_msg) then
       write(*,*) 'iele_fl_start, iele_fl_end',                         &
     &           fluid%iele_start_fld, fluid%iele_end_fld
       write(*,*) 'iele_cd_start, iele_cd_end',                         &
     &           conduct%iele_start_fld, conduct%iele_end_fld
       write(*,*) 'iele_ins_start, iele_ins_end',                       &
     &           insulate%iele_start_fld, insulate%iele_end_fld
      end if
!
!   ordereing of connectivity, element group, and surface group
!
      call reordering_element_info(ele, ele_grp, sf_grp)
!
!   ordereing of element parameters for SGS model
!
      if ( iflag_SGS_model.eq.id_SGS_NL_grad) then
        call reordering_ele_size(ele%numele)
      end if
!
      end subroutine s_reordering_by_layers
!
! -----------------------------------------------------------------------
!
      end module reordering_by_layers
