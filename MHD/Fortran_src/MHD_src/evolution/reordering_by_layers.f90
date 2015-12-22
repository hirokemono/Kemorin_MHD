!reordering_by_layers.f90
!     module reordering_by_layers
!
!      Written by H.Matsui
!      Modified by H. Matsui on Sep., 2007
!      Modified by H. Matsui on Feb., 2008
!
!      subroutine s_reordering_by_layers_snap
!      subroutine s_reordering_by_layers(ele_grp, sf_grp)
!
!.......................................................................
!
!     subroutine for re-orderinf of elements
!      by insulator, conductor, and fluid
!
!.......................................................................
!
      module reordering_by_layers
!
      use m_precision
!
      use m_geometry_data
      use m_work_4_MHD_layering
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers_snap
!
      use m_group_data
!
!
      call allocate_lists_4_layer(ele1%numele)
      call s_reordering_by_layers(ele_grp1, sf_grp1)
      call deallocate_lists_4_layer
!
      end subroutine s_reordering_by_layers_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers(ele_grp, sf_grp)
!
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
      use m_iccg_parameter
      use m_geometry_data
      use m_geometry_data_MHD
!
      use t_group_data
!
      use const_layering_table
      use reordering_element_MHD
      use reordering_element_size
!
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: sf_grp
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
     &   (ele1%numele, ele_grp%num_grp, ele_grp%num_item,               &
     &    ele_grp%istack_grp, ele_grp%grp_name, ele_grp%item_grp,       &
     &    mat_flag_mhd(1) )
!
!  set list vector for ordering
!
      call const_table_by_layers(ele1%numele, mat_flag_mhd(1),          &
     &    fluid1%iele_start_fld,    conduct1%iele_start_fld,            &
     &    insulate1%iele_start_fld, fluid1%iele_end_fld,                &
     &    conduct1%iele_end_fld,    insulate1%iele_end_fld,             &
     &          new2oldele_layer(1), old2newele_layer(1) )
!
      if (iflag_debug .gt. iflag_minimum_msg) then
       write(*,*) 'iele_fl_start, iele_fl_end',                         &
     &           fluid1%iele_start_fld, fluid1%iele_end_fld
       write(*,*) 'iele_cd_start, iele_cd_end',                         &
     &           conduct1%iele_start_fld, conduct1%iele_end_fld
       write(*,*) 'iele_ins_start, iele_ins_end',                       &
     &           insulate1%iele_start_fld, insulate1%iele_end_fld
      end if
!
!   ordereing of connectivity, element group, and surface group
!
      call allocate_element_connect_org(ele1%numele, ele1%nnod_4_ele)
      call reordering_element_info(ele_grp, sf_grp)
!
!   ordereing of element parameters for SGS model
!
      if ( iflag_SGS_model.eq.id_SGS_NL_grad) then
        call reordering_ele_size(ele1%numele)
      end if
!
      end subroutine s_reordering_by_layers
!
! -----------------------------------------------------------------------
!
      end module reordering_by_layers
