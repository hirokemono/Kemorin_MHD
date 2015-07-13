!reordering_by_layers.f90
!     module reordering_by_layers
!
!      Written by H.Matsui
!      Modified by H. Matsui on Sep., 2007
!      Modified by H. Matsui on Feb., 2008
!
!      subroutine s_reordering_by_layers_snap
!      subroutine s_reordering_by_layers
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
      use m_geometry_parameter
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
!
      call allocate_lists_4_layer(numele)
      call s_reordering_by_layers
      call deallocate_lists_4_layer
!
      end subroutine s_reordering_by_layers_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers
!
      use m_machine_parameter
      use m_control_parameter
      use m_iccg_parameter
      use calypso_mpi
      use m_geometry_data
      use m_geometry_data_MHD
      use m_element_group
!
      use const_layering_table
      use reordering_element_MHD
      use reordering_element_size
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
     &   (numele, ele_grp1%num_grp, ele_grp1%num_item,                  &
     &    ele_grp1%istack_grp, ele_grp1%grp_name, ele_grp1%item_grp,    &
     &    mat_flag_mhd(1) )
!
!  set list vector for ordering
!
      call const_table_by_layers(numele, mat_flag_mhd(1),               &
     &          iele_fl_start, iele_cd_start, iele_ins_start,           &
     &          iele_fl_end,   iele_cd_end,   iele_ins_end,             &
     &          new2oldele_layer(1), old2newele_layer(1) )
!
      if (iflag_debug .gt. iflag_minimum_msg) then
       write(*,*) 'iele_fl_start, iele_fl_end',                         &
     &           iele_fl_start, iele_fl_end
       write(*,*) 'iele_cd_start, iele_cd_end',                         &
     &           iele_cd_start, iele_cd_end
       write(*,*) 'iele_ins_start, iele_ins_end',                       &
     &           iele_ins_start, iele_ins_end
      end if
!
!   ordereing of connectivity, element group, and surface group
!
      call allocate_element_connect_org
      call reordering_element_info
!
!   ordereing of element parameters for SGS model
!
      if ( iflag_SGS_model.eq.id_SGS_NL_grad) then
        call reordering_ele_size
      end if
!
      end subroutine s_reordering_by_layers
!
! -----------------------------------------------------------------------
!
      end module reordering_by_layers
