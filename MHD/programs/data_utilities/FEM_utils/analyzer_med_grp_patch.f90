!analyzer_med_grp_patch.f90
!      module analyzer_med_grp_patch
!
!      subroutine initialize_med_grp_patch
!      subroutine analyze_med_grp_patch
!
!..................................................
!
      module analyzer_med_grp_patch
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
!      modified by H. Matsui on Nov., 2006 
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_med_grp_patch
!
      use input_control_udt_diff
      use const_mesh_info
!
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_grp_patch'
      call s_input_control_grp_patch
      if (iflag_debug.eq.1) write(*,*) 's_input_mesh_udt_diff'
      call s_input_mesh_udt_diff
!
!     --------------------- 
!
!      if (iflag_debug.eq.1) write(*,*) 'const_layers_4_dynamic'
!      call const_layers_4_dynamic(ele_grp1, layer_tbl1)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
      end subroutine initialize_med_grp_patch
!
! ----------------------------------------------------------------------
!
      subroutine analyze_med_grp_patch
!
      use m_group_data
      use m_ctl_params_4_diff_udt
      use set_med_patch_4_element_grp
!
!
      call set_med_patch_ele_grp(grouping_mesh_head, ele_grp1)
!
      end subroutine analyze_med_grp_patch
!
! ----------------------------------------------------------------------
!
      end module analyzer_med_grp_patch

