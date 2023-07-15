!>@file   c_link_VIZ_repartition_ctl.f90
!!@brief  module c_link_VIZ_repartition_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_viz_repart_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_viz_repart_ctl_block_name')
!!      type(c_ptr) function c_viz_repart_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_viz_repart_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!      type(c_ptr) function c_viz_repart_viz_plt_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_viz_repart_viz_plt_ctl')
!!      type(c_ptr) function c_viz_repart_Fmesh_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_viz_repart_Fmesh_ctl')
!!      type(c_ptr) function c_viz_repart_new_part_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_viz_repart_new_part_ctl')
!!      type(c_ptr) function c_viz_repart_Fsleeve_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_viz_repart_Fsleeve_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_FEM_sleeve_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_FEM_sleeve_ctl_block_name')
!!      type(c_ptr) function c_FEM_sleeve_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_FEM_sleeve_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_FEM_sleeve_extension_mode_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_FEM_sleeve_extension_mode_ctl')
!!      type(c_ptr) function c_FEM_sleeve_sleeve_level_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_FEM_sleeve_sleeve_level_ctl')
!!      type(c_ptr) function c_FEM_sleeve_sleeve_size_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_FEM_sleeve_sleeve_size_ctl')
!!      type(c_ptr) function c_FEM_sleeve_ref_vector_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_FEM_sleeve_ref_vector_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_repartition_ctl
!
      use iso_c_binding
      use t_ctl_data_volume_repart
      use t_ctl_data_FEM_sleeve_size
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_viz_repart_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_viz_repart_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(viz_repartition_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_viz_repart_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_viz_repart_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_viz_repart_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_viz_repart_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(viz_repartition_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_viz_repart_ctl_iflag = C_loc(f_ctl%i_viz_repartition_ctl)
      end function c_viz_repart_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_viz_repart_viz_plt_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_viz_repart_viz_plt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(viz_repartition_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_viz_repart_viz_plt_ctl = C_loc(f_ctl%viz_plt)
      end function c_viz_repart_viz_plt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_viz_repart_Fmesh_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_viz_repart_Fmesh_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(viz_repartition_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_viz_repart_Fmesh_ctl = C_loc(f_ctl%Fmesh_ctl)
      end function c_viz_repart_Fmesh_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_viz_repart_new_part_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_viz_repart_new_part_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(viz_repartition_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_viz_repart_new_part_ctl = C_loc(f_ctl%new_part_ctl)
      end function c_viz_repart_new_part_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_viz_repart_Fsleeve_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_viz_repart_Fsleeve_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(viz_repartition_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_viz_repart_Fsleeve_ctl = C_loc(f_ctl%Fsleeve_ctl)
      end function c_viz_repart_Fsleeve_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_sleeve_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_FEM_sleeve_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_sleeve_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_sleeve_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_FEM_sleeve_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_sleeve_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_FEM_sleeve_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_sleeve_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_sleeve_ctl_iflag = C_loc(f_ctl%i_FEM_sleeve_ctl)
      end function c_FEM_sleeve_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_sleeve_extension_mode_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_FEM_sleeve_extension_mode_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_sleeve_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_sleeve_extension_mode_ctl                                   &
     &            = C_loc(f_ctl%sleeve_extension_mode_ctl)
      end function c_FEM_sleeve_extension_mode_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_sleeve_sleeve_level_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_FEM_sleeve_sleeve_level_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_sleeve_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_sleeve_sleeve_level_ctl = C_loc(f_ctl%sleeve_level_ctl)
      end function c_FEM_sleeve_sleeve_level_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_sleeve_sleeve_size_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_FEM_sleeve_sleeve_size_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_sleeve_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_sleeve_sleeve_size_ctl = C_loc(f_ctl%sleeve_size_ctl)
      end function c_FEM_sleeve_sleeve_size_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_FEM_sleeve_ref_vector_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_FEM_sleeve_ref_vector_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(FEM_sleeve_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_FEM_sleeve_ref_vector_ctl = C_loc(f_ctl%ref_vector_ctl)
      end function c_FEM_sleeve_ref_vector_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_repartition_ctl
