!
!       module set_surface_group_types
!
!      Written by H. Matsui on Sep, 2005
!
!      subroutine set_surf_group_types_scalar(bc_type_ctl, ibc_type)
!      subroutine set_surf_group_types_vector(bc_type_ctl, ibc_type)
!      subroutine set_stress_free_group_types(bc_type_ctl, ibc_type)
!      subroutine set_surf_wall_group_types(bc_type_ctl, ibc_type)
!      subroutine set_surf_infty_group_types(bc_type_ctl, ibc_type)
!
      module set_surface_group_types
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_group_types_scalar(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
         if ( bc_type_ctl .eq. 'fixed_ctl' ) then
          ibc_type = 10
         else if ( bc_type_ctl .eq. 'fixed_dat' ) then
          ibc_type = 20
         else if ( bc_type_ctl .eq. 'sgs_correct' ) then
          ibc_type = 15
!
         else if ( bc_type_ctl .eq. 'grad_ctl' ) then
          ibc_type = 0
         else if ( bc_type_ctl .eq. 'grad_dat' ) then
          ibc_type = 1
!
         else if ( bc_type_ctl .eq. 'lead_grad' ) then
          ibc_type = 100
         end if
!
      end subroutine set_surf_group_types_scalar
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_group_types_vector(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
         if ( bc_type_ctl .eq. 'fix_ctl_x' ) then
          ibc_type = 11
         else if ( bc_type_ctl .eq. 'fix_ctl_y' ) then
          ibc_type = 12
         else if ( bc_type_ctl .eq. 'fix_ctl_z' ) then
          ibc_type = 13
         else if ( bc_type_ctl .eq. 'fix_dat_x' ) then
          ibc_type = 21
         else if ( bc_type_ctl .eq. 'fix_dat_y' ) then
          ibc_type = 22
         else if ( bc_type_ctl .eq. 'fix_dat_z' ) then
          ibc_type = 23
         else if ( bc_type_ctl .eq. 'sgs_x' ) then
          ibc_type = 15
         else if ( bc_type_ctl .eq. 'sgs_y' ) then
          ibc_type = 16
         else if ( bc_type_ctl .eq. 'sgs_z' ) then
          ibc_type = 17
!
         else if ( bc_type_ctl .eq. 'fix_norm_ctl' ) then
          ibc_type = 10
         else if ( bc_type_ctl .eq. 'fix_norm_dat' ) then
          ibc_type = -10
!
         else if ( bc_type_ctl .eq. 'grad_ctl_x' ) then
          ibc_type = 1
         else if ( bc_type_ctl .eq. 'grad_ctl_y' ) then
          ibc_type = 2
         else if ( bc_type_ctl .eq. 'grad_ctl_z' ) then
          ibc_type = 3
         else if ( bc_type_ctl .eq. 'grad_dat_x' ) then
          ibc_type = -1
         else if ( bc_type_ctl .eq. 'grad_dat_y' ) then
          ibc_type = -2
         else if ( bc_type_ctl .eq. 'grad_dat_z' ) then
          ibc_type = -3
!
         else if ( bc_type_ctl .eq. 'lead_grad_x' ) then
          ibc_type = 101
         else if ( bc_type_ctl .eq. 'lead_grad_y' ) then
          ibc_type = 102
         else if ( bc_type_ctl .eq. 'lead_grad_z' ) then
          ibc_type = 103
         end if
!
      end subroutine set_surf_group_types_vector
!
!-----------------------------------------------------------------------
!
      subroutine set_stress_free_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
         if ( bc_type_ctl .eq. 'free_sph_in' ) then
          ibc_type = 101
         else if ( bc_type_ctl .eq. 'free_sph_out' ) then
          ibc_type = 102
         end if
!
      end subroutine set_stress_free_group_types
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_wall_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
!
         if ( bc_type_ctl .eq. 'wall' ) then
          ibc_type = 70
         else if ( bc_type_ctl .eq. 'sph_in' ) then
          ibc_type = 71
         else if ( bc_type_ctl .eq. 'sph_out' ) then
          ibc_type = 72
         end if
!
      end subroutine set_surf_wall_group_types
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_infty_group_types(bc_type_ctl, ibc_type)
!
      character (len=kchara), intent(in) :: bc_type_ctl
      integer(kind = kint), intent(inout) :: ibc_type
!
      if ( bc_type_ctl .eq. 'infinity' ) then
       ibc_type = 0
      end if
!
      end subroutine set_surf_infty_group_types
!
!-----------------------------------------------------------------------
!
      end module set_surface_group_types
